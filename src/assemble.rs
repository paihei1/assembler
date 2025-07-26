
mod base;
mod other;
mod fma;
mod sse;
mod avx;
mod avx2;
pub mod linux;


pub mod builder {
    use std::alloc::*;
    use std::mem::*;

    #[derive(Debug)]
    pub struct Page {
        cap: usize,
        pub len: usize,
        next: Option<*mut Page>,
    }
    impl Page {
        pub fn body(&self) -> &[u8] {
            unsafe {std::slice::from_raw_parts(transmute::<&Self, *const u8>(self).add(size_of::<Self>()), self.cap)}
        }
        pub fn body_mut(&mut self) -> &mut [u8] {
            unsafe {std::slice::from_raw_parts_mut(transmute::<&mut Self, *mut u8>(self).add(size_of::<Self>()), self.cap)}
        }
        unsafe fn new(cap: usize) -> *mut Self { unsafe {
            let result: *mut Self = transmute(alloc(Layout::from_size_align(size_of::<Self>() + cap, align_of::<Self>()).unwrap()));
            (*result).cap = cap;
            (*result).len = 0;
            (*result).next = None;
            result
        }}
        unsafe fn drop(this: *mut Self) { unsafe {
            dealloc(transmute(this), Layout::from_size_align(size_of::<Self>() + (*this).cap, align_of::<Self>()).unwrap())
        }}
        unsafe fn append(&mut self, bytes: &[u8]) -> std::ops::Range<usize> { unsafe {
            let len = self.len;
            let target: *mut [u8]= transmute(&mut self.body_mut()[len..][..bytes.len()]);
            bytes.iter().zip((*target).iter_mut()).for_each(|(b,w)| *w = *b);
            self.len += bytes.len();
            len..self.len
        }}
    }
    struct Iter<'a> {
        page: Option<&'a Page>
    }
    impl<'a> Iterator for Iter<'a> {
        type Item = &'a [u8];
        fn next(&mut self) -> Option<&'a [u8]> {
            if let Some(page) = self.page {
                let result = Some(&page.body()[..page.len]);
                self.page = page.next.map(|p| unsafe{&(*p)});
                result
            } else {None}
        }
    }
    #[derive(Clone, Debug)]
    pub struct Slice {
        builder_first_page: *mut Page,
        pub page: *mut Page,
        pub range: std::ops::Range<usize>
    }
    impl Slice {
        pub fn subslice(&self, range: std::ops::Range<usize>) -> Self {
            assert!(range.end <= self.range.len());
            Self {
                builder_first_page: self.builder_first_page,
                page: self.page,
                range: self.range.start + range.start .. self.range.start + range.end, 
            }
        }
        pub fn on_same_page(&self, range: std::ops::Range<usize>) -> Self {
            unsafe {assert!(range.end <= (*self.page).len)};
            Self {
                builder_first_page: self.builder_first_page,
                page: self.page,
                range: range,
            }
        }
        pub fn len(&self) -> usize {self.range.len()}
    }
    #[derive(Debug)]
    pub struct Builder {
        pub len: usize,
        first_page: Option<*mut Page>,
        last_page: Option<*mut Page>
    }
    impl Builder {
        const CAP: usize = 1000; // so the pages are 1024 bytes
        pub fn new() -> Self {Self{ len: 0, first_page: None, last_page: None}}
        pub fn pages<'a>(&'a self) -> impl Iterator<Item=&'a [u8]> + 'a {Iter{page: self.first_page.map(|p| unsafe{&(*p)})}}
        pub fn len(&self) -> usize {self.len}
        pub fn append(&mut self, bytes: &[u8]) -> Slice {
            let page: &mut Page = match self.last_page {
                None => unsafe {
                    let page = Page::new(std::cmp::max(Self::CAP, bytes.len()));
                    self.last_page = Some(page);
                    self.first_page = Some(page);
                    transmute(page)
                },
                Some(page) if unsafe {(*page).cap - (*page).len >= bytes.len()} => unsafe{transmute(page)},
                Some(page) => unsafe {
                    let new = Page::new(std::cmp::max(Self::CAP, bytes.len()));
                    (*page).next = Some(new);
                    self.last_page = Some(new);
                    transmute(new)
                }
            };
            self.len += bytes.len();
            Slice {
                builder_first_page: self.first_page.unwrap(),
                page: page,
                range: unsafe{page.append(bytes)}
            }
        }

    }
    impl Drop for Builder {
        fn drop(&mut self) {
            while let Some(page) = self.first_page { unsafe {
                self.first_page = (*page).next;
                Page::drop(page);
            }}
        }
    }
    impl std::ops::Index<Slice> for Builder {
        type Output = [u8];
        fn index<'a>(&'a self, index: Slice) -> &'a Self::Output {
            assert!(self.first_page == Some(index.builder_first_page));
            unsafe{ &((*index.page).body()[index.range])}
        }
    }
    impl std::ops::IndexMut<Slice> for Builder {
        fn index_mut(&mut self, index: Slice) -> &mut Self::Output {
            assert!(self.first_page == Some(index.builder_first_page));
            unsafe{ &mut ((*index.page).body_mut()[index.range])}
        }
    }
}
#[derive(Debug)]
struct KnownLabel {
    label: usize,
    pos: usize,
}
#[derive(Debug)]
struct LabelReference {
    label: usize,
    offset_start: usize,
    pos: builder::Slice,
    pos_idx: usize,
    instruction_start: Option<usize>
}
#[derive(Debug)]
struct LinkedReference {
    offset_start: usize,
    target: usize,
    pos_index: usize,
    pos: builder::Slice
}
#[derive(Debug)]
pub struct LabelContext {
    labels: Vec<Vec<u8>>,
    known_labels: Vec<KnownLabel>,
    references: Vec<LabelReference>,
    linked_referencs: Vec<LinkedReference>,
    local_labels: Vec<usize>,
    global_labels: Vec<usize>,
    test_labels: Vec<usize>
}
impl LabelContext {
    fn index(&mut self, label: &[u8]) -> usize {
        if let Some(index) = self.labels.iter().position(|x| &x[..] == label) {index} else {
            self.labels.push(label.iter().cloned().collect());
            self.labels.len()-1
        }
    }
    pub fn label(&self, index: usize) -> &[u8] {&self.labels[index]}
    fn add_reference(&mut self, builder: &mut builder::Builder, pos: builder::Slice, offset_start: usize, index: usize, pos_idx: usize, instruction_start: Option<usize>) -> Result<(),&'static str> {
        if let Some(known) = self.known_labels.iter().find(|x| x.label == index) {
            self.link_reference(builder, offset_start, &mut known.pos.clone(), pos_idx, pos, instruction_start)
        } else {
            self.references.push(LabelReference { label: index, offset_start: offset_start, pos: pos , pos_idx: pos_idx, instruction_start: instruction_start});
            Ok(())
        }
    }
    fn set_label(&mut self, builder: &mut builder::Builder, mut pos: usize, index: usize) -> Result<(),&'static str> {
        if self.known_labels.iter().find(|known| known.label == index).is_some() {return Err("label is already defined somewhere else.")}
        let mut result = Ok(());
        let mut to_link = self.references.extract_if(.., |r| r.label == index).collect::<Vec<_>>();
        to_link.sort_by_key(|r| r.pos_idx);
        for r in to_link.into_iter().rev() {
            // start with the last, so shortening it doesn't need to move the remaining ones.
            result = result.and(self.link_reference(builder, r.offset_start, &mut pos, r.pos_idx, r.pos, r.instruction_start));
        }
        self.known_labels.push(KnownLabel { label: index, pos: pos });
        result
    }
    fn link_reference(&mut self, builder: &mut builder::Builder, offset_start: usize, target: &mut usize, pos_index: usize, pos: builder::Slice, instruction_start: Option<usize>) -> Result<(),&'static str> {
        let jump = *target as i64 - offset_start as i64;
        if let Some(inst) = instruction_start { if (-131..127).contains(&jump) {
            // -128 -> -131 because instruction is inside the jump and would get shorter
            let page: &mut builder::Page = unsafe{&mut (*pos.page)};
            
            let remove_range = match page.body()[pos.range.start - (pos_index - inst)] {
                0xe9 => {
                    page.body_mut()[pos.range.start - 1] = 0xeb;
                    page.body_mut()[pos.range.start] = if jump >= 0 {jump as u8} else {((jump + 3) as i8).cast_unsigned()};
                    pos.range.start + 1 .. pos.range.end
                },
                0x0f => {
                    page.body_mut()[pos.range.start - 2] = page.body_mut()[pos.range.start - 1] - 16; // 0f 8* -> 7*
                    page.body_mut()[pos.range.start - 1] = if jump >= 0 {jump as u8} else {((jump + 4) as i8).cast_unsigned()};
                    pos.range.clone()
                },
                _ => panic!("can't shorten a not JMP/Jcc instruction")
            };
            // fix known_labels after
            for known in self.known_labels.iter_mut() {
                if known.pos > offset_start {known.pos -= remove_range.len()}
            }
            // fix unlinked references after
            for r in self.references.iter_mut() {if r.pos_idx >= offset_start {
                r.instruction_start = r.instruction_start.map(|x| x-remove_range.len());
                r.offset_start -= remove_range.len();
                r.pos_idx -= remove_range.len();
                if r.pos.page == page {r.pos.range = r.pos.range.start - remove_range.len() .. r.pos.range.end - remove_range.len();}
            }}
            // fix linked references over/after
            for r in self.linked_referencs.iter_mut() {
                if r.target <= inst && r.pos_index >= offset_start {
                    // backwards over
                    r.offset_start -= remove_range.len();
                    r.pos_index -= remove_range.len();
                    match (r.target as i64 - r.offset_start as i64, &mut builder[r.pos.clone()]) {
                        (x, p) if p.len() == 1 && x as i8 as i64 == x => p[0] = x as i8 as u8,
                        (x, p) if p.len() == 2 && x as i16 as i64 == x => for (w,b) in p.into_iter().zip(x.to_le_bytes()) {*w = b},
                        (x, p) if p.len() == 4 && x as i32 as i64 == x => for (w,b) in p.into_iter().zip(x.to_le_bytes()) {*w = b},
                        (x, p) if p.len() == 8 => for (w,b) in p.into_iter().zip(x.to_le_bytes()) {*w = b},
                        _ => return Err("reference shortening failed, rollback not implemented.")
                    }
                    if r.pos.page == page {r.pos.range = r.pos.range.start - remove_range.len() .. r.pos.range.end - remove_range.len();}
                } else if r.offset_start <= inst && r.target >= offset_start {
                    // forwards over
                    r.target -= remove_range.len();
                    match (r.target as i64 - r.offset_start as i64, &mut builder[r.pos.clone()]) {
                        (x, p) if p.len() == 1 && x as i8 as i64 == x => p[0] = x as i8 as u8,
                        (x, p) if p.len() == 2 && x as i16 as i64 == x => for (w,b) in p.into_iter().zip(x.to_le_bytes()) {*w = b},
                        (x, p) if p.len() == 4 && x as i32 as i64 == x => for (w,b) in p.into_iter().zip(x.to_le_bytes()) {*w = b},
                        (x, p) if p.len() == 8 => for (w,b) in p.into_iter().zip(x.to_le_bytes()) {*w = b},
                        _ => return Err("reference shortening failed, rollback not implemented.")
                    }
                } else if r.offset_start >= offset_start && r.target >= offset_start {
                    // fully after
                    r.offset_start -= remove_range.len();
                    r.pos_index -= remove_range.len();
                    r.target -= remove_range.len();
                    if r.pos.page == page {r.pos.range = r.pos.range.start - remove_range.len() .. r.pos.range.end - remove_range.len();}
                }
            }
            // fix target
            if jump >= 0 {*target -= remove_range.len();}
            // actually shorten page
            let page_len = page.len;
            page.body_mut().copy_within(remove_range.end..page_len, remove_range.start);
            page.len -= remove_range.len();
            builder.len -= remove_range.len();

            // add the new linked reference
            self.linked_referencs.push(LinkedReference {
                offset_start: offset_start - remove_range.len(),
                target: *target,
                pos_index: inst + 1,
                pos: pos.on_same_page(remove_range.start - 1 .. remove_range.start)
            });
            return Ok(());
        }}
        match (jump, &mut builder[pos.clone()]) {
            (x, p) if p.len() == 1 && x as i8 as i64 == x => p[0] = x as i8 as u8,
            (x, p) if p.len() == 2 && x as i16 as i64 == x => for (w,b) in p.into_iter().zip(x.to_le_bytes()) {*w = b},
            (x, p) if p.len() == 4 && x as i32 as i64 == x => for (w,b) in p.into_iter().zip(x.to_le_bytes()) {*w = b},
            (x, p) if p.len() == 8 => for (w,b) in p.into_iter().zip(x.to_le_bytes()) {*w = b},
            _ => return Err("reference shortening failed, rollback not implemented.")
        }
        self.linked_referencs.push(LinkedReference { offset_start: offset_start, target: *target, pos_index: pos_index, pos: pos });
        Ok(())
    }
    pub fn any_unknown(&self) -> bool {self.references.len() != 0}
    pub fn check_unlinked_global(&self) -> Result<(), String> {
        let bad = self.references.iter().map(|r| r.label).filter(|l| self.global_labels.iter().find(|x| **x == *l).is_none()).map(|l| str::from_utf8(self.label(l)).unwrap_or("<invalid utf8>")).collect::<Vec<_>>();
        let len = bad.len();
        if len == 0 {Ok(())} else {
            let mut err = "some unlinked symbols are not declared global: ".into();
            for (n,symbol) in bad.into_iter().enumerate() {err = err + symbol + if n == len-1 {"."} else if n==len-2 {" and "} else {", "}};
            Err(err)
        }
    }
    pub fn tests(&self) -> impl Iterator<Item=(usize, &[u8])> {
        self.test_labels.iter().cloned().map(|idx| (self.known_labels.iter().find(|k| k.label == idx).unwrap().pos, self.label(idx)))
    }
}

use crate::parse::{alpha, Literal};

pub fn try_assemble(source: impl std::io::BufRead, mut tester: Option<&mut crate::test::TesterRustSide>) -> Result<(builder::Builder, LabelContext), String>{
    let mut builder = builder::Builder::new();
    let mut labels = LabelContext {
        labels: Vec::new(), 
        known_labels: Vec::new(), 
        references: Vec::new(), 
        linked_referencs: Vec::new(), 
        local_labels: Vec::new(), 
        global_labels: Vec::new(), 
        test_labels: Vec::new()
    };
    if tester.is_some() {
        builder.append(&crate::test::Tester::TEST_HEADER);
        labels.labels.push("__ZERO".to_owned().into());
        labels.known_labels.push(KnownLabel { label: 0, pos: 0 });
        labels.labels.push("__INIT_TEST".to_owned().into());
        labels.known_labels.push(KnownLabel { label: 1, pos: crate::test::Tester::INIT_TEST });
        labels.labels.push("__CLEANUP_TEST".to_owned().into());
        labels.known_labels.push(KnownLabel { label: 2, pos: crate::test::Tester::CLEANUP_TEST });
    }

    let mut string_error: String = "".to_string();

    let mut in_test_block = false;

    match crate::parse::iterate_lines(source, |line, linenr| {
        let mut tokens = crate::parse::Tokenizer::new(line);

        loop {match tokens.next() {
            Some((b"#", _)) | None => return Ok(()),
            Some((b"_TEST" | b"_test", _)) if !in_test_block => {
                let Some((identifier, _)) = tokens.next() else {return Err("'STARTTEST' must be followed by a test name.")};
                // print!("found test.\n");
                if tester.is_some() {
                    let pos = builder.len();
                    let index = labels.index(identifier);
                    labels.set_label(&mut builder, pos, index).expect("test name as label should not be referenced anywhere");
                    labels.test_labels.push(index);
                    {   // mov rcx QWORD PTR [__ZERO]
                        let len = builder.len();
                        let pos = builder.append(&[0x48, 0x8b, 0x0d,0,0,0,0]).subslice(3..7);
                        labels.add_reference(&mut builder, pos, len+7, 0, len+3, None).unwrap();
                    }
                    builder.append(&[0x48, 0x89, 0xa1, 0xa0, 0x02, 0x00,0x00]);
                }
                in_test_block = true;
            }
            Some((b"END" | b"end", _)) if in_test_block => {
                // print!("found end of test block.\n");
                in_test_block = false;
                if tester.is_some() {
                    {   // mov rcx QWORD PTR [__ZERO]
                        let len = builder.len();
                        let pos = builder.append(&[0x48, 0x8b, 0x0d,0,0,0,0]).subslice(3..7);
                        labels.add_reference(&mut builder, pos, len+7, 0, len+3, None).unwrap();
                    }
                    builder.append(&[0x48, 0x8b, 0xa1, 0xa0, 0x02, 0x00,0x00]);
                    builder.append(&[0xc3]);
                }
            },
            Some(_) if in_test_block && tester.is_none() => {},
            Some((b"SET" | b"set", _)) if tester.is_some() => {
                let mut operand_value_s = Vec::new();
                loop {match crate::test::OperandValue::parse(tokens.rest(), false) {
                    Ok(Some((operand_value,skip))) => {operand_value_s.push(operand_value); (&mut tokens).skip(skip);},
                    Ok(None) => break,
                    Err(e) => {string_error = e; return Err("")},
                }}
                {   // call INIT_TEST
                    let len = builder.len();
                    let pos = builder.append(&[0xe8,0,0,0,0]).subslice(1..5);
                    labels.add_reference(&mut builder, pos, len+5, 1, len+1, Some(len)).unwrap();
                }
                {   // mov QWORD PTR [rcx + 16] <test function index>
                    builder.append(&[0x48, 0xc7, 0x41, 0x10]);
                    builder.append(&(tester.as_deref().unwrap().test_functions.len() as u32).to_le_bytes());
                }
                builder.append(&[0x48, 0x83, 0xe4, 0xf0]); // and rsp -16     # (stack alignment)
                builder.append(&[0xff, 0x51, 0x18]); // call QWORD PTR [rcx + 24]
                {   // call CLEANUP_TEST
                    let len = builder.len();
                    let pos = builder.append(&[0xe8,0,0,0,0]).subslice(1..5);
                    labels.add_reference(&mut builder, pos, len+5, 2, len+1, Some(len)).unwrap();
                }
                builder.append(&[0x48, 0x8b, 0x61, 0x40]); // mov rsp QWORD PTR [rcx+64]
                builder.append(&[0x48, 0x8b, 0x49, 0x28]); // mov rcx QWORD PTR [rcx+40]

                tester.as_mut().unwrap().test_functions.push((crate::test::TestFn::Set(operand_value_s), linenr));
            },
            Some((b"DBG" | b"dbg", _)) if tester.is_some() => {
                let mut operand_value_s = Vec::new();
                loop {match crate::test::OperandValue::parse(tokens.rest(), true) {
                    Ok(Some((operand_value,skip))) => {operand_value_s.push(operand_value); (&mut tokens).skip(skip);},
                    Ok(None) => break,
                    Err(e) => {string_error = e; return Err("")},
                }}
                {   // call INIT_TEST
                    let len = builder.len();
                    let pos = builder.append(&[0xe8,0,0,0,0]).subslice(1..5);
                    labels.add_reference(&mut builder, pos, len+5, 1, len+1, Some(len)).unwrap();
                }
                {   // mov QWORD PTR [rcx + 16] <test function index>
                    builder.append(&[0x48, 0xc7, 0x41, 0x10]);
                    builder.append(&(tester.as_deref().unwrap().test_functions.len() as u32).to_le_bytes());
                }
                builder.append(&[0x48, 0x83, 0xe4, 0xf0]); // and rsp -16     # (stack alignment)
                builder.append(&[0xff, 0x51, 0x18]); // call QWORD PTR [rcx + 24]
                {   // call CLEANUP_TEST
                    let len = builder.len();
                    let pos = builder.append(&[0xe8,0,0,0,0]).subslice(1..5);
                    labels.add_reference(&mut builder, pos, len+5, 2, len+1, Some(len)).unwrap();
                }
                builder.append(&[0x48, 0x8b, 0x61, 0x40]); // mov rsp QWORD PTR [rcx+64]
                builder.append(&[0x48, 0x8b, 0x49, 0x28]); // mov rcx QWORD PTR [rcx+40]

                tester.as_mut().unwrap().test_functions.push((crate::test::TestFn::Dbg(operand_value_s), linenr));
            },
            Some((b"ASSERT" | b"assert", _)) if tester.is_some() => {
                let mut operand_value_s = Vec::new();
                loop {match crate::test::OperandValue::parse(tokens.rest(), true) {
                    Ok(Some((operand_value,skip))) => {operand_value_s.push(operand_value); (&mut tokens).skip(skip);},
                    Ok(None) => break,
                    Err(e) => {string_error = e; return Err("")},
                }}
                {   // call INIT_TEST
                    let len = builder.len();
                    let pos = builder.append(&[0xe8,0,0,0,0]).subslice(1..5);
                    labels.add_reference(&mut builder, pos, len+5, 1, len+1, Some(len)).unwrap();
                }
                {   // mov QWORD PTR [rcx + 16] <test function index>
                    builder.append(&[0x48, 0xc7, 0x41, 0x10]);
                    builder.append(&(tester.as_deref().unwrap().test_functions.len() as u32).to_le_bytes());
                }
                builder.append(&[0x48, 0x83, 0xe4, 0xf0]); // and rsp -16     # (stack alignment)
                builder.append(&[0xff, 0x51, 0x18]); // call QWORD PTR [rcx + 24]
                {   // call CLEANUP_TEST
                    let len = builder.len();
                    let pos = builder.append(&[0xe8,0,0,0,0]).subslice(1..5);
                    labels.add_reference(&mut builder, pos, len+5, 2, len+1, Some(len)).unwrap();
                }
                builder.append(&[0x48, 0x8b, 0x61, 0x40]); // mov rsp QWORD PTR [rcx+64]
                builder.append(&[0x48, 0x8b, 0x49, 0x28]); // mov rcx QWORD PTR [rcx+40]

                tester.as_mut().unwrap().test_functions.push((crate::test::TestFn::Assert(operand_value_s), linenr));
            },
            Some((b"PRINT" | b"print", mut j)) if tester.is_some() => {
                let mut msg = Vec::new();
                while let Some((i,Some((n, v)))) = tokens.peek().map(|(t,n)| n-t.len()).map(|i| (i,Literal::parse(&line[i..]))) {
                    (&mut tokens).skip(i-j+n);
                    j = i+n;
                    match v.into_type(Literal::STR_M, false) {
                        Ok(Literal::Str(s)) => msg.extend_from_slice(&s),
                        Err(err) => {string_error = err.clone(); return Err("")},
                        _ => panic!("unreachable")
                    }
                }
                {   // call INIT_TEST
                    let len = builder.len();
                    let pos = builder.append(&[0xe8,0,0,0,0]).subslice(1..5);
                    labels.add_reference(&mut builder, pos, len+5, 1, len+1, Some(len)).unwrap();
                }
                {   // mov QWORD PTR [rcx + 16] <test function index>
                    builder.append(&[0x48, 0xc7, 0x41, 0x10]);
                    builder.append(&(tester.as_deref().unwrap().test_functions.len() as u32).to_le_bytes());
                }
                builder.append(&[0x48, 0x83, 0xe4, 0xf0]); // and rsp -16     # (stack alignment)
                builder.append(&[0xff, 0x51, 0x18]); // call QWORD PTR [rcx + 24]
                {   // call CLEANUP_TEST
                    let len = builder.len();
                    let pos = builder.append(&[0xe8,0,0,0,0]).subslice(1..5);
                    labels.add_reference(&mut builder, pos, len+5, 2, len+1, Some(len)).unwrap();
                }
                builder.append(&[0x48, 0x8b, 0x61, 0x40]); // mov rsp QWORD PTR [rcx+64]
                builder.append(&[0x48, 0x8b, 0x49, 0x28]); // mov rcx QWORD PTR [rcx+40]

                tester.as_mut().unwrap().test_functions.push((crate::test::TestFn::Print(String::from_utf8_lossy(&msg).into_owned()), linenr));
            },
            Some((b"PTR"|b"ptr", _)) => {
                let t2 = tokens.next();
                match tokens.next() {
                    Some((t3, _)) if alpha(t3[0]) => {
                        let offset_start = builder.len();
                        let index = labels.index(t3);
                        let pos_idx = builder.len();
                        let pos = match t2 {
                            Some((b"BYTE"|b"byte",_)) => builder.append(&[0]),
                            Some((b"WORD"|b"word",_)) => builder.append(&[0,0]),
                            Some((b"DWORD"|b"dword",_)) => builder.append(&[0,0,0,0]),
                            Some((b"QWORD"|b"qword",_)) => builder.append(&[0,0,0,0,0,0,0,0]),
                            _ => return Err("'PTR' at start of expression can only be 'PTR BYTE/WORD/DWORD/QWORD <label>', inserting the relative offset of that label with the given data type.")
                        };
                        labels.add_reference(&mut builder, pos, offset_start, index, pos_idx, None)?;
                    },
                    _ => return Err("'PTR' at start of expression can only be 'PTR BYTE/WORD/DWORD/QWORD <label>', inserting the relative offset of that label with the given data type.")
                }
            },
            // deprecated, use typed literals instead, TODO write those
            Some((b"BYTE"|b"byte",_)) => match tokens.next() {
                Some((t2,i)) => {
                    let Some((n,l)) = Literal::parse(&line[i..]) else {return Err("'BYTE' must be followed by byte literal.")};
                    (&mut tokens).skip(n-t2.len());
                    match l.into_type(Literal::U8_M | Literal::I8_M, false) {
                        Ok(Literal::U8(x)) => {builder.append(&[x]);},
                        Ok(Literal::I8(x)) => {builder.append(&[x as u8]);},
                        Err(err) => {string_error = err; return Err("")}
                        _ => panic!("unreachable")
                    }
                },
                _ => return Err("'BYTE' must be followed by byte literal.")
            },
            // deprecated, use typed literals instead
            Some((b"WORD"|b"word",_)) => match tokens.next() {
                Some((t2,i)) => {
                    let Some((n,l)) = Literal::parse(&line[i..]) else {return Err("'WORD' must be followed by word literal.")};
                    (&mut tokens).skip(n-t2.len());
                    match l.into_type(Literal::U16_M | Literal::I16_M, false) {
                        Ok(Literal::U16(x)) => {builder.append(&x.to_le_bytes());},
                        Ok(Literal::I16(x)) => {builder.append(&x.to_le_bytes());},
                        Err(err) => {string_error = err; return Err("")}
                        _ => panic!("unreachable")
                    }
                },
                _ => return Err("'WORD' must be followed by word literal.")
            },
            // deprecated, use typed literals instead
            Some((b"DWORD"|b"dword",_)) => match tokens.next() {
                Some((t2,i)) => {
                    let Some((n,l)) = Literal::parse(&line[i..]) else {return Err("'DWORD' must be followed by dword literal.")};
                    (&mut tokens).skip(n-t2.len());
                    match l.into_type(Literal::U32_M | Literal::I32_M | Literal::F32_M, false) {
                        Ok(Literal::U32(x)) => {builder.append(&x.to_le_bytes());},
                        Ok(Literal::I32(x)) => {builder.append(&x.to_le_bytes());},
                        Ok(Literal::F32(x)) => {builder.append(&x.to_le_bytes());},
                        Err(err) => {string_error = err; return Err("")}
                        _ => panic!("unreachable")
                    }
                },
                _ => return Err("'DWORD' must be followed by integer or float literal.")
            },
            // deprecated, use typed literals instead
            Some((b"QWORD"|b"qword",_)) => match tokens.next() {
                Some((t2,i)) => {
                    let Some((n,l)) = Literal::parse(&line[i..]) else {return Err("'DWORD' must be followed by dword literal.")};
                    (&mut tokens).skip(n-t2.len());
                    match l.into_type(Literal::U64_M | Literal::I64_M | Literal::F64_M, false) {
                        Ok(Literal::U64(x)) => {builder.append(&x.to_le_bytes());},
                        Ok(Literal::I64(x)) => {builder.append(&x.to_le_bytes());},
                        Ok(Literal::F64(x)) => {builder.append(&x.to_le_bytes());},
                        Err(err) => {string_error = err; return Err("")}
                        _ => panic!("unreachable")
                    }
                },
                _ => return Err("'QWORD' must be followed by integer or float literal.")
            },
            Some((b"ALIGN"|b"align",_)) => {
                let Some((t2, i)) = tokens.next() else {return Err("'ALIGN' must be followed by positive integer literal")};
                let Some((n, l)) = Literal::parse(&line[i..]) else {return Err("'ALIGN' must be followed by positive integer literal")};
                (&mut tokens).skip(n-t2.len());
                let x = match l {
                    Literal::U8(x) => x as u64,
                    Literal::U16(x) => x as u64,
                    Literal::U32(x) => x as u64,
                    Literal::U64(x) | Literal::Uany(x) => x,
                    _ => return Err("'ALIGN' must be followed by positive integer literal")
                };
                let mut gap_size = builder.len() % (x as usize);
                const NOPS: [u8; 37] = [0x66, 0x90,
                                        0x0f, 0x1f, 0x00,
                                        0x0f, 0x1f, 0x40, 0x00,
                                        0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00,
                                        0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00,
                                        0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00];
                loop {match gap_size {
                    0 => break,
                    1 => {builder.append(&NOPS[1..2]); break},
                    2 => {builder.append(&NOPS[0..2]); break},
                    3 => {builder.append(&NOPS[2..5]); break},
                    4 => {builder.append(&NOPS[5..9]); break},
                    5 => {builder.append(&NOPS[10..15]); break},
                    6 => {builder.append(&NOPS[9..15]); break},
                    7 => {builder.append(&NOPS[15..22]); break},
                    8 => {builder.append(&NOPS[29..]); break},
                    9 => {builder.append(&NOPS[28..]); break},
                    10 => {builder.append(&NOPS[27..]); break},
                    11 => {builder.append(&NOPS[26..]); break},
                    12 => {builder.append(&NOPS[25..]); break},
                    13 => {builder.append(&NOPS[24..]); break},
                    14 => {builder.append(&NOPS[23..]); break},
                    15 => {builder.append(&NOPS[22..]); break},
                    _ => {builder.append(&NOPS[22..]); gap_size -= 15},
                }}
            },
            Some((b"LOCK"|b"lock",_)) => {builder.append(&[0xf0]);},
            Some((b"LOCAL"|b"local",_)) => match tokens.next() {
                Some((t,_)) if alpha(t[0]) => {
                    let label = labels.index(t);
                    labels.local_labels.push(label);
                    if let Some((b":", _)) = tokens.peek() {
                        tokens.next();
                        let pos = builder.len();
                        labels.set_label(&mut builder, pos, label)?;
                    }
                },
                _ => return Err("'LOCAL' must be followed by a label.")
            },
            Some((b"GLOBAL"|b"global",_)) => match tokens.next() {
                Some((t,_)) if alpha(t[0]) => {
                    let label = labels.index(t);
                    labels.global_labels.push(label);
                    if let Some((b":", _)) = tokens.peek() {
                        tokens.next();
                        let pos = builder.len();
                        labels.set_label(&mut builder, pos, label)?;
                    }
                },
                _ => return Err("'GLOBAL' must be followed by a label.")
            },
            Some((t, i)) => {
                if t == b"\r" {continue}
                if t[0] == b'-' || t[0] >= b'0' && t[0] <= b'9' || t[0] == b'"' {
                    let Some((n, l)) = Literal::parse(&line[i-t.len()..]) else {return Err("not a valid literal.")};
                    match l.into_type(Literal::STR_M, false) {
                        Err(err) => {string_error = err; return Err("")},
                        Ok(Literal::Str(x)) => builder.append(&x),
                        _ => panic!("unreachable")
                    };
                    (&mut tokens).skip(n-t.len());
                    continue;
                }
                if !alpha(t[0]) {return Err("unexpected token.")}
                // instruction mnemonic or identifier
                let mut matched= false;
                for encode in [base::encode, other::encode, fma::encode, sse::encode, avx::encode, avx2::encode] {match encode(t, tokens.rest(), &mut labels) {
                    Err(err) => {string_error = err; return Err("")},
                    Ok(None) => {},
                    Ok(Some((skip, instruction))) => {
                        (&mut tokens).skip(skip);
                        let slice = builder.append(&instruction[1..(instruction[0] & 15) as usize + 1]);
                        matched = true;
                        match instruction[0] >> 4 {0 => {}, n => {
                            let new_len = builder.len();
                            let subslice = slice.subslice(n as usize..n as usize + 4);
                            let mut index: u32 = 0;
                            for (n,b) in builder[subslice.clone()].iter().cloned().enumerate() {index |= (b as u32) << 8*n;}
                            let pos_idx = new_len - (instruction[0] & 0xf - n) as usize;
                            let is_jmp_jcc = instruction[0] == 0x15 && instruction[1] == 0xe9 
                                || instruction[0] == 0x26 && instruction[1] == 0x0f && instruction[2] & 0xf0 == 0x80;
                            labels.add_reference(&mut builder, subslice, new_len, index as usize, pos_idx, is_jmp_jcc.then_some(new_len - (instruction[0] & 0xf) as usize))?;
                        }}
                    }
                }}

                if !matched {
                    if let Some((b":",_)) = tokens.next() {} else {return Err("expected ':' after label.")}
                    let label = labels.index(t);
                    let pos = builder.len();
                    labels.set_label(&mut builder, pos, label)?;
                }
            }
        }}
    }) {Ok(()) => {}, Err(err) => {
        return Err(err + &string_error);
    }}
    Ok((builder, labels))
}
use crate::encode::{EncodingArm, Operand, try_encode};
fn encode_with_operands<const N: usize>(template: &[EncodingArm], mut operands: &[u8], labels: &mut LabelContext, immediate_type: u16) -> Result<(usize, [u8;16]), String> {
    // dbg!(str::from_utf8(operands).unwrap());
    let start_len = operands.len();
    let mut ops_uninit: [std::mem::MaybeUninit<Operand>; N] = [const { std::mem::MaybeUninit::uninit() }; N];
    for i in 0..N {match Operand::read(operands, &mut |t| u32::try_from(labels.index(t)).unwrap().cast_signed()) {
        Ok((n,op)) => {operands = &operands[n..]; ops_uninit[i].write(op);},
        Err(err) => return Err(format!("not enough valid operands: {}", err))
    }};
    let ops = unsafe{std::mem::transmute::<_, &[Operand;N]>(&ops_uninit)};
    // dbg!(&ops);
    Ok((start_len - operands.len(), try_encode(ops, template, immediate_type)?))
}
