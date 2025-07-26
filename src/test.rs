pub struct TesterRustSide {
    pub test_functions: Vec<(TestFn, usize)>
}
impl TesterRustSide {
    pub fn new() -> Self {Self {test_functions: Vec::new()}}
}
#[repr(C)]
pub struct Tester {
    pub buf: *mut u8,       // 8:
    pub len: usize,         // 16:
    test_fn_idx: usize, // 24:
    test_fn: extern "win64" fn(*mut Self), // 32:
    pub registers: [u64;16],  // 160:
    pub ymms: [[u8;32]; 16],    // 672:
    early_return_rsp: usize,
    early_return_rbp: usize,
    early_return_rbx: usize,
    early_return_r12: usize,
    early_return_r13: usize,
    early_return_r14: usize,
    early_return_r15: usize,
    ok: bool, msg: String,
    pub tester: TesterRustSide
}
impl Tester {
    pub const INIT_TEST: usize = 8;
    pub const CLEANUP_TEST: usize = 0xe3;
    pub const TEST_HEADER: [u8;0x1af] = [
        0,0,0,0,0,0,0,0, // *mut self
        // INIT_STATE starts at 8
        0x48, 0x51,                                 // push rcx
        0x48, 0x8b, 0x0d, 0xef, 0xff, 0xff, 0xff,   // mov rcx (*mut self)
        0x48, 0x89, 0x41, 0x20,                     // mov QWORD PTR [rcx+0x20] rax
        0x48, 0x8f, 0x41, 0x28,                     // pop QWORD PTR [rcx+0x28]
        0x48, 0x89, 0x51, 0x30,                     // mov QWORD PTR [rcx+0x30] rdx
        0x48, 0x89, 0x59, 0x38,                     // ...

        0x48, 0x89, 0x61, 0x40,                     // mov QWORD PTR [rcx+0x40] rsp
        0x48, 0x83, 0x41, 0x40, 0x08,               // add QWORD PTR [rcx+0x40] 8

        0x48, 0x89, 0x69, 0x48,                     // mov QWORD PTR [rcx+0x48] rbp
        0x48, 0x89, 0x71, 0x50,                     // ...
        0x48, 0x89, 0x79, 0x58,
        0x4c, 0x89, 0x41, 0x60,
        0x4c, 0x89, 0x49, 0x68,
        0x4c, 0x89, 0x51, 0x70,
        0x4c, 0x89, 0x59, 0x78,
        0x4c, 0x89, 0xa1, 0x80, 0,0,0,
        0x4c, 0x89, 0xa9, 0x88, 0,0,0,
        0x4c, 0x89, 0xb1, 0x90, 0,0,0,
        0x4c, 0x89, 0xb9, 0x98, 0,0,0,
        0xc5, 0xfe, 0x7f, 0x81, 0xa0, 0,0,0,
        0xc5, 0xfe, 0x7f, 0x89, 0xa8, 0,0,0,
        0xc5, 0xfe, 0x7f, 0x91, 0xb0, 0,0,0,
        0xc5, 0xfe, 0x7f, 0x99, 0xb8, 0,0,0,
        0xc5, 0xfe, 0x7f, 0xa1, 0xc0, 0,0,0,
        0xc5, 0xfe, 0x7f, 0xa9, 0xc8, 0,0,0,
        0xc5, 0xfe, 0x7f, 0xb1, 0xd0, 0,0,0,
        0xc5, 0xfe, 0x7f, 0xb9, 0xd8, 0,0,0,
        0xc5, 0x7e, 0x7f, 0x81, 0xe0, 0,0,0,
        0xc5, 0x7e, 0x7f, 0x89, 0xe8, 0,0,0,
        0xc5, 0x7e, 0x7f, 0x91, 0xf0, 0,0,0,
        0xc5, 0x7e, 0x7f, 0x99, 0xf8, 0,0,0,
        0xc5, 0x7e, 0x7f, 0xa1, 0x00, 1,0,0,
        0xc5, 0x7e, 0x7f, 0xa9, 0x08, 1,0,0,
        0xc5, 0x7e, 0x7f, 0xb1, 0x10, 1,0,0,
        0xc5, 0x7e, 0x7f, 0xb9, 0x18, 1,0,0,
        0xc3,
        // CLEANUP_TEST:
        0x48, 0x8b, 0x0d, 0x16, 0xff, 0xff, 0xff,   // mov rcx (*mut self)
        0x48, 0x8b, 0x41, 0x20,                     // mov rax QWORD PTR [rcx+0x20]
        0x48, 0x8b, 0x51, 0x30,                     // rdx
        0x48, 0x8b, 0x59, 0x38,                     // rbx
        0x48, 0x8b, 0x69, 0x48,                     // rbp
        0x48, 0x8b, 0x71, 0x50,                     // ...
        0x48, 0x8b, 0x79, 0x58,
        0x4c, 0x8b, 0x41, 0x60,
        0x4c, 0x8b, 0x49, 0x68,
        0x4c, 0x8b, 0x51, 0x70,
        0x4c, 0x8b, 0x59, 0x78,
        0x4c, 0x8b, 0xa1, 0x80, 0,0,0,
        0x4c, 0x8b, 0xa9, 0x88, 0,0,0,
        0x4c, 0x8b, 0xb1, 0x90, 0,0,0,
        0x4c, 0x8b, 0xb9, 0x98, 0,0,0,
        0xc5, 0xfe, 0x6f, 0x81, 0xa0, 0,0,0,
        0xc5, 0xfe, 0x6f, 0x89, 0xa8, 0,0,0,
        0xc5, 0xfe, 0x6f, 0x91, 0xb0, 0,0,0,
        0xc5, 0xfe, 0x6f, 0x99, 0xb8, 0,0,0,
        0xc5, 0xfe, 0x6f, 0xa1, 0xc0, 0,0,0,
        0xc5, 0xfe, 0x6f, 0xa9, 0xc8, 0,0,0,
        0xc5, 0xfe, 0x6f, 0xb1, 0xd0, 0,0,0,
        0xc5, 0xfe, 0x6f, 0xb9, 0xd8, 0,0,0,
        0xc5, 0x7e, 0x6f, 0x81, 0xe0, 0,0,0,
        0xc5, 0x7e, 0x6f, 0x89, 0xe8, 0,0,0,
        0xc5, 0x7e, 0x6f, 0x91, 0xf0, 0,0,0,
        0xc5, 0x7e, 0x6f, 0x99, 0xf8, 0,0,0,
        0xc5, 0x7e, 0x6f, 0xa1, 0x00, 1,0,0,
        0xc5, 0x7e, 0x6f, 0xa9, 0x08, 1,0,0,
        0xc5, 0x7e, 0x6f, 0xb1, 0x10, 1,0,0,
        0xc5, 0x7e, 0x6f, 0xb9, 0x18, 1,0,0,
        0xc3];
    pub fn new(builder: &crate::assemble::builder::Builder, tester: TesterRustSide) -> std::pin::Pin<Box<Self>> {
        let mut result = Box::into_pin(Box::new(Self {
            buf: std::ptr::null_mut(), 
            len: 0,
            test_fn_idx: 0,
            test_fn: Self::test_fn,
            registers: [0;16],
            ymms: [[0;32];16],
            early_return_rsp: 0,
            early_return_rbp: 0,
            early_return_rbx: 0,
            early_return_r12: 0,
            early_return_r13: 0,
            early_return_r14: 0,
            early_return_r15: 0,
            ok: true, msg: "".to_owned(),
            tester: tester
        }));
        let buf = unsafe {platform::alloc_executable(builder.len())}.unwrap();
        let mut i = 0;
        for page in builder.pages() {
            unsafe {libc::memcpy(buf.add(i), page.as_ptr() as *const _, page.len())};
            i += page.len();
        }
        let result_ptr: *mut Self = &mut *result;
        unsafe {libc::memcpy(buf, &result_ptr as *const *mut Self as *const _, 8);}
        result.buf = buf as *mut u8;
        result.len = i;
        result
    }
    pub fn run_test(&mut self, pos: usize) -> Result<String,String> {
        
        unsafe {std::arch::asm!("mov QWORD PTR [rax+672], rsp",
                                "mov QWORD PTR [rax+680], rbp",
                                "mov QWORD PTR [rax+688], rbx",
                                "mov QWORD PTR [rax+696], r12",
                                "mov QWORD PTR [rax+704], r13",
                                "mov QWORD PTR [rax+712], r14",
                                "mov QWORD PTR [rax+720], r15", in("rax") self)}
        let f: extern "C" fn() = unsafe {std::mem::transmute(&self[pos])};
        f();
        if self.ok {
            Ok(std::mem::take(&mut self.msg))
        } else {
            self.ok = true;
            Err(std::mem::take(&mut self.msg))
        }

        /*

            builder[pos..] should look like this:



                ...

                call    INIT_TEST
                mov     QWORD PTR [rcx + 16] 0  # test_fn_idx = 0
                call    [rcx + 24]              # test_fn
                call    CLEANUP_TEST
                mov     rsp QWORD PTR [rcx+64]
                mov     rcx QWORD PTR [rcx+40]
                
                ...

                ret
         */

    }
    extern "win64" fn test_fn(this: *mut Self) {
        
        let this: &mut Self = unsafe {&mut *this};
        // dbg!("test_fn\n");
        match this.tester.test_functions[this.test_fn_idx].0.run(&mut this.registers, &mut this.ymms, this.tester.test_functions[this.test_fn_idx].1) {
            Ok(s) => {this.msg += &s},
            Err(s) => {this.msg += &s; this.ok = false; this.test_fn_early_return();}
        }
    }
    fn test_fn_early_return(&self) {
        unsafe {std::arch::asm!("mov rsp, QWORD PTR [rax+672]",
                                "mov rbp, QWORD PTR [rax+680]",
                                "mov rbx, QWORD PTR [rax+688]",
                                "mov r12, QWORD PTR [rax+696]",
                                "mov r13, QWORD PTR [rax+704]",
                                "mov r14, QWORD PTR [rax+712]",
                                "mov r15, QWORD PTR [rax+720]",
                                "ret", in("rax") self)}
        std::unreachable!()
    }
}

impl std::ops::Deref for Tester {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {unsafe {
        std::slice::from_raw_parts(std::mem::transmute(self.buf), self.len)
    }}
}
impl std::ops::DerefMut for Tester {
    fn deref_mut(&mut self) -> &mut Self::Target {unsafe {
        std::slice::from_raw_parts_mut(std::mem::transmute(self.buf), self.len)
    }}
}
impl Drop for Tester {fn drop(&mut self) {unsafe {platform::free_executable(self.buf as *mut _, self.len);}}}

#[cfg(unix)]
mod platform {
    const PAGE_SIZE: usize = 4096;
    pub unsafe fn alloc_executable(size: usize) -> Result<*mut libc::c_void, &'static str> {
        let pad_size = ((size + PAGE_SIZE - 1) / PAGE_SIZE) * PAGE_SIZE;
        let buf = unsafe {libc::mmap(std::ptr::null_mut(), pad_size,
            libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
            libc::MAP_ANON | libc::MAP_PRIVATE, -1, 0)};
        if buf == libc::MAP_FAILED {Err("mmap failed")} else {Ok(buf)}
    }
    pub unsafe fn free_executable(buf: *mut libc::c_void, size: usize) {
        let pad_size = ((size + PAGE_SIZE - 1) / PAGE_SIZE) * PAGE_SIZE;
        unsafe {libc::munmap(buf, pad_size)};
    }
}
#[cfg(windows)]
mod platform {
    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn VirtualAlloc(addr: *mut libc::c_void,size: usize,alloc_type: u32,protect: u32) -> *mut libc::c_void;
        fn VirtualFree(addr: *mut std::ffi::c_void,size: usize,tree_type: u32) -> i32;
    }
    const MEM_COMMIT: u32 = 0x1000;
    const MEM_RESERVE: u32 = 0x2000;
    const PAGE_EXECUTE_READWRITE: u32 = 0x40;
    const MEM_RELEASE: u32 = 0x8000;
    pub unsafe fn alloc_executable(size: usize) -> Result<*mut libc::c_void, &'static str> {
        let buf = unsafe {VirtualAlloc(std::ptr::null_mut(), size, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE)};
        if buf.is_null() {Err("VirtualAlloc failed")} else {Ok(buf)}
    }
    pub unsafe fn free_executable(buf: *mut libc::c_void, size: usize) {
        unsafe {VirtualFree(buf, size, MEM_RELEASE)};
    }
}

use crate::encode::Operand;
use crate::parse::Literal;
#[derive(Debug, PartialEq)]
pub struct OperandValue {
    operand: Operand,
    values: Vec<Literal>,
    offset: usize,
    head: String
}
impl OperandValue {
    pub fn parse(line: &[u8], allow_type: bool) -> Result<Option<(Self, usize)>, String> {
        let mut identifier = false;
        let ws= line.iter().take_while(|c| **c == b' ' || **c == b'\t').count();
        let Ok((n,operand)) = Operand::read(&line[ws..], &mut |_| {identifier = true; 0})
            else {return Ok(None)};
        let head = str::from_utf8(&line[ws..ws+n]).map_err(|_| "invalid utf8".to_owned())?.to_owned();
        if identifier {return Err("Label not allowed in operand to SET/DBG/ASSERT.".to_owned())}
        let mut rest = &line[ws+n+line[ws+n..].iter().take_while(|c| **c == b' ' || **c == b'\t').count()..];
        if rest.get(0) != Some(&b'[') {
            let Some((i, l)) = Literal::parse(rest) else {return Err("expected literal or list of literals".to_owned())};
            let literal = match &operand {
                Operand::BytePtr(_) | Operand::LowByteRegister(_) | Operand::HighByteRegister(_) => l.into_type(Literal::U8_M | Literal::I8_M, allow_type)?,
                Operand::WordPtr(_) | Operand::WordRegister(_) => l.into_type(Literal::U16_M | Literal::I16_M, allow_type)?,
                Operand::DWordPtr(_) | Operand::DWordRegister(_) => l.into_type(Literal::U32_M | Literal::I32_M | Literal::F32_M, allow_type)?,
                Operand::QWordPtr(_) | Operand::QWordRegister(_) => l.into_type(Literal::U64_M | Literal::I64_M | Literal::F64_M, allow_type)?,
                Operand::Ptr(_) => l.into_type(Literal::STR_M, allow_type)?,
                Operand::Immediate(_) => return Err("expected register or memory location, found literal.".to_owned()),
                Operand::XMMRegister(_) => match &l {
                    Literal::Type(_) => l,
                    Literal::Str(s) if s.len() == 16 => l,
                    _ => return Err("expected type, byte string literal or literal list to fill xmm register".to_owned())
                },
                Operand::YMMRegister(_) => match &l {
                    Literal::Type(_) => l,
                    Literal::Str(s) if s.len() == 32 => l,
                    _ => return Err("expected type, byte string literal or literal list to fill ymm register".to_owned())
                },
            };
            return Ok(Some((Self {operand, values: vec![literal], offset: 0, head}, line.len() - rest.len() + i)))
        } else {rest = &rest[1..];}
        let mut result = Self {operand: operand, values: Vec::new(), offset: 0, head};
        loop {
            rest = &rest[rest.iter().take_while(|c| **c == b' ' || **c == b'\t').count()..];
            match rest.get(0) {
                Some(&b'|') if result.offset == 0 => {result.offset = result.values.len(); rest = &rest[1..]; continue},
                Some(&b'|') => return Err("more than one '|' in list of literals.".to_owned()),
                Some(&b']') => {rest = &rest[1..]; break}, _ => {}
            }
            let Some((i, l)) = Literal::parse(rest) else {return Err(format!("expected ']', '|' or a literal in literal list, found '{}'.", str::from_utf8(rest).unwrap_or("<invalid utf8>")))};
            rest = &rest[i..];
            result.values.push(l);
        }
        let size_bytes: usize = result.values.iter().map(|l| l.size()).sum();
        match &result.operand {
            Operand::Ptr(_) => Ok(Some((result, line.len()-rest.len()))),
            Operand::XMMRegister(_) => if size_bytes == 16 && result.offset == 0 {Ok(Some((result, line.len()-rest.len())))} 
                else {Err("literal sizes don't add up to size of xmm register.".to_owned())}
            Operand::YMMRegister(_) => if size_bytes == 32 && result.offset == 0 {Ok(Some((result, line.len()-rest.len())))}
                else {Err("literal sizes don't add up to size of ymm register.".to_owned())}
            _ => Err("literal list only allowed for xmm registers, ymm registers or untyped memory operand.".to_owned())
        }
    }
    fn len_offset_bytes(&self) -> (usize, usize) {
        match &self.operand {
            Operand::LowByteRegister(_) | Operand::HighByteRegister(_) => (1,0),
            Operand::WordRegister(_) => (2,0),
            Operand::DWordRegister(_) => (4,0),
            Operand::QWordRegister(_) => (8,0),
            _ => {
                let mut len = 0;
                let mut offset = 0;
                for (n,v) in self.values.iter().enumerate() {
                    if n == self.offset {offset = len;}
                    len += v.size();
                }
                if self.values.len() == self.offset {offset = len}
                (len, offset)
            }
        }
    }
    fn get<'a>(&self, registers: &'a mut [u64;16], ymms: &'a mut [[u8;32];16]) -> &'a mut [u8] {
        let reg = |r,regs: &mut [u64;16]| unsafe {std::slice::from_raw_parts_mut(&mut regs[r as usize] as *mut u64 as *mut u8, 8)};
        match &self.operand {
            Operand::LowByteRegister(r) => &mut reg(*r, registers)[0..1],
            Operand::HighByteRegister(r) => &mut reg(*r, registers)[1..2],
            Operand::WordRegister(r) => &mut reg(*r, registers)[0..2],
            Operand::DWordRegister(r) => &mut reg(*r, registers)[0..4],
            Operand::QWordRegister(r) => reg(*r, registers),
            Operand::XMMRegister(r) => &mut ymms[*r as usize][..16],
            Operand::YMMRegister(r) => &mut ymms[*r as usize][..],
            Operand::BytePtr(m) | Operand::WordPtr(m) | Operand::DWordPtr(m) | Operand::QWordPtr(m) | Operand::Ptr(m) => {
                let (len, offset) = self.len_offset_bytes();
                /*let pos = if m.base < 16 {registers[m.base as usize]} else {0}
                    + if m.index < 16 {registers[m.index as usize] * m.scale as u64} else {0}
                    + m.offset as i64 as u64
                    - offset as u64;
                let x = unsafe {m.to_ptr(registers).sub(offset as usize)};
                dbg!(pos);*/
                unsafe {std::slice::from_raw_parts_mut(m.to_ptr::<u8>(registers).sub(offset as usize), len)}
            },
            _ => panic!("unreachable")
        }
    }
    fn set(&self, write: &mut [u8]) -> Result<(), String> {
        if write.len() != self.len_offset_bytes().0 {return Err("bad 'SET' command, mismatched sizes.".into())}
        let mut i = 0;
        for v in self.values.iter() { 
            if let Ok(Literal::Str(bytes)) = v.into_type(Literal::STR_M, false) {
                for (b,w) in bytes.into_iter().zip(&mut write[i..]) {*w = b;}
            }
            i += v.size();
        }
        Ok(())
    }
    fn dbg(&self, read: &[u8]) -> Result<String, String> {
        if read.len() != self.len_offset_bytes().0 {return Err("bad 'DBG' command, mismatched sizes.".into())}
        let mut i = 0;
        let mut result: String = if self.values.len() == 1 {""} else {"["}.into();
        for (n,v) in self.values.iter().enumerate() {
            result = result + if self.values.len() == 1 {""} else if n == self.offset {" | "} else {" "} + &match v {
                Literal::U8(_) => format!("{:#04x}", read[i]),
                Literal::Type(Literal::U8_M) => format!("{}", read[i]),
                Literal::I8(_) | Literal::Type(Literal::I8_M) => format!("{}", read[i] as i8),
                Literal::U16(_) => format!("{:#06x}", u16::from_le_bytes(array(&read[i..]))),
                Literal::Type(Literal::U16_M) => format!("{}", u16::from_le_bytes(array(&read[i..]))),
                Literal::I16(_) | Literal::Type(Literal::I16_M) => format!("{}", i16::from_le_bytes(array(&read[i..]))),
                Literal::U32(_) => format!("{:#010x}", u32::from_le_bytes(array(&read[i..]))),
                Literal::Type(Literal::U32_M) => format!("{}", u32::from_le_bytes(array(&read[i..]))),
                Literal::I32(_) | Literal::Type(Literal::I32_M) => format!("{}", i32::from_le_bytes(array(&read[i..]))),
                Literal::F32(_) | Literal::Type(Literal::F32_M) => format!("{}", f32::from_le_bytes(array(&read[i..]))),
                Literal::U64(_) => format!("{:#018x}", u64::from_le_bytes(array(&read[i..]))),
                Literal::Type(Literal::U64_M) => format!("{}", u64::from_le_bytes(array(&read[i..]))),
                Literal::I64(_) | Literal::Type(Literal::I64_M) => format!("{}", i64::from_le_bytes(array(&read[i..]))),
                Literal::F64(_) | Literal::Type(Literal::F64_M) => format!("{}", f64::from_le_bytes(array(&read[i..]))),
                Literal::Str(v) => format!("\"{}\"", str::from_utf8(&read[i..i+v.len()]).unwrap_or("<invalid_utf8>")),
                _ => return Err("bad 'DBG' command, can't format with unsized literal.".into())
            }[..];
            i += v.size();
        }
        Ok(result + if self.values.len() == 1 {""} else {" ]"})
    }
    fn assert(&self, read: &[u8]) -> Result<(), String> {
        if read.len() != self.len_offset_bytes().0 {return Err("bad 'ASSERT' command, mismatched sizes.".into())}
        let mut i = 0;
        let mut result: String = "[".into();
        let mut fail = false;
        for (n,v) in self.values.iter().enumerate() {
            result = result + if n == self.offset {" | "} else {" "} + &match v {
                Literal::Type(_) => "unchecked".into(),
                Literal::U8(x) => assert_eq(*x, read[i], &mut fail),
                Literal::I8(x) => assert_eq(*x, read[i] as i8, &mut fail),
                Literal::U16(x) => assert_eq(*x, u16::from_le_bytes(array(&read[i..])), &mut fail),
                Literal::I16(x) => assert_eq(*x, i16::from_le_bytes(array(&read[i..])), &mut fail),
                Literal::U32(x) => assert_eq(*x, u32::from_le_bytes(array(&read[i..])), &mut fail),
                Literal::I32(x) => assert_eq(*x, i32::from_le_bytes(array(&read[i..])), &mut fail),
                Literal::F32(x) => assert_eq(*x, f32::from_le_bytes(array(&read[i..])), &mut fail),
                Literal::U64(x) => assert_eq(*x, u64::from_le_bytes(array(&read[i..])), &mut fail),
                Literal::I64(x) => assert_eq(*x, i64::from_le_bytes(array(&read[i..])), &mut fail),
                Literal::F64(x) => assert_eq(*x, f64::from_le_bytes(array(&read[i..])), &mut fail),
                Literal::Str(v) => assert_eq(str::from_utf8(v).unwrap_or("<invalid utf8>"), 
                                                        str::from_utf8(&read[i..i+v.len()]).unwrap_or("<invalid_utf8>"), &mut fail),
                _ => return Err("bad 'ASSERT' command, can't format with unsized literal.".into())
            }[..];
            i += v.size();
        }
        if fail {Err(result+"]")} else {Ok(())}
    }
}
fn array<const N: usize>(slice: &[u8]) -> [u8;N] {
    let mut result = [0;N];
    for (b,w) in slice.iter().cloned().zip(&mut result) {*w = b}
    result
}
fn assert_eq<T: PartialEq + std::fmt::Display>(expect: T, found: T, fail: &mut bool) -> String {
    if expect == found {"ok".into()} else {*fail = true; format!("<expected {} found {}>", expect, found)}
}

pub enum TestFn {
    Set(Vec<OperandValue>),
    Dbg(Vec<OperandValue>),
    Assert(Vec<OperandValue>),
    Print(String)
}

impl TestFn {
    fn run(&self, registers: &mut [u64;16], ymms: &mut [[u8;32];16], linenr: usize) -> Result<String, String> {
        match self {
            Self::Set(values) => {
                for value in values {
                    value.set(value.get(registers, ymms))
                        .map_err(|e| format!("line {}: SET {}: {}", linenr, value.head, e))?;
                }
                Ok("".into())
            }
            Self::Dbg(values) => {
                let mut result = format!("line {}: DBG", linenr);
                for value in values {
                    result += &format!(" {} {}", value.head, value.dbg(value.get(registers, ymms))
                        .map_err(|e| format!("line {}: DBG {}: {}", linenr, value.head, e))?);
                }
                Ok(result+"\n")
            }
            Self::Assert(values) => {
                for value in values {
                    value.assert(value.get(registers, ymms))
                        .map_err(|e| format!("line {}: SET {}: {}", linenr, value.head, e))?;
                }
                Ok("".into())
            },
            Self::Print(msg) => Ok(format!("line {}: {}\n", linenr, msg))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test::Literal;
    use crate::encode::MemoryOperand;
    /*
        A *position* is specified in the same way as an instruction [operand](#operands),
        except for that it can't be an immediate value and that if it is a memory operand it can't reference a label.
        If the *position* is any general purpose register, the *value* must be a literal of the correct size
        (or for `DBG` the *value/type* can also just be one of the literal types `u8`,`u16`,... if an unsigned integer value is specified 
        (e.g. `0u32`), the *position* is printed in hexadecimal, otherwise the normal format is decimal).
        If the *position* is a SIMD register, the *value* must be a whitespace seperated and `[...]` enclosed list of literals or literal types with sizes adding up to the register size (16 bytes or 32 bytes), if only a literal type is specified, that part is not `SET`/`ASSERT`-ed.
        If the *position* is a memory location, the *value* must be a literal, a literal type or a whitespace seperated and `[...]` enclosed list of literals or literal types that can have one `|` between two values that specifies where the actual pointer points, literals and literal types are interpreted like before.
    */
    #[test] fn parse_test_operand() {
        assert_eq!(OperandValue::parse(b" RBX 1f64 ", true), Ok(Some((OperandValue {
            operand: Operand::QWordRegister(3), 
            values: vec![Literal::F64(1.)], 
            offset: 0, 
            head: "RBX".into()
        }, 9))));
        assert_eq!(OperandValue::parse(b"XMM0 [ \"abc\" 0r64 1i32 0u8 ]", false), Ok(Some((OperandValue {
            operand: Operand::XMMRegister(0),
            values: vec![Literal::Str(b"abc".into()), Literal::F64(0.), Literal::I32(1), Literal::U8(0)],
            offset: 0,
            head: "XMM0".into()
        }, 28))));
        assert_eq!(OperandValue::parse(b"PTR [rax + 4 * rbp - 123] [ u8 | u32 \"abcd\" ]", true), Ok(Some((OperandValue {
            operand: Operand::Ptr(MemoryOperand {
                base: 0,
                index: 5,
                scale: 4,
                offset: -123
            }),
            values: vec![Literal::Type(Literal::U8_M), Literal::Type(Literal::U32_M), Literal::Str(b"abcd".into())],
            offset: 1,
            head: "PTR [rax + 4 * rbp - 123]".into()
        }, 45))));
        assert_eq!(OperandValue::parse(b"PTR [rax] [ u8 | ]", true), Ok(Some((OperandValue {
            operand: Operand::Ptr(MemoryOperand {
                base: 0,
                index: 16,
                scale: 0,
                offset: 0
            }),
            values: vec![Literal::Type(Literal::U8_M)],
            offset: 1,
            head: "PTR [rax]".into()
        },18))));
    }
}