
pub fn iterate_lines(mut source: impl std::io::BufRead, mut f: impl FnMut(&[u8], usize) -> Result<(),&'static str>) -> Result<(),String> {
    let mut buf = Vec::new();
    let mut linenr: usize = 1;
    loop {
        buf.clear();
        match source.read_until(b'\n', &mut buf) {
            Err(err) => return Err(format!("line {}: {}", linenr, err)),
            Ok(len) if len != 0 && buf[len-1] == b'\n' => f(&buf[..len-1], linenr).map_err(|err| format!("line {}: {}", linenr, err))?,
            Ok(_) => return f(&buf[..], linenr).map_err(|err| format!("line {}: {}", linenr, err))
        }
        linenr += 1;
    }
}
pub struct Tokenizer<'a> {
    line: &'a [u8],
    i: usize
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = (&'a [u8], usize);
    fn next(&mut self) -> Option<Self::Item> {if let Some((token, i)) = self.peek() {self.i = i; Some((token,i))} else {None}}
}
impl<'a> Tokenizer<'a> {
    pub fn peek(&mut self) -> Option<<Self as Iterator>::Item> {
        let mut i = self.i;
        while i != self.line.len() && (self.line[i] == b' ' || self.line[i] == b'\t' || self.line[i] == b',') {i += 1}
        let j = i;
        // ([_A-Za-z][_A-Za-z0-9]*) | ([-0-9][._A-Za-z0-9]*) | (.[0-9_A-Za-z]*)
        while i != self.line.len() && (self.line[i] >= b'0' && self.line[i] <= b'9' || alpha(self.line[i]) || i == j && self.line[i] == b'-' || self.line[i] == b'.' && (i == j || self.line[j] == b'-' || self.line[j] >= b'0' && self.line[j] <= b'9')) {i += 1}
        if i != j {Some((&self.line[j..i], i))} else if i == self.line.len() {None} else {i += 1; Some((&self.line[i-1..i], i))}
    }
    pub fn new(line: &'a [u8]) -> Self {Self {line: line, i: 0}}
    pub fn skip(&mut self, i: usize) {self.i += i;}
    pub fn rest(&self) -> &[u8] {&self.line[self.i..]}
}
pub fn alpha(c: u8) -> bool {c & 0xdf >= b'A' && c & 0xdf <= b'Z' || c == b'_' || c >= 128}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Uany(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Iany(i64),
    F32(f32),
    F64(f64),
    Fany(f64),
    Str(Vec<u8>),
    Type(u16)
}
impl Literal {

    pub fn parse(line: &[u8]) -> Option<(usize, Self)> {
        // syntax: 
        // <decimal integer>(u8|u16|u32|u64|i8|i16|i32|i64|f32|f64)?
        // 0x<hex integer>(u8|u16|u32|u64|i8|i16|i32|i64|r32|r64)?      no postfix: unsigned, nr digits gives size
        // <float literal>((e|E)<exponent>)?(f32|f64)?
        // "<string literal>"
        // '_' allowed at any point in number literals

        let (mut i, minus) = match line.get(0) {
            Some(&b'"') => {    // string literal, we don't do any escapes
                let Some(j) = line[1..].into_iter().position(|c| *c == b'"') else {return None};
                return Some((j+2, Self::Str(line[1..j+1].to_owned())));
            },
            Some(&b'0') if line.get(1) == Some(&b'x') || line.get(1) == Some(&b'X') => {
                // hexadecimal
                let mut i = 2; // the incrementing map will be called for all valid digits / '_'s, and for the first char after
                let digits = line[2..].into_iter().cloned().map_while(|c| 
                    if b'0' <= c && c <= b'9' {i += 1; Some(Some(c-b'0'))} 
                    else if b'A' <= c && c <= b'F' {i += 1; Some(Some(c-b'A'+10))} 
                    else if b'a' <= c && c <= b'f' {i += 1; Some(Some(c-b'a'+10))}
                    else if c == b'_' {i += 1; Some(None)}
                    else {None}).flatten().collect::<Vec<_>>();
                return match line.get(i..i+2) {
                    Some(b"u8"|b"U8") => (digits.len() < 3)
                        .then_some((i+2, Self::U8(digits.into_iter().fold(0, |acc, d| acc << 4 | d)))),
                    Some(b"u1"|b"U1") if line.get(i+2) == Some(&b'6') => (digits.len() < 5)
                        .then_some((i+3, Self::U16(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u16)))),
                    Some(b"u3"|b"U3") if line.get(i+2) == Some(&b'2') => (digits.len() < 9)
                        .then_some((i+3, Self::U32(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u32)))),
                    Some(b"u6"|b"U6") if line.get(i+2) == Some(&b'4') => (digits.len() < 17)
                        .then_some((i+3, Self::U64(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u64)))),
                    Some(b"i8"|b"I8") => (digits.len() < 3)
                        .then_some((i+2, Self::I8(digits.into_iter().fold(0, |acc, d| acc << 4 | d).cast_signed()))),
                    Some(b"i1"|b"I1") if line.get(i+2) == Some(&b'6') => (digits.len() < 5)
                        .then_some((i+3, Self::I16(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u16).cast_signed()))),
                    Some(b"i3"|b"I3") if line.get(i+2) == Some(&b'2') => (digits.len() < 9)
                        .then_some((i+3, Self::I32(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u32).cast_signed()))),
                    Some(b"i6"|b"I6") if line.get(i+2) == Some(&b'4') => (digits.len() < 17)
                        .then_some((i+3, Self::I64(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u64).cast_signed()))),
                    Some(b"r3"|b"R3") if line.get(i+2) == Some(&b'2') => (digits.len() < 9)
                        .then_some((i+3, Self::F32(f32::from_bits(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u32))))),
                    Some(b"r6"|b"R6") if line.get(i+2) == Some(&b'4') => (digits.len() < 17)
                        .then_some((i+3, Self::F64(f64::from_bits(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u64))))),
                    _ => Some((i, match digits.len() {
                        2 => Self::U8(digits.into_iter().fold(0, |acc, d| acc << 4 | d)),
                        4 => Self::U16(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u16)),
                        8 => Self::U32(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u32)),
                        16 => Self::U64(digits.into_iter().fold(0, |acc, d| acc << 4 | d as u64)),
                        l => Self::Str((0..(l+1)/2).map(|i| if 2*i+1 == l {0} else {digits[l-2-2*i]} << 4 | digits[l-1-2*i]).collect())
                    }))
                }
            },
            Some(&b'-') => (1,true),
            None => return None,
            _ => (0,false)
        };
        enum State { U64(u64), F64(f64), F64Dot(f64, f64), F64Exp(f64, u64), F64NegExp(f64, u64) }
        let mut state = State::U64(0);
        loop {state = match (line.get(i), &state) {
            (Some(d), State::U64(x)) if b'0' <= *d && *d <= b'9' => if *x > u64::MAX/10-10 {State::F64(*x as f64 * 10. + (*d-b'0') as f64)} else {State::U64(*x*10+(*d-b'0') as u64)},
            (Some(d), State::F64(x)) if b'0' <= *d && *d <= b'9' => State::F64(x * 10. + (*d-b'0') as f64),
            (Some(d), State::F64Dot(x, f)) if b'0' <= *d && *d <= b'9' => State::F64Dot(x + (*d-b'0') as f64 * f, f*0.1),
            (Some(d), State::F64Exp(x, e)) if b'0' <= *d && *d <= b'9' => if *e > u64::MAX/10-10 {return None} else {State::F64Exp(*x, e*10 + (*d-b'0') as u64)},
            (Some(d), State::F64NegExp(x, e)) if b'0' <= *d && *d <= b'9' => if *e > u64::MAX/10-10 {return None} else {State::F64NegExp(*x, e*10 + (*d-b'0') as u64)},
            (Some(&b'_'), _) => state,
            (Some(&b'.'), State::U64(x)) => State::F64Dot(*x as f64, 0.1),
            (Some(&b'.'), State::F64(x)) => State::F64Dot(*x, 0.1),
            (Some(&b'e'|&b'E'), State::U64(x)) => if line.get(i+1) == Some(&b'-') {i += 1; State::F64NegExp(*x as f64, 0)} else {State::F64Exp(*x as f64, 0)},
            (Some(&b'e'|&b'E'), State::F64(x) | State::F64Dot(x, _)) => if line.get(i+1) == Some(&b'-') {i += 1; State::F64NegExp(*x, 0)} else {State::F64Exp(*x, 0)},
            _ => break
        }; i += 1;}
        // if i == 0 {return None;}
        let x = match (line.get(i..i+2), state) {
            (Some(b"u8"|b"U8"), State::U64(x)) if !minus && x < 0x100 => 
                return (!minus).then_some((i+2, if i == 0 {Self::Type(Self::U8_M)} else {Self::U8(x as u8)})),
            (Some(b"u1"|b"U1"), State::U64(x)) if line.get(i+2) == Some(&b'6') && !minus && x < 0x10000 => 
                return (!minus).then_some((i+3, if i == 0 {Self::Type(Self::U16_M)} else {Self::U16(x as u16)})),
            (Some(b"u3"|b"U3"), State::U64(x)) if line.get(i+2) == Some(&b'2') && !minus && x < 0x100000000 => 
                return (!minus).then_some((i+3, if i == 0 {Self::Type(Self::U32_M)} else {Self::U32(x as u32)})),
            (Some(b"u6"|b"U6"), State::U64(x)) if line.get(i+2) == Some(&b'4') && !minus => 
                return (!minus).then_some((i+3, if i == 0 {Self::Type(Self::U64_M)} else {Self::U64(x)})),
            (Some(b"i8"|b"I8"), State::U64(x)) => 
                return if minus && x < 0x81 {Some((i+2, Self::I8(-(x as i64) as i8)))}
                else if x < 0x80 {Some((i+2, if i == 0 {Self::Type(Self::I8_M)} else {Self::I8(x as i8)}))} else {None},
            (Some(b"i1"|b"I1"), State::U64(x)) if line.get(i+2) == Some(&b'6') => 
                return if minus && x < 0x8001 {Some((i+3, Self::I16(-(x as i64) as i16)))}
                else if x < 0x8000 {Some((i+3, if i == 0 {Self::Type(Self::I16_M)} else {Self::I16(x as i16)}))} else {None},
            (Some(b"i3"|b"I3"), State::U64(x)) if line.get(i+2) == Some(&b'2') => 
                return if minus && x < 0x80000001 {Some((i+3, Self::I32(-(x as i64) as i32)))}
                else if x < 0x80000000 {Some((i+3, if i == 0 {Self::Type(Self::I32_M)} else {Self::I32(x as i32)}))} else {None},
            (Some(b"i6"|b"I6"), State::U64(x)) if line.get(i+2) == Some(&b'4') => 
                return if minus && x < 0x80000000_00000000 {Some((i+3, Self::I64(-(x as i64))))}
                else if minus && x == 0x80000000_00000000 {Some((i+3, Self::I64(i64::MIN)))} 
                else if x < 0x80000000_00000000 {Some((i+3, if i == 0 {Self::Type(Self::I64_M)} else {Self::I64(x as i64)}))} else {None},
            (Some(b"f3"|b"F3"|b"r3"|b"R3"), State::U64(x)) if line.get(i+2) == Some(&b'2') => 
                if i == 0 {return Some((3, Self::Type(Self::F32_M)))} else {x as f64},
            (Some(b"f6"|b"F6"|b"r6"|b"R6"), State::U64(x)) if line.get(i+2) == Some(&b'4') => 
                if i == 0 {return Some((3, Self::Type(Self::F64_M)))} else {x as f64},
            (_, State::U64(x)) => return (i!=0).then_some((i, 
                if !minus {Self::Uany(x)} else if x<0x80000000_00000000 {Self::Iany(-(x as i64))} 
                else if x==0x80000000_00000000 {Self::Iany(i64::MIN)} else {return None})),
            (_, State::F64(x) | State::F64Dot(x, _)) => x,
            (_, State::F64Exp(x, e)) => (0..e).into_iter().fold(x, |x,_| x*10.),
            (_, State::F64NegExp(x, e)) => (0..e).into_iter().fold(x, |x,_| x*0.1)
        };
        if i == 0 {return None}
        match line.get(i..i+2) {
            Some(b"f3"|b"F3"|b"r3"|b"R3") if line.get(i+2) == Some(&b'2') => 
                if !(x as f32).is_finite() || (x as f32).is_subnormal() {None} else {Some((i+3, Self::F32(if minus {-x as f32} else {x as f32})))},
            Some(b"f6"|b"F6"|b"r6"|b"R6") if line.get(i+2) == Some(&b'4') => 
                if !(x).is_finite() || (x).is_subnormal() {None} else {Some((i+3, Self::F64(if minus {-x} else {x})))},
            _ => if !(x).is_finite() || (x).is_subnormal() {None} else {Some((i, Self::Fany(if minus {-x} else {x})))}
        }
    }
    pub const U8_M: u16 = 1;
    pub const U16_M: u16 = 2;
    pub const U32_M: u16 = 4;
    pub const U64_M: u16 = 8;
    pub const I8_M: u16 = 16;
    pub const I16_M: u16 = 32;
    pub const I32_M: u16 = 64;
    pub const I64_M: u16 = 128;
    pub const F32_M: u16 = 256;
    pub const F64_M: u16 = 512;
    pub const STR_M: u16 = 1024;
    pub fn into_type(&self, mask: u16, allow_type: bool) -> Result<Self, String> {
        match self {
            Self::U8(x) => if mask & Self::U8_M != 0 {Ok(Self::U8(*x))} 
                else if mask & Self::STR_M != 0 {Ok(Self::Str(vec![*x]))} else {Err("u8")},
            Self::U16(x) => if mask & Self::U16_M != 0 {Ok(Self::U16(*x))} 
                else if mask & Self::STR_M != 0 {Ok(Self::Str((*x).to_le_bytes().to_vec()))} else {Err("u16")},
            Self::U32(x) => if mask & Self::U32_M != 0 {Ok(Self::U32(*x))} 
                else if mask & Self::STR_M != 0 {Ok(Self::Str((*x).to_le_bytes().to_vec()))} else {Err("u32")},
            Self::U64(x) => if mask & Self::U64_M != 0 {Ok(Self::U64(*x))} 
                else if mask & Self::STR_M != 0 {Ok(Self::Str((*x).to_le_bytes().to_vec()))} else {Err("u64")},
            Self::I8(x) => if mask & Self::I8_M != 0 {Ok(Self::I8(*x))} 
                else if mask & Self::STR_M != 0 {Ok(Self::Str((*x).to_le_bytes().to_vec()))} else {Err("i8")},
            Self::I16(x) => if mask & Self::I16_M != 0 {Ok(Self::I16(*x))} 
                else if mask & Self::STR_M != 0 {Ok(Self::Str((*x).to_le_bytes().to_vec()))} else {Err("i16")},
            Self::I32(x) => if mask & Self::I32_M != 0 {Ok(Self::I32(*x))} 
                else if mask & Self::STR_M != 0 {Ok(Self::Str((*x).to_le_bytes().to_vec()))} else {Err("i32")},
            Self::I64(x) => if mask & Self::I64_M != 0 {Ok(Self::I64(*x))} 
                else if mask & Self::STR_M != 0 {Ok(Self::Str((*x).to_le_bytes().to_vec()))} else {Err("i64")},
            Self::F32(x) => if mask & Self::F32_M != 0 {Ok(Self::F32(*x))} 
                else if mask & Self::STR_M != 0 {Ok(Self::Str((*x).to_le_bytes().to_vec()))} else {Err("f32")},
            Self::F64(x) => if mask & Self::F64_M != 0 {Ok(Self::F64(*x))} 
                else if mask & Self::STR_M != 0 {Ok(Self::Str((*x).to_le_bytes().to_vec()))} else {Err("f64")},

            Self::Uany(x) if mask & Self::U8_M != 0 && *x < 0x100 => Ok(Self::U8(*x as u8)),
            Self::Uany(x) if mask & Self::I8_M != 0 && *x < 0x80 => Ok(Self::I8(*x as i8)),
            Self::Uany(x) if mask & Self::U16_M != 0 && *x < 0x10000 => Ok(Self::U16(*x as u16)),
            Self::Uany(x) if mask & Self::I16_M != 0 && *x < 0x8000 => Ok(Self::I16(*x as i16)),
            Self::Uany(x) if mask & Self::U32_M != 0 && *x < 0x100000000 => Ok(Self::U32(*x as u32)),
            Self::Uany(x) if mask & Self::I32_M != 0 && *x < 0x80000000 => Ok(Self::I32(*x as i32)),
            Self::Uany(x) if mask & Self::U64_M != 0 => Ok(Self::U64(*x)),
            Self::Uany(x) if mask & Self::I64_M != 0 && *x < 0x80000000_00000000 => Ok(Self::I64(*x as i64)),
            Self::Uany(x) if mask & Self::F32_M != 0 => Ok(Self::F32(*x as f32)),
            Self::Uany(x) if mask & Self::F64_M != 0 => Ok(Self::F64(*x as f64)),
            Self::Uany(_) => Err("<unsigned integer>"),

            Self::Iany(x) if mask & Self::U8_M != 0 && *x as u8 as i64 == *x => Ok(Self::U8(*x as u8)),
            Self::Iany(x) if mask & Self::I8_M != 0 && *x as i8 as i64 == *x => Ok(Self::I8(*x as i8)),
            Self::Iany(x) if mask & Self::U16_M != 0 && *x as u16 as i64 == *x => Ok(Self::U16(*x as u16)),
            Self::Iany(x) if mask & Self::I16_M != 0 && *x as i16 as i64 == *x => Ok(Self::I16(*x as i16)),
            Self::Iany(x) if mask & Self::U32_M != 0 && *x as u32 as i64 == *x => Ok(Self::U32(*x as u32)),
            Self::Iany(x) if mask & Self::I32_M != 0 && *x as i32 as i64 == *x => Ok(Self::I32(*x as i32)),
            Self::Iany(x) if mask & Self::U64_M != 0 && *x >= 0 => Ok(Self::U64(*x as u64)),
            Self::Iany(x) if mask & Self::I64_M != 0 => Ok(Self::I64(*x)),
            Self::Iany(x) if mask & Self::F32_M != 0 => Ok(Self::F32(*x as f32)),
            Self::Iany(x) if mask & Self::F64_M != 0 => Ok(Self::F64(*x as f64)),
            Self::Iany(_) => Err("<signed integer>"),

            Self::Fany(x) if mask & Self::F32_M != 0 => Ok(Self::F32(*x as f32)),
            Self::Fany(x) if mask & Self::F64_M != 0 => Ok(Self::F64(*x as f64)),
            Self::Fany(_) => Err("<float>"),

            Self::Str(x) => if mask & Self::STR_M != 0 {Ok(Self::Str(x.clone()))} else {Err("<byte string>")}
            Self::Type(t) => if allow_type && *t & mask != 0 {Ok(Self::Type(*t))} else {Err("<type name>")}
        }.map_err(|got| {
            [   (Self::U8_M,"u8"), (Self::U16_M,"u16"),(Self::U32_M,"u32"),(Self::U64_M, "u64"),
                (Self::I8_M,"i8"), (Self::I16_M,"i16"),(Self::I32_M,"i32"),(Self::I64_M, "i64"),
                (Self::F32_M,"f32"),(Self::F64_M, "f64"),(Self::STR_M, "<byte string>")].into_iter()
                .filter_map(|(m,s)| (m & mask != 0).then_some(s)).collect::<Vec<_>>()
                .into_iter().enumerate().rev().enumerate()
                .fold("wrong literal type, expected ".to_owned(), |error, (m, (n,expect))| match (n,m) {
                    (0,_) => format!("{} or {}, got {}.", error, expect, got),
                    (_,0) => error + expect,
                    _ => error + ", " + expect
                })
        })
    }
    pub fn size(&self) -> usize {
        match self {
            Self::U8(_) | Self::I8(_) | Self::Type(Self::U8_M | Self::I8_M) => 1,
            Self::U16(_) | Self::I16(_) | Self::Type(Self::U16_M | Self::I16_M) => 2,
            Self::U32(_) | Self::I32(_) | Self::F32(_) | Self::Type(Self::U32_M | Self::I32_M | Self::F32_M) => 4,
            Self::U64(_) | Self::I64(_) | Self::F64(_) | Self::Type(Self::U64_M | Self::I64_M | Self::F64_M) => 8,
            Self::Str(v) => v.len(),
            _ => panic!("Literal::size called on unsized literal."),
        }
    } 
}

#[cfg(test)]
mod tests {
    use super::*;
    /*  # literals
        A literal can have the following types: unsigned integers `u8`,`u16`,`u32` and `u64`, 
        signed integers `i8`,`i16`,`i32` and `i64`, floating point literals `f/r32` and `f/r64`, or type 'byte string of arbitrary length'.
        A type is selected from context (e.g. for `ASSERT rax 1` the size is obvious from the register) and the following table:

        decimal literal with any suffix `[-]?[0-9_]+(u8|u16|...)`:                                      type as specified if compatible
        floating point literal with suffix `[-]?[0-9_]*.[0-9_]*([eE][-]?[0-9_]+)?(f32|r32|f64|r64)`:    type as specified if compatible
        hexadecimal literal with any suffix `0[xX][0-9a-fA-F_]*(u8|u16|...)`:                           type as specified if compatible
        string literal `"[^"]*"`, there are no escapes:                                                 byte string of arbitrary length
        decimal literal `[-]?[0-9_]+`:                                                                  any number type that fits
        floating point literal `[-]?[0-9_]*.[0-9_]*([eE][-]?[0-9_]+)?`:                                 `f/r32` or `f/r64`
        hexadecimal literal `0[xX][0-9a-fA-F_]+`:                                                       unsigned integer with size specified by number of digits or byte string of arbitrary length
    */
    #[test] fn literal_reader() {
        assert_eq!(Literal::parse(b"1_23u8 "), Some((6,Literal::U8(123))));
        assert_eq!(Literal::parse(b"-_.3_2f32  "), Some((9, Literal::F32(-0.32))));
        assert_eq!(Literal::parse(b"0xa_i8"), Some((6, Literal::I8(10))));
        assert_eq!(Literal::parse(b"\"abc 1\""), Some((7, Literal::Str(Vec::from(b"abc 1")))));
        assert_eq!(Literal::parse(b"-142"), Some((4, Literal::Iany(-142))));
        assert_eq!(Literal::parse(b"123.45e-0"), Some((9, Literal::Fany(123.45))));
        assert_eq!(Literal::parse(b"0X_c01d_beef"), Some((12, Literal::U32(0xc01dbeef))));
        assert_eq!(Literal::parse(b"i64 "), Some((3, Literal::Type(Literal::I64_M))));
        
        Literal::parse(b"\"Hello World\" 0x0a 0").unwrap();
        Literal::parse(b"0x0a 0").unwrap();
        Literal::parse(b"0").unwrap();
    }

}