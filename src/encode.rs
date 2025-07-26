
use crate::parse::Literal;
#[derive(Debug, PartialEq)]
pub struct MemoryOperand {
    pub base: u8,   // >= 16 means no base
    pub index: u8,  // >= 16 means no index
    pub scale: u8, // != 0 when base,index >= 16 means offset is a label index
    pub offset: i32
}
impl MemoryOperand {
    pub unsafe fn to_ptr<T>(&self, registers: &[u64;16]) -> *mut T {
        if self.base > 15 && self.index > 15 {panic!("absolute memory address.")}
        let result = if self.base < 16 {registers[self.base as usize]} else {0} 
            + if self.index < 16 {self.scale as u64 * registers[self.index as usize]} else {0}
            + (self.offset as i64).cast_unsigned(); // relies on overflow
        unsafe{std::mem::transmute(result)}
    }
}
#[derive(Debug, PartialEq)]
pub enum Operand {
    LowByteRegister(u8),
    HighByteRegister(u8),
    WordRegister(u8),
    DWordRegister(u8),
    QWordRegister(u8),
    Immediate(Literal),
    Ptr(MemoryOperand),
    BytePtr(MemoryOperand),
    WordPtr(MemoryOperand),
    DWordPtr(MemoryOperand),
    QWordPtr(MemoryOperand),
    XMMRegister(u8),
    YMMRegister(u8),
}
impl Operand {
    fn type_mask(&self) -> u32 {
        let reg_mask = |r: u8| match r {
            0 => EncodingArm::A,
            1 => EncodingArm::C,
            2 => EncodingArm::D,
            3 => EncodingArm::B,
            _ => 0
        };
        match self {
            Self::LowByteRegister(r) | Self::HighByteRegister(r) => 
                (reg_mask(*r) >> 7) | EncodingArm::M8 | EncodingArm::R8,
            Self::WordRegister(r) | Self::DWordRegister(r) | Self::QWordRegister(r) => 
                reg_mask(*r) | EncodingArm::M | EncodingArm::R | EncodingArm::V,
            Self::Immediate(i) => if i.into_type(Literal::I8_M | Literal::U8_M, false).is_ok() {EncodingArm::I | EncodingArm::I8} else {EncodingArm::I}
            Self::Ptr(_) => EncodingArm::M8 | EncodingArm::M | EncodingArm::MX | EncodingArm::MY,
            Self::BytePtr(_) => EncodingArm::M8,
            Self::WordPtr(_) | Self::DWordPtr(_) | Self::QWordPtr(_) => EncodingArm::M,
            Self::XMMRegister(_) => EncodingArm::MX | EncodingArm::RX | EncodingArm::VX,
            Self::YMMRegister(_) => EncodingArm::MY | EncodingArm::RY | EncodingArm::VY
        }
    }
    pub fn read<'a, F: FnMut(&[u8]) -> i32>(line: &'a [u8], get_identifier: &mut F) -> Result<(usize, Self), String> {
        let mut i: usize = 0;
        while i != line.len() && (line[i] == b' ' || line[i] == b'\t') {i += 1;}
        if i == line.len() {return Err("expected Operand, found end of line.".into());}

        if line[i] == b'-' || line[i] >= b'0' && line[i] <= b'9' {
            return if let Some((n, l)) = Literal::parse(&line[i..]) {Ok((i+n, Self::Immediate(l)))} else {Err("invalid number literal.".into())}
        }

        let mut j = i;
        while j != line.len() && (line[j] >= b'0' && line[j] <= b'9' || crate::parse::alpha(line[j])) {j += 1}
        match &line[i..j] {
            b"" => Err("Expected Operand, found non-alphanumeric character.".into()),
            b"AL" | b"al" =>        Ok((j,Operand::LowByteRegister(0))),
            b"CL" | b"cl" =>        Ok((j,Operand::LowByteRegister(1))),
            b"DL" | b"dl" =>        Ok((j,Operand::LowByteRegister(2))),
            b"BL" | b"bl" =>        Ok((j,Operand::LowByteRegister(3))),
            b"SPL" | b"spl" =>      Ok((j,Operand::LowByteRegister(4))),
            b"BPL" | b"bpl" =>      Ok((j,Operand::LowByteRegister(5))),
            b"SIL" | b"sil" =>      Ok((j,Operand::LowByteRegister(6))),
            b"DIL" | b"dil" =>      Ok((j,Operand::LowByteRegister(7))),
            b"R8L" | b"r8l" =>      Ok((j,Operand::LowByteRegister(8))),
            b"R9L" | b"r9l" =>      Ok((j,Operand::LowByteRegister(9))),
            b"R10L" | b"r10l" =>    Ok((j,Operand::LowByteRegister(10))),
            b"R11L" | b"r11l" =>    Ok((j,Operand::LowByteRegister(11))),
            b"R12L" | b"r12l" =>    Ok((j,Operand::LowByteRegister(12))),
            b"R13L" | b"r13l" =>    Ok((j,Operand::LowByteRegister(13))),
            b"R14L" | b"r14l" =>    Ok((j,Operand::LowByteRegister(14))),
            b"R15L" | b"r15l" =>    Ok((j,Operand::LowByteRegister(15))),

            b"AH" | b"ah" =>        Ok((j,Operand::HighByteRegister(0))),
            b"CH" | b"ch" =>        Ok((j,Operand::HighByteRegister(1))),
            b"DH" | b"dh" =>        Ok((j,Operand::HighByteRegister(2))),
            b"BH" | b"bh" =>        Ok((j,Operand::HighByteRegister(3))),

            b"AX" | b"ax" =>        Ok((j,Operand::WordRegister(0))),
            b"CX" | b"cx" =>        Ok((j,Operand::WordRegister(1))),
            b"DX" | b"dx" =>        Ok((j,Operand::WordRegister(2))),
            b"BX" | b"bx" =>        Ok((j,Operand::WordRegister(3))),
            b"SP" | b"sp" =>        Ok((j,Operand::WordRegister(4))),
            b"BP" | b"bp" =>        Ok((j,Operand::WordRegister(5))),
            b"SI" | b"si" =>        Ok((j,Operand::WordRegister(6))),
            b"DI" | b"di" =>        Ok((j,Operand::WordRegister(7))),
            b"R8W" | b"r8w" =>      Ok((j,Operand::WordRegister(8))),
            b"R9W" | b"r9w" =>      Ok((j,Operand::WordRegister(9))),
            b"R10W" | b"r10w" =>    Ok((j,Operand::WordRegister(10))),
            b"R11W" | b"r11w" =>    Ok((j,Operand::WordRegister(11))),
            b"R12W" | b"r12w" =>    Ok((j,Operand::WordRegister(12))),
            b"R13W" | b"r13w" =>    Ok((j,Operand::WordRegister(13))),
            b"R14W" | b"r14w" =>    Ok((j,Operand::WordRegister(14))),
            b"R15W" | b"r15w" =>    Ok((j,Operand::WordRegister(15))),

            b"EAX" | b"eax" =>      Ok((j,Operand::DWordRegister(0))),
            b"ECX" | b"ecx" =>      Ok((j,Operand::DWordRegister(1))),
            b"EDX" | b"edx" =>      Ok((j,Operand::DWordRegister(2))),
            b"EBX" | b"ebx" =>      Ok((j,Operand::DWordRegister(3))),
            b"ESP" | b"esp" =>      Ok((j,Operand::DWordRegister(4))),
            b"EBP" | b"ebp" =>      Ok((j,Operand::DWordRegister(5))),
            b"ESI" | b"esi" =>      Ok((j,Operand::DWordRegister(6))),
            b"EDI" | b"edi" =>      Ok((j,Operand::DWordRegister(7))),
            b"R8D" | b"r8d" =>      Ok((j,Operand::DWordRegister(8))),
            b"R9D" | b"r9d" =>      Ok((j,Operand::DWordRegister(9))),
            b"R10D" | b"r10d" =>    Ok((j,Operand::DWordRegister(10))),
            b"R11D" | b"r11d" =>    Ok((j,Operand::DWordRegister(11))),
            b"R12D" | b"r12d" =>    Ok((j,Operand::DWordRegister(12))),
            b"R13D" | b"r13d" =>    Ok((j,Operand::DWordRegister(13))),
            b"R14D" | b"r14d" =>    Ok((j,Operand::DWordRegister(14))),
            b"R15D" | b"r15d" =>    Ok((j,Operand::DWordRegister(15))),

            b"RAX" | b"rax" =>      Ok((j,Operand::QWordRegister(0))),
            b"RCX" | b"rcx" =>      Ok((j,Operand::QWordRegister(1))),
            b"RDX" | b"rdx" =>      Ok((j,Operand::QWordRegister(2))),
            b"RBX" | b"rbx" =>      Ok((j,Operand::QWordRegister(3))),
            b"RSP" | b"rsp" =>      Ok((j,Operand::QWordRegister(4))),
            b"RBP" | b"rbp" =>      Ok((j,Operand::QWordRegister(5))),
            b"RSI" | b"rsi" =>      Ok((j,Operand::QWordRegister(6))),
            b"RDI" | b"rdi" =>      Ok((j,Operand::QWordRegister(7))),
            b"R8" | b"r8" =>        Ok((j,Operand::QWordRegister(8))),
            b"R9" | b"r9" =>        Ok((j,Operand::QWordRegister(9))),
            b"R10" | b"r10" =>      Ok((j,Operand::QWordRegister(10))),
            b"R11" | b"r11" =>      Ok((j,Operand::QWordRegister(11))),
            b"R12" | b"r12" =>      Ok((j,Operand::QWordRegister(12))),
            b"R13" | b"r13" =>      Ok((j,Operand::QWordRegister(13))),
            b"R14" | b"r14" =>      Ok((j,Operand::QWordRegister(14))),
            b"R15" | b"r15" =>      Ok((j,Operand::QWordRegister(15))),

            b"XMM0" | b"xmm0" =>    Ok((j,Operand::XMMRegister(0))),
            b"XMM1" | b"xmm1" =>    Ok((j,Operand::XMMRegister(1))),
            b"XMM2" | b"xmm2" =>    Ok((j,Operand::XMMRegister(2))),
            b"XMM3" | b"xmm3" =>    Ok((j,Operand::XMMRegister(3))),
            b"XMM4" | b"xmm4" =>    Ok((j,Operand::XMMRegister(4))),
            b"XMM5" | b"xmm5" =>    Ok((j,Operand::XMMRegister(5))),
            b"XMM6" | b"xmm6" =>    Ok((j,Operand::XMMRegister(6))),
            b"XMM7" | b"xmm7" =>    Ok((j,Operand::XMMRegister(7))),
            b"XMM8" | b"xmm8" =>    Ok((j,Operand::XMMRegister(8))),
            b"XMM9" | b"xmm9" =>    Ok((j,Operand::XMMRegister(9))),
            b"XMM10" | b"xmm10" =>  Ok((j,Operand::XMMRegister(10))),
            b"XMM11" | b"xmm11" =>  Ok((j,Operand::XMMRegister(11))),
            b"XMM12" | b"xmm12" =>  Ok((j,Operand::XMMRegister(12))),
            b"XMM13" | b"xmm13" =>  Ok((j,Operand::XMMRegister(13))),
            b"XMM14" | b"xmm14" =>  Ok((j,Operand::XMMRegister(14))),
            b"XMM15" | b"xmm15" =>  Ok((j,Operand::XMMRegister(15))),
            
            b"YMM0" | b"ymm0" =>    Ok((j,Operand::YMMRegister(0))),
            b"YMM1" | b"ymm1" =>    Ok((j,Operand::YMMRegister(1))),
            b"YMM2" | b"ymm2" =>    Ok((j,Operand::YMMRegister(2))),
            b"YMM3" | b"ymm3" =>    Ok((j,Operand::YMMRegister(3))),
            b"YMM4" | b"ymm4" =>    Ok((j,Operand::YMMRegister(4))),
            b"YMM5" | b"ymm5" =>    Ok((j,Operand::YMMRegister(5))),
            b"YMM6" | b"ymm6" =>    Ok((j,Operand::YMMRegister(6))),
            b"YMM7" | b"ymm7" =>    Ok((j,Operand::YMMRegister(7))),
            b"YMM8" | b"ymm8" =>    Ok((j,Operand::YMMRegister(8))),
            b"YMM9" | b"ymm9" =>    Ok((j,Operand::YMMRegister(9))),
            b"YMM10" | b"ymm10" =>  Ok((j,Operand::YMMRegister(10))),
            b"YMM11" | b"ymm11" =>  Ok((j,Operand::YMMRegister(11))),
            b"YMM12" | b"ymm12" =>  Ok((j,Operand::YMMRegister(12))),
            b"YMM13" | b"ymm13" =>  Ok((j,Operand::YMMRegister(13))),
            b"YMM14" | b"ymm14" =>  Ok((j,Operand::YMMRegister(14))),
            b"YMM15" | b"ymm15" =>  Ok((j,Operand::YMMRegister(15))),

            b"BYTE" | b"byte" =>    Self::try_parse_memory(line, j, get_identifier, Operand::BytePtr), 
            b"WORD" | b"word" =>    Self::try_parse_memory(line, j, get_identifier, Operand::WordPtr),
            b"DWORD" | b"dword" =>  Self::try_parse_memory(line, j, get_identifier, Operand::DWordPtr),
            b"QWORD" | b"qword" =>  Self::try_parse_memory(line, j, get_identifier, Operand::QWordPtr),
            b"PTR" | b"ptr" => Self::try_parse_memory(line, i, get_identifier, Operand::Ptr),
            _ => Err("unexpected token, Operand should start with a register name or '(BYTE/WORD/DWORD/QWORD) PTR'.".into())
        }
    }
    fn try_parse_memory<F: FnMut(&[u8]) -> i32, G: Fn(MemoryOperand) -> Operand>(line: &[u8], i: usize, mut get_identifier: F, pointer_type: G) -> Result<(usize, Self), String> {
        // dbg!(str::from_utf8(line).unwrap());
        let mut tokens = crate::parse::Tokenizer::new(&line[i..]);
        if let Some((b"PTR" | b"ptr",_)) = tokens.next() {} else {return Err("unexpected token in memory operand, expected 'PTR'.".into())}
        if let Some((b"[",_)) = tokens.next() {} else {return Err("unexpected token in memory operand, expected '['.".into())}
        let mut result = MemoryOperand{
            base: 16,
            index: 16,
            scale: 0,
            offset: 0
        };
        let mut first = true;
        const EOL: &'static str = "unexpected end of line in memory Operand.";
        const MULTI_BASE: &'static str = "can't have multiple base terms in memory operand.";
        loop {
            let Some((sign_token,_)) = tokens.next() else {return Err(EOL.into())};
            
            let (neg, t) = match sign_token {
                b"]" => return Ok((line.len() - tokens.rest().len(), pointer_type(result))),
                b"+" => if let Some((t,_)) = tokens.next() {(false, t)} else {return Err(EOL.into())},
                b"-" => if let Some((t,_)) = tokens.next() {(true, t)} else {return Err(EOL.into())},
                t => if first {(false, t)} else {return Err("unexpected token in memory Operand, expected '+','-' or ']'.".into())}
            };

            if t[0] >= b'0' && t[0] <= b'9' {
                let Some((n,l)) = Literal::parse(t) else {return Err("invalid integer literal.".into())};
                if n != t.len() {return Err("invalid integer literal.".into());}
                let i = match l.into_type(Literal::I8_M | Literal::I32_M, false) {
                    Ok(Literal::I8(i)) => i as i32,
                    Ok(Literal::I32(i)) if i >= -128 && i <= 127 => return Err("can't force 32 bit offset encoding in memory literal if the offset fits 8 bits.".into()),
                    Ok(Literal::I32(i)) => i,
                    Err(e) => return Err(e),
                    _ => panic!("unreachable")
                };
                
                match tokens.peek() {
                    Some((t,_)) if !neg && t == b"*" => {
                        if result.scale != 0 || result.index != 16 {return Err("memory literal can only have one scale*index term.".into())}
                        result.scale = i as u8;
                        tokens.next();
                        let Some((index,_)) = tokens.next() else {return Err(EOL.into())};
                        result.index = match index {
                            b"RAX" | b"rax" => 0,
                            b"RCX" | b"rcx" => 1,
                            b"RDX" | b"rdx" => 2,
                            b"RBX" | b"rbx" => 3,
                            b"RSP" | b"rsp" => 4,
                            b"RBP" | b"rbp" => 5,
                            b"RSI" | b"rsi" => 6,
                            b"RDI" | b"rdi" => 7,
                            b"R8" | b"r8"   => 8,
                            b"R9" | b"r9"   => 9,
                            b"R10" | b"r10" => 10,
                            b"R11" | b"r11" => 11,
                            b"R12" | b"r12" => 12,
                            b"R13" | b"r13" => 13,
                            b"R14" | b"r14" => 14,
                            b"R15" | b"r15" => 15,
                            _ => return Err("unexpected token in meomry Operand after '*', expected a 64 bit register.".into())
                        };
                    },
                    _ if result.base > 16 || result.offset != 0 => return Err("memory operand with label reference can't contain any other terms.".into()),
                    _ if neg => if i < -i32::MAX {return Err("literal in memory operand does not fit into 32-bit integer.".into())} else {result.offset = -i},
                    _ => result.offset = i
                }
            } else if neg {
                return Err("unexpected token in memory operand, only the offset term can be negative".into())
            } else { match t {
                b"RAX" | b"rax" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 0},
                b"RCX" | b"rcx" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 1},
                b"RDX" | b"rdx" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 2},
                b"RBX" | b"rbx" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 3},
                b"RSP" | b"rsp" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 4},
                b"RBP" | b"rbp" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 5},
                b"RSI" | b"rsi" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 6},
                b"RDI" | b"rdi" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 7},
                b"R8" | b"r8"   => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 8},
                b"R9" | b"r9"   => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 9},
                b"R10" | b"r10" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 10},
                b"R11" | b"r11" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 11},
                b"R12" | b"r12" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 12},
                b"R13" | b"r13" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 13},
                b"R14" | b"r14" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 14},
                b"R15" | b"r15" => {if result.base != 16 {return Err(MULTI_BASE.into())} result.base = 15},
                _ if t[0] & 0xdf >= b'A' && t[0] & 0xdf <= b'Z' || t[0] == b'_' || t[0] >= 0x80 => {
                    if result.base != 16 || result.index != 16 {return Err("memory operand with label reference can't contain any other terms.".into())}
                    result.base = 17;
                    result.index = 17;
                    result.scale = 1;
                    result.offset = get_identifier(t);
                }
                _ => return Err("unexpected token in memory operand, expected integer literal, 64-bit register name or label.".into())
            }}
            first = false; 
        }
    }
}
pub struct EncodingArm {
    nr_operands: u8,
    operands: [u32;4], // bit flags, see the definitions in impl EncodingArm
    encodings: Vec<[u8;4]> // 0x4f fills the rest, all 0x4f means the pattern can't be encoded
}
impl EncodingArm {
    const A8: u32 = 1;
    const B8: u32 = 2;
    const C8: u32 = 4;
    const D8: u32 = 8;
    const I8: u32 = 0x10;
    const M8: u32 = 0x20;
    const R8: u32 = 0x40;
    const _V8: u32 = 0x80; // unused

    const A: u32 = 0x100;
    const B: u32 = 0x200;
    const C: u32 = 0x400;
    const D: u32 = 0x800;
    const I: u32 = 0x1000;
    const M: u32 = 0x2000;
    const R: u32 = 0x4000;
    const V: u32 = 0x8000;

    const _IX: u32 = 0x10000; // unused
    const MX: u32 = 0x20000;
    const RX: u32 = 0x40000;
    const VX: u32 = 0x80000;

    const _IY: u32 = 0x100000; // unused
    const MY: u32 = 0x200000;
    const RY: u32 = 0x400000;
    const VY: u32 = 0x800000;
    

    pub fn new(s: &str) -> Self {
        // dbg!(s);
        let mut result = Self {
            nr_operands: 0,
            operands:  [0,0,0,0],
            encodings: Vec::new()
        };
        let mut iter = s.chars();
        let mut operand: u32 = 0;
        while let Some(c) = iter.next() { match c {
            ' '|'\t' => if result.nr_operands != 4 && result.operands[result.nr_operands as usize] != 0 {result.nr_operands += 1},
            ':' => break,
            '8' => {
                if operand & Self::V != 0 {panic!("bad encoding arm definition")}
                result.operands[result.nr_operands as usize] |= operand >> 8;
                operand = 0
            },
            '*' => {
                result.operands[result.nr_operands as usize] |= operand;
                operand = 0
            },
            'x'|'X' => {
                if operand & 0x4f00 != 0 {panic!("bad encoding arm definition")}
                result.operands[result.nr_operands as usize] |= operand << 4;
                operand = 0
            }
            'y'|'Y' => {
                if operand & 0x4f00 != 0 {panic!("bad encoding arm definition")}
                result.operands[result.nr_operands as usize] |= operand << 8;
                operand = 0
            }
            'a'|'A' => operand |= Self::A,
            'b'|'B' => operand |= Self::B,
            'c'|'C' => operand |= Self::C,
            'd'|'D' => operand |= Self::D,
            'i'|'I' => operand |= Self::I,
            'm'|'M' => operand |= Self::M,
            'r'|'R' => operand |= Self::R,
            'v'|'V' => operand |= Self::V,
            _ => panic!("bad encoding arm definition")
        }}
        let mut encoding: [u8;4] = [0x4f,0x4f,0x4f,0x4f];
        let mut nr_set_bytes = 0;
        while let Some(c) = iter.next() {
            let x: u8 = if c >= '0' && c <= '9' {(c as u8)-b'0'}
                else if c >= 'a' && c <= 'f' {(c as u8)-b'a' + 10}
                else if c >= 'A' && c <= 'F' {(c as u8)-b'A' + 10}
                else if c == '/' {4}
                else if c == '$' {encoding[nr_set_bytes] = 0xc4; nr_set_bytes += 1; continue;}
                else if c == '+' {encoding[nr_set_bytes] = 0x4e; nr_set_bytes += 1; continue;}
                else if encoding[0] != 0x4f {
                    result.encodings.push(encoding);
                    if c == '-' {result.encodings.push([0x4f,0x4f,0x4f,0x4f]);}
                    encoding = [0x4f, 0x4f, 0x4f, 0x4f];
                    nr_set_bytes = 0;
                    continue;
                } else if c == '-' {
                    result.encodings.push([0x4f,0x4f,0x4f,0x4f]);
                    continue;
                } else {continue;};
            let d = iter.next().unwrap();
            encoding[nr_set_bytes] = (x << 4) | if d >= '0' && d <= '9' {(d as u8)-b'0'}
                else if d >= 'a' && d <= 'f' {(d as u8)-b'a' + 10}
                else if d >= 'A' && d <= 'F' {(d as u8)-b'A' + 10}
                else {panic!("bad encoding arm definition")};
            nr_set_bytes += 1;
        }
        result.encodings.push(encoding); // not always necessary, but sometimes
        // dbg!(&result.encodings);
        return result;
    }
}
pub const IMM_SIGNED: u16 = Literal::I8_M | Literal::I16_M | Literal::I32_M | Literal::I64_M;
pub const IMM_UNSIGNED: u16 = Literal::U8_M | Literal::U16_M | Literal::U32_M | Literal::U64_M;
pub const IMM_FLOAT: u16 = Literal::F32_M | Literal::F64_M;
pub const IMM_INT: u16 = IMM_SIGNED | IMM_UNSIGNED;
pub const IMM_ANY: u16 = IMM_SIGNED | IMM_UNSIGNED | IMM_FLOAT;
pub const NO_16BIT: u16 = 2048;

// result: 1 byte instruction length (bytes), then 15 bytes buffer for the instruction.
// if len >> 4 != 0, there is a 4 byte immediate starting at len >> 4
// which is currently a label index, and must be replaced with the RIP-relative displacement of that label
pub fn try_encode(operands: &[Operand], template: &[EncodingArm], immediate_type: u16) -> Result<[u8;16], String> {
    let mut errors = Vec::<&'static str>::new();
    let mut encodings = Vec::<[u8;16]>::new();
    for arm in template {
        if arm.nr_operands as usize != operands.len() {errors.push("wrong number of arguments."); continue;}

        let mut iter: Box<dyn Iterator<Item=[u32;4]>> = Box::new(std::iter::once([0,0,0,0]));
        for n in 0..arm.nr_operands as usize {
            iter = Box::new(iter.flat_map(move |arr| {
                let xs = (0..16).into_iter().map(|i| 1 << i).filter(move |x| x & arm.operands[n] != 0);
                xs.map(move |x| {let mut tmp = arr; tmp[n] = x; tmp})
            }));
        }

        // dbg!(iter.collect::<Vec<_>>());
        // panic!();

        // dbg!(&operands);
        'outer: for (operand_types, opcode) in iter.zip(arm.encodings.iter()) {
            // print!("trying an encoding...\n");
            
            let mut opcode = *opcode;
            // print!("{:02x} {:02x} {:02x} {:02x}\n", opcode[0], opcode[1], opcode[2], opcode[3]);
            if operands.iter().zip(operand_types.iter()).any(|(op,op_type)| {
                // print!("operand: {:032b}\nmask:    {:032b}\n", *op_type, op.type_mask());
                op.type_mask() & *op_type == 0
            }) || opcode[0] == 0x4f {continue;}
            
            // print!("test.\n");
            // print!("operand types are ok...\n");
            // dbg!(operand_types, opcode);
            let mut rex_prefix: u8 = 0;
            // 0xff: can't have prefix (because high byte)
            // != 0: needs prefix
            let mut modrm = Vec::<u8>::new();
            // modrm, sid, sid offset immediate
            let mut immediate_vec = Vec::<u8>::new();
            let mut op_size_bytes: Option<u8> = None;

            let mut vex_map_pp: Option<u8> = None;
            // & 0x1f: VEX.map_select, >> 6: VEX.pp, & 0x40: 0

            let mut vex_vvvv: Option<u8> = None;

            let mut rip_relative = false;

            let mut immediate: Option<Literal> = None;

            let mut simd_size_256: Option<bool> = None;

            let add_register_to_opcode = opcode.iter().position(|x| *x == 0x4e);

            let mut i = 0;
            if opcode[i] == 0xc4 {
                vex_map_pp = Some(opcode[i+1]);
                i += 2;
            }
            for b in opcode {if b & 0xf8 == 0x40 {modrm.push((b << 3) & 0x38)}}

            const E_REX: &str = "\ninvalid combination of REX prefix and high byte.";
            const E_VEX: &str = "\ninvalid combination of VEX prefix and high byte.";
            const E_SIZE: &str = "\nincompatible operand sizes.";
            const E_15: &str = "\ncould not encode within 15 bytes.";
            let put_reg = |r: u8, rex_prefix: &mut u8, modrm: &mut Vec<u8>, opcode: &mut [u8;4]| {
                // print!("put_reg called.\n");
                // dbg!(&modrm);
                if r >= 8 {
                    if *rex_prefix == 0xff {return Some(E_REX)}
                    *rex_prefix |= 4 & (r >> 1);
                }
                if let Some(p) = add_register_to_opcode {
                    opcode[p-1] |= r & 7;
                } else {
                    assert!(modrm.len() == 0 || (modrm[0] & 0x38) == 0);
                    if let Some(l) = modrm.first_mut() {*l |= 0x38 & (r << 3)} else {modrm.push(0x38 & (r << 3))}
                }
                // dbg!(&modrm);
                None
            };
            let put_rm_reg = |r: u8, rex_prefix: &mut u8, modrm: &mut Vec<u8>| {
                // print!("put_rm_reg called...\n");
                // dbg!(&modrm);
                if r >= 8 {
                    if *rex_prefix == 0xff {return Some(E_REX)}
                    *rex_prefix |= r >> 3;
                }
                assert!(modrm.len() == 0 || modrm.len() == 1 && (modrm[0] & 0xc7) == 0);
                if let Some(l) = modrm.first_mut() {*l |= 0xc0 | 0x07 & r} else {modrm.push(0xc0 | 0x07 & r);}
                // dbg!(&modrm);
                None
            };
            let put_rm_mem = |mem: &MemoryOperand, rex_prefix: &mut u8, modrm: &mut Vec<u8>, rip_relative: &mut bool| -> Option<&'static str> {
                if mem.index == 4 {return Some("\ninvalid memory operand: index register RSP is impossible.");}
                
                if modrm.len() == 0 {modrm.push(0);}
                assert!(modrm.len() == 1 && (modrm[0] & 0xc7) == 0);

                let base = if mem.base >= 16 {5} else {mem.base};

                if base >= 8 {
                    if *rex_prefix == 0xff {return Some(E_REX);}
                    *rex_prefix |= 1;
                }
                let offset_size = 
                    if mem.base >= 16 {4}
                    else if mem.offset == 0 && mem.base != 5 && mem.base != 13 {0} 
                    else if mem.offset as i8 as i32 == mem.offset {modrm[0] |= 0x40; 1} 
                    else {modrm[0] |= 0x80; 4};

                if mem.index < 16 {
                    if mem.index >= 8 {
                        if *rex_prefix == 0xff {return Some(E_REX);}
                        *rex_prefix |= 2;
                    }
                    modrm[0] |= 4;
                    modrm.push(base & 7 | mem.index << 3 & 0x38 | match mem.scale {1 => 0, 2 => 1, 4 => 2, 8 => 3, _ => return Some("invalid memory operand: invalid index scale.")} << 6);
                } else {
                    modrm[0] |= base & 7;
                    *rip_relative = mem.scale != 0;
                    if mem.base == 4 || mem.base == 12 {modrm.push(0x24);}
                }
                for i in 0..offset_size {modrm.push((mem.offset as u32 >> 8*i & 0xff) as u8)}
                None
            };
            let mut put_vex_reg = |r: u8| {
                assert!(vex_vvvv.is_none());
                vex_vvvv = Some(r);
            };
            let mut put_size = |size: u8| {
                if let Some(s) = op_size_bytes {if s != size {return Some(E_SIZE)}};
                op_size_bytes = Some(size);
                None
            };
            let mut put_simd_size = |is_256: bool| {
                if let Some(s) = simd_size_256 {if s != is_256 {return Some(E_SIZE)}};
                simd_size_256 = Some(is_256);
                None
            };
            for op_op_type in operands.iter().zip(operand_types) {if let Some(e) = match op_op_type {
                (_, EncodingArm::A8 | EncodingArm::B8 | EncodingArm::C8 | EncodingArm::D8 
                  | EncodingArm::A  | EncodingArm::B  | EncodingArm::C  | EncodingArm::D) => None,
                (Operand::LowByteRegister(r), EncodingArm::M8) => {
                    if *r >= 4 {
                        if rex_prefix == 0xff {errors.push(E_REX); continue 'outer}
                        rex_prefix |= r >> 3;
                    }
                    assert!(modrm.len() == 0 || modrm.len() == 1 && (modrm[0] & 0xc7) == 0);
                    if let Some(l) = modrm.first_mut() {*l |= 0xc0 | 0x07 & r} else {modrm.push(0xc0 | 0x07 & r);}
                    put_size(1)
                },
                (Operand::LowByteRegister(r), EncodingArm::R8) => {
                    if *r >= 4 {
                        if rex_prefix == 0xff {errors.push(E_REX); continue 'outer}
                        rex_prefix |= 4 & (r >> 1);
                    }
                    if let Some(p) = add_register_to_opcode {
                        opcode[p-1] |= r & 7;
                    } else {
                        assert!(modrm.len() == 0 || (modrm[0] & 0x38) == 0);
                        if let Some(l) = modrm.first_mut() {*l |= 0x38 & (r << 3)} else {modrm.push(0x38 & (r << 3))}
                    }
                    put_size(1)
                },
                (Operand::HighByteRegister(r), EncodingArm::M8) => {
                    if rex_prefix != 0 && rex_prefix != 0xff {errors.push(E_REX); continue 'outer}
                    rex_prefix = 0xff;
                    assert!(modrm.len() == 0 || modrm.len() == 1 && (modrm[0] & 0xc7) == 0);
                    if let Some(l) = modrm.first_mut() {*l |= 0xc4 | r} else {modrm.push(0xc4 | r);}
                    None
                },
                (Operand::HighByteRegister(r), EncodingArm::R8) => {
                    if rex_prefix != 0 && rex_prefix != 0xff {errors.push(E_REX); continue 'outer}
                    rex_prefix = 0xff;
                    if let Some(p) = add_register_to_opcode {
                        opcode[p-1] |= r & 3 | 4;
                    } else {
                        assert!(modrm.len() == 0 || (modrm[0] & 0x38) == 0);
                        if let Some(l) = modrm.first_mut() {*l |= 0x38 & (r << 3)} else {modrm.push(0x38 & (r << 3))}
                    }
                    None
                },
                (Operand::WordRegister(r), EncodingArm::M) =>
                    put_rm_reg(*r, &mut rex_prefix, &mut modrm).or_else(|| put_size(2)),
                (Operand::WordRegister(r), EncodingArm::R) =>
                    put_reg(*r, &mut rex_prefix, &mut modrm, &mut opcode).or_else(|| put_size(2)),
                (Operand::WordRegister(r), EncodingArm::V) => 
                    {put_vex_reg(*r); put_size(2)}
                (Operand::DWordRegister(r), EncodingArm::M) =>
                    put_rm_reg(*r, &mut rex_prefix, &mut modrm).or_else(|| put_size(4)),
                (Operand::DWordRegister(r), EncodingArm::R) =>
                    put_reg(*r, &mut rex_prefix, &mut modrm, &mut opcode).or_else(||put_size(4)),
                (Operand::DWordRegister(r), EncodingArm::V) => 
                    {put_vex_reg(*r); put_size(4)}
                (Operand::QWordRegister(r), EncodingArm::M) =>
                    put_rm_reg(*r, &mut rex_prefix, &mut modrm).or_else(|| put_size(8)),
                (Operand::QWordRegister(r), EncodingArm::R) =>
                    put_reg(*r, &mut rex_prefix, &mut modrm, &mut opcode).or_else(||put_size(8)),
                (Operand::QWordRegister(r), EncodingArm::V) => 
                    {put_vex_reg(*r); put_size(8)}
                (Operand::Immediate(i), EncodingArm::I8) => {immediate_vec.push(match i.into_type((Literal::I8_M | Literal::U8_M) & immediate_type, false)? {
                    Literal::I8(x) => x as u8,
                    Literal::U8(x) => x,
                    _ => panic!("unreachable")
                }); None}
                (Operand::Immediate(i), EncodingArm::I) => {immediate = Some(i.clone()); None},
                (Operand::Ptr(op) | Operand::BytePtr(op), EncodingArm::M8) => 
                    put_rm_mem(op, &mut rex_prefix, &mut modrm, &mut rip_relative).or_else(||put_size(1)),
                (Operand::Ptr(op), EncodingArm::M) => 
                    put_rm_mem(op, &mut rex_prefix, &mut modrm, &mut rip_relative),
                (Operand::Ptr(op), EncodingArm::MX) => 
                    put_rm_mem(op, &mut rex_prefix, &mut modrm, &mut rip_relative).or_else(||put_size(16)),
                (Operand::Ptr(op), EncodingArm::MY) => 
                    put_rm_mem(op, &mut rex_prefix, &mut modrm, &mut rip_relative).or_else(||put_size(32)),
                (Operand::WordPtr(op), EncodingArm::M) => 
                    put_rm_mem(op, &mut rex_prefix, &mut modrm, &mut rip_relative).or_else(|| put_size(2)),
                (Operand::DWordPtr(op), EncodingArm::M) => 
                    put_rm_mem(op, &mut rex_prefix, &mut modrm, &mut rip_relative).or_else(|| put_size(4)),
                (Operand::QWordPtr(op), EncodingArm::M) => 
                    put_rm_mem(op, &mut rex_prefix, &mut modrm, &mut rip_relative).or_else(|| put_size(8)),
                (Operand::XMMRegister(r), EncodingArm::MX) => 
                    put_rm_reg(*r, &mut rex_prefix, &mut modrm).or_else(|| put_simd_size(false)),
                (Operand::XMMRegister(r), EncodingArm::RX) => 
                    put_reg(*r, &mut rex_prefix, &mut modrm, &mut opcode).or_else(|| put_simd_size(false)),
                (Operand::XMMRegister(r), EncodingArm::VX) => {
                    put_vex_reg(*r); put_simd_size(false)},
                (Operand::YMMRegister(r), EncodingArm::MY) => 
                    put_rm_reg(*r, &mut rex_prefix, &mut modrm).or_else(|| put_simd_size(true)),
                (Operand::YMMRegister(r), EncodingArm::RY) => 
                    put_reg(*r, &mut rex_prefix, &mut modrm, &mut opcode).or_else(|| put_simd_size(true)),
                (Operand::YMMRegister(r), EncodingArm::VY) => {
                    put_vex_reg(*r); put_simd_size(true)},
                _ => panic!("unreachable")
            } {errors.push(e);}}


            // dbg!(rex_prefix, &modrm, &immediate, vex_map_pp, vex_vvvv, rip_relative, opcode);

            let mut instruction: [u8;16] = [0;16];
            match (&op_size_bytes, &immediate) {
                (None, Some(Literal::I8(_)|Literal::U8(_))) => op_size_bytes = Some(1),
                (None, Some(Literal::I16(_)|Literal::U16(_))) => op_size_bytes = Some(2),
                (None, Some(Literal::I32(_)|Literal::U32(_)|Literal::F32(_))) => op_size_bytes = Some(4),
                _ => {}
            }
            match op_size_bytes {
                Some(2) if immediate_type & NO_16BIT != 0 => return Err("this instruction does not allow a 16-bit operand size.".into()),
                Some(2) => if let Some(immediate) = &immediate {
                    immediate_vec.extend_from_slice(& match immediate.into_type((Literal::U16_M | Literal::I16_M) & immediate_type, false)? {
                        Literal::U16(x) => x.to_le_bytes(),
                        Literal::I16(x) => x.to_le_bytes(),
                        _ => panic!("unreachable")
                    });
                }
                Some(4) => if let Some(immediate) = &immediate {
                    immediate_vec.extend_from_slice(& match immediate.into_type((Literal::U32_M | Literal::I32_M | Literal::F32_M) & immediate_type, false)? {
                        Literal::U32(x) => x.to_le_bytes(),
                        Literal::I32(x) => x.to_le_bytes(),
                        Literal::F32(x) => x.to_le_bytes(),
                        _ => panic!("unreachable")
                    });
                }
                Some(8) => {
                    if rex_prefix == 0xff {errors.push(E_REX); continue 'outer} else {rex_prefix |= 8}
                    if let Some(immediate) = immediate {
                        if add_register_to_opcode.is_some() {
                            immediate_vec.extend_from_slice(& match immediate.into_type((Literal::U64_M | Literal::I64_M | Literal::F64_M) & immediate_type, false)? {
                                Literal::U64(x) => x.to_le_bytes(),
                                Literal::I64(x) => x.to_le_bytes(),
                                Literal::F64(x) => x.to_le_bytes(),
                                _ => panic!("unreachable")
                            });
                        } else {
                            immediate_vec.extend_from_slice(& match immediate.into_type((Literal::U32_M | Literal::I32_M | Literal::F32_M) & immediate_type, false)? {
                                Literal::U32(x) => x.to_le_bytes(),
                                Literal::I32(x) => x.to_le_bytes(),
                                Literal::F32(x) => x.to_le_bytes(),
                                _ => panic!("unreachable")
                            });
                        }
                    }
                }
                _ => {}
            }
            
            // prefixes
            if vex_map_pp.is_some() || vex_vvvv.is_some() {
                if rex_prefix == 0xff {errors.push(E_VEX); continue 'outer}
                let vex_byte_2 = !rex_prefix << 5 | vex_map_pp.unwrap_or(1) & 0x1f;
                let vex_byte_3 = 0x80 & (!rex_prefix << 4) | !vex_vvvv.unwrap() << 3 
                    | if simd_size_256.unwrap() {4} else {0} | (vex_map_pp.unwrap_or(1) >> 6);
                if vex_byte_2 & 0x7f == 0x61 && vex_byte_3 & 0x80 == 0 {
                    instruction[1] = 0xc5;
                    instruction[2] = 0x80 & vex_byte_2 | 0x7f & vex_byte_3;
                    instruction[0] = 2;
                } else {
                    instruction[1] = 0xc4;
                    instruction[2] = vex_byte_2;
                    instruction[3] = vex_byte_3;
                    instruction[0] = 3;
                }
            } else {
                for c in opcode.iter_mut() {
                    if let 0xf0 | 0xf2 | 0xf3 | 0x2e | 0x36 | 0x26 | 0x64 | 0x65 | 0x3e | 0x66 | 0x67 = *c {
                        instruction[1 + instruction[0] as usize] = *c;
                        instruction[0] += 1;
                        *c = 0x4f;
                    }
                };
                if rex_prefix != 0 && rex_prefix != 0xff {
                    instruction[1 + instruction[0] as usize] = rex_prefix | 0x40;
                    instruction[0] += 1;
                }
                if let Some(2) = op_size_bytes {
                    instruction[1 + instruction[0] as usize] = 0x66;
                    instruction[0] += 1;
                }
            }
            // opcode, MODRM, immediate
            for op in opcode[i..].iter().filter(|op| **op & 0xf0 != 0x40).chain(modrm.iter()).chain(immediate_vec.iter()) {
                if instruction[0] == 15 {errors.push(E_15); continue 'outer}
                instruction[1 + instruction[0] as usize] = *op;
                instruction[0] += 1;
            }
            if rip_relative {instruction[0] |= (instruction[0] - 4 - immediate_vec.len() as u8) << 4}
            // for c in &instruction[1..(1+instruction[0] as usize & 0xf)] {print!("{:b} ", c)}
            // print!("   {}\n", instruction[0] >> 4);
            // print!("pushing an encoding.\n");
            encodings.push(instruction);
        }
    }
    if encodings.len() == 0 {
        if errors.len() == 1 {return Err(errors[0][1..].to_string());}
        let mut err = "all possible encodings failed:".to_owned();
        for e in errors {err += e;}
        return Err(err);
    }
    Ok(encodings.into_iter().min_by_key(|enc| enc[0]).unwrap())
}

