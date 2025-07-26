use super::{LabelContext, encode_with_operands};
use crate::encode::{try_encode, EncodingArm, Operand, IMM_UNSIGNED, IMM_INT, IMM_ANY};
use crate::parse::Literal;

pub fn encode(mnemonic: &[u8], operands: &[u8], labels: &mut LabelContext) -> Result<Option<(usize, [u8;16])>, String> {
    match mnemonic {
        b"ADC" | b"adc" => encode_with_operands::<2>(&[
            EncodingArm::new("amr8 imr8 : 14--80/2-10-12-"), 
            EncodingArm::new("amr* i8imr* : -15--83/2 81/2-11--13-")], operands, labels, IMM_INT).map(Some),
        b"ADD" | b"add" => encode_with_operands::<2>(&[
            EncodingArm::new("amr8 imr8 : 04--80/0-00-02-"), 
            EncodingArm::new("amr* i8imr* : -05--83/0 81/0-01--03-")], operands, labels, IMM_INT).map(Some),
        b"AND" | b"and" => encode_with_operands::<2>(&[
            EncodingArm::new("amr8 imr8 : 24--80/4-20-22-"), 
            EncodingArm::new("amr* i8imr* : -25--83/4 81/2-21--23-")], operands, labels, IMM_INT).map(Some),
        b"BSF" | b"bsf" => encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0fbc")], operands, labels, 0).map(Some),
        b"BSWAP" | b"bswap" => match Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())? {
            (n,Operand::DWordRegister(r)) => Ok(Some((n, if r > 7 {[3,0x44, 0x0f, 0xc0 + r, 0,0,0,0,0,0,0,0,0,0,0,0]} else {[2, 0x0f, 0xc8+r, 0,0,0,0,0,0,0,0,0,0,0,0,0]}))),
            (n, Operand::QWordRegister(r)) => Ok(Some((n, [3,0x48 + (r >> 1) & 4, 0x0f, 0xc8 | r, 0,0,0,0,0,0,0,0,0,0,0,0]))),
            _ => Err("invalid operand type for 'BSWAP', expected 32- or 64-bit register name.".into())
        },
        b"BT" | b"bt" => encode_with_operands::<2>(&[
            EncodingArm::new("m* i8r* : 0fba/4 0fa3")], operands, labels, IMM_UNSIGNED).map(Some),
        b"BTC" | b"btc" => encode_with_operands::<2>(&[
            EncodingArm::new("m* i8r* : 0fba/7 0fbb")], operands, labels, IMM_UNSIGNED).map(Some),
        b"BTR" | b"btr" => encode_with_operands::<2>(&[
            EncodingArm::new("m* i8r* : 0fba/6 0fb3")], operands, labels, IMM_UNSIGNED).map(Some),
        b"BTS" | b"bts" => encode_with_operands::<2>(&[
            EncodingArm::new("m* i8r* : 0fba/5 0fab")], operands, labels, IMM_UNSIGNED).map(Some),
        b"CALL" | b"call" => if let Ok((n,op1)) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed()) {
            let op = match op1 {
                Operand::QWordRegister(r) => Operand::DWordRegister(r),
                Operand::QWordPtr(mem) | Operand::Ptr(mem) => Operand::DWordPtr(mem),
                _ => return Err("invalid operand type for 'CALL', expected 64-bit register/memory location or label.".into())
            };
            Ok(Some((n,try_encode(&[op], &[EncodingArm::new("m* : ff/2")], 0)?)))
        } else {
            let i = operands.iter().take_while(|b| **b == b' ' || **b == b'\t').count();
            let j = i + operands[i..].iter().take_while(|b| crate::parse::alpha(**b)).count();
            let mut result = [0x15,0xe8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0];
            result[2..].iter_mut().zip((labels.index(&operands[i..j]) as u32).to_le_bytes()).for_each(|(w,b)| *w = b);
            Ok(Some((j, result)))
        }, // if you need far calls, do them yourself
        b"CBW" | b"cbw" => Ok(Some((0, [2, 0x66, 0x98, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CDQ" | b"cdq" => Ok(Some((0, [1, 0x99, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CLC" | b"clc" => Ok(Some((0, [1,0xf8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CLD" | b"cld" => Ok(Some((0, [1,0xfc, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CLI" | b"cli" => Ok(Some((0, [1,0xfa, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CLTS" | b"clts" => Ok(Some((0, [2,0x0f, 0x06,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CMC" | b"cmc" => Ok(Some((0, [1,0xf5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CMOVA" | b"cmova" | b"CMOVNBE" | b"cmovnbe"=> encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f47")], operands, labels,0).map(Some),
        b"CMOVAE" | b"cmovae" | b"CMOVNB" | b"cmovnb" | b"CMOVNC" | b"cmovnc" => encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f43")], operands, labels,0).map(Some),
        b"CMOVB" | b"cmovb" | b"CMOVC" | b"cmovc" | b"CMOVNAE" | b"cmovnae" => encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f42")], operands, labels,0).map(Some),
        b"CMOVBE" | b"cmovbe" | b"CMOVNA" | b"cmovna"=> encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f46")], operands, labels,0).map(Some),
        b"CMOVE" | b"cmove" | b"CMOVZ" | b"cmovz"=> encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f44")], operands, labels,0).map(Some),
        b"CMOVG" | b"cmovg" | b"CMOVNLE" | b"cmovnle"=> encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f4f")], operands, labels,0).map(Some),
        b"CMOVGE" | b"cmovge" | b"CMOVNL" | b"cmovnl"=> encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f4d")], operands, labels,0).map(Some),
        b"CMOVL" | b"cmovl" | b"CMOVNGE" | b"cmovnge"=> encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f4c")], operands, labels,0).map(Some),
        b"CMOVLE" | b"cmovle" | b"CMOVNG" | b"cmovng"=> encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f4e")], operands, labels,0).map(Some),
        b"CMOVNE" | b"cmovne" | b"CMOVNZ" | b"cmovnz"=> encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f45")], operands, labels,0).map(Some),
        b"CMOVNO" | b"cmovno" => encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f41")], operands, labels,0).map(Some),
        b"CMOVNP" | b"cmovnp" | b"CMOVPO" | b"cmovpo"=> encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f4b")], operands, labels,0).map(Some),
        b"CMOVNS" | b"cmovns" => encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f49")], operands, labels,0).map(Some),
        b"CMOVO" | b"cmovo" => encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f40")], operands, labels,0).map(Some),
        b"CMOVP" | b"cmovp" | b"CMOVPE" | b"cmovpe"=> encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f4a")], operands, labels,0).map(Some),
        b"CMOVS" | b"cmovs" => encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 0f48")], operands, labels,0).map(Some),
        b"CMP" | b"cmp" => encode_with_operands::<2>(&[
            EncodingArm::new("amr8 imr8 : 3c--80/7-38-3a-"),
            EncodingArm::new("amr* i8imr* : 3d---83/7 81/7-39--3b-")], operands, labels,IMM_ANY).map(Some),
        b"CMPS" | b"cmps" => Err("'CMPS' with operands is not supported, use 'CMPSB'|'CMPSW'|'CMPSD'|'CMPSQ' instead.".into()),
        b"CMPSB" | b"cmpsb" => Ok(Some((0, [1,0xa6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        // b"CMPSD" | b"cmpsd" => Ok(Some((0, [1,0xa7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        // not a useful instruction, so provide only the SSE2 instruction of the same name.
        b"CMPSW" | b"cmpsw" => Ok(Some((0, [2,0x66, 0xa7, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CMPXCHG" | b"cmpxchg" => encode_with_operands::<2>(&[
            EncodingArm::new("m8m* r8r* : 0fb0--0fb1")], operands, labels,0).map(Some),
        b"CMPXCHG8B" | b"cmpxchg8b" => encode_with_operands::<1>(&[
            EncodingArm::new("m8 : 0fc7/1")], operands, labels,0).map(Some), 
        // this allows you to put 'BYTE PTR' or even a register, but not 'QWORD PTR', but w.e, should use 'PTR [...]'
        b"CPUID" | b"cpuid" => Ok(Some((0, [2,0x0f, 0xa2,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CWD" | b"cwd" => Ok(Some((0, [2,0x66, 0x99,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CWDE" | b"cwde" => Ok(Some((0, [1,0x98, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"DEC" | b"dec" => encode_with_operands::<1>(&[
            EncodingArm::new("m8m* : fe/1 ff/1")], operands, labels,0).map(Some),
        b"DIV" | b"div" => encode_with_operands::<1>(&[
            EncodingArm::new("m8m* : f6/6 f7/6")], operands, labels,0).map(Some),
        b"ENTER" | b"enter" => {
            let i = operands.iter().take_while(|b| **b == b' ' || **b == b'\t').count();
            let Some((j,l1)) = Literal::parse(&operands[i..]) else {return Err("expected u16 literal.".into())};
            let Literal::U16(x) = l1.into_type(Literal::U16_M, false)? else {panic!("unreachable")};

            let k = j + operands[j..].iter().take_while(|b| **b == b' ' || **b == b'\t').count();
            let Some((l,l2)) = Literal::parse(&operands[k..]) else {return Err("expected u8 literal.".into())};
            let Literal::U8(y) = l2.into_type(Literal::U8_M, false)? else {panic!("unreachable")};
            Ok(Some((l, [4, 0xc8, x as u8, (x >> 8) as u8, y as u8, 0,0,0,0,0,0,0,0,0,0,0])))
        },
        b"HLT" | b"hlt" => Ok(Some((0, [1,0xf4, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"IDIV" | b"idiv" => encode_with_operands::<1>(&[
            EncodingArm::new("m8m* : f6/7 f7/7")], operands, labels,0).map(Some),
        b"IMUL" | b"imul" => {
            let (operands_vec,i) = read_operands(operands, labels);
            Ok(Some((i, try_encode(&operands_vec, &[
                EncodingArm::new("m8m* : f6/5 f7/5"),
                EncodingArm::new("r* m* : 0faf"),
                EncodingArm::new("r* m* i8i* : 6b 69")],0)?)))
        },
        b"IN" | b"in" => {
            let (n, op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m, op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            match (op1, op2) {
                (Operand::LowByteRegister(0), Operand::Immediate(i)) => if let Literal::U8(x) = i.into_type(Literal::U8_M, false)? 
                    {Ok(Some((n+m,[2, 0xe4, x, 0,0,0,0,0,0,0,0,0,0,0,0,0])))} else {panic!("unreachable")},
                (Operand::WordRegister(0), Operand::Immediate(i)) => if let Literal::U8(x) = i.into_type(Literal::U8_M, false)? 
                    {Ok(Some((n+m,[3, 0x66, 0xe5, x, 0,0,0,0,0,0,0,0,0,0,0,0])))} else {panic!("unreachable")},
                (Operand::DWordRegister(0), Operand::Immediate(i)) => if let Literal::U8(x) = i.into_type(Literal::U8_M, false)? 
                    {Ok(Some((n+m,[2, 0xe5, x, 0,0,0,0,0,0,0,0,0,0,0,0,0])))} else {panic!("unreachable")},
                (Operand::LowByteRegister(0), Operand::WordRegister(2)) =>
                    Ok(Some((n+m,[1, 0xec, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
                (Operand::WordRegister(0), Operand::WordRegister(2)) =>
                    Ok(Some((n+m,[2, 0x66, 0xed, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
                (Operand::DWordRegister(0), Operand::WordRegister(2)) =>
                    Ok(Some((n+m,[1, 0xed, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
                _ => Err("invalid operand types".into())
            }
        },
        b"INC" | b"inc" => encode_with_operands::<1>(&[
            EncodingArm::new("m8m* : fe/0 ff/0")], operands, labels,0).map(Some),
        b"INSB" | b"insb" => Ok(Some((0, [1, 0x6c, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"INSW" | b"insw" => Ok(Some((0, [2, 0x66, 0x6d, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"INT" | b"int" => {
            let (n, op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            match op1 {
                Operand::Immediate(i) => if let Literal::U8(x) = i.into_type(Literal::U8_M, false)? 
                    {Ok(Some((n, [2, 0xcd, x, 0,0,0,0,0,0,0,0,0,0,0,0,0])))} else {panic!("unreachable")},
                _ => Err("invalid operand type for 'INT', expected unsigned 8-bit immediate".into())
            }
        },
        b"INT1" | b"int1" => Ok(Some((0, [1, 0xf1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"INT3" | b"int3" => Ok(Some((0, [1, 0xcc, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"INTO" | b"into" => Ok(Some((0, [1, 0xce, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"INVD" | b"invd" => Ok(Some((0, [2, 0x0f, 0x08, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"IRET" | b"iret" => Ok(Some((0, [2, 0x66, 0xcf, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"IRETD" | b"iretd" => Ok(Some((0, [1,  0xcf, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"JMP" | b"jmp" => {
            if let Ok((n,op)) = Operand::read(&operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed()) { 
                Ok(Some((n, try_encode(&[match op {
                    Operand::QWordPtr(mem) | Operand::Ptr(mem) => Operand::DWordPtr(mem),
                    Operand::QWordRegister(r) => Operand::DWordRegister(r),
                    _ => return Err("wrong operand type".into())
                }], &[EncodingArm::new("m* : ff/4")],0)?)))
            } else {
                let i = operands.iter().take_while(|b| **b == b' ' || **b == b'\t').count();
                let j = i + operands[i..].iter().take_while(|b| crate::parse::alpha(**b)).count();
                let mut result = [0x15,0xe9,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
                result[2..].iter_mut().zip(labels.index(&operands[i..j]).to_le_bytes()).for_each(|(w,b)| *w = b);
                Ok(Some((j, result)))
            }
        },
        b"JA" | b"ja" | b"JNBE" | b"jnbe" => encode_jcc(operands, labels, 0x87),
        b"JAE" | b"jae" | b"JNB" | b"jnb" | b"JNC" | b"jnc" => encode_jcc(operands, labels, 0x83),
        b"JB" | b"jb" | b"JC" | b"jc" | b"JNAE" | b"jnae" => encode_jcc(operands, labels, 0x82),
        b"JBE" | b"jbe" | b"JNA" | b"jna" => encode_jcc(operands, labels, 0x86),
        b"JE" | b"je" | b"JZ" | b"jz" => encode_jcc(operands, labels, 0x84),
        b"JG" | b"jg" | b"JNLE" | b"jnle" => encode_jcc(operands, labels, 0x8f),
        b"JGE" | b"jge" | b"JNL" | b"jnl" => encode_jcc(operands, labels, 0x8d),
        b"JL" | b"jl" | b"JNGE" | b"jnge" => encode_jcc(operands, labels, 0x8c),
        b"JLE" | b"jle" | b"JNG" | b"jng" => encode_jcc(operands, labels, 0x8e),
        b"JNE" | b"jne" | b"JNZ" | b"jnz" => encode_jcc(operands, labels, 0x85),
        b"JNO" | b"jno" => encode_jcc(operands, labels, 0x81),
        b"JNP" | b"jnp" | b"JPO" | b"jpo" => encode_jcc(operands, labels, 0x8b),
        b"JNS" | b"jns" => encode_jcc(operands, labels, 0x89),
        b"JO" | b"jo" => encode_jcc(operands, labels, 0x80),
        b"JP" | b"jp" | b"JPE" | b"jpe" => encode_jcc(operands, labels, 0x8a),
        b"JS" | b"js" => encode_jcc(operands, labels, 0x88),
        b"LAHF" | b"lahf" => Ok(Some((0, [1,  0x9f, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"LEA" | b"lea" => encode_with_operands::<2>(&[
            EncodingArm::new("r* m* : 8d")], operands, labels,0).map(Some),
        b"LEAVE" | b"leave" => Ok(Some((0, [1,  0xc9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"LODS" | b"lods" => Err("'LODS' with operand is not supported, use 'LODSB'|'LODSW'|'LODSB'|'LODSQ' instead.".into()),
        b"LODSB" | b"lodsb" => Ok(Some((0, [1,  0xac, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"LODSD" | b"lodsd" => Ok(Some((0, [1,  0xad, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"LODSW" | b"lodsw" => Ok(Some((0, [2,  0x66, 0xad, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"MOV" | b"mov" => encode_with_operands::<2>(&[
            EncodingArm::new("mr8 imr8 : c6/0-88 b0+ 8a-"),
            EncodingArm::new("mr* imr* : c7/0-89 b8+ 8b-")], operands, labels, IMM_ANY).map(Some),
        b"MOVS" | b"movs" => Err("'MOVS' with operand is not supported, use 'MOVSB'|'MOVSW'|'MOVSB'|'MOVSQ' instead.".into()),
        b"MOVSB" | b"movsb" => Ok(Some((0, [1, 0xa4, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        // b"MOVSD" | b"movsd" => Ok(Some((0, [1, 0xa5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        // the SSE2 instruction is more important
        b"MOVSW" | b"movsw" => Ok(Some((0, [2,  0x66, 0xa5, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"MOVSX" | b"movsx" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m, match (op1, op2) {
                (Operand::WordRegister(r), Operand::LowByteRegister(s)) if r | s < 8 =>
                    [4, 0x66, 0x0f, 0xbe, 0xc0 | (r << 3) | s,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::WordRegister(r), Operand::LowByteRegister(s)) =>
                    [5, 0x66, 0x40 | 4 & (r >> 1) | (s >> 3), 0x0f, 0xbe, 0xc0 | 0x38 & (r << 3) | 7 & s,0,0,0,0,0,0,0,0,0,0],
                (Operand::DWordRegister(r), Operand::LowByteRegister(s)) if r | s < 8 =>
                    [3, 0x0f, 0xbe, 0xc0 | (r << 3) | s,0,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::DWordRegister(r), Operand::LowByteRegister(s)) =>
                    [4, 0x40 | 4 & (r >> 1) | (s >> 3), 0x0f, 0xbe, 0xc0 | 0x38 & (r << 3) | 7 & s,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::QWordRegister(r), Operand::LowByteRegister(s)) =>
                    [4, 0x48 | 4 & (r >> 1) | (s >> 3), 0x0f, 0xbe, 0xc0 | 0x38 & (r << 3) | 7 & s,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::WordRegister(r), Operand::HighByteRegister(s)) if r < 8 =>
                    [4, 0x66, 0x0f, 0xbe, 0xc4 | (r << 3) | s,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::DWordRegister(r), Operand::HighByteRegister(s)) if r < 8 =>
                    [3, 0x0f, 0xbe, 0xc4 | (r << 3) | s,0,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::DWordRegister(r), Operand::WordRegister(s)) if r | s < 8 => 
                    [3, 0x0f, 0xbf, 0xc0 | 0x38 & (r << 3) | 7 & (s), 0,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::DWordRegister(r), Operand::WordRegister(s)) => 
                    [4, 0x40 | 4 & (r >> 1) | (s >> 3), 0x0f, 0xbf, 0xc0 | 0x38 & (r << 3) | 7 & (s), 0,0,0,0,0,0,0,0,0,0,0],
                (Operand::QWordRegister(r), Operand::WordRegister(s)) => 
                    [4, 0x48 | 4 & (r >> 1) | (s >> 3), 0x0f, 0xbf, 0xc0 | 0x38 & (r << 3) | 7 & (s), 0,0,0,0,0,0,0,0,0,0,0],
                (Operand::QWordRegister(r), Operand::DWordRegister(s)) => 
                    [3, 0x48 | 4 & (r >> 1) | (s >> 3), 0x63, 0xc0 | 0x38 & (r << 3) | 7 & (s), 0,0,0,0,0,0,0,0,0,0,0,0],
                (op1, Operand::BytePtr(mem)) => try_encode(&[op1, Operand::Ptr(mem)], &[
                    EncodingArm::new("r* m* : 0fbe")],0)?,
                (op1, Operand::WordPtr(mem)) => try_encode(&[op1, Operand::Ptr(mem)], &[
                    EncodingArm::new("r* m* : 0fbf")],0)?,
                (Operand::QWordRegister(r), Operand::DWordPtr(mem)) => try_encode(&[Operand::QWordRegister(r), Operand::Ptr(mem)], &[
                    EncodingArm::new("r* m* : 0fbe")],0)?,
                _ => return Err("wrong operand types.".into())
            })))
        },
        b"MOVZX" | b"movzx" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m, match (op1, op2) {
                (Operand::WordRegister(r), Operand::LowByteRegister(s)) if r | s < 8 =>
                    [4, 0x66, 0x0f, 0xb6, 0xc0 | (r << 3) | s,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::WordRegister(r), Operand::LowByteRegister(s)) =>
                    [5, 0x66, 0x40 | 4 & (r >> 1) | (s >> 3), 0x0f, 0xb6, 0xc0 | 0x38 & (r << 3) | 7 & s,0,0,0,0,0,0,0,0,0,0],
                (Operand::DWordRegister(r), Operand::LowByteRegister(s)) if r | s < 8 =>
                    [3, 0x0f, 0xb6, 0xc0 | (r << 3) | s,0,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::DWordRegister(r), Operand::LowByteRegister(s)) =>
                    [4, 0x40 | 4 & (r >> 1) | (s >> 3), 0x0f, 0xb6, 0xc0 | 0x38 & (r << 3) | 7 & s,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::QWordRegister(r), Operand::LowByteRegister(s)) =>
                    [4, 0x48 | 4 & (r >> 1) | (s >> 3), 0x0f, 0xb6, 0xc0 | 0x38 & (r << 3) | 7 & s,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::WordRegister(r), Operand::HighByteRegister(s)) if r < 8 =>
                    [4, 0x66, 0x0f, 0xb6, 0xc4 | (r << 3) | s,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::DWordRegister(r), Operand::HighByteRegister(s)) if r < 8 =>
                    [3, 0x0f, 0xb6, 0xc4 | (r << 3) | s,0,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::DWordRegister(r), Operand::WordRegister(s)) if r | s < 8 => 
                    [3, 0x0f, 0xb7, 0xc0 | 0x38 & (r << 3) | 7 & (s), 0,0,0,0,0,0,0,0,0,0,0,0],
                (Operand::DWordRegister(r), Operand::WordRegister(s)) => 
                    [4, 0x40 | 4 & (r >> 1) | (s >> 3), 0x0f, 0xb7, 0xc0 | 0x38 & (r << 3) | 7 & (s), 0,0,0,0,0,0,0,0,0,0,0],
                (Operand::QWordRegister(r), Operand::WordRegister(s)) => 
                    [4, 0x48 | 4 & (r >> 1) | (s >> 3), 0x0f, 0xb7, 0xc0 | 0x38 & (r << 3) | 7 & (s), 0,0,0,0,0,0,0,0,0,0,0],
                (op1, Operand::BytePtr(mem)) => try_encode(&[op1, Operand::Ptr(mem)], &[
                    EncodingArm::new("r* m* : 0fb6")],0)?,
                (op1, Operand::WordPtr(mem)) => try_encode(&[op1, Operand::Ptr(mem)], &[
                    EncodingArm::new("r* m* : 0fb7")],0)?,
                _ => return Err("wrong operand types.".into())
            })))
        },
        b"MUL" | b"mul" => encode_with_operands::<1>(&[
            EncodingArm::new("m8m* : f6/4 f7/4")], operands, labels,0).map(Some),
        b"NEG" | b"neg" => encode_with_operands::<1>(&[
            EncodingArm::new("m8m* : f6/3 f7/3")], operands, labels,0).map(Some),
        b"NOT" | b"not" => encode_with_operands::<1>(&[
            EncodingArm::new("m8m* : f6/2 f7/2")], operands, labels,0).map(Some),
        b"OR" | b"or" => encode_with_operands::<2>(&[
            EncodingArm::new("amr8 imr8 : 0c--80/1-08-0a-"), 
            EncodingArm::new("amr* i8imr* : -0d--83/1 81/1-09--0b-")], operands, labels,IMM_INT).map(Some),
        b"OUT" | b"out" => {
            let i = operands.iter().take_while(|b| **b == b' ' || **b == b'\t').count();
            if let Some((j,literal)) = Literal::parse(&operands[i..]) {
                let Literal::U8(x) = literal.into_type(Literal::U8_M, false)? else {panic!("unreachable")};
                let k = i+j+operands[i+j..].iter().take_while(|b| **b == b' ' || **b == b'\t').count();
                let l = k + operands[k..].iter().take_while(|b| crate::parse::alpha(**b)).count();
                return match &operands[k..l] {
                    b"AL" | b"al" => Ok(Some((l, [2, 0xe6, x as u8, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
                    b"AX" | b"ax" => Ok(Some((l, [3, 0x66, 0xe7, x as u8, 0,0,0,0,0,0,0,0,0,0,0,0]))),
                    b"EAX" | b"eax" => Ok(Some((l, [2, 0xe7, x as u8, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
                    _ => Err("bad operand type.".into())
                }
            }
            let j = i + operands[i..].iter().take_while(|b| crate::parse::alpha(**b)).count();
            let k = j + operands[j..].iter().take_while(|b| **b == b' ' || **b == b'\t').count();
            let l = k + operands[k..].iter().take_while(|b| crate::parse::alpha(**b)).count();
            match (&operands[i..j],&operands[k..l]) {
                (b"DX" | b"dx", b"AL" | b"al") => Ok(Some((l, [1, 0xee, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
                (b"DX" | b"dx", b"AX" | b"ax") => Ok(Some((l, [2, 0x66, 0xef, 0, 0,0,0,0,0,0,0,0,0,0,0,0]))),
                (b"DX" | b"dx", b"EAX" | b"eax") => Ok(Some((l, [1, 0xef, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
                _ => Err("bad operand type.".into())
            }
        },
        b"OUTSB" | b"outsb" => Ok(Some((0, [1, 0x6e, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"OUTSD" | b"outsd" => Ok(Some((0, [1, 0x6f, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"OUTSW" | b"outsw" => Ok(Some((0, [2,  0x66, 0x6f, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"POP" | b"pop" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n,try_encode(&[match op1 {
                Operand::WordRegister(r) => Operand::WordRegister(r),
                Operand::WordPtr(mem) => Operand::WordPtr(mem),
                Operand::QWordRegister(r) => Operand::DWordRegister(r),
                Operand::QWordPtr(mem) => Operand::DWordPtr(mem),
                _ => return Err("wrong operand type to 'POP', expected 16- or 64-bit register or pointer.".into())
            }], &[EncodingArm::new("mr* : 8f/0 58+")], 0)?)))
        },
        // does not do POP FS, POP GS
        b"POPF" | b"popf" => Ok(Some((0, [2, 0x66, 0x9d, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"PUSH" | b"push" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n,try_encode(&[match op1 {
                Operand::WordRegister(r) => Operand::WordRegister(r),
                Operand::WordPtr(mem) => Operand::WordPtr(mem),
                Operand::QWordRegister(r) => Operand::DWordRegister(r),
                Operand::QWordPtr(mem) => Operand::DWordPtr(mem),
                Operand::Immediate(literal) => Operand::Immediate(literal.into_type(Literal::U8_M|Literal::I8_M|Literal::U16_M|Literal::I16_M|Literal::U32_M|Literal::I32_M|Literal::F32_M, false)?),
                _ => return Err("wrong operand type to 'POP', expected 16- or 64-bit register or pointer.".into())
            }], &[EncodingArm::new("i8imr* : 6a 68 ff/6 50+")], IMM_ANY)?)))
        },
        // does not do PUSH FS, PUSH GS
        b"PUSHF" | b"pushf" => Ok(Some((0, [2, 0x66, 0x9c, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"RCL" | b"rcl" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m, match op2 {
                Operand::Immediate(Literal::Uany(1)) => try_encode(&[op1], &[EncodingArm::new("m8m* : d0/2 d1/2")],0)?,
                Operand::LowByteRegister(1) => try_encode(&[op1], &[EncodingArm::new("m8m* : d2/2 d3/2")],0)?,
                other => try_encode(&[op1, other], &[EncodingArm::new("m8m* i8 : c0/2 c1/2")],0)?
            })))
        },
        b"RCR" | b"rcr" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m, match op2 {
                Operand::Immediate(Literal::Uany(1)) => try_encode(&[op1], &[EncodingArm::new("m8m* : d0/3 d1/3")],0)?,
                Operand::LowByteRegister(1) => try_encode(&[op1], &[EncodingArm::new("m8m* : d2/3 d3/3")],0)?,
                other => try_encode(&[op1, other], &[EncodingArm::new("m8m* i8 : c0/3 c1/3")],0)?
            })))
        },
        b"RDPMC" | b"rdpmc" => Ok(Some((0, [2, 0x0f, 0x33, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"RDTSC" | b"rdtsc" => Ok(Some((0, [2, 0x0f, 0x31, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"RET" | b"ret" => Ok(Some((0, [1, 0xc3, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        // no far return, no (far) RET imm16
        b"ROL" | b"rol" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m, match op2 {
                Operand::Immediate(Literal::Uany(1)) => try_encode(&[op1], &[EncodingArm::new("m8m* : d0/0 d1/0")],0)?,
                Operand::LowByteRegister(1) => try_encode(&[op1], &[EncodingArm::new("m8m* : d2/0 d3/0")],0)?,
                other => try_encode(&[op1, other], &[EncodingArm::new("m8m* i8 : c0/0 c1/0")],0)?
            })))
        },
        b"ROR" | b"ror" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m, match op2 {
                Operand::Immediate(Literal::Uany(1)) => try_encode(&[op1], &[EncodingArm::new("m8m* : d0/1 d1/1")],0)?,
                Operand::LowByteRegister(1) => try_encode(&[op1], &[EncodingArm::new("m8m* : d2/1 d3/1")],0)?,
                other => try_encode(&[op1, other], &[EncodingArm::new("m8m* i8 : c0/1 c1/1")],0)?
            })))
        },
        b"SAL" | b"sal" | b"SHL" | b"shl" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m, match op2 {
                Operand::Immediate(Literal::Uany(1)) => try_encode(&[op1], &[EncodingArm::new("m8m* : d0/4 d1/4")],0)?,
                Operand::LowByteRegister(1) => try_encode(&[op1], &[EncodingArm::new("m8m* : d2/4 d3/4")],0)?,
                other => try_encode(&[op1, other], &[EncodingArm::new("m8m* i8 : c0/4 c1/4")],0)?
            })))
        },
        b"SAR" | b"sar" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m, match op2 {
                Operand::Immediate(Literal::Uany(1)) => try_encode(&[op1], &[EncodingArm::new("m8m* : d0/7 d1/7")],0)?,
                Operand::LowByteRegister(1) => try_encode(&[op1], &[EncodingArm::new("m8m* : d2/7 d3/7")],0)?,
                other => try_encode(&[op1, other], &[EncodingArm::new("m8m* i8 : c0/7 c1/7")],0)?
            })))
        },
        b"SBB" | b"sbb" => encode_with_operands::<2>(&[
            EncodingArm::new("amr8 imr8 : 1c--80/3-18-1a-"), 
            EncodingArm::new("amr* i8imr* : -1d--83/3 81/3-19--1b-")], operands, labels, IMM_INT).map(Some),
        b"SCAS" | b"scas" => Err("'SCAS' with operand is not supported, use 'SCASB'|'SCASW'|'SCASD'|'SCASQ' instead.".into()),
        b"SCASB" | b"scasb" => Ok(Some((0, [1, 0xae, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"SCASD" | b"scasd" => Ok(Some((0, [1, 0xaf, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"SCASW" | b"scasw" => Ok(Some((0, [2,  0x66, 0xaf, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"SETA" | b"seta" | b"SETNBE" | b"setnbe" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f97")], operands, labels,0).map(Some),
        b"SETAE" | b"setae" | b"SETNB" | b"setnb" | b"SETNC" | b"setnc" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f93")], operands, labels,0).map(Some),
        b"SETB" | b"setb" | b"SETC" | b"setc" | b"SETNAE" | b"setnae" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f92")], operands, labels,0).map(Some),
        b"SETBE" | b"setbe" | b"SETNA" | b"setna" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f96")], operands, labels,0).map(Some),
        b"SETE" | b"sete" | b"SETZ" | b"setz" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f94")], operands, labels,0).map(Some),
        b"SETG" | b"setg" | b"SETNLE" | b"setnle" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f9f")], operands, labels,0).map(Some),
        b"SETGE" | b"setge" | b"SETNL" | b"setnl" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f9d")], operands, labels,0).map(Some),
        b"SETL" | b"setl" | b"SETNGE" | b"setnge" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f9c")], operands, labels,0).map(Some),
        b"SETLE" | b"setle" | b"SETNG" | b"setng" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f9e")], operands, labels,0).map(Some),
        b"SETNE" | b"setne" | b"SETNZ" | b"setnz" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f95")], operands, labels,0).map(Some),
        b"SETNO" | b"setno" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f91")], operands, labels,0).map(Some),
        b"SETNP" | b"setnp" | b"SETPO" | b"setpo" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f9b")], operands, labels,0).map(Some),
        b"SETNS" | b"setns" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f99")], operands, labels,0).map(Some),
        b"SETO" | b"seto" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f90")], operands, labels,0).map(Some),
        b"SETP" | b"setp" | b"SETPE" | b"setpe" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f9a")], operands, labels,0).map(Some),
        b"SETS" | b"sets" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f98")], operands, labels,0).map(Some),
        b"SHLD" | b"shld" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (l,op3) = Operand::read(&operands[n+m..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m+l , if let Operand::LowByteRegister(1) = op3 {
                try_encode(&[op1, op2], &[EncodingArm::new("m* r* : 0fa5")],0)?
            } else {
                try_encode(&[op1, op2, op3], &[EncodingArm::new("m* r* i8 : 0fa4")],IMM_UNSIGNED)?
            })))
        },
        b"SHR" | b"shr" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m, match op2 {
                Operand::Immediate(Literal::Uany(1)) => try_encode(&[op1], &[EncodingArm::new("m8m* : d0/5 d1/5")],0)?,
                Operand::LowByteRegister(1) => try_encode(&[op1], &[EncodingArm::new("m8m* : d2/5 d3/5")],0)?,
                other => try_encode(&[op1, other], &[EncodingArm::new("m8m* i8 : c0/5 c1/5")],IMM_UNSIGNED)?
            })))
        },
        b"SHRD" | b"shrd" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (l,op3) = Operand::read(&operands[n+m..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n+m+l , if let Operand::LowByteRegister(1) = op3 {
                try_encode(&[op1, op2], &[EncodingArm::new("m* r* : 0fad")],0)?
            } else {
                try_encode(&[op1, op2, op3], &[EncodingArm::new("m* r* i8 : 0fac")], IMM_UNSIGNED)?
            })))
        },
        b"SLDT" | b"sldt" => encode_with_operands::<1>(&[EncodingArm::new("m* : 0f00/0")], operands, labels,0).map(Some),
        b"STC" | b"stc" => Ok(Some((0, [1, 0xf9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"STD" | b"std" => Ok(Some((0, [1, 0xfd, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"STI" | b"sti" => Ok(Some((0, [1, 0xfb, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"STOS" | b"stos" => Err("'STOS' with operand is not supported, use 'STOSB'|'STOSW'|'STOSD'|'STOSQ' instead.".into()),
        b"STOSB" | b"stosb" => Ok(Some((0, [1, 0xaa, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"STOSD" | b"stosd" => Ok(Some((0, [1, 0xab, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"STOSW" | b"stosw" => Ok(Some((0, [2, 0x66, 0xab, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"SUB" | b"sub" => encode_with_operands::<2>(&[
            EncodingArm::new("amr8 imr8 : 2c--80/5-28-2a-"), 
            EncodingArm::new("amr* i8imr* : -2d--83/5 81/5-29--2b-")], operands, labels, IMM_INT).map(Some),
        b"SYSCALL" | b"syscall" => Ok(Some((0, [2,0x0f, 0x05, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"SYSENTER" | b"sysenter" => Ok(Some((0, [2,0x0f, 0x34, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"TEST" | b"test" => encode_with_operands::<2>(&[
            EncodingArm::new("am8 ir8 : a8-f6/0 84"), 
            EncodingArm::new("am* ir* : a9-f7/0 85")], operands, labels, IMM_ANY).map(Some),
        b"VERR" | b"verr" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n, match op1 {
                Operand::WordRegister(r) if r < 8 => [3, 0x0f, 0x00, 0xe0 | r, 0,0,0,0,0,0,0,0,0,0,0,0],
                Operand::WordRegister(r) => [4, 0x41, 0x0f, 0x00, 0xe0 | r & 7, 0,0,0,0,0,0,0,0,0,0,0],
                Operand::WordPtr(mem) | Operand::Ptr(mem) => 
                    try_encode(&[Operand::BytePtr(mem)], &[EncodingArm::new("m8 : 0f00/4")],0)?,
                _ => return Err("wrong operand type".into())
            })))
        },
        b"VERW" | b"verw" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n, match op1 {
                Operand::WordRegister(r) if r < 8 => [3, 0x0f, 0x00, 0xe8 | r, 0,0,0,0,0,0,0,0,0,0,0,0],
                Operand::WordRegister(r) => [4, 0x41, 0x0f, 0x00, 0xe8 | r & 7, 0,0,0,0,0,0,0,0,0,0,0],
                Operand::WordPtr(mem) | Operand::Ptr(mem) => 
                    try_encode(&[Operand::BytePtr(mem)], &[EncodingArm::new("m8 : 0f00/5")],0)?,
                _ => return Err("wrong operand type".into())
            })))
        },
        b"WBINVD" | b"wbinvd" => Ok(Some((0, [2, 0x0f, 0x09,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"XADD" | b"xadd" => encode_with_operands::<2>(&[
            EncodingArm::new("m8m* r8r* : 0fc0--0fc1")], operands, labels,0).map(Some),
        b"XCHG" | b"xchg" => encode_with_operands::<2>(&[
            EncodingArm::new("m8am* r8r* : 86--90-87"),
            EncodingArm::new("r8r* m8am* : 86---90 87")], operands, labels,0).map(Some),
        b"XLATB" | b"xlatb" => Ok(Some((0, [2, 0x48, 0xd7, 0,0,0,0,0,0,0,0,0,0,0,0,0]))), // should use the REX.W version in 64-bit mode
        b"XOR" | b"xor" => encode_with_operands::<2>(&[
            EncodingArm::new("amr8 imr8 : 34--80/6-30-32-"), 
            EncodingArm::new("amr* i8imr* : -35--83/6 81/6-31--33-")], operands, labels, IMM_INT).map(Some),
        _ => Ok(None)
    }
}
fn encode_jcc(operands: &[u8], labels: &mut LabelContext, opcode: u8) -> Result<Option<(usize, [u8;16])>, String> {
    let i = operands.iter().take_while(|b| **b == b' ' || **b == b'\t').count();
    let j = i + operands[i..].iter().take_while(|b| crate::parse::alpha(**b)).count();
    let mut result = [0x26,0x0f,opcode,0,0,0,0,0,0,0,0,0,0,0,0,0];
    result[3..].iter_mut().zip(labels.index(&operands[i..j]).to_le_bytes()).for_each(|(w,b)| *w = b);
    Ok(Some((j, result)))
}
fn read_operands(operands: &[u8], labels: &mut LabelContext) -> (Vec<Operand>, usize) {
    let mut i = 0;
    let mut operands_vec = Vec::new();
    while let Ok((n,op)) = Operand::read(&operands[i..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed()) {
        i += n;
        operands_vec.push(op);
    }
    (operands_vec, i)
}

