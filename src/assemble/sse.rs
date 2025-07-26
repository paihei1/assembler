use super::{LabelContext, encode_with_operands};
use crate::encode::{try_encode, EncodingArm, Operand, IMM_UNSIGNED, NO_16BIT};
use crate::parse::Literal;

pub fn encode(mnemonic: &[u8], operands: &[u8], labels: &mut LabelContext) -> Result<Option<(usize, [u8;16])>, String> {
    match mnemonic {
        // SSE
        b"ADDPS" | b"addps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f58")], operands, labels, 0).map(Some),
        b"ADDSS" | b"addss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f58")], operands, labels, 0).map(Some),
        b"ANDNPS" | b"andnps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f55")], operands, labels, 0).map(Some),
        b"ANDPS" | b"andps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f54")], operands, labels, 0).map(Some),
        b"CMPPS" | b"cmpps" => encode_with_operands::<3>(&[
            EncodingArm::new("rx mx i8 : 0fc2")], operands, labels, IMM_UNSIGNED).map(Some),
        b"CMPSS" | b"cmpss" => encode_with_operands::<3>(&[
            EncodingArm::new("rx mx i8 : f30fc2")], operands, labels, IMM_UNSIGNED).map(Some),
        b"COMISS" | b"comiss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f2f")], operands, labels, 0).map(Some),
        b"CVTPI2PS" | b"cvtpi2ps" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op2 {
                Operand::QWordPtr(mem) | Operand::Ptr(mem) => [op1, Operand::Ptr(mem)],
                _ => return Err("second operand to 'CVTPI2PS' should be 64-bit memory location.".into())
            }, &[EncodingArm::new("rx m8 : 0f2a")], 0).map(|x| Some((n+m, x)))
        }
        b"CVTSI2SS" | b"cvtsi2ss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx m* : f30f2a")], operands, labels, NO_16BIT).map(Some),
        b"CVTSS2SI" | b"cvtss2si" => encode_with_operands::<2>(&[
            EncodingArm::new("r* mx : f30f2d")], operands, labels, NO_16BIT).map(Some),
        b"CVTTSS2SI" | b"cvttss2si" => encode_with_operands::<2>(&[
            EncodingArm::new("r* mx : f30f2c")], operands, labels, NO_16BIT).map(Some),
        b"DIVPS" | b"divps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f5e")], operands, labels, 0).map(Some),
        b"DIVSS" | b"divss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f5e")], operands, labels, 0).map(Some),
        b"FXRSTOR" | b"fxrstor" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op1 {
                Operand::Ptr(mem) => [Operand::Ptr(mem)],
                _ => return Err("operand to 'FXRSTOR' should be memory location.".into())
            }, &[EncodingArm::new("m8 : 0fae/1")], 0).map(|x| Some((n, x)))
        },
        b"FXRSTOR64" | b"fxrstor64" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op1 {
                Operand::Ptr(mem) => [Operand::QWordPtr(mem)],
                _ => return Err("operand to 'FXRSTOR64' should be memory location.".into())
            }, &[EncodingArm::new("m* : 0fae/1")], 0).map(|x| Some((n, x)))
        },
        b"FXSAVE" | b"fxsave" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op1 {
                Operand::Ptr(mem) => [Operand::Ptr(mem)],
                _ => return Err("operand to 'FXSAVE' should be memory location.".into())
            }, &[EncodingArm::new("m8 : 0fae/0")], 0).map(|x| Some((n, x)))
        },
        b"FXSAVE64" | b"fxsave64" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op1 {
                Operand::Ptr(mem) => [Operand::QWordPtr(mem)],
                _ => return Err("operand to 'FXSAVE64' should be memory location.".into())
            }, &[EncodingArm::new("m* : 0fae/0")], 0).map(|x| Some((n, x)))
        },
        b"LDMXCSR" | b"ldmxcsr" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op1 {
                Operand::Ptr(mem) | Operand::DWordPtr(mem) => [Operand::DWordPtr(mem)],
                _ => return Err("operand to 'LDMXCSR' should be 32-bit memory location.".into())
            }, &[EncodingArm::new("m* : 0fae/2")], 0).map(|x| Some((n, x)))
        },
        b"MAXPS" | b"maxps" => encode_with_operands::<2>(&[EncodingArm::new("rx mx : 0f5f")], operands, labels, 0).map(Some),
        b"MAXSS" | b"maxss" => encode_with_operands::<2>(&[EncodingArm::new("rx mx : f30f5f")], operands, labels, 0).map(Some),
        b"MINPS" | b"minps" => encode_with_operands::<2>(&[EncodingArm::new("rx mx : 0f5d")], operands, labels, 0).map(Some),
        b"MINSS" | b"minss" => encode_with_operands::<2>(&[EncodingArm::new("rx mx : f30f5d")], operands, labels, 0).map(Some),
        b"MOVAPS" | b"movaps" => encode_with_operands::<2>(&[EncodingArm::new("rx mx : 0f28")], operands, labels, 0).map(Some),
        b"MOVHLPS" | b"movhlps" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op2 {
                Operand::XMMRegister(_) => [op1, op2],
                _ => return Err("second operand to 'MOVHLPS' should be XMM-register.".into())
            }, &[EncodingArm::new("rx mx : 0f12")], 0).map(|x| Some((n+m, x)))
        },
        b"MOVHPS" | b"movhps" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op2 {
                Operand::QWordPtr(mem) | Operand::Ptr(mem) => [op1, Operand::Ptr(mem)],
                _ => return Err("second operand to 'MOVHPS' should be 64-bit memory location.".into())
            }, &[EncodingArm::new("rx mx : 0f16")], 0).map(|x| Some((n+m, x)))
        },
        b"MOVLHPS" | b"movlhps" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op2 {
                Operand::XMMRegister(_) => [op1, op2],
                _ => return Err("second operand to 'MOVLHPS' should be XMM-register.".into())
            }, &[EncodingArm::new("rx mx : 0f16")], 0).map(|x| Some((n+m, x)))
        },
        b"MOVLPS" | b"movlps" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op2 {
                Operand::QWordPtr(mem) | Operand::Ptr(mem) => [op1, Operand::Ptr(mem)],
                _ => return Err("second operand to 'MOVLPS' should be 64-bit memory location.".into())
            }, &[EncodingArm::new("rx mx : 0f12")], 0).map(|x| Some((n+m, x)))
        },
        b"MOVMSKPS" | b"movmskps" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match (op1,op2) {
                (Operand::LowByteRegister(r) | Operand::WordRegister(r) | Operand::DWordRegister(r) | Operand::QWordRegister(r), Operand::XMMRegister(x)) => [Operand::LowByteRegister(r), Operand::XMMRegister(x)],
                _ => return Err("operands of 'MOVMSKPS' should be a general purpose register and an XMM-register.".into())
            }, &[EncodingArm::new("r8 mx : 0f50")], 0).map(|x| Some((n+m, x)))
        },
        b"MOVNTPS" | b"movntps" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op1 {
                Operand::Ptr(mem) => [Operand::Ptr(mem), op2],
                _ => return Err("first operand of 'MOVNTPS' should be memory location.".into())
            }, &[EncodingArm::new("mx rx : 0f2b")], 0).map(|x| Some((n+m, x)))
        },
        b"MOVSS" | b"movss" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx mrx : -f30f11 f30f10-")], operands, labels, 0).map(Some),
        b"MOVUPS" | b"movups" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx mrx : -0f11 0f10-")], operands, labels, 0).map(Some),
        b"MULPS" | b"mulps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f59")], operands, labels, 0).map(Some),
        b"MULSS" | b"mulss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f59")], operands, labels, 0).map(Some),
        b"ORPS" | b"orps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f56")], operands, labels, 0).map(Some),
        b"PREFETCHNTA" | b"prefetchnta" => encode_with_operands::<1>(&[
            EncodingArm::new("m8 : 0f18/0")], operands, labels, 0).map(Some),
        b"PREFETCH0" | b"prefetch0" => encode_with_operands::<1>(&[
            EncodingArm::new("m8 : 0f18/1")], operands, labels, 0).map(Some),
        b"PREFETCH1" | b"prefetch1" => encode_with_operands::<1>(&[
            EncodingArm::new("m8 : 0f18/2")], operands, labels, 0).map(Some),
        b"PREFETCH2" | b"prefetch2" => encode_with_operands::<1>(&[
            EncodingArm::new("m8 : 0f18/3")], operands, labels, 0).map(Some),
        b"RCPPS" | b"rcpps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f53")], operands, labels, 0).map(Some),
        b"RCPSS" | b"rcpss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f53")], operands, labels, 0).map(Some),
        b"RSQRTPS" | b"rsqrtps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f52")], operands, labels, 0).map(Some),
        b"RSQRTSS" | b"rsqrtss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f52")], operands, labels, 0).map(Some),
        b"SFENCE" | b"sfence" => Ok(Some((0, [3, 0x0f, 0xae, 0xf8, 0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"SHUFPS" | b"shufps" => encode_with_operands::<3>(&[
            EncodingArm::new("rx mx i8 : 0fc6")], operands, labels, IMM_UNSIGNED).map(Some),
        b"SQRTPS" | b"sqrtps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f51")], operands, labels, 0).map(Some),
        b"SQRTSS" | b"sqrtss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f51")], operands, labels, 0).map(Some),
        b"STMXCSR" | b"stmxcsr" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op1 {
                Operand::Ptr(mem) | Operand::DWordPtr(mem) => [Operand::DWordPtr(mem)],
                _ => return Err("operand to 'STMXCSR' should be 32-bit memory location.".into())
            }, &[EncodingArm::new("m* : 0fae/3")], 0).map(|x| Some((n, x)))
        },
        b"SUBPS" | b"subps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f5c")], operands, labels, 0).map(Some),
        b"SUBSS" | b"subss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f5c")], operands, labels, 0).map(Some),
        b"UCOMISS" | b"ucomiss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f2e")], operands, labels, 0).map(Some),
        b"UNPCKHPS" | b"unpckhps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f15")], operands, labels, 0).map(Some),
        b"UNPCKLPS" | b"unpcklps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f14")], operands, labels, 0).map(Some),
        b"XORPS" | b"xorps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f57")], operands, labels, 0).map(Some),

        // SSE2
        b"ADDPD" | b"addpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f58")], operands, labels, 0).map(Some),
        b"ADDSD" | b"addsd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f58")], operands, labels, 0).map(Some),
        b"ANDNPD" | b"andnpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f55")], operands, labels, 0).map(Some),
        b"ANDPD" | b"andpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f54")], operands, labels, 0).map(Some),
        b"CMPPD" | b"cmppd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx i8 : 660fc2")], operands, labels, 0).map(Some),
        b"CMPSD" | b"cmpsd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx i8 : f20fc2")], operands, labels, 0).map(Some),
        b"COMISD" | b"comisd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f2f")], operands, labels, 0).map(Some),
        b"CVTDQ2PD" | b"cvtdq2pd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30fe6")], operands, labels, 0).map(Some),
        b"CVTDQ2PS" | b"cvtdq2ps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f5b")], operands, labels, 0).map(Some),
        b"CVTPD2DQ" | b"cvtpd2dq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20fe6")], operands, labels, 0).map(Some),
        b"CVTPD2PI" | b"cvtpd2pi" | b"CVTPI2PD" | b"cvtpi2pd" | b"CVTTPD2PI" | b"cvttpd2pi"=> 
            Err("Instructions involving MMX registers are not supported.".into()),
        b"CVTPD2PS" | b"cvtpd2ps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f5a")], operands, labels, 0).map(Some),
        b"CVTPS2DQ" | b"cvtps2dq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f5b")], operands, labels, 0).map(Some),
        b"CVTPS2PD" | b"cvtps2pd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 0f5a")], operands, labels, 0).map(Some),
        b"CVTSD2SI" | b"cvtsd2si" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op1 {
                Operand::DWordRegister(r) => [Operand::DWordRegister(r), op2],
                _ => return Err("first operand of 'CVTSD2SI' should be 32-bit general purpose register.".into())
            }, &[EncodingArm::new("r* mx : f20f2d")], 0).map(|x| Some((n+m, x)))
        },
        b"CVTSD2SS" | b"cvtsd2ss" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f5a")], operands, labels, 0).map(Some),
        b"CVTSI2SD" | b"cvtsi2sd" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op2 {
                Operand::DWordRegister(r) => [op1, Operand::DWordRegister(r)],
                Operand::DWordPtr(mem) | Operand::Ptr(mem) => [op1, Operand::DWordPtr(mem)],
                _ => return Err("second operand of 'CVTSI2SD' should be 32-bit register or memory location.".into())
            }, &[EncodingArm::new("rx m* : f20f2a")], 0).map(|x| Some((n+m, x)))
        },
        b"CVTSS2SD" | b"cvtss2sd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f5a")], operands, labels, 0).map(Some),
        b"CVTTPD2DQ" | b"cvttpd2dq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fe6")], operands, labels, 0).map(Some),
        b"CVTTPS2DQ" | b"cvttps2dq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f5b")], operands, labels, 0).map(Some),
        b"CVTTSD2SI" | b"cvttsd2si" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match &op1 {
                Operand::DWordRegister(_) => [op1, op2],
                _ => return Err("first operand of 'CVTTSD2SI' should be 32-bit register.".into())
            }, &[EncodingArm::new("r* mx : f20f2c")], 0).map(|x| Some((n+m, x)))
        },
        b"DIVPD" | b"divpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f5e")], operands, labels, 0).map(Some),
        b"DIVSD" | b"divsd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f5e")], operands, labels, 0).map(Some),
        b"LFENCE" | b"lfence" => Ok(Some((0, [2, 0x0f, 0xae, 0xe8, 0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"MASKMOVDQU" | b"maskmovdqu" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match &op2 {
                Operand::XMMRegister(_) => [op1, op2],
                _ => return Err("second operand of 'MASKMOVDQU' should be XMM-register.".into())
            }, &[EncodingArm::new("rx mx : 660ff7")], 0).map(|x| Some((n+m, x)))
        },
        b"MAXPD" | b"maxpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f5f")], operands, labels, 0).map(Some),
        b"MAXSD" | b"maxsd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f5f")], operands, labels, 0).map(Some),
        b"MFENCE" | b"mfence" => Ok(Some((0, [3, 0x0f, 0xae, 0xf0, 0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"MINPD" | b"minpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f5d")], operands, labels, 0).map(Some),
        b"MINSD" | b"minsd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f5d")], operands, labels, 0).map(Some),
        b"MOVAPD" | b"movapd" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx mrx : -660f29 660f28-")], operands, labels, 0).map(Some),
        b"MOVD" | b"movd" => encode_with_operands::<2>(&[
            EncodingArm::new("m*rx m*rx : -660f7e 660f6e-")], operands, labels, NO_16BIT).map(Some),
        b"MOVDQ2Q" | b"movdq2q" | b"MOVQ2DQ" | b"movq2dq" => 
            Err("Instructions involving MMX registers are not supported.".into()),
        b"MOVDQA" | b"movdqa" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx mrx : -660f7f 660f6f-")], operands, labels, 0).map(Some),
        b"MOVDQU" | b"movdqu" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx mrx : -f30f7f f30f6f-")], operands, labels, 0).map(Some),
        b"MOVHPD" | b"movhpd" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match op2 {
                Operand::Ptr(mem) | Operand::QWordPtr(mem) => [op1, Operand::Ptr(mem)],
                _ => return Err("second operand of 'MOVHPD' should be 64-bit memory location.".into())
            }, &[EncodingArm::new("rx m8 : 660f16")], 0).map(|x| Some((n+m, x)))
        },
        b"MOVMSKPD" | b"movmskpd" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match (op1,op2) {
                (Operand::LowByteRegister(r) | Operand::WordRegister(r) | Operand::DWordRegister(r) | Operand::QWordRegister(r), Operand::XMMRegister(x)) => [Operand::LowByteRegister(r), Operand::XMMRegister(x)],
                _ => return Err("operands of 'MOVMSKPD' should be a general purpose register and an XMM-register.".into())
            }, &[EncodingArm::new("r8 mx : 660f50")], 0).map(|x| Some((n+m, x)))
        },
        b"MOVNTDQ" | b"movntdq" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match &op1 {
                Operand::Ptr(_) => [op1, op2],
                _ => return Err("first operand of 'MOVNTDQ' should be a memory location.".into())
            }, &[EncodingArm::new("mx rx : 660fe7")], 0).map(|x| Some((n+m, x)))
        },
        b"MOVNTI" | b"movnti" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match &op1 {
                Operand::Ptr(_) | Operand::DWordPtr(_) | Operand::QWordPtr(_) => [op1, op2],
                _ => return Err("first operand of 'MOVNTI' should be a memory location.".into())
            }, &[EncodingArm::new("m* r* : 0fc3")], NO_16BIT).map(|x| Some((n+m, x)))
        },
        b"MOVNTPD" | b"movntpd" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match &op1 {
                Operand::Ptr(_) => [op1, op2],
                _ => return Err("first operand of 'MOVNTPD' should be a memory location.".into())
            }, &[EncodingArm::new("mx rx : 660f2b")], 0).map(|x| Some((n+m, x)))
        },
        b"MOVQ" | b"movq" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            match (op1,op2) {
                (Operand::XMMRegister(x), Operand::QWordRegister(r)) => Ok(Some((n+m, 
                    [5, 0x66, 0x48 | (x >> 1) & 4 | r >> 3, 0x0f, 0x6e, 0xc0 | (x << 3) & 0x38 | r & 7, 0,0,0,0,0,0,0,0,0,0]))),
                (Operand::QWordRegister(r), Operand::XMMRegister(x)) => Ok(Some((n+m, 
                    [5, 0x66, 0x48 | (x >> 1) & 4 | r >> 3, 0x0f, 0x7e, 0xc0 | (x << 3) & 0x38 | r & 7, 0,0,0,0,0,0,0,0,0,0]))),
                (op1, op2) => Ok(Some((n+m,
                    try_encode(&[op1, op2], &[EncodingArm::new("mrx mrx : -660fd6 f30f7e-")], 0)?)))
            }
        },
        b"MOVSD" | b"movsd" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx mrx : -f20f11 f20f10-")], operands, labels, 0).map(Some),
        b"MOVUPD" | b"movupd" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx mrx : -660f11 660f10-")], operands, labels, 0).map(Some),
        b"MULPD" | b"mulpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f59")], operands, labels, 0).map(Some),
        b"MULSD" | b"mulsd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f59")], operands, labels, 0).map(Some),
        b"ORPD" | b"orpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f56")], operands, labels, 0).map(Some),
        b"PACKSSDW" | b"packssdw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f6b")], operands, labels, 0).map(Some),
        b"PACKSSWB" | b"packsswb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f63")], operands, labels, 0).map(Some),
        b"PACKUSWB" | b"packuswb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f67")], operands, labels, 0).map(Some),
        b"PADDB" | b"paddb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ffc")], operands, labels, 0).map(Some),
        b"PADDD" | b"paddd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ffe")], operands, labels, 0).map(Some),
        b"PADDQ" | b"paddq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fd4")], operands, labels, 0).map(Some),
        b"PADDSB" | b"paddsb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fec")], operands, labels, 0).map(Some),
        b"PADDSW" | b"paddsw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fed")], operands, labels, 0).map(Some),
        b"PADDUSB" | b"paddusb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fdc")], operands, labels, 0).map(Some),
        b"PADDUSW" | b"paddusw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fdd")], operands, labels, 0).map(Some),
        b"PADDW" | b"paddw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ffd")], operands, labels, 0).map(Some),
        b"PAND" | b"pand" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fdb")], operands, labels, 0).map(Some),
        b"PANDN" | b"pandn" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fdf")], operands, labels, 0).map(Some),
        b"PAVGB" | b"pavgb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fe0")], operands, labels, 0).map(Some),
        b"PAVGW" | b"pavgw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fe3")], operands, labels, 0).map(Some),
        b"PCMPEQB" | b"pcmpeqb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f74")], operands, labels, 0).map(Some),
        b"PCMPEQD" | b"pcmpeqd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f76")], operands, labels, 0).map(Some),
        b"PCMPEQW" | b"pcmpeqw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f75")], operands, labels, 0).map(Some),
        b"PCMPGTB" | b"pcmpgtb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f64")], operands, labels, 0).map(Some),
        b"PCMPGTD" | b"pcmpgtd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f66")], operands, labels, 0).map(Some),
        b"PCMPGTW" | b"pcmpgtw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f65")], operands, labels, 0).map(Some),
        b"PEXTRW" | b"pextrw" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (k, op3) = Operand::read(&operands[n+m..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let Operand::XMMRegister(x) = op2 else {return Err("second operand for 'PEXTRW' should be XMM-register.".into())};
            let Operand::Immediate(l) = op3 else {return Err("third operand for 'PEXTRW' should be 8-bit immediate.".into())};
            let Literal::U8(i) = l.into_type(Literal::U8_M, false)? else {panic!("unreachable")};
            match op1 {
                Operand::WordRegister(r) | Operand::DWordRegister(r) | Operand::QWordRegister(r) => 
                    Ok(Some((n+m+k,  if r > 7 || x > 7 {
                        [6, 0x66, 0x40 | (x >> 3) | (r >> 1) & 4, 0x0f, 0xc5, 0xc0 | 0x38 & (r << 3) | 7 & x, i, 0,0,0,0,0,0,0,0,0]} else {
                        [5, 0x66, 0x0f, 0xc5, 0xc0 | (r << 3) | x, i, 0,0,0,0,0,0,0,0,0,0]}))),
                Operand::WordPtr(mem) | Operand::Ptr(mem) =>
                    Ok(Some((n+m+k,try_encode(&[Operand::Ptr(mem), Operand::XMMRegister(x), Operand::Immediate(Literal::U8(i))], 
                    &[EncodingArm::new("m8 rx i8 : 660f3a15")], IMM_UNSIGNED)?))),
                _ => Err("first Operand to 'PEXTRW' should be Register or 16-bit memory location.".into())
            }
        },
        b"PINSRW" | b"pinsrw" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (k, op3) = Operand::read(&operands[n+m..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let Operand::XMMRegister(x) = op1 else {return Err("first operand for 'PINSRW' should be XMM-register.".into())};
            let Operand::Immediate(l) = op3 else {return Err("third operand for 'PINSRW' should be 8-bit immediate.".into())};
            let Literal::U8(i) = l.into_type(Literal::U8_M, false)? else {panic!("unreachable")};
            match op2 {
                Operand::WordRegister(r) | Operand::DWordRegister(r) | Operand::QWordRegister(r) => 
                    Ok(Some((n+m+k,  if r > 7 || x > 7 {
                        [6, 0x66, 0x40 | (r >> 3) | (x >> 1) & 4, 0x0f, 0xc4, 0xc0 | 0x38 & (x << 3) | 7 & r, i, 0,0,0,0,0,0,0,0,0]} else {
                        [5, 0x66, 0x0f, 0xc4, 0xc0 | (x << 3) | r, i, 0,0,0,0,0,0,0,0,0,0]}))),
                Operand::WordPtr(mem) | Operand::Ptr(mem) =>
                    Ok(Some((n+m+k,try_encode(&[Operand::XMMRegister(x), Operand::Ptr(mem), Operand::Immediate(Literal::U8(i))], 
                    &[EncodingArm::new("rx m8 i8 : 660fc4")], IMM_UNSIGNED)?))),
                _ => Err("second Operand to 'PINSRW' should be Register or 16-bit memory location.".into())
            }
        },
        b"PMADDWD" | b"pmaddwd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ff5")], operands, labels, 0).map(Some),
        b"PMAXSW" | b"pmaxsw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fee")], operands, labels, 0).map(Some),
        b"PMAXUB" | b"pamxub" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fde")], operands, labels, 0).map(Some),
        b"PMINSW" | b"pminsw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fea")], operands, labels, 0).map(Some),
        b"PMINUB" | b"pminub" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fda")], operands, labels, 0).map(Some),
        b"PMOVMSKB" | b"pmovmskb" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (Operand::WordRegister(r) | Operand::DWordRegister(r) | Operand::QWordRegister(r)) = op1 else {return Err("first operand for 'PMOVMSKB' should be general-purpose register".into())};
            let Operand::XMMRegister(x) = op2 else {return Err("second operand for 'PMOVMSKB' should be XMM-register.".into())};
            Ok(Some((n+m, if r < 8 || x < 8 {[4, 0x66, 0x0f, 0xd7, 0xc0 | (r << 3) | x, 0,0,0,0,0,0,0,0,0,0,0]} else {
                [5, 0x66, 0x40 | 4 & (r >> 1) | x >> 3, 0x0f, 0xd7, 0xc0 | 0x38 & (r << 3) | 7 & x, 0,0,0,0,0,0,0,0,0,0]})))
        },
        b"PMULHUW" | b"pmulhuw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fe4")], operands, labels, 0).map(Some),
        b"PMULHW" | b"pmulhw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fe5")], operands, labels, 0).map(Some),
        b"PMULLW" | b"pmullw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fd5")], operands, labels, 0).map(Some),
        b"PMULUDQ" | b"pmuludq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ff4")], operands, labels, 0).map(Some),
        b"POR" | b"por" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660feb")], operands, labels, 0).map(Some),
        b"PSADBW" | b"psadbw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ff6")], operands, labels, 0).map(Some),
        b"PSHUFD" | b"pshufd" => encode_with_operands::<3>(&[
            EncodingArm::new("rx mx i8: 660f70")], operands, labels, 0).map(Some),
        b"PSHUFHW" | b"pshufhw" => encode_with_operands::<3>(&[
            EncodingArm::new("rx mx i8: f30f70")], operands, labels, 0).map(Some),
        b"PSHUFLW" | b"pshuflw" => encode_with_operands::<3>(&[
            EncodingArm::new("rx mx i8: f20f70")], operands, labels, 0).map(Some),
        b"PSLLD" | b"pslld" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ff2")], operands, labels, 0).map(Some),
        b"PSLLDQ" | b"pslldq" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let Operand::XMMRegister(x) = op1 else {return Err("first operand for 'PSLLDQ' should be XMM-register.".into())};
            let Operand::Immediate(l) = op2 else {return Err("second operand for 'PSLLDQ' should be 8-bit immediate.".into())};
            let Literal::U8(i) = l.into_type(Literal::U8_M, false)? else {panic!("unreachable")};
            Ok(Some((n+m,if x<8 {[6, 0x66, 0x0f, 0x73, 0xf8 | x, i, 0,0,0,0,0,0,0,0,0,0]} else {
                [7, 0x66, 0x41, 0x0f, 0x73, 0xf8 | x, i, 0,0,0,0,0,0,0,0,0]})))
        },
        b"PSLLQ" | b"psllq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ff3")], operands, labels, 0).map(Some),
        b"PSLLW" | b"psllw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f71")], operands, labels, 0).map(Some),
        b"PSRAD" | b"psrad" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx i8mx : 660f72/4--660fe2")], operands, labels, 0).and_then(|(n,x)| match x {
                [_,0x66, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_,_] | [_, 0x66, _, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 != 0xc0 => 
                    Err("first operand for 'PSRAD' should be XMM-register.".into()),_ => Ok(Some((n,x)))}),
        b"PSRAW" | b"psraw" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx i8mx : 660f71/4--660fe1")], operands, labels, 0).and_then(|(n,x)| match x {
                [_,0x66, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_,_] | [_, 0x66, _, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 != 0xc0 => 
                    Err("first operand for 'PSRAW' should be XMM-register.".into()),_ => Ok(Some((n,x)))}),
        b"PSRLD" | b"psrld" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx i8mx : 660f72/2--660fd2")], operands, labels, 0).and_then(|(n,x)| match x {
                [_,0x66, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_,_] | [_, 0x66, _, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 != 0xc0 => 
                    Err("first operand for 'PSRLD' should be XMM-register.".into()),_ => Ok(Some((n,x)))}),
        b"PSRLDQ" | b"psrldq" => encode_with_operands::<2>(&[
            EncodingArm::new("mx i8 : 660f73/3")], operands, labels, 0).and_then(|(n,x)| match x {
                [_,0x66, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_,_] | [_, 0x66, _, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 != 0xc0 => 
                    Err("first operand for 'PSRLDQ' should be XMM-register.".into()),_ => Ok(Some((n,x)))}),
        b"PSRLQ" | b"psrlq" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx i8mx : 660f73/2--660fd3")], operands, labels, 0).and_then(|(n,x)| match x {
                [_,0x66, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_,_] | [_, 0x66, _, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 != 0xc0 => 
                    Err("first operand for 'PSRLQ' should be XMM-register.".into()),_ => Ok(Some((n,x)))}),
        b"PSRLW" | b"psrlw" => encode_with_operands::<2>(&[
            EncodingArm::new("mrx i8mx : 660f71/2--660fd1")], operands, labels, 0).and_then(|(n,x)| match x {
                [_,0x66, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_,_] | [_, 0x66, _, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 != 0xc0 => 
                    Err("first operand for 'PSRLW' should be XMM-register.".into()),_ => Ok(Some((n,x)))}),
        b"PSUBB" | b"psubb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ff8")], operands, labels, 0).map(Some),
        b"PSUBD" | b"psubd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ffa")], operands, labels, 0).map(Some),
        b"PSUBQ" | b"psubq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ffb")], operands, labels, 0).map(Some),
        b"PSUBSB" | b"psubsb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fe8")], operands, labels, 0).map(Some),
        b"PSUBSW" | b"psubsw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fe9")], operands, labels, 0).map(Some),
        b"PSUBUSB" | b"psubusb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fd8")], operands, labels, 0).map(Some),
        b"PSUBUSW" | b"psubusw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fd9")], operands, labels, 0).map(Some),
        b"PSUBW" | b"psubw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660ff9")], operands, labels, 0).map(Some),
        b"PUNPCKHBW" | b"punpckhbw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f68")], operands, labels, 0).map(Some),
        b"PUNPCKHDQ" | b"punpckhdq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f6a")], operands, labels, 0).map(Some),
        b"PUNPCKHQDQ" | b"punpckhqdq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f6d")], operands, labels, 0).map(Some),
        b"PUNPCKHWD" | b"punpckhwd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f69")], operands, labels, 0).map(Some),
        b"PUNPCKLBW" | b"punpcklbw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f60")], operands, labels, 0).map(Some),
        b"PUNPCKLDQ" | b"punpckldq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f62")], operands, labels, 0).map(Some),
        b"PUNPCKLQDQ" | b"punpcklqdq" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f6c")], operands, labels, 0).map(Some),
        b"PUNPCKLWD" | b"punpcklwd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f61")], operands, labels, 0).map(Some),
        b"PXOR" | b"pxor" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fef")], operands, labels, 0).map(Some),
        b"SHUFPD" | b"shufpd" => encode_with_operands::<3>(&[
            EncodingArm::new("rx mx i8 : 660fc6")], operands, labels, 0).map(Some),
        b"SQRTPD" | b"sqrtpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f51")], operands, labels, 0).map(Some),
        b"SQRTSD" | b"sqrtsd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f51")], operands, labels, 0).map(Some),
        b"SUBPD" | b"subpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f5c")], operands, labels, 0).map(Some),
        b"SUBSD" | b"subsd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f5c")], operands, labels, 0).map(Some),
        b"UCOMISD" | b"ucomisd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f2e")], operands, labels, 0).map(Some),
        b"UNPCKHPD" | b"unpckhpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f15")], operands, labels, 0).map(Some),
        b"UNPCKLPD" | b"unpcklpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f14")], operands, labels, 0).map(Some),
        b"XORPD" | b"XORpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f57")], operands, labels, 0).map(Some),
        
        // SSE3
        b"ADDSUBPD" | b"addsubpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660fd0")], operands, labels, 0).map(Some),
        b"ADDSUBPS" | b"addsubps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20fd0")], operands, labels, 0).map(Some),
        b"FISTTP" | b"fisttp" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n, match op1 {
                Operand::WordPtr(m) => try_encode(&[Operand::Ptr(m)], &[EncodingArm::new("m8 : df/1")], 0),
                Operand::DWordPtr(m) => try_encode(&[Operand::Ptr(m)], &[EncodingArm::new("m8 : db/1")], 0),
                Operand::QWordPtr(m) => try_encode(&[Operand::Ptr(m)], &[EncodingArm::new("m8 : dd/1")], 0),
                _ => Err("operand for 'FISTTP' should be 16-, 32- or 64-bit memory location.".into())
            }?)))
        }
        b"HADDPD" | b"haddpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f7c")], operands, labels, 0).map(Some),
        b"HADDPS" | b"haddps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f7c")], operands, labels, 0).map(Some),
        b"HSUBPD" | b"hsubpd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f7d")], operands, labels, 0).map(Some),
        b"HSUBPS" | b"hsubps" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f7d")], operands, labels, 0).map(Some),
        b"LDDQU" | b"lddqu" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20ff0")], operands, labels, 0).and_then(|(n,x)| match x {
                [_,0x66, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_,_] | [_, 0x66, _, 0x0f, _, modrm,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 == 0xc0 => 
                    Err("first operand for 'LLDQU' should be memory position.".into()),_ => Ok(Some((n,x)))}),
        b"MOVDDUP" | b"movddup" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f20f12")], operands, labels, 0).map(Some),
        b"MOVSHDUP" | b"movshdup" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f16")], operands, labels, 0).map(Some),
        b"MOVSLDUP" | b"movsldup" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : f30f12")], operands, labels, 0).map(Some),

        // SSSE3
        b"PABSB" | b"pabsb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f381c")], operands, labels, 0).map(Some),
        b"PABSD" | b"pabsd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f381e")], operands, labels, 0).map(Some),
        b"PABSW" | b"pabsw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f381d")], operands, labels, 0).map(Some),
        b"PALIGNR" | b"palignr" => encode_with_operands::<3>(&[
            EncodingArm::new("rx mx i8 : 660f3a0f")], operands, labels, IMM_UNSIGNED).map(Some),
        b"PHADDD" | b"phaddd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f3802")], operands, labels, 0).map(Some),
        b"PHADDSW" | b"phaddsw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f3803")], operands, labels, 0).map(Some),
        b"PHADDW" | b"phaddw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f3801")], operands, labels, 0).map(Some),
        b"PHSUBD" | b"phsubd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f3806")], operands, labels, 0).map(Some),
        b"PHSUBSW" | b"phsubsw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f3807")], operands, labels, 0).map(Some),
        b"PHSUBW" | b"phsubw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f3805")], operands, labels, 0).map(Some),
        b"PMADDUBSW" | b"pmaddubsw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f3804")], operands, labels, 0).map(Some),
        b"PMULHRSW" | b"pmulhrsw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f380b")], operands, labels, 0).map(Some),
        b"PSHUFB" | b"pshufb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f3800")], operands, labels, 0).map(Some),
        b"PSIGNB" | b"psignb" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f3808")], operands, labels, 0).map(Some),
        b"PSIGND" | b"psignd" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f380a")], operands, labels, 0).map(Some),
        b"PSIGNW" | b"psignw" => encode_with_operands::<2>(&[
            EncodingArm::new("rx mx : 660f3809")], operands, labels, 0).map(Some),

        // TODO SSE4

        _ => Ok(None)
    }
}