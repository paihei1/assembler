use super::{LabelContext, encode_with_operands};
use crate::encode::{try_encode, EncodingArm, Operand, IMM_UNSIGNED, NO_16BIT};

pub fn encode(mnemonic: &[u8], operands: &[u8], labels: &mut LabelContext) -> Result<Option<(usize, [u8;16])>, String> {
    match mnemonic {
        b"ADCX" | b"adcx" => encode_with_operands::<2>(&[EncodingArm::new("r* m* : 0f38f6")],
            operands, labels, NO_16BIT).map(Some),
        b"ADOX" | b"adox" => encode_with_operands::<2>(&[EncodingArm::new("r* m* : 0f38f6")],
            operands, labels, NO_16BIT).map(Some),
        b"ANDN" | b"andn" => encode_with_operands::<3>(&[EncodingArm::new("r* v* m* : $02f2")],
            operands, labels, NO_16BIT).map(Some),
        b"BEXTR" | b"bextr" => encode_with_operands::<3>(&[EncodingArm::new("r* m* v* : $02f7")],
            operands, labels, NO_16BIT).map(Some),
        b"BLSI" | b"blsi" => encode_with_operands::<2>(&[EncodingArm::new("v* m* : $02f3/3")],
            operands, labels, NO_16BIT).map(Some),
        b"BLSMSK" | b"blsmsk" => encode_with_operands::<2>(&[EncodingArm::new("v* m* : $02f3/2")],
            operands, labels, NO_16BIT).map(Some),
        b"BLSR" | b"blsr" => encode_with_operands::<2>(&[EncodingArm::new("v* m* : $02f3/1")],
            operands, labels, NO_16BIT).map(Some),
        b"BZHI" | b"bzhi" => encode_with_operands::<3>(&[EncodingArm::new("r* m* v* : $02f5")],
            operands, labels, NO_16BIT).map(Some),
        b"CDQE" | b"cdqe" => Ok(Some((0,[2, 0x48, 0x98, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CLDEMOTE" | b"cldemote" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0f1c/0")],
            operands, labels, 0).and_then(|(n,x)| match x {
                [_, 0x0f, 0x1c, modrm, _,_,_,_,_,_,_,_,_,_,_,_] | [_,_, 0x0f, 0x1c, modrm, _,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 == 0xc0 => 
                Err("'CLDEMOTE' can't have register operand.".into()),
                _ => Ok(Some((n,x)))}),
        b"CLFLUSH" | b"clflush" | b"CLFSH" | b"clfsh" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 0fae/7")],
            operands, labels, 0).and_then(|(n,x)| match x {
                [_, 0x0f, 0xae, modrm, _,_,_,_,_,_,_,_,_,_,_,_] | [_,_, 0x0f, 0xae, modrm, _,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 == 0xc0 => 
                    Err("'CLFLUSH' can't have register operand.".into()),
                _ => Ok(Some((n,x)))}),
        b"CLFLUSHOPT" | b"clflushopt" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 660fae/7")],
            operands, labels, 0).and_then(|(n,x)| match x {
                [_, 0x66, 0x0f, 0xae, modrm, _,_,_,_,_,_,_,_,_,_,_] | [_,0x66,_, 0x0f, 0xae, modrm, _,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 == 0xc0 => 
                    Err("'CLFLUSHOPT' can't have register operand.".into()),
                _ => Ok(Some((n,x)))
            }),
        b"CLWB" | b"clwb" => encode_with_operands::<1>(&[EncodingArm::new("m8 : 660fae/6")],
            operands, labels, 0).and_then(|(n,x)| match x {
                [_, 0x66, 0x0f, 0xae, modrm, _,_,_,_,_,_,_,_,_,_,_] | [_,0x66,_, 0x0f, 0xae, modrm, _,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 == 0xc0 =>
                    Err("'CLWB' can't have register operand.".into()),
                _ => Ok(Some((n,x)))}),
        b"CMPSQ" | b"cmpsq" => Ok(Some((0, [2, 0x48, 0xa7, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"CMPXCHG16B" | b"cmpxchg16b" => encode_with_operands::<1>(&[
            EncodingArm::new("m8 : 0fc7/1")], operands, labels,0).and_then(|(n,x)| match x {
                [_, 0x0f, 0xc7, modrm, _,_,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 == 0xc0 => Err("'CMPXCHG16B' can't have register operand.".into()),
                [_, 0x0f, 0xc7, _,_,_,_,_,_,_,_,_,_,_,_,_] => Ok(Some((n, [x[0]+1, 0x48, 0x0f, 0xc7, x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10],x[11],x[12],x[13],x[14]]))),
                [_, _, 0x0f, 0xc7, modrm, _,_,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 == 0xc0 => Err("'CMPXCHG16B' can't have register operand.".into()),
                [_, rex, 0x0f, 0xc7, _,_,_,_,_,_,_,_,_,_,_,_] => Ok(Some((n, [x[0], rex | 8, 0x0f, 0xc7, x[4],x[5],x[6],x[7],x[8],x[9],x[10],x[11],x[12],x[13],x[14],x[15]]))),
                _ => panic!("unreachable.")
            }),
        // this allows you to put 'BYTE PTR', but w.e, should use 'PTR [...]'
        b"CQO" | b"cqo" => Ok(Some((0, [2, 0x48, 0x99, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"IRETQ" | b"iretq" => Ok(Some((0, [2, 0x48, 0xcf, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"LODSQ" | b"lodsq" => Ok(Some((0, [2, 0x48, 0xad, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"LZCNT" | b"lzcnt" => encode_with_operands::<2>(&[EncodingArm::new("r* m* : f30fbd")], operands, labels, 0).map(Some),
        b"MOVBE" | b"movbe" => encode_with_operands::<2>(&[EncodingArm::new("rm* rm* : -0f38f0 0f38f1-")], operands, labels, 0).and_then(|(n,x)| match x {
            [_, 0x0f, 0x38, _, modrm, _,_,_,_,_,_,_,_,_,_,_] | [_, _, 0x0f, 0x38, _, modrm, _,_,_,_,_,_,_,_,_,_]
                if modrm & 0xc0 == 0xc0 => Err("'MOVBE' can't have two register operands.".into()),
            _ => Ok(Some((n, x)))
        }),
        b"MOVDIR64B" | b"movdir64b" => encode_with_operands::<2>(&[EncodingArm::new("r* m* : 0f38f8")], operands, labels, NO_16BIT).map(|(n,x)| match x {
            [_, 0x0f, 0x38, 0xf8, _,_,_,_,_,_,_,_,_,_,_,_] => Some((n, [x[0]+2, 0x66, 0x67, 0x0f, 0x38, 0xf8, x[4],x[5],x[6],x[7],x[8],x[9],x[10],x[11],x[12],x[13]])),
            [_, rex, 0x0f, 0x38, 0xf8, _,_,_,_,_,_,_,_,_,_,_] if rex & 8 == 0 => Some((n, [x[0]+2, 0x66, 0x67, rex, 0x0f, 0x38, 0xf8, x[5],x[6],x[7],x[8],x[9],x[10],x[11],x[12],x[13]])),
            _ => Some((n,[x[0]+1, 0x66, x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10],x[11],x[12],x[13],x[14]]))
        }),
        b"MOVDIRI" | b"movdiri" => encode_with_operands::<2>(&[EncodingArm::new("m* r* : 0f38f9")], operands, labels, NO_16BIT).and_then(|(n,x)| match x {
            [_, 0x0f, 0x38, 0xf9, modrm, _,_,_,_,_,_,_,_,_,_,_] | [_, _, 0x0f, 0x38, 0xf9, modrm, _,_,_,_,_,_,_,_,_,_] if modrm & 0xc0 == 0xc0 => Err("'MOVDIRI' can't have two register operands.".into()),
            _ => Ok(Some((n,x)))}),
        b"MOVSQ" | b"movsq" => Ok(Some((0, [2, 0x48, 0xa5, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"MOVSXD" | b"movsxd" => {
            let (n,op1) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            let (m,op2) = Operand::read(&operands[n..], &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            try_encode(&match (op1,op2) {
                (Operand::QWordRegister(r1), Operand::DWordRegister(r2)) => [Operand::QWordRegister(r1), Operand::QWordRegister(r2)],
                (Operand::QWordRegister(r), Operand::DWordPtr(mem) | Operand::Ptr(mem)) => [Operand::QWordRegister(r), Operand::QWordPtr(mem)],
                _ => return Err("operand types for 'MOVSXD' should be r64, r/m32.".into()),
            }, &[EncodingArm::new("r* m* : 63")], 0).map(|x| Some((n+m, x)))
        },
        b"MULX" | b"mulx" => encode_with_operands::<3>(&[EncodingArm::new("r* v* m* : $c2f6")],
            operands, labels, NO_16BIT).map(Some),
        b"PAUSE" | b"pause" => Ok(Some((0, [2, 0xf3, 0x90, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"PCLMULQDQ" | b"pclmulqdq" => encode_with_operands::<3>(&[EncodingArm::new("rx mx i8 : 660f3a44")],
            operands, labels, IMM_UNSIGNED).map(Some),
        b"PDEP" | b"pdep" => encode_with_operands::<3>(&[EncodingArm::new("r* v* m* : $c2f5")], 
            operands, labels, NO_16BIT).map(Some),
        b"PEXT" | b"pext" => encode_with_operands::<3>(&[EncodingArm::new("r* v* m* : $82f5")],
            operands, labels, NO_16BIT).map(Some),
        b"POPFQ" | b"popfq" => Ok(Some((0,[1, 0x9d, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"PUSHFQ" | b"pushfq" => Ok(Some((0, [1, 0x9c, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"RDPID" | b"rdpid" => {
            let (n,Operand::QWordRegister(r)) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())? else {return Err("RDPID operand should be a 64-bit register.".into())};
            if r >= 8 {Ok(Some((n, [5, 0xf3, 0x41, 0x0f, 0xc7, 0xf8 | r, 0,0,0,0,0,0,0,0,0,0])))} 
            else {Ok(Some((n, [4, 0xf3, 0x0f, 0xc7, 0xf8 | r, 0,0,0,0,0,0,0,0,0,0,0])))}
        },
        b"RDPKRU" | b"rdpkru" => Ok(Some((0, [3, 0x0f, 0x01, 0xee, 0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"RDRAND" | b"rdrand" => {
            let (n,op) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n, match op {
                Operand::WordRegister(r) if r >= 8 => [5, 0x66, 0x41, 0x0f, 0xc7, 0xf0 | r & 7, 0,0,0,0,0,0,0,0,0,0],
                Operand::WordRegister(r) => [4, 0x66, 0x0f, 0xc7, 0xf0 | r, 0,0,0,0,0,0,0,0,0,0,0],
                Operand::DWordRegister(r) if r >= 8 => [4, 0x41, 0x0f, 0xc7, 0xf0 | r & 7, 0,0,0,0,0,0,0,0,0,0,0],
                Operand::DWordRegister(r) => [3, 0x0f, 0xc7, 0xf0 | r, 0,0,0,0,0,0,0,0,0,0,0,0],
                Operand::QWordRegister(r) => [4, 0x48 | (r >> 3), 0x0f, 0xc7, 0xf0 | r & 7, 0,0,0,0,0,0,0,0,0,0,0],
                _ => return Err("'RDRAND' operand should be 16/32/64-bit register".into())
            })))
        },
        b"RDSEED" | b"rdseed" => {
            let (n,op) = Operand::read(operands, &mut |l| u32::try_from(labels.index(l)).unwrap().cast_signed())?;
            Ok(Some((n, match op {
                Operand::WordRegister(r) if r >= 8 => [5, 0x66, 0x41, 0x0f, 0xc7, 0xf8 | r, 0,0,0,0,0,0,0,0,0,0],
                Operand::WordRegister(r) => [4, 0x66, 0x0f, 0xc7, 0xf8 | r, 0,0,0,0,0,0,0,0,0,0,0],
                Operand::DWordRegister(r) if r >= 8 => [4, 0x41, 0x0f, 0xc7, 0xf8 | r, 0,0,0,0,0,0,0,0,0,0,0],
                Operand::DWordRegister(r) => [3, 0x0f, 0xc7, 0xf8 | r, 0,0,0,0,0,0,0,0,0,0,0,0],
                Operand::QWordRegister(r) => [4, 0x48 | (r >> 3), 0x0f, 0xc7, 0xf8 | r, 0,0,0,0,0,0,0,0,0,0,0],
                _ => return Err("'RDSEED' operand should be 16/32/64-bit register".into())
            })))
        },
        b"RDTSCP" | b"rdtscp" => Ok(Some((0, [3, 0x0f, 0x01, 0xf9, 0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"RORX" | b"rorx" => encode_with_operands::<3>(&[EncodingArm::new("r* m* i8 : $c3f0")],
            operands, labels, IMM_UNSIGNED | NO_16BIT).map(Some),
        b"SARX" | b"sarx" => encode_with_operands::<3>(&[EncodingArm::new("r* m* v* : $82f7")],
            operands, labels, NO_16BIT).map(Some),
        b"SCASQ" | b"scasq" => Ok(Some((0, [2, 0x48, 0xaf, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"SERIALIZE" | b"serialize" => Ok(Some((0, [3, 0x0f, 0x01, 0xe8, 0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"SHLX" | b"shlx" => encode_with_operands::<3>(&[EncodingArm::new("r* m* v* : $42f7")],
            operands, labels, NO_16BIT).map(Some),
        b"SHRX" | b"shrx" => encode_with_operands::<3>(&[EncodingArm::new("r* m* v* : $c2f7")],
            operands, labels, NO_16BIT).map(Some),
        b"STOSQ" | b"stosq" => Ok(Some((0, [2, 0x48, 0xab, 0,0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"SWAPGS" | b"swapgs" => Ok(Some((0, [3, 0x0f, 0x01, 0xf8, 0,0,0,0,0,0,0,0,0,0,0,0]))),
        b"TZCNT" | b"tzcnt" => encode_with_operands::<2>(&[EncodingArm::new("r* m* : f30fbc")],
            operands, labels, 0).map(Some),
        b"WRPKRU" | b"wrpkru" => Ok(Some((0, [3, 0x0f, 0x01, 0xef, 0,0,0,0,0,0,0,0,0,0,0,0]))),
        _ => Ok(None)
    }
}
// TODO: LDTILECFG, STTILECFG, TILE*
// TODO: TDPB*

// TODO: VPMADD52HUQ,VBCSTNEBF162PS,VBCSTNESH2PS,VCVTNEEBF162PS,VCVTNEEPH2PS,VCVTNEOBF162PS,VCVTNEOPH2PS,VCVTNEPS2BF16,VCVTPH2PS,VCVTPS2PH,VPCLMULQDQ,
//       VPDPBUSD,VPDPBUSDS,VPDPWSSD,VPDPWSSDS,VPDPBSSD,VPDPBSSDS,VPDPBSUD,VPDPBSUDS,VPDPBUUD,VPDPBUUDS
