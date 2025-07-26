
pub fn write_elf((builder, labels): (super::builder::Builder, super::LabelContext), out: &mut impl std::io::Write) -> Result<(),String> {
    
    let mut rela_text = Vec::<[u8;24]>::new();
    let mut strtab: Vec<u8> = vec![0];
    let shstrtab: Vec<u8> = if labels.references.len() != 0 {
        b"\0.symtab\0.strtab\0.shstrtab\0.rela.text\0.data\0.bss\0".into()
    } else {
        b"\0.symtab\0.strtab\0.shstrtab\0.text\0.data\0.bss\0".into()
    };
    let mut symtab: Vec<[u8;24]> = vec![[0;24]];
    for (export_sym, global) in labels.local_labels.iter().cloned().map(|l| (l,false)).chain(labels.global_labels.iter().cloned().map(|l| (l,true))) {
        if let Some(known) = labels.known_labels.iter().find(|k| k.label == export_sym) {
            let mut symbol: [u8;24] = [0; 24];
            for (w,b) in symbol.iter_mut().zip(u32::to_le_bytes(strtab.len() as u32)) {*w = b;}
            strtab.extend_from_slice(labels.label(export_sym));
            strtab.push(0);
            if global {symbol[4] = 0x10;}
            symbol[6] = 1; // relative to .text, which is index 1 in section_headers
            for (w,b) in symbol[8..].iter_mut().zip(u64::to_le_bytes(known.pos as u64)) {*w = b};
            symtab.push(symbol);
        } else {
            return Err(format!("can't export undefined symbol '{}'.", &str::from_utf8(labels.label(export_sym)).unwrap_or("<invalid utf8>")));
        }
    }
    
    let mut extern_symbols = labels.references.iter().map(|r| r.label).collect::<Vec<_>>();
    extern_symbols.sort();
    extern_symbols.dedup();
    
    for r in labels.references.iter() {
        let mut rela: [u8;24] = [0;24];
        for (w,b) in rela.iter_mut().zip(u64::to_le_bytes(r.pos_idx as u64)) {*w = b;}
        let symbol_idx = extern_symbols.iter().position(|s| *s == r.label).unwrap() + symtab.len();
        for (w,b) in rela[12..].iter_mut().zip(u32::to_le_bytes(symbol_idx as u32)) {*w = b;}
        rela[8] = match r.pos.len() {
            1 => 15, // R_X86_64_PC8
            2 => 13, // R_X86_64_PC16
            4 => 2,  // R_X86_64_PC32
            8 => 1,  // R_X86_64_64
            _ => panic!("bad LabelReference size.")
        };
        for (w,b) in rela[16..].iter_mut().zip(i64::to_le_bytes(r.pos_idx as i64 - r.offset_start as i64)) {*w = b;}
        rela_text.push(rela);
    }
    
    for extern_sym in extern_symbols.iter().cloned() {
        let mut symbol: [u8;24] = [0;24];
        for (w,b) in symbol.iter_mut().zip(u32::to_le_bytes(strtab.len() as u32)) {*w = b;}
        strtab.extend_from_slice(labels.label(extern_sym));
        strtab.push(0);
        symbol[4] = 0x10;
        symtab.push(symbol);
    }

    // text, rela_text, str_tab, symtab, shstrtab are done, need  section_headers

    let text_start = 0x40;
    let text_end = text_start + builder.len();
    let symtab_start = usize::next_multiple_of(text_end, 8);
    let symtab_end = symtab_start + symtab.len() * 24;
    let strtab_start = symtab_end;
    let strtab_end = strtab_start + strtab.len();
    let rela_text_start = usize::next_multiple_of(strtab_end, 8);
    let rela_text_end = rela_text_start + rela_text.len() * 24;
    let shstrtab_start = rela_text_end;
    let shstrtab_end = shstrtab_start + shstrtab.len();
    let sections_start = usize::next_multiple_of(shstrtab_end, 8);

    

    let errmap = |err| {format!("Error writing ELF file: {}",err)};
    // ELF header
    out.write_all(&[0x7f, 0x45, 0x4c, 0x46, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]).map_err(errmap)?; // Magic
    out.write_all(&[1, 0, 0x3e, 0, 1, 0, 0, 0]).map_err(errmap)?; // e_type = ET_REL, e_machine = AMD x86-64, e_version = 1
    out.write_all(&[0, 0, 0, 0, 0, 0, 0, 0]).map_err(errmap)?; // no entry point
    out.write_all(&[0, 0, 0, 0, 0, 0, 0, 0]).map_err(errmap)?; // no program header
    out.write_all(&u64::to_le_bytes(sections_start as u64)).map_err(errmap)?;
    if rela_text.len() == 0 {
        out.write_all(&[0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 64, 0, 7, 0, 6, 0]).map_err(errmap)?;
    } else {
        out.write_all(&[0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 64, 0, 8, 0, 7, 0]).map_err(errmap)?;
    }
    // no flags, this header is 64 bytes, no program header, section header entries are 64 bytes, there are 8, nr 7 is shstrtab

    // .text
    for page in builder.pages() {out.write_all(page).map_err(errmap)?;}
    out.write_all(&vec![0;symtab_start-text_end]).map_err(errmap)?;

    // .symtab
    for sym in symtab.iter() {out.write_all(sym).map_err(errmap)?;}

    // .strtab
    out.write_all(&strtab).map_err(errmap)?;
    out.write_all(&vec![0;rela_text_start-strtab_end]).map_err(errmap)?;

    // .rela.text
    for rela in rela_text.iter() {out.write_all(rela).map_err(errmap)?;}

    // .shstrtab
    out.write_all(&shstrtab).map_err(errmap)?;
    out.write_all(&vec![0;sections_start-shstrtab_end]).map_err(errmap)?;

    // null section header
    out.write_all(&[0;64]).map_err(errmap)?;

    // .text section header
    out.write_all(&[if rela_text.len() != 0 {0x20} else {0x1b},0,0,0, 1,0,0,0, 6,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0]).map_err(errmap)?;
    out.write_all(&u64::to_le_bytes(text_start as u64)).map_err(errmap)?;
    out.write_all(&u64::to_le_bytes((text_end-text_start) as u64)).map_err(errmap)?;
    out.write_all(&[0,0,0,0, 0,0,0,0, 1,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0]).map_err(errmap)?;

    // .rela.text section header
    if rela_text.len() != 0 {
        out.write_all(&[0x1b,0,0,0, 4,0,0,0, 64,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0]).map_err(errmap)?;
        out.write_all(&u64::to_le_bytes(rela_text_start as u64)).map_err(errmap)?;
        out.write_all(&u64::to_le_bytes((rela_text_end-rela_text_start) as u64)).map_err(errmap)?;
        out.write_all(&[5,0,0,0, 1,0,0,0, 8,0,0,0,0,0,0,0, 24,0,0,0,0,0,0,0]).map_err(errmap)?;
    }

    // .data section header
    out.write_all(&[if rela_text.len() != 0 {0x26} else {0x21},0,0,0, 1,0,0,0, 3,0,0,0,0,0,0,0, 
                         0,0,0,0,0,0,0,0, 0x40,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
                         1,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0]).map_err(errmap)?;

    // .bss section header
    out.write_all(&[if rela_text.len() != 0 {0x2c} else {0x27},0,0,0, 8,0,0,0, 3,0,0,0,0,0,0,0, 
                         0,0,0,0,0,0,0,0, 0x40,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
                         1,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0]).map_err(errmap)?;

    // .symtab section header
    out.write_all(&[1,0,0,0, 2,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0]).map_err(errmap)?;
    out.write_all(&u64::to_le_bytes(symtab_start as u64)).map_err(errmap)?;
    out.write_all(&u64::to_le_bytes((symtab_end-symtab_start) as u64)).map_err(errmap)?;
    out.write_all(&[if rela_text.len() == 0 {5} else {6},0,0,0]).map_err(errmap)?;
    out.write_all(&u32::to_le_bytes(labels.local_labels.len() as u32 + 1)).map_err(errmap)?;
    // index of the first global symbol, all global symbols come after all local symbols
    out.write_all(&[8,0,0,0,0,0,0,0, 24,0,0,0,0,0,0,0]).map_err(errmap)?;

    // .strtab section header
    out.write_all(&[9,0,0,0, 3,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0]).map_err(errmap)?;
    out.write_all(&u64::to_le_bytes(strtab_start as u64)).map_err(errmap)?;
    out.write_all(&u64::to_le_bytes((strtab_end-strtab_start) as u64)).map_err(errmap)?;
    out.write_all(&[0,0,0,0, 0,0,0,0, 1,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0]).map_err(errmap)?;

    // .shstrtab section header
    out.write_all(&[0x11,0,0,0, 3,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0]).map_err(errmap)?;
    out.write_all(&u64::to_le_bytes(shstrtab_start as u64)).map_err(errmap)?;
    out.write_all(&u64::to_le_bytes((shstrtab_end-shstrtab_start) as u64)).map_err(errmap)?;
    out.write_all(&[0,0,0,0, 0,0,0,0, 1,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0]).map_err(errmap)?;

    Ok(())
}
