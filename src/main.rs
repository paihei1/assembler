
mod encode;
mod assemble;
mod test;

mod parse;


fn main() {
    let mut args = std::env::args().peekable();
    let _ = args.next(); // executable name
    let Some(source_name) = args.next() else {panic!("no source file provided.")};
    let Some(target_name) = args.next() else {panic!("no target file provided.")};
    
    let None = args.next() else {panic!("too many command line arguments.")};

    let source = std::fs::File::open(source_name).expect("could not find source file.");
    
    // -t means test and don't print output of successful tests
    // -T means test and print output from all tests
    if target_name == "-t" || target_name == "-T" {
        const YELLOW: &'static str = "\x1b[33m";
        const GREEN: &'static str = "\x1b[32m";
        const RED: &'static str = "\x1b[31m";
        const NORMAL: &'static str = "\x1b[m";
        
        let mut tester_rust_side = test::TesterRustSide::new();
        let (builder, labels) = assemble::try_assemble(std::io::BufReader::new(source), Some(&mut tester_rust_side)).unwrap();
        print!("Done building.\n");
        if labels.any_unknown() {
            print!("{YELLOW}Warning{NORMAL}: there are unlinked external symbols while testing, may segfault.\n");
        }
        //let mut target = std::fs::File::create("dbg_test.o").expect("could not create output file.");
        //assemble::linux::write_elf((builder, labels), &mut target).unwrap();
        let mut tester = test::Tester::new(&builder, tester_rust_side);
        
        /*for (n,b) in unsafe {std::slice::from_raw_parts(tester.buf, tester.len)}.into_iter().enumerate() {
            if n % 16 == 15 {print!("{:02x}\n", *b);} else {print!("{:02x} ", *b);}
        }*/
        for (pos, name) in labels.tests() {
            let test_name = str::from_utf8(name).unwrap_or("<invalid utf8>");
            // print!("running test '{}'\n", test_name);
            match tester.run_test(pos) {
                Ok(_) if target_name == "-t" => print!("{GREEN}Test \"{}\": success{NORMAL}\n", test_name),
                Ok(msg) => print!("{GREEN}Test \"{}\": success{NORMAL}\n{}", test_name, msg),
                Err(e) => print!("{RED}Test \"{}\": failed{NORMAL}\n{}", test_name, e)
            }
        }
        // dbg!(&*tester as *const test::Tester);
    } else {
        let (builder, labels) = assemble::try_assemble(std::io::BufReader::new(source), None).unwrap();
        let mut target = std::fs::File::create(target_name).expect("could not create output file.");
        labels.check_unlinked_global().unwrap();
        assemble::linux::write_elf((builder, labels), &mut target).unwrap();
    }
}

// TODO let...else
