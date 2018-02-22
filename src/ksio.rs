use std::io::{self, Write};

#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    let x = x as u8;
    let stdout = io::stdout();
    let mut handle = stdout.lock();

    handle.write(&[x]).unwrap();
    handle.flush().expect("Could not flush to standard output.");
    0.0
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    let stdout = io::stdout();
    let mut handle = stdout.lock();

    handle.flush().expect("Could not flush to standard output.");
    0.0
}
