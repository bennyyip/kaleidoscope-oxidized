// build.rs

use std::process::Command;
use std::env;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();

    Command::new("rustc")
        .args(&["src/ksio.rs", "--crate-type=cdylib", "-o"])
        .arg(&format!("{}/libksio.so", out_dir))
        .status()
        .unwrap();

    println!("cargo:rustc-link-search=native={}", out_dir);
    println!("cargo:rustc-link-lib=ksio");
}
