use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() {
    let out_dir = env::var("OUT_DIR").expect("No out dir");
    let dest_path = Path::new(&out_dir);
    let mut f = File::create(&dest_path.join("link.ld"))
        .expect("Could not create file");

    f.write_all(include_bytes!("src/link.ld"))
        .expect("Could not write file");

    println!("cargo:rustc-link-search={}", dest_path.display());

    println!("cargo:rerun-if-changed=src/link.x");
    println!("cargo:rerun-if-changed=build.rs");
}