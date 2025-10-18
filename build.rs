// build.rs 

use std::process::Command;
use std::env;
use std::path::PathBuf;
use anyhow::Context; 

fn main() -> anyhow::Result<()> {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").context("CARGO_MANIFEST_DIR not set")?);
    let out_dir = PathBuf::from(env::var("OUT_DIR").context("OUT_DIR not set")?);
    
    let c_parser_dir = manifest_dir.join("c_parser");
    let bison_file_path = c_parser_dir.join("parser.y");
    let yylex_driver_path = c_parser_dir.join("yylex_driver.c");

    if !bison_file_path.exists() {
        anyhow::bail!("Bison input file not found: {}", bison_file_path.display());
    }
    if !yylex_driver_path.exists() {
        anyhow::bail!("C driver file not found: {}", yylex_driver_path.display());
    }

    println!("cargo:rerun-if-changed={}", bison_file_path.display());
    
    let output = Command::new("bison")
        .arg("-d") 
        .arg("-v")
        .arg("-o")
        .arg(out_dir.join("parser.tab.c")) 
        .arg(&bison_file_path) 
        .current_dir(&out_dir) 
        .output()
        .context("Failed to execute bison. Is it installed and in your PATH?")?; 

    if !output.status.success() {
        eprintln!("Bison failed!");
        eprintln!("stdout: {}", String::from_utf8_lossy(&output.stdout));
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        anyhow::bail!("Bison execution failed.");
    }
    
    let c_parser_dir = manifest_dir.join("c_parser"); 
     cc::Build::new()
        .file(out_dir.join("parser.tab.c"))
        .file(&yylex_driver_path) 
        .include(&out_dir) 
        .include(&c_parser_dir) 
        .compile("bison_parser");

    println!("cargo:rustc-link-lib=static=bison_parser");
    
    Ok(())
}