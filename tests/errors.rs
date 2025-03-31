use std::env;
use std::process::{Command, Stdio};
use std::path::Path;
use std::fs;

#[test]
fn test_errors() {
    println!("y");
    let errors_dir = Path::new("tests/errors");
    let exe_filepath = env!("CARGO_BIN_EXE_ren");

    for entry in fs::read_dir(errors_dir).unwrap() {
        let path = entry.unwrap().path();
        println!("{:?}:", path);
        let output = Command::new(exe_filepath)
        .arg(path)
        .stdout(Stdio::null())   // <<< Discard standard output
        .stderr(Stdio::piped())  // <<< Capture standard error via a pipe
        .output() // <<< Spawns, waits, and collects output
        .expect("Failed to execute command");
        println!("{}", String::from_utf8_lossy(&output.stderr));
    }
    println!();
}