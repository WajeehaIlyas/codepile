use std::fs;
use std::io;

/// Reads the entire source file into a String.
pub fn read_source_file(path: &str) -> io::Result<String> {
    fs::read_to_string(path)
}
