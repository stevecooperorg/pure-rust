use crate::idl_parser::idl;
use std::fs::{self, DirEntry};
use std::io;
use std::path::Path;

mod dom;
mod idl_parser;
mod idl_to_dom;

pub type Result<T> = std::result::Result<T, failure::Error>;

pub fn run_generator() -> Result<()> {
    let file_path_str = "assets/valid_so_far.idl";
    let byte_vec: Vec<u8> = std::fs::read(file_path_str).unwrap();
    let byte_slice: &[u8] = &byte_vec;
    let parser = idl();
    let parsed = parser.parse(byte_slice);
    println!("{:?}", parsed);
    Ok(())
}

// one possible implementation of walking a directory only visiting files
pub fn visit_dirs(dir: &Path, cb: &dyn Fn(&DirEntry)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&entry);
            }
        }
    }
    Ok(())
}
