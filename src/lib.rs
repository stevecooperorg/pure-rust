use crate::idl_parser::idl;

mod idl_parser;

pub(crate) type Result<T> = std::result::Result<T, failure::Error>;

pub fn run_generator() -> Result<()> {
    let file_path_str = "assets/valid_so_far.idl";
    let byte_vec: Vec<u8> = std::fs::read(file_path_str).unwrap();
    let byte_slice: &[u8] = &byte_vec;
    let parser = idl();
    let parsed = parser.parse(byte_slice);
    println!("{:?}", parsed);
    Ok(())
}
