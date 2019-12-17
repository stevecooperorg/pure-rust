mod idl_parser;

pub(crate) type Result<T> = std::result::Result<T, failure::Error>;

pub fn run_generator() -> Result<()> {
    let f = std::fs::read_to_string("assets/html5_partial.idl")?;

    let parse_result = weedle::parse(&f);

    match parse_result {
        Ok(parsed) => {
            println!("{:?}", parsed);
            println!("success!");
        }
        Err(e) => {
            eprintln!("{:?}", e);
            eprintln!("failure!");
        }
    }

    Ok(())
}