extern crate nom;
//
//use nom::{
//    IResult,
//    bytes::complete::tag};
//use nom::*;
//use self::nom::error::VerboseError;
//use self::nom::character::complete::{alpha1, multispace0,multispace1};
//use self::nom::combinator::cut;

// interface Window {
//    readonly attribute Storage sessionStorage;
//};

#[derive(Debug, PartialEq)]
pub struct Interface {
    name: String,
    members: Vec<Member>,
}

#[derive(Debug, PartialEq)]
pub enum Member {
    Attribute
}

//type StringParser<Result> = Fn(&str) -> IResult<&str, Result, VerboseError<str>>;

//fn<'a, Result>(&'a str) -> IResult<&'a str, Result, VerboseError<&'a str>>;

//impl Fn(I) -> IResult<I, O, E>

/// Next up is number parsing. We're keeping it simple here by accepting any number (> 1)
/// of digits but ending the program if it doesn't fit into an i32.
fn parse_interface_keyword<'a>(i: &'a str) -> IResult<&'a str, &str, VerboseError<&'a str>> {
    tag("interface")(i)
}

fn parse_name<'a>(i: &'a str) -> IResult<&'a str, &str, VerboseError<&'a str>> {
    cut(alpha1)(i)
}

fn parse_op<'a>(i: &'a str) -> IResult<&'a str, &str, VerboseError<&'a str>> {
    tag("{")(i)
}

fn parse_cl<'a>(i: &'a str) -> IResult<&'a str, &str, VerboseError<&'a str>> {
    tag("}")(i)
}


fn parse_interface<'a>(i: &'a str) -> IResult<&'a str, Interface, VerboseError<&'a str>> {
//    let buf = do_parse![
//        i,
//        parse_interface_keyword >>
//        multispace1 >>
//        name:parse_name >>
//        multispace0 >>
//        parse_op >>
//        multispace0 >>
//        parse_cl >>
//        (name)
//    ](i)?;

    let name = "unknown";

    Ok((i, Interface {
        name: String::from(name),
        members: vec![],
    }))
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_parses_clean<'a, Result>(res: IResult<&'a str, Result, VerboseError<&'a str>>) {
        match res {
            Ok( (out, _)) => assert_eq![ "", out, "did not consume '{}'", out ],
            Err(e) => panic![ "failed to parse" ]
        }
    }

    #[test]
    fn parse_keywords() {
        assert_parses_clean(parse_name("foo"));
        assert_parses_clean(parse_interface_keyword("interface"));
        assert_parses_clean(parse_cl("}"));
        assert_parses_clean(parse_op("{"));

        assert_parses_clean(parse_interface("interface foo {}"))
    }
}