extern crate pom;

use pom::parser::*;
//use pom::parser::Parser;
use self::pom::char_class::{alpha, alphanum, multispace};
use failure::Error;
use std::path::Path;

#[derive(Debug, PartialEq)]
pub struct Interface {
    name: String,
    members: Vec<Member>,
}

#[derive(Debug, PartialEq)]
pub struct AttributeDef {
    readonly: bool,
    name: String,
    typ: String,
}

#[derive(Debug, PartialEq)]
pub enum Member {
    Attribute(AttributeDef),
}

fn space<'a>() -> Parser<'a, u8, ()> {
    is_a(multispace).repeat(0..).discard()
}

fn name<'a>() -> Parser<'a, u8, String> {
    (is_a(alpha) + is_a(alphanum).repeat(0..))
        .map(|(first, rest)| format!("{}{}", first as char, String::from_utf8(rest).unwrap()))
}

fn member<'a>() -> Parser<'a, u8, Member> {
    // attribute DOMString value;
    let readonly = space() * seq(b"readonly").discard().opt().map(|ro| ro.is_some());
    let attribute = space() * seq(b"attribute").discard() * space();
    let typ = space() * name();
    let nam = space() * name();

    let member_raw = readonly - attribute + typ + nam - space() - sym(b';');

    member_raw.map(move |((readonly, typ), name)| {
        Member::Attribute(AttributeDef {
            readonly,
            name,
            typ,
        })
    })
}

fn member_list<'a>() -> Parser<'a, u8, Vec<Member>> {
    member().repeat(0..)
}

fn interface<'a>() -> Parser<'a, u8, Interface> {
    let interface_start = (seq(b"interface") * space() * name() - space() - sym(b'{'));

    let interface_close = space() - sym(b'}') - space() - sym(b';');
    let interface = interface_start + member_list() - interface_close;

    interface.map(|(name, members)| Interface { name, members })
}

fn idl<'a>() -> Parser<'a, u8, Vec<Interface>> {
    interface().repeat(0..)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::error::Error;
    use std::fmt::Display;
    use std::path::Path;

    macro_rules! assert_consumes_all {
        ( $ parser: expr, $input: expr ) => {
            let terminating_parser = $parser - space() - end();
            let res = terminating_parser.parse($input);
            if let Err(e) = res {
                panic!("parser failed to match and consume everthing")
            }
        };
        ( $ parser: expr, $input: expr, $expected: expr) => {
            let terminating_parser = $parser - space() - end();
            let res = terminating_parser.parse($input);
            match res {
                Ok(answer) => {
                    // it parsed, but was it right?
                    assert_eq!(answer, $expected)
                }
                Err(e) => panic!("parser failed to match and consume everthing"),
            }
        };
    }

    #[test]
    fn parse_keywords() {
        assert_consumes_all![space(), b""];
        assert_consumes_all![space(), b"  "];
        assert_consumes_all![space(), b"  \t \n \r "];

        assert_consumes_all!(
            member(),
            b"attribute DOMString value;",
            Member::Attribute(AttributeDef {
                readonly: false,
                name: String::from("value"),
                typ: String::from("DOMString"),
            })
        );

        assert_consumes_all!(
            member(),
            b"readonly attribute DOMString value;",
            Member::Attribute(AttributeDef {
                readonly: true,
                name: String::from("value"),
                typ: String::from("DOMString"),
            })
        );

        assert_consumes_all![
            interface(),
            b"interface foo {};",
            Interface {
                name: String::from("foo"),
                members: vec![]
            }
        ];
        assert_consumes_all!(
            interface(),
            b"interface foo { attribute DOMString value; };"
        );

        assert_consumes_all!(
            member_list(),
            b"
readonly attribute DOMString value;
attribute HTMLElement body;",
            vec![
                Member::Attribute(AttributeDef {
                    readonly: true,
                    name: String::from("value"),
                    typ: String::from("DOMString"),
                }),
                Member::Attribute(AttributeDef {
                    readonly: false,
                    name: String::from("body"),
                    typ: String::from("HTMLElement"),
                }),
            ]
        );

        assert_consumes_all!(
            interface(),
            b"interface Window {
           readonly attribute DOMString label;
           attribute boolean defaultSelected;
};
",
            Interface {
                name: String::from("Window"),
                members: vec![
                    Member::Attribute(AttributeDef {
                        readonly: true,
                        name: String::from("label"),
                        typ: String::from("DOMString"),
                    }),
                    Member::Attribute(AttributeDef {
                        readonly: false,
                        name: String::from("defaultSelected"),
                        typ: String::from("boolean"),
                    }),
                ]
            }
        );
    }

    #[test]
    fn parse_files() {
        let byte_vec: Vec<u8> = std::fs::read("assets/valid_so_far.idl").unwrap();
        let byte_slice: &[u8] = &byte_vec;
        let idl_parser = idl() - space() - end();
        let parse_result = idl_parser.parse(byte_slice).unwrap();
        println!("{:?}", parse_result);
    }
}
