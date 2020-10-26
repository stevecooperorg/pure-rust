use failure::Error;
use pom::char_class::{alpha, alphanum, multispace};
use pom::parser::*;

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
pub struct ArgDef {
    name: String,
    typ: String,
}

#[derive(Debug, PartialEq)]
pub struct GetterDef {
    args: Vec<ArgDef>,
    typ: String,
}

#[derive(Debug, PartialEq)]
pub struct DeleterDef {
    args: Vec<ArgDef>,
    typ: String,
}

#[derive(Debug, PartialEq)]
pub struct SetterDef {
    args: Vec<ArgDef>,
    name: String,
    typ: String,
}

#[derive(Debug, PartialEq)]
pub enum Member {
    Attribute(AttributeDef),
    Getter(GetterDef),
    Deleter(DeleterDef),
    Setter(SetterDef),
}

fn space<'a>() -> Parser<'a, u8, ()> {
    is_a(multispace).repeat(0..).discard()
}

fn name<'a>() -> Parser<'a, u8, String> {
    (is_a(alpha) + is_a(alphanum).repeat(0..))
        .map(|(first, rest)| format!("{}{}", first as char, String::from_utf8(rest).unwrap()))
}

fn attribute<'a>() -> Parser<'a, u8, Member> {
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

fn argument<'a>() -> Parser<'a, u8, ArgDef> {
    let typ = space() * name();
    let name = space() * name();
    let parser_raw = typ + name - space();
    parser_raw.map(move |(typ, name)| ArgDef { name, typ })
}
fn argument_list<'a>() -> Parser<'a, u8, Vec<ArgDef>> {
    list(argument(), sym(b',') - space())
}

fn getter<'a>() -> Parser<'a, u8, Member> {
    // getter DOMString (DOMString name, DOMString value);
    let args = argument_list();
    let getter = space() * seq(b"getter").discard() * space();
    let typ = space() * name();

    let getter_raw = getter * typ - space() - sym(b'(') + args - sym(b')') - sym(b';');

    getter_raw.map(move |(typ, args)| Member::Getter(GetterDef { args, typ }))
}

fn deleter<'a>() -> Parser<'a, u8, Member> {
    // deleter void (DOMString name);
    let args = argument_list();
    let getter = space() * seq(b"deleter").discard() * space();
    let typ = space() * name();

    let deleter_raw = getter * typ - space() - sym(b'(') + args - sym(b')') - sym(b';');

    deleter_raw.map(move |(typ, args)| Member::Deleter(DeleterDef { args, typ }))
}

fn setter<'a>() -> Parser<'a, u8, Member> {
    // setter creator void (DOMString name, DOMString value);
    let args = argument_list();
    let setter = space() * seq(b"setter").discard() * space();
    let nam = (space() * name()).name("name");
    let typ = (space() * name()).name("type");

    let setter_raw =
        setter * nam - space() + typ - space() - sym(b'(') + args - sym(b')') - sym(b';');

    setter_raw.map(move |((name, typ), args)| Member::Setter(SetterDef { name, args, typ }))
}

fn member<'a>() -> Parser<'a, u8, Member> {
    attribute() | getter() | setter() | deleter()
}

fn member_list<'a>() -> Parser<'a, u8, Vec<Member>> {
    member().repeat(0..)
}

fn interface<'a>() -> Parser<'a, u8, Interface> {
    let interface_start = seq(b"interface") * space() * name() - space() - sym(b'{');

    let interface_close = space() - sym(b'}') - space() - sym(b';') - space();
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
    //use std::path::Path;

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
                Err(e) => {
                    //
                    panic!("parser failed to match and consume everthing")
                }
            }
        };
    }

    #[test]
    fn parse_keywords() {
        assert_consumes_all![space(), b""];
        assert_consumes_all![space(), b"  "];
        assert_consumes_all![space(), b"  \t \n \r "];

        assert_consumes_all!(
            argument(),
            b"ArgType name",
            ArgDef {
                typ: "ArgType".into(),
                name: "name".into()
            }
        );
        assert_consumes_all!(
            argument_list(),
            b"ArgType1 name1, ArgType2 name2",
            vec![
                ArgDef {
                    typ: "ArgType1".into(),
                    name: "name1".into()
                },
                ArgDef {
                    typ: "ArgType2".into(),
                    name: "name2".into()
                }
            ]
        );

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

        assert_consumes_all!(
            member(),
            b"getter ReturnType ();",
            Member::Getter(GetterDef {
                args: vec![],
                typ: String::from("ReturnType"),
            })
        );

        assert_consumes_all!(
            member(),
            b"getter ReturnType (ArgType name);",
            Member::Getter(GetterDef {
                args: vec![ArgDef {
                    name: String::from("name"),
                    typ: String::from("ArgType"),
                }],
                typ: String::from("ReturnType"),
            })
        );

        assert_consumes_all!(
            member(),
            b"setter creator void (ArgType name);",
            Member::Setter(SetterDef {
                args: vec![ArgDef {
                    name: "name".into(),
                    typ: "ArgType".into(),
                }],
                name: "creator".into(),
                typ: "void".into(),
            })
        );

        assert_consumes_all!(
            member(),
            b"deleter void (ArgType name);",
            Member::Deleter(DeleterDef {
                args: vec![ArgDef {
                    name: "name".into(),
                    typ: "ArgType".into(),
                }],
                typ: "void".into(),
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
    fn parse_valid_so_far_file() {
        let file_path_str = "assets/valid_so_far.idl";
        assert_parse_file(file_path_str);
    }

    //#[test]
    fn parse_full_html5_file() {
        let file_path_str = "assets/html5.idl";
        assert_parse_file(file_path_str);
    }

    fn assert_parse_file(file_path_str: &str) {
        let byte_vec: Vec<u8> = std::fs::read(file_path_str).unwrap();
        let byte_slice: &[u8] = &byte_vec;
        let idl_parser = idl() - space() - end();
        let parse_result = idl_parser.parse(byte_slice).unwrap();
        println!("{:?}", parse_result);
    }
}
