use pom::char_class::{alpha, alphanum, multispace};
use pom::parser::*;

#[derive(Debug, PartialEq)]
pub struct Interface {
    name: String,
    supr: Option<String>,
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

fn spaced<'a, T>(parser: Parser<'a, u8, T>) -> Parser<'a, u8, T>
where
    T: 'a,
{
    space() * parser - space()
}

fn name<'a>() -> Parser<'a, u8, String> {
    let it = (is_a(alpha) + is_a(alphanum).repeat(0..))
        .map(|(first, rest)| format!("{}{}", first as char, String::from_utf8(rest).unwrap()));

    spaced(it).name("name")
}

fn typ<'a>() -> Parser<'a, u8, String> {
    spaced(name()).name("type")
}

fn semi<'a>() -> Parser<'a, u8, ()> {
    spaced(sym(b';')).discard().name("semi")
}

fn attribute<'a>() -> Parser<'a, u8, Member> {
    // attribute DOMString value;
    let x = b"readonly";
    let readonly = spaced(seq(b"readonly"))
        .discard()
        .opt()
        .map(|ro| ro.is_some());
    let attribute = spaced(seq(b"attribute")).discard();
    let typ = typ();
    let nam = name();

    let member_raw = readonly - attribute + typ + nam - semi();

    member_raw.map(move |((readonly, typ), name)| {
        Member::Attribute(AttributeDef {
            readonly,
            name,
            typ,
        })
    })
}

fn argument<'a>() -> Parser<'a, u8, ArgDef> {
    let parser_raw = typ() + name();
    parser_raw.map(move |(typ, name)| ArgDef { name, typ })
}
fn argument_list<'a>() -> Parser<'a, u8, Vec<ArgDef>> {
    spaced(list(argument(), sym(b',')))
}

fn getter<'a>() -> Parser<'a, u8, Member> {
    // getter DOMString (DOMString name, DOMString value);
    let args = argument_list();
    let getter = spaced(seq(b"getter")).discard();

    let getter_raw = getter * typ() - sym(b'(') + args - sym(b')') - semi();

    getter_raw.map(move |(typ, args)| Member::Getter(GetterDef { args, typ }))
}

fn deleter<'a>() -> Parser<'a, u8, Member> {
    // deleter void (DOMString name);
    let args = argument_list();
    let getter = spaced(seq(b"deleter")).discard();
    let typ = typ();

    let deleter_raw = getter * typ - sym(b'(') + args - sym(b')') - semi();

    deleter_raw.map(move |(typ, args)| Member::Deleter(DeleterDef { args, typ }))
}

fn setter<'a>() -> Parser<'a, u8, Member> {
    // setter creator void (DOMString name, DOMString value);
    let args = argument_list();
    let setter = spaced(seq(b"setter")).discard();
    let nam = name();
    let typ = typ();

    let setter_raw = setter * nam + typ - sym(b'(') + args - sym(b')') - semi();

    setter_raw.map(move |((name, typ), args)| Member::Setter(SetterDef { name, args, typ }))
}

fn member<'a>() -> Parser<'a, u8, Member> {
    attribute() | getter() | setter() | deleter()
}

fn member_list<'a>() -> Parser<'a, u8, Vec<Member>> {
    member().repeat(0..)
}

fn interface<'a>() -> Parser<'a, u8, Interface> {
    let interface_start = spaced(seq(b"interface")).discard() * name()
        + (spaced(sym(b':')) * name()).opt()
        - spaced(sym(b'{'));

    let interface_close = spaced(sym(b'}')) - semi();
    let interface = interface_start + member_list() - interface_close;

    interface.map(|((name, supr), members)| Interface {
        name,
        supr,
        members,
    })
}

fn idl<'a>() -> Parser<'a, u8, Vec<Interface>> {
    interface().repeat(0..)
}

#[cfg(test)]
mod test {
    use super::*;

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
                supr: None,
                members: vec![]
            }
        ];
        assert_consumes_all!(
            interface(),
            b"interface foo { attribute DOMString value; };"
        );

        assert_consumes_all!(
            interface(),
            b"interface foo : bar { };",
            Interface {
                name: "foo".into(),
                supr: Some("bar".into()),
                members: vec![]
            }
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
                supr: None,
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
