use pom::char_class::{alpha, alphanum, multispace};
use pom::parser::*;
use std::iter::Cloned;

#[derive(Debug, PartialEq)]
pub struct Interface {
    attribs: Vec<AttribDef>,
    name: String,
    supr: Option<String>,
    members: Vec<Member>,
}

impl Default for Interface {
    fn default() -> Self {
        Interface {
            attribs: vec![],
            name: "".to_string(),
            supr: None,
            members: vec![],
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct AttributeDef {
    readonly: bool,
    name: String,
    typ: TypeDef,
    attribs: Vec<AttribDef>,
}

#[derive(Debug, PartialEq)]
pub struct AttribDef {
    name: String,
    value: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct ArgDef {
    name: String,
    typ: TypeDef,
    default_value: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct GetterDef {
    args: Vec<ArgDef>,
    name: Option<String>,
    typ: TypeDef,
}

#[derive(Debug, PartialEq)]
pub struct TypeDef {
    names: Vec<String>,
    optional: bool,
    is_rest: bool,
}

impl TypeDef {
    fn from_str(s: &str) -> Self {
        TypeDef {
            names: vec![s.to_string()],
            optional: false,
            is_rest: false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct DeleterDef {
    args: Vec<ArgDef>,
    typ: TypeDef,
}

#[derive(Debug, PartialEq)]
pub struct SetterDef {
    args: Vec<ArgDef>,
    name: String,
    typ: TypeDef,
}

#[derive(Debug, PartialEq)]
pub struct ImplementsStmt {
    typ: TypeDef,
    implements: TypeDef,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDef {
    args: Vec<ArgDef>,
    name: String,
    typ: TypeDef,
}

#[derive(Debug, PartialEq)]
pub struct IndexerDef {
    args: Vec<ArgDef>,
    typ: TypeDef,
}

#[derive(Debug, PartialEq)]
pub struct EnumDef {
    options: Vec<String>,
    name: String,
}

#[derive(Debug, PartialEq)]
enum Statement {
    Interface(Interface),
    ImplementsStmt(ImplementsStmt),
    CallbackStmt(CallbackDef),
    EnumStmt(EnumDef),
}

#[derive(Debug, PartialEq)]
pub enum Member {
    Attribute(AttributeDef),
    Getter(GetterDef),
    Deleter(DeleterDef),
    Indexer(IndexerDef),
    Setter(SetterDef),
    Function(FunctionDef),
}

fn callback<'a>() -> Parser<'a, u8, CallbackDef> {
    // callback FileCallback = void (File file);
    (keyword(b"callback") * name() - keyword(b"=") + name() - op() + argument_list()
        - cl()
        - semi())
    .map(|((name, fn_name), args)| CallbackDef {
        name,
        fn_name,
        args,
    })
}

#[derive(Debug, PartialEq)]
pub struct CallbackDef {
    name: String,
    fn_name: String,
    args: Vec<ArgDef>,
}

fn ws<'a>() -> Parser<'a, u8, ()> {
    is_a(multispace).discard()
}
fn space<'a>() -> Parser<'a, u8, ()> {
    (ws() | comment()).repeat(0..).discard()
}

fn spaced<'a, T>(parser: Parser<'a, u8, T>) -> Parser<'a, u8, T>
where
    T: 'a,
{
    space() * parser - space()
}

fn is_cr(term: u8) -> bool {
    term == b'\r'
}

fn is_lf(term: u8) -> bool {
    term == b'\n'
}

fn keyword<'a>(literal: &'static [u8]) -> Parser<'a, u8, ()> {
    spaced(seq(literal)).discard().name("keyword")
}

fn literal<'a>(literal: &'static [u8]) -> Parser<'a, u8, String> {
    spaced(seq(literal))
        .map(|u8s| String::from_utf8(u8s.to_vec()).expect("can only parse utf"))
        .name("literal")
}

fn op<'a>() -> Parser<'a, u8, ()> {
    keyword(b"(")
}

fn cl<'a>() -> Parser<'a, u8, ()> {
    keyword(b")")
}

fn opc<'a>() -> Parser<'a, u8, ()> {
    keyword(b"{")
}

fn clc<'a>() -> Parser<'a, u8, ()> {
    keyword(b"}")
}

fn eol<'a>() -> Parser<'a, u8, ()> {
    ((is_a(is_cr) * is_a(is_lf)) | is_a(is_lf) | is_a(is_cr)).discard()
}

fn to_eol<'a>() -> Parser<'a, u8, String> {
    fn anything_else(term: u8) -> bool {
        !is_cr(term) && !is_lf(term)
    }

    is_a(anything_else)
        .repeat(0..)
        .map(|u8s| String::from_utf8(u8s).expect("can only parse utf"))
}

fn semi<'a>() -> Parser<'a, u8, ()> {
    keyword(b";").name("semi")
}

fn line_comment<'a>() -> Parser<'a, u8, ()> {
    (seq(b"//") * to_eol() - eol()).discard()
}

fn star_comment<'a>() -> Parser<'a, u8, ()> {
    fn anything_else(term: u8) -> bool {
        term != b'*'
    }

    (seq(b"/*") * is_a(anything_else).repeat(0..) - seq(b"*/")).discard()
}

fn comment<'a>() -> Parser<'a, u8, ()> {
    line_comment() | star_comment()
}

fn name<'a>() -> Parser<'a, u8, String> {
    let it = (is_a(alpha) + is_a(alphanum).repeat(0..))
        .map(|(first, rest)| format!("{}{}", first as char, String::from_utf8(rest).unwrap()));

    spaced(it).name("name")
}

fn type_name<'a>() -> Parser<'a, u8, String> {
    literal(b"unsigned long") | name()
}

fn typ<'a>() -> Parser<'a, u8, TypeDef> {
    //(VideoTrack or AudioTrack or TextTrack)
    let compound_type = (op() * list(type_name(), keyword(b"or")) - cl()).map(|names| names);

    //AudioTrack
    let simple_type = type_name().map(|n| vec![n]);

    // TODO optional - eg (A or B)?
    let is_optional = keyword(b"?").opt().map(|x| x.is_some());
    let is_rest = keyword(b"...").opt().map(|x| x.is_some());

    ((compound_type | simple_type) + is_optional + is_rest).map(|((names, optional), is_rest)| {
        TypeDef {
            names,
            optional,
            is_rest,
        }
    })
}

fn attribute<'a>() -> Parser<'a, u8, AttributeDef> {
    // [PutForwards=href, Unforgeable] attribute DOMString value;
    let attribs = attrib_list();
    let readonly = keyword(b"readonly").opt().map(|ro| ro.is_some());
    let attribute = keyword(b"attribute");
    let member_raw = attribs + readonly - attribute + typ() + name() - semi();

    member_raw.map(move |(((attribs, readonly), typ), name)| AttributeDef {
        readonly,
        name,
        typ,
        attribs,
    })
}

fn value<'a>() -> Parser<'a, u8, String> {
    // TODO: bug - I don't distinguish between "value" and value -- will need to fix
    name() | string()
}

fn enum_def<'a>() -> Parser<'a, u8, EnumDef> {
    // enum DocumentReadyState { "loading", "interactive", "complete" };
    (keyword(b"enum") * name() - opc() + list(string(), keyword(b",")) - clc() - semi())
        .map(|(name, options)| EnumDef { name, options })
}
fn implements_stmt<'a>() -> Parser<'a, u8, ImplementsStmt> {
    // Foo implements Bar;
    let raw = typ() - keyword(b"implements") + typ() - semi();
    raw.map(|(typ, implements)| ImplementsStmt { typ, implements })
}

fn argument<'a>() -> Parser<'a, u8, ArgDef> {
    // (HTMLOptionElement or HTMLOptGroupElement) element, optional (HTMLElement or long)? before = null
    let parser_raw = keyword(b"optional").opt() * typ() + name() + (keyword(b"=") * value()).opt();
    parser_raw.map(move |((typ, name), default_value)| ArgDef {
        name,
        typ,
        default_value,
    })
}
fn argument_list<'a>() -> Parser<'a, u8, Vec<ArgDef>> {
    spaced(list(argument(), sym(b',')))
}

fn attrib_list<'a>() -> Parser<'a, u8, Vec<AttribDef>> {
    let attrib =
        (name() + (keyword(b"=") * value()).opt()).map(|(name, value)| AttribDef { name, value });

    let comma_sep = keyword(b"[") * list(attrib, keyword(b",")) - keyword(b"]");

    let list = comma_sep.repeat(0..);

    list.map(|list_of_lists| {
        let mut result = vec![];
        for outer in list_of_lists.into_iter() {
            for inner in outer.into_iter() {
                result.push(inner);
            }
        }
        result
    })
}

fn attrib<'a>() -> Parser<'a, u8, AttribDef> {
    // [PutForwards=value]
    // [OverrideBuiltins]
    // [NamedConstructor=Audio(optional DOMString src)]
    (keyword(b"[") * name() + (keyword(b"=") * value()).opt() - keyword(b"]"))
        .map(|(name, value)| AttribDef { name, value })
}

fn getter<'a>() -> Parser<'a, u8, GetterDef> {
    // getter DOMString (DOMString name, DOMString value);
    let getter_raw = keyword(b"legacycaller").opt() * keyword(b"getter") * typ() + name().opt()
        - op()
        + argument_list()
        - cl()
        - semi();
    getter_raw.map(move |((typ, name), args)| GetterDef { args, name, typ })
}

fn indexer<'a>() -> Parser<'a, u8, IndexerDef> {
    // DOMString (DOMString name, DOMString value);
    let indexer_raw =
        keyword(b"legacycaller").opt() * typ() - op() + argument_list() - cl() - semi();
    indexer_raw.map(move |(typ, args)| IndexerDef { args, typ })
}

fn deleter<'a>() -> Parser<'a, u8, DeleterDef> {
    // deleter void (DOMString name);
    let deleter_raw = keyword(b"legacycaller").opt() * keyword(b"deleter") * typ() - op()
        + argument_list()
        - cl()
        - semi();
    deleter_raw.map(move |(typ, args)| DeleterDef { args, typ })
}

fn setter<'a>() -> Parser<'a, u8, SetterDef> {
    // setter creator void (DOMString name, DOMString value);
    let setter_raw = keyword(b"legacycaller").opt() * keyword(b"setter") * name() + typ() - op()
        + argument_list()
        - cl()
        - semi();
    setter_raw.map(move |((name, typ), args)| SetterDef { name, args, typ })
}

fn function<'a>() -> Parser<'a, u8, FunctionDef> {
    //HTMLAllCollection tags(DOMString tagName);
    let function_raw =
        keyword(b"legacycaller").opt() * typ() + name() - op() + argument_list() - cl() - semi();
    function_raw.map(|((typ, name), args)| FunctionDef { name, args, typ })
}

fn member<'a>() -> Parser<'a, u8, Member> {
    attribute().map(|a| Member::Attribute(a))
        | getter().map(|g| Member::Getter(g))
        | indexer().map(|i| Member::Indexer(i))
        | setter().map(|s| Member::Setter(s))
        | deleter().map(|d| Member::Deleter(d))
        | function().map(|f| Member::Function(f))
}

fn member_list<'a>() -> Parser<'a, u8, Vec<Member>> {
    member().repeat(0..)
}

fn interface<'a>() -> Parser<'a, u8, Interface> {
    let interface = attrib_list() - keyword(b"partial").opt().discard() * keyword(b"interface")
        + name()
        + (keyword(b":") * name()).opt()
        - keyword(b"{")
        + member_list()
        - keyword(b"}")
        - semi();
    interface.map(|(((attribs, name), supr), members)| Interface {
        attribs,
        name,
        supr,
        members,
    })
}

fn stmt<'a>() -> Parser<'a, u8, Statement> {
    interface().map(|i| Statement::Interface(i))
        | implements_stmt().map(|i| Statement::ImplementsStmt(i))
        | callback().map(|c| Statement::CallbackStmt(c))
        | enum_def().map(|e| Statement::EnumStmt(e))
}

fn string<'a>() -> Parser<'a, u8, String> {
    let special_char = sym(b'\\')
        | sym(b'/')
        | sym(b'"')
        | sym(b'b').map(|_| b'\x08')
        | sym(b'f').map(|_| b'\x0C')
        | sym(b'n').map(|_| b'\n')
        | sym(b'r').map(|_| b'\r')
        | sym(b't').map(|_| b'\t');
    let escape_sequence = sym(b'\\') * special_char;
    let string = sym(b'"') * (none_of(b"\\\"") | escape_sequence).repeat(0..) - sym(b'"');
    string.convert(String::from_utf8)
}

fn idl<'a>() -> Parser<'a, u8, Vec<Statement>> {
    stmt().repeat(0..)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::cmp::min;
    use std::error::Error;

    macro_rules! assert_consumes_all {
        ( $ parser: expr, $input: expr ) => {
            let terminating_parser = $parser - space() - end();
            let res = terminating_parser.parse($input);
            if let Err(_) = res {
                panic!("parser failed to match and consume everything")
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
                Err(_) => {
                    //
                    panic!("parser failed to match and consume everything")
                }
            }
        };
    }

    #[test]
    fn parse_keywords() -> Result<(), Box<dyn Error>> {
        assert_consumes_all![eol(), b"\r"];
        assert_consumes_all![eol(), b"\r\n"];
        assert_consumes_all![eol(), b"\n"];

        assert_consumes_all![space(), b""];
        assert_consumes_all![space(), b"  "];
        assert_consumes_all![space(), b"  \t \n \r "];

        assert_consumes_all![line_comment(), b"//\r"];
        assert_consumes_all![line_comment(), b"//\n"];
        assert_consumes_all![line_comment(), b"//\r\n"];
        assert_consumes_all![line_comment(), b"// xyz \r\n"];

        assert_consumes_all![star_comment(), b"/*  thing */"];
        assert_consumes_all![star_comment(), b"/*  thing \r\n thing */"];

        assert_consumes_all!(
            enum_def(),
            b"enum DocumentReadyState { \"loading\", \"interactive\", \"complete\" };"
        );

        assert_consumes_all![typ(), b"TypeName"];
        assert_consumes_all![typ(), b"(TypeName1 or TypeName2)"];

        assert_consumes_all!(argument(), b"ArgType name");
        assert_consumes_all!(argument(), b"optional ArgType name");
        assert_consumes_all!(argument(), b"ArgType name = null");
        assert_consumes_all!(
            argument(),
            b"ArgType name",
            ArgDef {
                typ: TypeDef::from_str("ArgType"),
                name: "name".into(),
                default_value: None
            }
        );

        assert_consumes_all!(
            implements_stmt(),
            b"ArrayBuffer implements Transferable;",
            ImplementsStmt {
                typ: TypeDef::from_str("ArrayBuffer"),
                implements: TypeDef::from_str("Transferable")
            }
        );

        assert_consumes_all!(attrib_list(), b"[OverrideBuiltins]");
        assert_consumes_all!(attrib_list(), b"[A][B]");
        assert_consumes_all!(attrib_list(), b"[A,B]");
        assert_consumes_all!(attrib_list(), b"[A,B][C]");
        assert_consumes_all!(attrib_list(), b"[PutForwards=value]");
        // assert_consumes_all!(
        //     attrib(),
        //     b"[NamedConstructor=Audio(optional DOMString src)]"
        // );

        assert_consumes_all!(
            argument_list(),
            b"ArgType1 name1, ArgType2 name2",
            vec![
                ArgDef {
                    typ: TypeDef::from_str("ArgType1"),
                    name: "name1".into(),
                    default_value: None
                },
                ArgDef {
                    typ: TypeDef::from_str("ArgType2"),
                    name: "name2".into(),
                    default_value: None
                }
            ]
        );

        assert_consumes_all!(type_name(), b"unsigned long");
        assert_consumes_all!(type_name(), b"DomString");

        assert_consumes_all!(callback(), b"callback FileCallback = void (File file);");
        assert_consumes_all!(string(), b"\"thing\"");

        assert_consumes_all!(typ(), b"HTMLOptionElement?");
        assert_consumes_all!(
            member(),
            b"[PutForwards=href, Unforgeable] readonly attribute DomString length;"
        );
        assert_consumes_all!(
            member(),
            b"legacycaller HTMLOptionElement? (DOMString name);"
        );

        assert_consumes_all!(
            member(),
            b"attribute DOMString value;",
            Member::Attribute(AttributeDef {
                readonly: false,
                name: String::from("value"),
                typ: TypeDef::from_str("DOMString"),
                attribs: vec![]
            })
        );

        assert_consumes_all!(
            member(),
            b"readonly attribute DOMString value;",
            Member::Attribute(AttributeDef {
                readonly: true,
                name: String::from("value"),
                typ: TypeDef::from_str("DOMString"),
                attribs: vec![]
            })
        );

        assert_consumes_all!(
            member(),
            b"getter ReturnType ();",
            Member::Getter(GetterDef {
                args: vec![],
                name: None,
                typ: TypeDef::from_str("ReturnType"),
            })
        );

        assert_consumes_all!(
            member(),
            b"legacycaller getter ReturnType namedItem();",
            Member::Getter(GetterDef {
                args: vec![],
                name: Some("namedItem".into()),
                typ: TypeDef::from_str("ReturnType"),
            })
        );

        assert_consumes_all!(
            member(),
            b"getter ReturnType (ArgType name);",
            Member::Getter(GetterDef {
                args: vec![ArgDef {
                    name: String::from("name"),
                    typ: TypeDef::from_str("ArgType"),
                    default_value: None
                }],
                name: None,
                typ: TypeDef::from_str("ReturnType"),
            })
        );

        assert_consumes_all!(
            member(),
            b"setter creator void (ArgType name);",
            Member::Setter(SetterDef {
                args: vec![ArgDef {
                    name: "name".into(),
                    typ: TypeDef::from_str("ArgType"),
                    default_value: None
                }],
                name: "creator".into(),
                typ: TypeDef::from_str("void"),
            })
        );

        assert_consumes_all!(
            member(),
            b"HTMLAttribute read (ArgType name);",
            Member::Function(FunctionDef {
                args: vec![ArgDef {
                    name: "name".into(),
                    typ: TypeDef::from_str("ArgType"),
                    default_value: None
                }],
                name: "read".into(),
                typ: TypeDef::from_str("HTMLAttribute"),
            })
        );

        assert_consumes_all!(
            member(),
            b"deleter void (ArgType name);",
            Member::Deleter(DeleterDef {
                args: vec![ArgDef {
                    name: "name".into(),
                    typ: TypeDef::from_str("ArgType"),
                    default_value: None
                }],
                typ: TypeDef::from_str("void"),
            })
        );

        assert_consumes_all![
            interface(),
            b"interface foo {};",
            Interface {
                name: String::from("foo"),
                ..Default::default()
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
                ..Default::default()
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
                    typ: TypeDef::from_str("DOMString"),
                    attribs: vec![]
                }),
                Member::Attribute(AttributeDef {
                    readonly: false,
                    name: String::from("body"),
                    typ: TypeDef::from_str("HTMLElement"),
                    attribs: vec![]
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
                        typ: TypeDef::from_str("DOMString"),
                        attribs: vec![]
                    }),
                    Member::Attribute(AttributeDef {
                        readonly: false,
                        name: String::from("defaultSelected"),
                        typ: TypeDef::from_str("boolean"),
                        attribs: vec![]
                    }),
                ],
                ..Default::default()
            }
        );

        Ok(())
    }

    #[test]
    fn parse_valid_so_far_file() {
        let file_path_str = "assets/valid_so_far.idl";
        assert_parse_file(file_path_str);
    }

    fn count_lines(byte_slice: &[u8]) -> usize {
        let line_parser = (to_eol() - eol()).repeat(0..); // (to_eol() + (eol() | end())).discard().repeat(0..);

        let parse_result = line_parser.parse(byte_slice).unwrap();

        parse_result.len()
    }

    #[test]
    fn line_counter_works() {
        let file_path_str = "assets/html5.idl";
        let byte_vec: Vec<u8> = std::fs::read(file_path_str).unwrap();
        let actual = count_lines(&byte_vec);
        assert_eq!(1388, actual);
    }

    #[test]
    fn parse_full_html5_file() {
        let file_path_str = "assets/html5_ordered_by_steve.idl";
        assert_parse_file(file_path_str);
    }

    fn assert_parse_file(file_path_str: &str) {
        let byte_vec: Vec<u8> = std::fs::read(file_path_str).unwrap();
        let file_content =
            String::from_utf8(byte_vec.clone()).expect("should be able to read the file");
        let byte_slice: &[u8] = &byte_vec;
        let idl_parser = idl() - space() - end();
        let parse_result = match idl_parser.parse(byte_slice) {
            Ok(parse_result) => parse_result,
            Err(pom::Error::Mismatch { message, position }) => {
                let start_str = &byte_vec[0..position];
                let line = count_lines(start_str) + 1;
                let end = min(position + 50, file_content.len() - position);
                let extract = &file_content[position..end];
                let extract = extract
                    .to_string()
                    .replace("\n", "\\n")
                    .replace("\r", "\\r")
                    .replace("\t", "\\t");
                let err_location = format!("{}:{}:{}", file_path_str, line, 1);
                // thread 'idl_parser::test::parse_full_html5_file' panicked at 'whoops', src/idl_parser.rs:428:9
                let better_message = format!(
                    "thread 'idl_parser::test::parse_full_html5_file' panicked at 'parsing', {}\n\n{}",
                    err_location, extract
                );
                println!("{}", better_message);
                panic!(message)
            }
            Err(e) => panic!("{}", e),
        };
        println!("{:?}", parse_result);
    }
}
