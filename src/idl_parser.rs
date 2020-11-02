use pom::char_class::{alpha, alphanum, multispace};
use pom::parser::*;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub struct Attribute {
    readonly: bool,
    name: String,
    typ: Type,
    attribs: Vec<Attrib>,
}

#[derive(Debug, PartialEq)]
pub struct Field {
    name: String,
    typ: Type,
    attribs: Vec<Attrib>,
}

#[derive(Debug, PartialEq)]
pub struct Dictionary {
    attribs: Vec<Attrib>,
    name: String,
    supr: Option<String>,
    members: Vec<Member>,
}

#[derive(Debug, PartialEq)]
pub struct Interface {
    attribs: Vec<Attrib>,
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
pub enum Attrib {
    Attrib(AttribDef),
    Constructor(Constructor),
}

#[derive(Debug, PartialEq)]
pub struct AttribDef {
    name: String,
    value: Option<AttribRHS>,
}

#[derive(Debug, PartialEq)]
pub struct Constructor {
    args: Vec<Argument>,
}

#[derive(Debug, PartialEq)]
pub enum AttribRHS {
    Identifier(String),
    Literal(String),
    FunctionSignature(FunctionSignature),
}

#[derive(Debug, PartialEq)]
pub struct Argument {
    name: String,
    typ: Type,
    default_value: Option<Value>,
}

#[derive(Debug, PartialEq)]
pub struct Getter {
    args: Vec<Argument>,
    name: Option<String>,
    typ: Type,
}

#[derive(Debug, PartialEq)]
pub struct Type {
    names: Vec<String>,
    optional: bool,
    is_rest: bool,
}

impl Type {
    pub fn from_str(s: &str) -> Self {
        Type {
            names: vec![s.to_string()],
            optional: false,
            is_rest: false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Deleter {
    args: Vec<Argument>,
    typ: Type,
}

#[derive(Debug, PartialEq)]
pub struct Setter {
    args: Vec<Argument>,
    name: String,
    typ: Type,
}

#[derive(Debug, PartialEq)]
pub struct ImplementsStmt {
    typ: Type,
    implements: Type,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    args: Vec<Argument>,
    name: String,
    typ: Type,
}

#[derive(Debug, PartialEq)]
pub struct Indexer {
    args: Vec<Argument>,
    typ: Type,
}

#[derive(Debug, PartialEq)]
pub struct Enum {
    options: Vec<String>,
    name: String,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Interface(Interface),
    ImplementsStmt(ImplementsStmt),
    DictionaryStmt(Dictionary),
    CallbackStmt(Callback),
    EnumStmt(Enum),
    TypeDefStmt(TypeDef),
}

#[derive(Debug, PartialEq)]
pub enum Member {
    Attribute(Attribute),
    Getter(Getter),
    Deleter(Deleter),
    Indexer(Indexer),
    Setter(Setter),
    Function(Function),
    Constant(Constant),
    Field(Field),
}

#[derive(Debug, PartialEq)]
enum Value {
    String(String),
    Identifier(String),
    Number(f64),
}

fn fn_signature<'a>() -> Parser<'a, u8, FunctionSignature> {
    (typ() - op() + argument_list() - cl())
        .map(|(return_type, args)| FunctionSignature { return_type, args })
}

fn callback<'a>() -> Parser<'a, u8, Callback> {
    // callback FileCallback = void (File file);
    (attrib_list() - keyword(b"callback") + name() - keyword(b"=") + fn_signature() - semi()).map(
        |((attribs, name), signature)| Callback {
            attribs,
            name,
            signature,
        },
    )
}

fn const_<'a>() -> Parser<'a, u8, Constant> {
    // const unsigned short NONE = 0;
    (keyword(b"const") * typ() + name() - keyword(b"=") + value() - semi())
        .map(|((typ, name), value)| Constant { name, typ, value })
}

#[derive(Debug, PartialEq)]
pub struct Callback {
    attribs: Vec<Attrib>,
    name: String,
    signature: FunctionSignature,
}

#[derive(Debug, PartialEq)]
pub struct Constant {
    name: String,
    typ: Type,
    value: Value,
}

#[derive(Debug, PartialEq)]
pub struct TypeDef {
    name: String,
    typ: Type,
}

#[derive(Debug, PartialEq)]
pub struct FunctionSignature {
    return_type: Type,
    args: Vec<Argument>,
}

/// space, tab, etc
fn ws<'a>() -> Parser<'a, u8, ()> {
    is_a(multispace).discard()
}

/// whitespace and comments
fn space<'a>() -> Parser<'a, u8, ()> {
    (ws() | comment()).repeat(0..).discard()
}

/// a parser wrapped in whitespace
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

fn is_underscore(term: u8) -> bool {
    term == b'_'
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
    keyword(b"(").name("open paren")
}

fn cl<'a>() -> Parser<'a, u8, ()> {
    keyword(b")").name("close paren")
}

fn opc<'a>() -> Parser<'a, u8, ()> {
    keyword(b"{").name("open curly")
}

fn clc<'a>() -> Parser<'a, u8, ()> {
    keyword(b"}").name("close curly")
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
    (seq(b"//") * to_eol() - eol())
        .discard()
        .name("line comment")
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
    let it = ((is_a(alpha) | is_a(is_underscore))
        + (is_a(alphanum) | is_a(is_underscore)).repeat(0..))
    .map(|(first, rest)| format!("{}{}", first as char, String::from_utf8(rest).unwrap()));

    spaced(it).name("name")
}

fn type_name<'a>() -> Parser<'a, u8, String> {
    ((literal(b"unsigned") | literal(b"unsigned") | literal(b"unrestricted")).opt() + name()).map(
        |(prefix, name)| match prefix {
            Some(prefix) => format!("{} {}", prefix, name),
            None => name,
        },
    )
}

fn typ<'a>() -> Parser<'a, u8, Type> {
    //(VideoTrack or AudioTrack or TextTrack)
    let compound_type = (op() * list(type_name(), keyword(b"or")) - cl()).map(|names| names);

    //AudioTrack
    let simple_type = type_name().map(|n| vec![n]);

    // TODO optional - eg (A or B)?
    let is_optional = keyword(b"?").opt().map(|x| x.is_some());
    let is_rest = keyword(b"...").opt().map(|x| x.is_some());

    ((compound_type | simple_type) + is_optional + is_rest).map(|((names, optional), is_rest)| {
        Type {
            names,
            optional,
            is_rest,
        }
    })
}

fn attribute<'a>() -> Parser<'a, u8, Attribute> {
    // [PutForwards=href, Unforgeable] attribute DOMString value;
    let attribs = attrib_list();
    let readonly = keyword(b"readonly").opt().map(|ro| ro.is_some());
    let attribute = keyword(b"attribute");
    let raw = attribs + readonly - attribute + typ() + name() - semi();

    raw.map(move |(((attribs, readonly), typ), name)| Attribute {
        readonly,
        name,
        typ,
        attribs,
    })
}

fn field<'a>() -> Parser<'a, u8, Field> {
    // [PutForwards=href, Unforgeable] attribute DOMString value;
    let attribs = attrib_list();
    let raw = attribs + typ() + name() - semi();

    raw.map(move |((attribs, typ), name)| Field { name, typ, attribs })
}

fn number<'a>() -> Parser<'a, u8, f64> {
    let integer = one_of(b"123456789") - one_of(b"0123456789").repeat(0..) | sym(b'0');
    let frac = sym(b'.') + one_of(b"0123456789").repeat(1..);
    let exp = one_of(b"eE") + one_of(b"+-").opt() + one_of(b"0123456789").repeat(1..);
    let number = sym(b'-').opt() + integer + frac.opt() + exp.opt();
    number
        .collect()
        .convert(std::str::from_utf8)
        .convert(f64::from_str)
}

fn value<'a>() -> Parser<'a, u8, Value> {
    number().map(|f| Value::Number(f))
        | name().map(|s| Value::Identifier(s))
        | string().map(|s| Value::String(s))
}

fn enum_<'a>() -> Parser<'a, u8, Enum> {
    // enum DocumentReadyState { "loading", "interactive", "complete" };
    (keyword(b"enum") * name() - opc() + list(string(), keyword(b",")) - clc() - semi())
        .map(|(name, options)| Enum { name, options })
}

fn implements_stmt<'a>() -> Parser<'a, u8, ImplementsStmt> {
    // Foo implements Bar;
    let raw = typ() - keyword(b"implements") + typ() - semi();
    raw.map(|(typ, implements)| ImplementsStmt { typ, implements })
}

fn argument<'a>() -> Parser<'a, u8, Argument> {
    // (HTMLOptionElement or HTMLOptGroupElement) element, optional (HTMLElement or long)? before = null
    let parser_raw = keyword(b"optional").opt() * typ() + name() + (keyword(b"=") * value()).opt();
    parser_raw.map(move |((typ, name), default_value)| Argument {
        name,
        typ,
        default_value,
    })
}
fn argument_list<'a>() -> Parser<'a, u8, Vec<Argument>> {
    spaced(list(argument(), sym(b',')))
}

fn attrib<'a>() -> Parser<'a, u8, Attrib> {
    let attrib_rhs = fn_signature().map(|f| AttribRHS::FunctionSignature(f))
        | name().map(|v| AttribRHS::Identifier(v))
        | string().map(|s| AttribRHS::Literal(s));

    (name() + (keyword(b"=") * attrib_rhs).opt())
        .map(|(name, value)| Attrib::Attrib(AttribDef { name, value }))
}

fn ctor<'a>() -> Parser<'a, u8, Attrib> {
    (keyword(b"Constructor") * op() + argument_list() - cl())
        .map(|(_, args)| Attrib::Constructor(Constructor { args }))
}

fn attrib_list<'a>() -> Parser<'a, u8, Vec<Attrib>> {
    let comma_sep = keyword(b"[") * list(ctor() | attrib(), keyword(b",")) - keyword(b"]");

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

fn getter<'a>() -> Parser<'a, u8, Getter> {
    // getter DOMString (DOMString name, DOMString value);
    let getter_raw = keyword(b"legacycaller").opt() * keyword(b"getter") * typ() + name().opt()
        - op()
        + argument_list()
        - cl()
        - semi();
    getter_raw.map(move |((typ, name), args)| Getter { args, name, typ })
}

fn indexer<'a>() -> Parser<'a, u8, Indexer> {
    // DOMString (DOMString name, DOMString value);
    let indexer_raw =
        keyword(b"legacycaller").opt() * typ() - op() + argument_list() - cl() - semi();
    indexer_raw.map(move |(typ, args)| Indexer { args, typ })
}

fn deleter<'a>() -> Parser<'a, u8, Deleter> {
    // deleter void (DOMString name);
    let deleter_raw = keyword(b"legacycaller").opt() * keyword(b"deleter") * typ() - op()
        + argument_list()
        - cl()
        - semi();
    deleter_raw.map(move |(typ, args)| Deleter { args, typ })
}

fn setter<'a>() -> Parser<'a, u8, Setter> {
    // setter creator void (DOMString name, DOMString value);
    let setter_raw = keyword(b"legacycaller").opt() * keyword(b"setter") * name() + typ() - op()
        + argument_list()
        - cl()
        - semi();
    setter_raw.map(move |((name, typ), args)| Setter { name, args, typ })
}

fn function<'a>() -> Parser<'a, u8, Function> {
    //HTMLAllCollection tags(DOMString tagName);
    let function_raw =
        keyword(b"legacycaller").opt() * typ() + name() - op() + argument_list() - cl() - semi();
    function_raw.map(|((typ, name), args)| Function { name, args, typ })
}

fn member<'a>() -> Parser<'a, u8, Member> {
    const_().map(|c| Member::Constant(c))
        | getter().map(|g| Member::Getter(g))
        | indexer().map(|i| Member::Indexer(i))
        | setter().map(|s| Member::Setter(s))
        | deleter().map(|d| Member::Deleter(d))
        | function().map(|f| Member::Function(f))
        | attribute().map(|a| Member::Attribute(a))
        | field().map(|a| Member::Field(a))

    // | skip_a_line()
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

fn typedef<'a>() -> Parser<'a, u8, TypeDef> {
    // typedef (CanvasRenderingContext2D or WebGLRenderingContext) RenderingContext;
    (keyword(b"typedef") * typ() + name() - semi()).map(|(typ, name)| TypeDef { typ, name })
}

fn dictionary<'a>() -> Parser<'a, u8, Dictionary> {
    let interface = attrib_list() - keyword(b"partial").opt().discard() * keyword(b"dictionary")
        + name()
        + (keyword(b":") * name()).opt()
        - keyword(b"{")
        + member_list()
        - keyword(b"}")
        - semi();
    interface.map(|(((attribs, name), supr), members)| Dictionary {
        attribs,
        name,
        supr,
        members,
    })
}

fn stmt<'a>() -> Parser<'a, u8, Statement> {
    interface().map(|i| Statement::Interface(i))
        | dictionary().map(|d| Statement::DictionaryStmt(d))
        | typedef().map(|d| Statement::TypeDefStmt(d))
        | implements_stmt().map(|i| Statement::ImplementsStmt(i))
        | callback().map(|c| Statement::CallbackStmt(c))
        | enum_().map(|e| Statement::EnumStmt(e))
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

pub fn idl<'a>() -> Parser<'a, u8, Vec<Statement>> {
    stmt().repeat(0..) - space() - end()
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

        assert_consumes_all!(type_name(), b"unsigned long");
        assert_consumes_all!(type_name(), b"unsigned short");
        assert_consumes_all!(type_name(), b"unrestricted double");
        assert_consumes_all!(type_name(), b"DomString");

        assert_consumes_all![typ(), b"TypeName"];
        assert_consumes_all![typ(), b"(TypeName1 or TypeName2)"];
        assert_consumes_all![typ(), b"unsigned short"];
        assert_consumes_all!(typ(), b"HTMLOptionElement?");

        assert_consumes_all![const_(), b"const unsigned short X = 2;"];
        assert_consumes_all![const_(), b"const unsigned short X = \"2\";"];
        assert_consumes_all![const_(), b"const Zorp XXX = \"x\";"];
        assert_consumes_all![const_(), b"const unsigned short X = \"x\";"];
        assert_consumes_all![const_(), b"const unsigned short NETWORK_EMPTY = 0;"];
        assert_consumes_all![member(), b"const unsigned short X = \"x\";"];

        assert_consumes_all![line_comment(), b"//\r"];
        assert_consumes_all![line_comment(), b"//\n"];
        assert_consumes_all![line_comment(), b"//\r\n"];
        assert_consumes_all![line_comment(), b"// xyz \r\n"];

        assert_consumes_all![star_comment(), b"/*  thing */"];
        assert_consumes_all![star_comment(), b"/*  thing \r\n thing */"];

        assert_consumes_all!(
            typedef(),
            b"typedef (CanvasRenderingContext2D or WebGLRenderingContext) RenderingContext;"
        );

        assert_consumes_all!(
            enum_(),
            b"enum DocumentReadyState { \"loading\", \"interactive\", \"complete\" };"
        );

        assert_consumes_all!(argument(), b"ArgType name");
        assert_consumes_all!(argument(), b"optional ArgType name");
        assert_consumes_all!(argument(), b"ArgType name = null");
        assert_consumes_all!(
            argument(),
            b"ArgType name",
            Argument {
                typ: Type::from_str("ArgType"),
                name: "name".into(),
                default_value: None
            }
        );

        assert_consumes_all!(
            implements_stmt(),
            b"ArrayBuffer implements Transferable;",
            ImplementsStmt {
                typ: Type::from_str("ArrayBuffer"),
                implements: Type::from_str("Transferable")
            }
        );

        assert_consumes_all!(attrib_list(), b"[OverrideBuiltins]");
        assert_consumes_all!(attrib_list(), b"[A][B]");
        assert_consumes_all!(attrib_list(), b"[A,B]");
        assert_consumes_all!(attrib_list(), b"[A,B][C]");
        assert_consumes_all!(attrib_list(), b"[PutForwards=value]");
        assert_consumes_all!(
            ctor(),
            b"Constructor(DOMString type, optional TrackEventInit eventInitDict)"
        );
        assert_consumes_all!(attrib_list(), b"[Constructor()]");

        assert_consumes_all!(
            attrib_list(),
            b"[Constructor(DOMString type, optional TrackEventInit eventInitDict)]"
        );

        assert_consumes_all!(fn_signature(), b"Image(Foo bar)");
        assert_consumes_all!(attrib_list(), b"[NamedConstructor=Image(Foo bar)]");
        assert_consumes_all!(
            attrib_list(),
            b"[NamedConstructor=Image(optional unsigned long width, optional unsigned long height)]"
        );

        assert_consumes_all!(
            argument_list(),
            b"ArgType1 name1, ArgType2 name2",
            vec![
                Argument {
                    typ: Type::from_str("ArgType1"),
                    name: "name1".into(),
                    default_value: None
                },
                Argument {
                    typ: Type::from_str("ArgType2"),
                    name: "name2".into(),
                    default_value: None
                }
            ]
        );

        assert_consumes_all!(callback(), b"callback FileCallback = void (File file);");
        assert_consumes_all!(string(), b"\"thing\"");

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
            Member::Attribute(Attribute {
                readonly: false,
                name: String::from("value"),
                typ: Type::from_str("DOMString"),
                attribs: vec![]
            })
        );

        assert_consumes_all!(
            member(),
            b"readonly attribute DOMString value;",
            Member::Attribute(Attribute {
                readonly: true,
                name: String::from("value"),
                typ: Type::from_str("DOMString"),
                attribs: vec![]
            })
        );

        assert_consumes_all!(
            member(),
            b"getter ReturnType ();",
            Member::Getter(Getter {
                args: vec![],
                name: None,
                typ: Type::from_str("ReturnType"),
            })
        );

        assert_consumes_all!(
            member(),
            b"legacycaller getter ReturnType namedItem();",
            Member::Getter(Getter {
                args: vec![],
                name: Some("namedItem".into()),
                typ: Type::from_str("ReturnType"),
            })
        );

        assert_consumes_all!(
            member(),
            b"getter ReturnType (ArgType name);",
            Member::Getter(Getter {
                args: vec![Argument {
                    name: String::from("name"),
                    typ: Type::from_str("ArgType"),
                    default_value: None
                }],
                name: None,
                typ: Type::from_str("ReturnType"),
            })
        );

        assert_consumes_all!(
            member(),
            b"setter creator void (ArgType name);",
            Member::Setter(Setter {
                args: vec![Argument {
                    name: "name".into(),
                    typ: Type::from_str("ArgType"),
                    default_value: None
                }],
                name: "creator".into(),
                typ: Type::from_str("void"),
            })
        );

        assert_consumes_all!(
            member(),
            b"HTMLAttribute read (ArgType name);",
            Member::Function(Function {
                args: vec![Argument {
                    name: "name".into(),
                    typ: Type::from_str("ArgType"),
                    default_value: None
                }],
                name: "read".into(),
                typ: Type::from_str("HTMLAttribute"),
            })
        );

        assert_consumes_all!(
            member(),
            b"deleter void (ArgType name);",
            Member::Deleter(Deleter {
                args: vec![Argument {
                    name: "name".into(),
                    typ: Type::from_str("ArgType"),
                    default_value: None
                }],
                typ: Type::from_str("void"),
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
                Member::Attribute(Attribute {
                    readonly: true,
                    name: String::from("value"),
                    typ: Type::from_str("DOMString"),
                    attribs: vec![]
                }),
                Member::Attribute(Attribute {
                    readonly: false,
                    name: String::from("body"),
                    typ: Type::from_str("HTMLElement"),
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
                    Member::Attribute(Attribute {
                        readonly: true,
                        name: String::from("label"),
                        typ: Type::from_str("DOMString"),
                        attribs: vec![]
                    }),
                    Member::Attribute(Attribute {
                        readonly: false,
                        name: String::from("defaultSelected"),
                        typ: Type::from_str("boolean"),
                        attribs: vec![]
                    }),
                ],
                ..Default::default()
            }
        );

        Ok(())
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
        let file_path_str = "assets/html5.idl";
        assert_parse_file(file_path_str);
    }

    fn assert_parse_file(file_path_str: &str) {
        let byte_vec: Vec<u8> = std::fs::read(file_path_str).unwrap();
        let file_content =
            String::from_utf8(byte_vec.clone()).expect("should be able to read the file");
        let byte_slice: &[u8] = &byte_vec;
        let idl_parser = idl();
        let parse_result = match idl_parser.parse(byte_slice) {
            Ok(parse_result) => parse_result,
            Err(pom::Error::Mismatch { message, position }) => {
                let start_str = &byte_vec[0..position];
                let line = count_lines(start_str) + 1;
                let end = min(position + 50, file_content.len() - 1);
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
