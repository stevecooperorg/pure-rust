extern crate pom;
use pom::parser::*;
use pom::Parser;
use self::pom::char_class::{alpha, alphanum, multispace};

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
    Attribute(AttributeDef)
}

fn space() -> Parser<u8, ()> {
    is_a(multispace).repeat(0..).discard()
}

fn name() -> Parser<u8, String> {
    (is_a(alpha) + is_a(alphanum).repeat(0..))
        .map(|(first, rest)| format!("{}{}", first as char, String::from_utf8(rest).unwrap()))
}

fn member() -> Parser<u8, Member> {
    // attribute DOMString value;
    let readonly = space() * seq(b"readonly").discard().opt().map(|ro| ro.is_some());
    let attribute = space() * seq(b"attribute").discard() * space();
    let typ = space() * name();
    let nam = space() * name();

    let member_raw = readonly - attribute + typ + nam  - space() - sym(b';');

    member_raw.map(move |((readonly, typ), name)| Member::Attribute(AttributeDef {
        readonly,
        name,
        typ
    }))
}

fn member_list() -> Parser<u8, Vec<Member>> {
    member().repeat(0..)
}

fn interface() -> Parser<u8, Interface> {
    let interface_start = seq(b"interface") * space() * name() - space() - sym(b'{') ;

    let interface_close = space() - sym(b'}');
    let interface = interface_start - interface_close;

    interface.map(|name| Interface {
        name,
        members: vec![],
    })
}

fn idl() -> Parser<u8, Vec<Interface>> {
    interface().repeat(0..)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::error::Error;

    #[test]
    fn parse_keywords() -> Result<(), Box<dyn Error>> {
        assert_eq!(Ok(()), (sym(b'>').discard() * space() * sym(b'<').discard()).parse(b"> \n \r \t <"));

        assert_eq!(Ok(Interface { name: String::from("foo"), members:vec![]}), interface().parse(b"interface foo { }"));

        assert_eq!(Ok(Member::Attribute(AttributeDef {
            readonly: true,
            name: String::from("value"),
            typ: String::from("DOMString")
        })), member().parse(b"readonly attribute DOMString value;"));

        let expected = vec![
            Member::Attribute(AttributeDef {
                readonly: true,
                name: String::from("value"),
                typ: String::from("DOMString")
            }),
            Member::Attribute(AttributeDef {
                readonly: false,
                name: String::from("body"),
                typ: String::from("HTMLElement")
            }),
        ];

        assert_eq!(member_list().parse(r"
readonly attribute DOMString value;
attribute HTMLElement body;".as_bytes()), Ok(expected));

        let valid_so_far: Vec<u8> = std::fs::read("assets/valid_so_far.idl")?;

        let valid_slice: &[u8] = &valid_so_far[0..];

        //let parse_res = idl().parse(valid_slice);

        println![ "file was {} characters long", valid_so_far.len()];
//        match idl().parse(&valid_slice) {
//            Ok(_) => Ok(()),
//            Err(e) => panic!(format!("error! {:?}", e))
//        }

        Ok(())

    }
}