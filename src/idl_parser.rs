extern crate pom;
use pom::parser::*;
use pom::Parser;
use self::pom::char_class::{alpha, alphanum, multispace};
use std::fs::read;

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

    let member_inner = readonly - attribute + typ + nam;


    member_inner.map(move |((readonly, typ), name)| Member::Attribute(AttributeDef {
        readonly,
        name,
        typ
    }))
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_keywords() {
        assert_eq!(Ok(()), space().parse(b"x"));
        assert_eq!(Ok(()), space().parse(b"  x"));

        assert_eq!(Ok(Interface { name: String::from("foo"), members:vec![]}), interface().parse(b"interface foo { }"));

        assert_eq!(Ok(Member::Attribute(AttributeDef {
            readonly: true,
            name: String::from("value"),
            typ: String::from("DOMString")
        })), member().parse(b"readonly attribute DOMString value;"));


    }
}