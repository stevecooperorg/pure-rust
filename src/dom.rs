//' contains a domain model for HTML, which we'll use to generate HTML

use crate::idl_parser::Attribute;
use failure::_core::fmt::Formatter;

#[derive(Clone, Debug)]
pub struct Tag {
    pub name: String,
    pub attributes: Vec<Attribute>,
}

impl Tag {
    pub fn new(name: &str) -> Self {
        Tag {
            name: name.to_string(),
            attributes: vec![],
        }
    }
}

impl std::fmt::Display for Tag {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "<{} ", self.name)?;
        for attr in self.attributes.iter() {
            let type_name = attr.typ.names.join(", ");
            writeln!(f, "  {}=\"{}\" ", attr.name, type_name)?;
        }
        Ok(writeln!(f, "  />")?)
    }
}
