use crate::dom::*;
use crate::idl_parser::*;
use std::collections::HashMap;

fn idl_to_dom(idl: &Vec<Statement>) -> Vec<Tag> {
    //  throw away everything that's not an interface;
    let interfaces: Vec<&Interface> = idl
        .iter()
        .filter_map(|stmt| match stmt {
            Statement::Interface(interface) => Some(interface),
            _ => None,
        })
        .collect();

    // we can have interfaces and partial interfaces, so we group them by name
    let mut interface_hash: HashMap<String, Vec<&Interface>> = HashMap::new();
    for interface in interfaces {
        interface_hash
            .entry(interface.name.to_string())
            .and_modify(|x| x.push(interface))
            .or_insert(vec![interface]);
    }

    let mut tags = HashMap::new();

    for (name, interfaces) in interface_hash.iter() {
        // we can now mash together all the definitions into one;
        let mut tag = Tag::new(name);

        let mut remaining = interfaces.clone();

        while let Some(interface) = remaining.pop() {
            add_attributes(&mut tag, interface);

            // make sure we process the super class, if it exists
            if let Some(supr_) = &interface.supr {
                let mut supr_interface: &Vec<&Interface> = interface_hash
                    .get(supr_)
                    .expect(&format!("cannot find super-type '{}'", supr_));

                for x in supr_interface.iter() {
                    remaining.push(x);
                }
            }
        }

        tags.insert(name, tag);
    }

    tags.values().cloned().collect()
}

fn add_attributes(tag: &mut Tag, interface: &Interface) {
    for member in interface.members.iter() {
        let attr = match member {
            Member::Attribute(attribute) => attribute,
            _ => continue,
        };

        tag.attributes.push(attr.clone());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn can_convert_all() {
        let byte_vec: Vec<u8> = std::fs::read("assets/html5.idl").unwrap();
        let idl_parser = idl();
        let parse_result = idl_parser.parse(&byte_vec).expect("cannot parse file");

        let dom = idl_to_dom(&parse_result);
        for tag in dom.iter() {
            println!("{}", tag);
        }
    }
}
