use ::dbc;
use std::collections::HashMap;

/// Trait for converting `Entry` values into a library's own entries.
pub trait FromDbc {
    type Err;

    /// Converts an `Entity` value from scratch.
    fn from_entry(entry: dbc::Entry) -> Result<Self, Self::Err>
        where
            Self: Sized;

    /// Merges the given `Entity` with a `mut` version of the library's entity.  Useful for when
    /// multiple `Entry` types contribute to various attributes within the same destination.
    fn merge_entry(&mut self, entry: dbc::Entry) -> Result<(), Self::Err>;
}


type SignalAttribute = String;
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Signal {
    /// e.g., {"SPN", "190"}
    /// BA_ "SPN" SG_ 2364540158 EngSpeed 190;
    /// BA_ "SigType" SG_ 2364540158 EngSpeed 1;
    attributes: HashMap<String, SignalAttribute>,
    /// e.g., "Actual engine speed which is calculated over a minimum
    /// crankshaft angle of 720 degrees divided by the number of cylinders."
    description: Option<String>,
    /// e.g, SG_ EngSpeed : 24|16@1+ (0.125,0) [0|8031.875] "rpm" Vector__XXX
    definition: Option<dbc::SignalDefinition>, // FIXME: hate that this has to be Option

    /// Only applicable for enum types
    /// e.g., VAL_ 2364540158 ActlEngPrcntTrqueHighResolution 8 "1111NotAvailable" 7 "0875" 1 "0125" 0 "0000" ;
    value_definition: Option<dbc::ValueDefinition>,
}

type MessageAttribute = String;

#[derive(Clone, Debug, Default)]
pub struct Message {
    name: String,
    message_len: u32,
    sending_node: String,

    /// e.g., BA_ "VFrameFormat" BO_ 2364540158 3;
    attributes: HashMap<String, MessageAttribute>,
    /// e.g., CM_ BO_ 2364540158 "Electronic Engine Controller 1";
    description: Option<String>,
    signals: HashMap<String, Signal>,
}

impl FromDbc for Message {
    type Err = ();

    fn from_entry(entry: dbc::Entry) -> Result<Self, Self::Err> where
        Self: Sized {

        match entry {
            Entry::MessageDefinition(dbc::MessageDefinition {
                                         id: _id,
                                         name,
                                         message_len,
                                         sending_node,
                                     }) => {
                Ok(Message {
                    name: name,
                    message_len: message_len,
                    sending_node: sending_node,
                    .. Default::default()
                })
            },
            Entry::MessageDescription(dbc::MessageDescription {
                                          id: _id,
                                          signal_name: _signal_name,
                                          description,
                                      }) => {
                Ok(Message {
                    description: Some(description),
                    .. Default::default()
                })
            },
            Entry::MessageAttribute(dbc::MessageAttribute {
                                        name,
                                        signal_name: _signal_name,
                                        id: _id,
                                        value,
                                    }) => {
                let mut attributes = HashMap::new();
                attributes.insert(name, value);

                Ok(Message {
                    attributes: attributes,
                    .. Default::default()
                })
            },
            // TODO: Need to propogate Signal FromDbc in here..maybe, or just search in DbcLibrary
            _ => Err(())
        }
    }

    fn merge_entry(&mut self, entry: dbc::Entry) -> Result<(), Self::Err> {

        match entry {
            Entry::MessageDefinition(
                dbc::MessageDefinition {
                    id: _id,
                    name,
                    message_len,
                    sending_node,
                }) => {
                self.name = name;
                self.message_len = message_len;
                self.sending_node = sending_node;
                Ok(())
            },
            Entry::MessageDescription(
                dbc::MessageDescription {
                    id: _id,
                    signal_name: _signal_name,
                    description,
                }) => {
                self.description = Some(description);
                Ok(())
            },
            Entry::MessageAttribute(
                dbc::MessageAttribute {
                    name,
                    signal_name: _signal_name,
                    id: _id,
                    value,
                }) => {
                if let Some(_previous_value) = self.attributes.insert(name, value) {
                    // TODO: Warn that we somehow already had an existing entry
                }
                Ok(())
            },
            Entry::SignalDefinition(inner) => {
                if self.signals.contains_key(&inner.name) {
                    (*self.signals.get_mut(&inner.name).expect("Already checked for Signal key"))
                        .merge_entry(Entry::SignalDefinition(inner))
                } else {
                    let name = inner.name.clone();
                    let signal = Signal::from_entry(Entry::SignalDefinition(inner))?;
                    self.signals.insert(name, signal);
                    Ok(())
                }
            },
            Entry::SignalDescription(inner) => {
                if self.signals.contains_key(&inner.signal_name) {
                    (*self.signals.get_mut(&inner.signal_name).expect("Already checked for Signal key"))
                        .merge_entry(Entry::SignalDescription(inner))
                } else {
                    let name = inner.signal_name.clone();
                    let signal = Signal::from_entry(Entry::SignalDescription(inner))?;
                    self.signals.insert(name, signal);
                    Ok(())
                }
            },
            Entry::SignalAttribute(inner) => {
                if self.signals.contains_key(&inner.signal_name) {
                    (*self.signals.get_mut(&inner.signal_name).expect("Already checked for Signal key"))
                        .merge_entry(Entry::SignalAttribute(inner))
                } else {
                    let name = inner.signal_name.clone();
                    let signal = Signal::from_entry(Entry::SignalAttribute(inner))?;
                    self.signals.insert(name, signal);
                    Ok(())
                }
            },
            _ => Err(())
        }
    }
}

impl FromDbc for Signal {
    type Err = ();

    fn from_entry(entry: dbc::Entry) -> Result<Self, Self::Err> where
        Self: Sized {
        match entry {
            Entry::SignalDefinition(definition) => {
                Ok(Signal {
                    attributes: HashMap::new(),
                    description: None,
                    definition: Some(definition),
                    value_definition: None,
                })
            },
            Entry::SignalDescription(
                dbc::SignalDescription {
                    id: _id,
                    signal_name: _signal_name,
                    description,
                }) => {
                Ok(Signal {
                    attributes: HashMap::new(),
                    description: Some(description),
                    definition: None,
                    value_definition: None,
                })
            },
            Entry::SignalAttribute(dbc::SignalAttribute {
                                       name,
                                       id: _id,
                                       signal_name: _signal_name,
                                       value,
                                   }) => {
                let mut attributes = HashMap::new();
                attributes.insert(name, value);
                Ok(Signal {
                    attributes,
                    description: None,
                    definition: None,
                    value_definition: None
                })
            },
            _ => Err(()),
        }
    }

    fn merge_entry(&mut self, entry: dbc::Entry) -> Result<(), Self::Err> {
        match entry {
            Entry::SignalDefinition(definition) => {
                self.definition = Some(definition);
                Ok(())
            },
            Entry::SignalDescription(
                dbc::SignalDescription {
                    id: _id,
                    signal_name: _signal_name,
                    description,
                }) => {
                self.description = Some(description);
                Ok(())
            },
            Entry::SignalAttribute(dbc::SignalAttribute {
                                       name,
                                       id: _id,
                                       signal_name: _signal_name,
                                       value,
                                   }) => {
                if let Some(_previous_value) = self.attributes.insert(name, value) {
                    // TODO: Warn that we somehow already had an existing entry
                }
                Ok(())
            },
            _ => Err(()),
        }
    }
}

/// A struct that represents a CANdb file, and provides APIs for interacting
/// with CAN messages and signals.
#[derive(Clone, Debug, Default)]
pub struct DbcLibrary {
    last_id: Option<u32>,
    messages: HashMap<u32, Message>,
}

use std::fs::File;
use std::io;
use std::io::prelude::*;
use encoding::{DecoderTrap, Encoding};
use encoding::all::ISO_8859_1;
use std::path::Path;
use super::{nom as nomparse, *};
use nom;

impl DbcLibrary {
    /// Creates a new `DbcLibrary` instance given an existing lookup table.
    pub fn new(messages: HashMap<u32, Message>) -> Self {
        DbcLibrary {
            last_id: None,
            messages,
        }
    }

    /// Convenience function for loading an entire DBC file into a returned `DbcLibrary`.  This
    /// function ignores unparseable lines as well as `Entry` variants which don't apply to
    /// `DbcLibrary` (such as `Entry::Version`).  Fails on `io::Error`.
    ///
    /// # Example
    ///
    /// ```rust
    /// use canparse::dbc::DbcLibrary;
    ///
    /// let lib: DbcLibrary = DbcLibrary::from_dbc_file("./tests/data/sample.dbc").unwrap();
    ///
    /// ```
    pub fn from_dbc_file<P>(path: P) -> io::Result<Self>
        where
            P: AsRef<Path>,
    {
        Self::from_encoded_dbc_file(path, ISO_8859_1)
    }

    #[doc(hidden)]
    pub fn from_encoded_dbc_file<P, E>(path: P, encoding: &E) -> io::Result<Self>
        where
            P: AsRef<Path>,
            E: Encoding,
    {
        let mut lib = DbcLibrary::default();

        let data = File::open(path)
            .and_then(|mut f| {
                let mut contents: Vec<u8> = Vec::new();
                f.read_to_end(&mut contents).map(|_bytes_read| contents)
            })
            .and_then(|contents| {
                encoding
                    .decode(contents.as_slice(), DecoderTrap::Replace)
                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
            })?;

        let mut i = data.as_str();
        while !i.is_empty() {
            match nomparse::entry(i) {
                Ok((new_i, entry)) => {
                    if let Err(_e) = lib.add_entry(entry) {
                        // TODO: Handle add_entry error
                    }
                    i = new_i;
                }
                // FIXME: handling `IResult::Err`s could be better
                Err(nom::Err::Incomplete(_)) => {
                    break;
                }
                Err(_) => {
                    i = &i[1..];
                }
            }
        }

        Ok(lib)
    }
}

impl DbcLibrary {
    pub fn add_entry(&mut self, entry: Entry) -> Result<(), String> {
        let _id: u32 = *match entry {
            Entry::MessageDefinition(dbc::MessageDefinition { ref id, .. }) => id,
            Entry::MessageDescription(dbc::MessageDescription { ref id, .. }) => id,
            Entry::MessageAttribute(dbc::MessageAttribute { ref id, .. }) => id,
            Entry::SignalDefinition(..) => {
                // no id, and by definition must follow MessageDefinition
                if let Some(last_id) = self.last_id.as_ref() {
                    last_id
                } else {
                    return Err("Tried to add SignalDefinition without last ID.".to_string());
                }
            }
            Entry::SignalDescription(dbc::SignalDescription { ref id, .. }) => id,
            Entry::SignalAttribute(dbc::SignalAttribute { ref id, .. }) => id,
            _ => {
                return Err(format!("Unsupported entry: {}.", entry).to_string());
            }
        };

        self.messages.entry(_id)
            .and_modify(|cur_entry| cur_entry.merge_entry(entry.clone())
                .unwrap_or_else(|_| panic!("Already checked for Signal key: {:?}", entry))
            ).or_insert_with(|| Message::from_entry(entry.clone())
                .unwrap_or_else(|_| panic!("Some inserted a Signal for empty key: {:?}", _id)));

        self.last_id = Some(_id);
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use dbc::{Entry, SignalDefinition, Version};
    use super::{DbcLibrary};

    lazy_static! {
        static ref DBCLIB_EMPTY: DbcLibrary = DbcLibrary::default();
        static ref DBCLIB_ONE: DbcLibrary = DbcLibrary::from_dbc_file("./tests/data/sample.dbc")
            .expect("Failed to create DbcLibrary from file");
        static ref SIGNALDEF: SignalDefinition = SignalDefinition {
            name: "Engine_Speed".to_string(),
            start_bit: 24,
            bit_len: 16,
            little_endian: true,
            signed: false,
            scale: 0.125,
            offset: 0.0,
            min_value: 0.0,
            max_value: 8031.88,
            units: "rpm".to_string(),
            receiving_node: "Vector__XXX".to_string()
        };
        static ref SIGNALDEF_BE: SignalDefinition = {
            let mut _spndef = SIGNALDEF.clone();
            _spndef.little_endian = false;
            _spndef
        };
        static ref MSG: [u8; 8] = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
        static ref MSG_BE: [u8; 8] = [0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11];
    }

    #[test]
    fn default_pgnlibrary() {
        assert_eq!(DBCLIB_EMPTY.messages.len(), 0);
    }

    #[test]
    fn get_spndefinition() {
        assert_eq!(
            *DBCLIB_ONE
                .messages
                .get(&2364539904)
                .expect("failed to get DbcDefinition from DbcLibrary")
                .signals
                .get("Engine_Speed")
                .expect("failed to get Signal from DbcDefinition")
                .definition.as_ref()
                .expect("failed to get SignalDefinition from DbcDefinition")
            ,*SIGNALDEF
        );
    }

    #[test]
    fn unsupported_entry() {
        let mut dbclib: DbcLibrary = DbcLibrary::default();
        let unsupported = Entry::Version(Version("Don't care about version entry".to_string()));
        let res = dbclib.add_entry(unsupported);

        assert!(res.is_err(), "Unsupported entry: Version".to_string());
    }
}
