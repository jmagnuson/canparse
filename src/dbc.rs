//! CANdb definition parsing

#![allow(non_upper_case_globals)]

use enum_primitive::FromPrimitive;
use std::str::FromStr;
use rustc_serialize::{Decodable, Decoder};
use regex::{Regex, RegexSet};
use std::fmt::{Display, Formatter};
use std::fmt;
use std::error::Error;
use std;

#[derive(Debug, PartialEq, Clone)]
pub struct Version(pub String);

#[derive(Debug, PartialEq, Clone)]
pub struct BusConfiguration(pub f32);

#[derive(Debug, PartialEq, Clone)]
pub struct MessageDefinition {
    pub id: String,
    pub name: String,
    pub message_len: u32,
    pub sending_node: String
}

#[derive(Debug, PartialEq, Clone)]
pub struct MessageDescription {
    pub id: String,
    pub signal_name: String,
    pub description: String
}

#[derive(Debug, PartialEq, Clone)]
pub struct MessageAttribute {
    pub name: String,
    pub id: String,
    pub signal_name: String,
    pub value: String
}

#[derive(Debug, PartialEq, Clone)]
pub struct SignalDefinition {
    pub name: String,
    pub start_bit: usize,
    pub bit_len: usize,
    pub little_endian: bool,
    pub signed: bool,
    pub scale: f32,
    pub offset: f32,
    pub min_value: f32,
    pub max_value: f32,
    pub units: String,
    pub receiving_node: String
}

#[derive(Debug, PartialEq, Clone)]
pub struct SignalDescription {
    pub id: String,
    pub signal_name: String,
    pub description: String
}

#[derive(Debug, PartialEq, Clone)]
pub struct SignalAttribute {
    pub name: String,
    pub id: String,
    pub signal_name: String,
    pub value: String
}

/// Composed DBC entry.
#[derive(Debug, PartialEq, Clone)]
pub enum Entry {

    /// `VERSION`
    Version(Version),

    /// BS_: <Speed>
    BusConfiguration(BusConfiguration),

    // TODO: ??
    // CanNodes,
    // `CM_ BU_ [can id] [signal name] "[description]"`
    // CanNodesDescription,
    // CanNodesAttribute,

    /// `BO_ [can id] [message name]: [message length] [sending node]`
    MessageDefinition(MessageDefinition),
    /// `CM_ BO_ [can id] [signal name] "[description]"`
    MessageDescription(MessageDescription),
    /// `BA_ "[attribute name]" BO_ [node|can id] [signal name] [attribute value];`
    MessageAttribute(MessageAttribute),

    /// `SG_ [signal name] [...] : [start bit]|[length]@[endian][sign] [[min]|[max]] "[unit]" [receiving nodes]`
    SignalDefinition(SignalDefinition),
    /// `CM_ SG_ [can id] [signal name] "[description]"`
    SignalDescription(SignalDescription),
    /// `BA_ "[attribute name]" SG_ [node|can id] [signal name] [attribute value];`
    SignalAttribute(SignalAttribute),

    // `CM_ [BU_|BO_|SG_] [can id] [signal name] "[description]"`
    // Description, -- flatten subtypes instead

    // `BA_DEF_ ...`
    // AttributeDefinition,

    // `BA_DEF_DEF_ ...`
    // AttributeDefault,

    // `BA_ "[attribute name]" [BU_|BO_|SG_] [node|can id] [signal name] [attribute value];`
    // Attribute

    Unknown(String),
}

impl Entry {
    /// Returns an opaque `EntryType` for an `Entry` structure variant.
    // TODO: Finalize naming convention and expose
    pub(super) fn get_type(&self) -> EntryType {
        match self {
            Entry::Version(_) => EntryType::Version,
            Entry::BusConfiguration(_) => EntryType::BusConfiguration,
            Entry::MessageDefinition(_) => EntryType::MessageDefinition,
            Entry::MessageDescription(_) => EntryType::MessageDescription,
            Entry::MessageAttribute(_) => EntryType::MessageAttribute,
            Entry::SignalDefinition(_) => EntryType::SignalDefinition,
            Entry::SignalDescription(_) => EntryType::SignalDescription,
            Entry::SignalAttribute(_) => EntryType::SignalAttribute,
            Entry::Unknown(_) => EntryType::Unknown,
        }
    }
}

impl Display for Entry {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.get_type().fmt(f)
    }
}

/// Internal type for DBC `Entry` line.
enum_from_primitive! {
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntryType {
    Version = 0,

    BusConfiguration,

    // CanNodes,
    // CanNodesDescription,
    // CanNodesAttribute

    MessageDefinition,
    MessageDescription,
    MessageAttribute,

    SignalDefinition,
    SignalDescription,
    SignalAttribute,

    // AttributeDefinition,
    // AttributeDefault,
    // Attribute

    Unknown,
}
}

impl Display for EntryType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let entry_str = match *self {
            EntryType::Version => "Version",
            EntryType::BusConfiguration => "BusConfiguration",
            EntryType::MessageDefinition => "MessageDefinition",
            EntryType::MessageDescription => "MessageDescription",
            EntryType::MessageAttribute => "MessageAttribute",
            EntryType::SignalDefinition => "SignalDefinition",
            EntryType::SignalDescription => "SignalDescription",
            EntryType::SignalAttribute => "SignalAttribute",

            EntryType::Unknown => "Unknown",
        };
        write!(f, "{}", entry_str)
    }
}

pub mod nom {
    use super::*;
    use nom::{alphanumeric, anychar, digit, float, line_ending, space, space0};
    use std::str::FromStr;

    // TODO: convert `tag!(" ")` to `space`

    named! {
        quoted_str<&str, String>,
        map!(
            delimited!(
                tag!("\""),
                escaped_transform!(
                    none_of!("\\\""),
                    '\\',
                    alt!(
                        tag!("\\") => { |_| "\\" }
                      | tag!("\"") => { |_| "\"" }
                    )),
                tag!("\"")),
            |data| data)
    }

    named!(pub entry<&str, Entry>, alt!(
        version                => { |r| Entry::Version(r) } |
        bus_configuration      => { |r| Entry::BusConfiguration(r) } |
        message_definition     => { |r| Entry::MessageDefinition(r) } |
        message_description    => { |r| Entry::MessageDescription(r) } |
        message_attribute      => { |r| Entry::MessageAttribute(r) } |
        signal_definition      => { |r| Entry::SignalDefinition(r) } |
        signal_description     => { |r| Entry::SignalDescription(r) } |
        signal_attribute       => { |r| Entry::SignalAttribute(r) } |
        unknown                => { |r| Entry::Unknown(r) }
    ));

    named!(pub unknown<&str, String>,
        do_parse!(
            // FIXME: many0!(quoted_str) >> line_ending
            data: take_until_either!("\r\n") >>
            line_ending >>
            ( data.to_string() )
        )
    );

    named!(pub version<&str, Version>,
        do_parse!(
            tag!("VERSION")   >>
            tag!(" ")   >>
            data: quoted_str >>
            line_ending >>
            ( Version(data) )
        )
    );

    named!(pub bus_configuration<&str, BusConfiguration>,
        do_parse!(
            tag!("BS_:")   >>
            tag!(" ")   >>
            data: map_res!(
                take_until_either!("\r\n"),
                FromStr::from_str) >>
            line_ending >>
            ( BusConfiguration(data) )
        )
    );

    // FIXME: `space` isn't really correct since there should only be ONE (probably need alt)
    named!(pub message_definition<&str, MessageDefinition>,
        do_parse!(
            tag!("BO_")   >>
            space >>
            id: digit >>
            space >>
            name: alphanumeric >>
            space0 >>
            tag!(":")   >>
            space >>
            len: map_res!(
                digit,
                FromStr::from_str) >>
            space >>
            sending_node: take_until_either!(" \t\r\n") >>
            space0 >>
            line_ending >>
            ( MessageDefinition {
                id: id.to_string(),
                name: name.to_string(),
                message_len: len,
                sending_node: sending_node.to_string(),
            } )
        )
    );

    named!(pub message_description<&str, MessageDescription>,
        do_parse!(
            tag!("CM_")   >>
            space >>
            tag!("BO_")   >>
            space >>
            id: digit >>
            space >>
            description: quoted_str >>
            tag!(";") >>
            line_ending >>
            ( MessageDescription {
                id: id.to_string(),
                signal_name: "".to_string(),
                description: description.to_string(),
            } )
        )
    );

    named!(pub message_attribute<&str, MessageAttribute>,
        do_parse!(
            tag!("BA_")   >>
            space >>
            name: quoted_str >>
            space >>
            tag!("BO_")   >>
            space >>
            id: digit >>
            space >>
            value: digit >>
            tag!(";") >>
            line_ending >>
            ( MessageAttribute {
                name: name.to_string(),
                signal_name: "".to_string(),
                id: id.to_string(),
                value: value.to_string()
            } )
        )
    );

    named!(pub signal_definition<&str, SignalDefinition>,
        do_parse!(
            space >>
            tag!("SG_") >>
            space >>
            name: take_until_either!(" \t") >>
            space >>
            tag!(":") >>
            space >>
            start_bit: map_res!(
                digit,
                FromStr::from_str) >>
            tag!("|") >>
            bit_len: map_res!(
                digit,
                FromStr::from_str) >>
            tag!("@") >>
            little_endian: map!(digit, |d| d == "1") >>
            signed: alt!(
                tag!("+") => { |_| false } |
                tag!("-") => { |_| true } ) >>
            space >>
            tag!("(") >>
            scale: float >>
            tag!(",") >>
            offset: float >>
            tag!(")") >>
            space >>
            tag!("[") >>
            min_value: float >>
            tag!("|") >>
            max_value: float >>
            tag!("]") >>
            space >>
            units: quoted_str >>
            space >>
            receiving_node: take_until_either!(" \t\r\n") >>
            line_ending >>
            ( SignalDefinition {
                name: name.to_string(),
                start_bit: start_bit,
                bit_len: bit_len,
                little_endian: little_endian,
                signed: signed,
                scale: scale,
                offset: offset,
                min_value: min_value,
                max_value: max_value,
                units: units.to_string(),
                receiving_node: receiving_node.to_string(),
            } )
        )
    );

    named!(pub signal_description<&str, SignalDescription>,
        do_parse!(
            tag!("CM_")   >>
            space >>
            tag!("SG_")   >>
            space >>
            id: digit >>
            space >>
            signal_name: take_until_either!(" \t") >>
            space >>
            description: quoted_str >>
            tag!(";") >>
            line_ending >>
            ( SignalDescription {
                id: id.to_string(),
                signal_name: signal_name.to_string(),
                description: description.to_string()
            } )
        )
    );

    named!(pub signal_attribute<&str, SignalAttribute>,
        do_parse!(
            tag!("BA_")   >>
            space >>
            name: quoted_str >>
            space >>
            tag!("SG_")   >>
            space >>
            id: digit >>
            space >>
            signal_name: take_until_either!(" \t") >>
            space >>
            value: digit >>
            tag!(";") >>
            line_ending >>
            ( SignalAttribute {
                name: name.to_string(),
                id: id.to_string(),
                signal_name: signal_name.to_string(),
                value: value.to_string()
            } )
        )
    );
}

lazy_static!{
/// Regex patterns for matching DBC entry lines.
///
/// Allows for leading whitespace so that the pattern strings don't strictly
/// need be formatted as they would be coming from a dbc file.
static ref _patterns: [&'static str; 8] = [
    // Version
    r#"^\s*VERSION "(.*)""#,

    // BusConfiguration
    r#"^\s*BUS_: (.*)"#,

    // CanNodes
    // CanNodesDescription
    // CanNodesAttribute

    // MessageDefinition
    r#"^\s*BO_ (\w+) (\w+) *: (\w+) (\w+)"#,
    // MessageDescription
    r#"^\s*CM_ BO_ (.*) "(.*?)";"#,
    // MessageAttribute
    r#"^\s*BA_ "(.*)" BO_ (.*) (.*);"#,

    // SignalDefinition
    r#"^\s*SG_ (.*) : (.*)\|(.*)@(.*)(\+|-) \((.*),(.*)\) \[(.*)\|(.*)\] "(.*)" (.*)"#,
    // SignalDescription
    r#"^\s*CM_ SG_ (.*) (.*) "(.*?)";"#,
    // SignalAttribute
    r#"^\s*BA_ "(.*)" SG_ (.*) (.*) (.*);"#

];
static ref patterns: [Regex; 8] = [
    Regex::new(_patterns[0]).unwrap(),
    Regex::new(_patterns[1]).unwrap(),
    Regex::new(_patterns[2]).unwrap(),
    Regex::new(_patterns[3]).unwrap(),
    Regex::new(_patterns[4]).unwrap(),
    Regex::new(_patterns[5]).unwrap(),
    Regex::new(_patterns[6]).unwrap(),
    Regex::new(_patterns[7]).unwrap(),
];

static ref Patterns: RegexSet = RegexSet::new(_patterns.into_iter()).unwrap();
}

impl Decodable for Entry {
    fn decode<D: Decoder>(d: &mut D) -> Result<Entry, D::Error> {
        let line = try!(d.read_str());
        match line.parse() {
            Ok(entry) => Ok(entry),
            Err(_) => Err(d.error(&*format!(
                "Could not decode '{}' as an entry.", line))),
        }
    }
}

/// Error returned on failure to parse DBC `Entry`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseEntryError {
    kind: EntryErrorKind,
}

impl ParseEntryError {
    #[doc(hidden)]
    pub fn __description(&self) -> &str {
        self.kind.__description()
    }

    #[doc(hidden)]
    pub fn __cause(&self) -> Option<&Error> {
        self.kind.__cause()
    }
}

impl Display for ParseEntryError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.__description())
    }
}

impl Error for ParseEntryError {
    fn description(&self) -> &str {
        self.__description()
    }

    fn cause(&self) -> Option<&Error> {
        self.__cause()
    }
}

/// Internal type DBC `Entry` parsing error.
#[derive(Debug, Clone, PartialEq, Eq)]
enum EntryErrorKind {
    /// Could not find a regex match for input
    RegexNoMatch,
    /// Integer could not be converted into valid `EntryType`
    UnknownEntryType(i32),
    /// Failure to combine all values from regex capture
    RegexCapture,
}

impl EntryErrorKind {
    #[doc(hidden)]
    pub fn __description(&self) -> &str {
        match *self {
            EntryErrorKind::RegexNoMatch => "could not find a regex match for input",
            EntryErrorKind::UnknownEntryType(_) => "integer could not be converted into valid EntryType",
            EntryErrorKind::RegexCapture => "failure to combine all values from regex capture"
        }
    }
    #[doc(hidden)]
    pub fn __cause(&self) -> Option<&Error> {
        match *self {
            EntryErrorKind::RegexNoMatch => None,
            EntryErrorKind::UnknownEntryType(_) => None,
            EntryErrorKind::RegexCapture => None,
        }
    }
}

impl Display for EntryErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = self.__description();
        write!(f, "{}", s)
    }
}

impl From<EntryErrorKind> for ParseEntryError {
    fn from(kind: EntryErrorKind) -> Self {
        ParseEntryError {
            kind
        }
    }
}

impl FromStr for Entry {
    type Err = ParseEntryError;

    fn from_str(line: &str) -> Result<Entry, Self::Err> {
        let entry_idx = {
            match Patterns.matches(line).iter().next() {
                Some(idx) => idx,
                None => return Err(EntryErrorKind::RegexNoMatch.into())
            }
        };

        let entry_type = {
            match EntryType::from_i32(entry_idx as i32) {
                Some(ty) => ty,
                None => return Err(EntryErrorKind::UnknownEntryType(entry_idx as i32).into())
            }
        };

        let entry = match entry_type {

            EntryType::Version => {
                patterns[EntryType::Version as usize].captures(line).map(|caps| {
                    Entry::Version(Version(caps.get(1).unwrap().as_str().to_string()))
                })
            },
            EntryType::BusConfiguration => {
                patterns[EntryType::BusConfiguration as usize].captures(line).map(|caps| {
                    Entry::BusConfiguration(BusConfiguration(caps.get(1).unwrap().as_str().parse().unwrap()))
                })
            },
            EntryType::MessageDefinition => {
                patterns[EntryType::MessageDefinition as usize].captures(line).map(|caps| {
                    Entry::MessageDefinition ( MessageDefinition {
                        id: caps.get(1).unwrap().as_str().to_string(),
                        name: caps.get(2).unwrap().as_str().to_string(),
                        message_len: caps.get(3).unwrap().as_str().parse().unwrap(),
                        sending_node: caps.get(4).unwrap().as_str().to_string()
                    })
                })
            },
            EntryType::MessageDescription => {
                patterns[EntryType::MessageDescription as usize].captures(line).map(|caps| {
                    Entry::MessageDescription ( MessageDescription {
                        id: caps.get(1).unwrap().as_str().to_string(),
                        signal_name: "".to_string(),
                        description: caps.get(2).unwrap().as_str().to_string()
                    })
                })
            },
            EntryType::MessageAttribute => {
                patterns[EntryType::MessageAttribute as usize].captures(line).map(|caps| {
                    Entry::MessageAttribute ( MessageAttribute {
                        name: caps.get(1).unwrap().as_str().to_string(),
                        signal_name: "".to_string(),
                        id: caps.get(2).unwrap().as_str().to_string(),
                        value: caps.get(3).unwrap().as_str().to_string()
                    })
                })
            }
            EntryType::SignalDefinition => {
                patterns[EntryType::SignalDefinition as usize].captures(line).map(|caps| {
                    Entry::SignalDefinition ( SignalDefinition {
                        name: caps.get(1).unwrap().as_str().to_string(),
                        start_bit: caps.get(2).unwrap().as_str().parse().unwrap(),
                        bit_len: caps.get(3).unwrap().as_str().parse().unwrap(),
                        little_endian: caps.get(4).unwrap().as_str() == "1",
                        signed: caps.get(5).unwrap().as_str() == "-",
                        scale: caps.get(6).unwrap().as_str().parse().unwrap(),
                        offset: caps.get(7).unwrap().as_str().parse().unwrap(),
                        min_value: caps.get(8).unwrap().as_str().parse().unwrap(),
                        max_value: caps.get(9).unwrap().as_str().parse().unwrap(),
                        units: caps.get(10).unwrap().as_str().to_string(),
                        receiving_node: caps.get(11).unwrap().as_str().to_string(),
                    })
                })

            },
            EntryType::SignalDescription => {
                patterns[EntryType::SignalDescription as usize].captures(line).map(|caps| {
                    Entry::SignalDescription ( SignalDescription {
                        id: caps.get(1).unwrap().as_str().to_string(),
                        signal_name: caps.get(2).unwrap().as_str().to_string(),
                        description: caps.get(3).unwrap().as_str().to_string()
                    })
                })
            },
            EntryType::SignalAttribute => {
                patterns[EntryType::SignalAttribute as usize].captures(line).map(|caps| {
                    Entry::SignalAttribute ( SignalAttribute {
                        name: caps.get(1).unwrap().as_str().to_string(),
                        id: caps.get(2).unwrap().as_str().to_string(),
                        signal_name: caps.get(3).unwrap().as_str().to_string().to_string(),
                        value: caps.get(4).unwrap().as_str().to_string()
                    })
                })
            },
            EntryType::Unknown => {
                unreachable!()
            }
        };

        if let Some(_entry) = entry {
            Ok(_entry)
        } else {
            Err(EntryErrorKind::RegexCapture.into())
        }

    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    macro_rules! test_entry {
        ($test_name: ident, $entry_type: ident, $test_line: expr, $expected: expr) => (
            mod $test_name {
                use dbc::*;
                use std::str::FromStr;

                #[test]
                fn from_str() {
                    assert_eq!(
                        Entry::from_str($test_line),
                        Ok(Entry::$entry_type ( $expected ))
                    );
                }

                #[test]
                fn from_str_err() {
                    let failstr = format!("GONNAFAIL {}", $test_line);
                    assert!(
                        Entry::from_str(&failstr).is_err(),
                        "Result of entry parse failure should be Err"
                    );
                }

                #[test]
                fn entry_type() {
                    let entry = Entry::$entry_type( $expected );
                    let entry_type = EntryType::$entry_type;

                    assert_eq!(entry.get_type(), entry_type);
                    assert_eq!(
                        format!("{}", entry),
                        format!("{}", entry_type),
                    );
                }

                #[test]
                fn nom_parse() {
                    assert_eq!(
                        nom::$test_name($test_line).unwrap().1,
                        $expected
                    );
                    assert_eq!(
                        nom::entry($test_line).unwrap().1,
                        Entry::$entry_type( $expected )
                    );
                }
            }
        )
    }

    test_entry!( version, Version,
        "VERSION \"A version string\"\n",
        Version ( "A version string".to_string() )
    );

    test_entry!( message_definition, MessageDefinition,
        "BO_ 2364539904 EEC1 : 8 Vector__XXX\n",
        MessageDefinition {
            id: "2364539904".to_string(),
            name: "EEC1".to_string(),
            message_len: 8,
            sending_node: "Vector__XXX".to_string()
        }
    );

    test_entry!( message_description, MessageDescription,
        "CM_ BO_ 2364539904 \"Engine Controller\";\n",
        MessageDescription { id: "2364539904".to_string(), signal_name: "".to_string(), description: "Engine Controller".to_string()}
    );

    test_entry!( message_attribute, MessageAttribute,
        "BA_ \"SingleFrame\" BO_ 2364539904 0;\n",
        MessageAttribute {
            name: "SingleFrame".to_string(),
            signal_name: "".to_string(),
            id: "2364539904".to_string(),
            value: "0".to_string()
        }
    );

    test_entry!( signal_definition, SignalDefinition,
        " SG_ Engine_Speed : 24|16@1+ (0.125,0) [0|8031.88] \"rpm\" Vector__XXX\n",
        SignalDefinition {
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
        }
    );

    test_entry!( signal_description, SignalDescription,
        "CM_ SG_ 2364539904 Engine_Speed \"A description for Engine speed.\";\n",
        SignalDescription {
            id: "2364539904".to_string(),
            signal_name: "Engine_Speed".to_string(),
            description: "A description for Engine speed.".to_string()
        }
    );

    test_entry!( signal_attribute, SignalAttribute,
        "BA_ \"SPN\" SG_ 2364539904 Engine_Speed 190;\n",
        SignalAttribute {
            name: "SPN".to_string(),
            id: "2364539904".to_string(),
            signal_name: "Engine_Speed".to_string(),
            value: "190".to_string()
        }
    );

    mod multiline {
        test_entry!( signal_description, SignalDescription,
            "CM_ SG_ 2364539904 Actual_Engine___Percent_Torque_High_Resolution \"A multi- \r \
            \r \
            line description for Engine torque.\";\n",
            SignalDescription {
                id: "2364539904".to_string(),
                signal_name: "Actual_Engine___Percent_Torque_High_Resolution".to_string(),
                description: "A multi- \r \
                \r \
                line description for Engine torque.".to_string()
            }
        );
    }
}
