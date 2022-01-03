//! Signal processing using PGN/SPN definitions.

#![allow(clippy::trivially_copy_pass_by_ref, clippy::too_many_arguments)]

use crate::dbc::{parser as nomparse, *};
use byteorder::{BigEndian, ByteOrder, LittleEndian};
use encoding::all::ISO_8859_1;
use encoding::{DecoderTrap, Encoding};
use nom;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::marker::Sized;
use std::path::Path;
use std::str::FromStr;

#[cfg(feature = "use-socketcan")]
use socketcan::CANFrame;

/// Trait for converting `Entry` values into a library's own entries.
pub trait FromDbc {
    type Err;

    /// Converts an `Entity` value from scratch.
    fn from_entry(entry: Entry) -> Result<Self, Self::Err>
    where
        Self: Sized;

    /// Merges the given `Entity` with a `mut` version of the library's entity.  Useful for when
    /// multiple `Entry` types contribute to various attributes within the same destination.
    fn merge_entry(&mut self, entry: Entry) -> Result<(), Self::Err>;
}

/// A library used to translate CAN signals into desired values.
#[derive(Debug, PartialEq, Clone)]
pub struct PgnLibrary {
    last_id: u32,
    pgns: HashMap<u32, PgnDefinition>,
}

impl PgnLibrary {
    /// Creates a new `PgnLibrary` instance given an existing lookup table.
    pub fn new(pgns: HashMap<u32, PgnDefinition>) -> Self {
        PgnLibrary {
            last_id: 0,
            pgns: pgns,
        }
    }

    /// Convenience function for loading an entire DBC file into a returned `PgnLibrary`.  This
    /// function ignores unparseable lines as well as `Entry` variants which don't apply to
    /// `PgnLibrary` (such as `Entry::Version`).  Fails on `io::Error`.
    ///
    /// # Example
    ///
    /// ```rust
    /// use canparse::pgn::PgnLibrary;
    ///
    /// let lib: PgnLibrary = PgnLibrary::from_dbc_file("./tests/data/sample.dbc").unwrap();
    ///
    /// ```
    pub fn from_dbc_file<P>(path: P) -> io::Result<Self>
    where
        P: AsRef<Path>,
    {
        Self::from_encoded_dbc_file(path, ISO_8859_1)
    }

    /// Convenience function for loading an entire DBC file from buffer into a returned `PgnLibrary`.  This
    /// function ignores unparseable lines as well as `Entry` variants which don't apply to
    /// `PgnLibrary` (such as `Entry::Version`).  Fails on `io::Error`.
    ///
    /// # Example
    ///
    /// ```rust
    /// use canparse::pgn::PgnLibrary;
    ///
    /// let lib: PgnLibrary = PgnLibrary::from_buffer(buffer).unwrap();
    ///
    /// ```
    pub fn from_buffer(buffer: &[u8]) -> io::Result<Self>
    {
        Self::from_not_encoded_buffer(buffer)
    }

    /// Convenience function for loading an entire DBC file into a returned `PgnLibrary`, using
    /// a specified `Encoding` codec. This function ignores unparseable lines as well as `Entry`
    /// variants which don't apply to `PgnLibrary` (such as `Entry::Version`).
    ///
    /// This function is currently considered unstable and subject to change or removal.
    ///
    /// # Example
    ///
    /// ```rust
    /// extern crate canparse;
    /// extern crate encoding;
    ///
    /// use canparse::pgn::PgnLibrary;
    /// use encoding::Encoding;
    /// use encoding::all::ISO_8859_1;
    ///
    /// let lib: PgnLibrary = PgnLibrary::from_encoded_dbc_file("./tests/data/sample.dbc",
    ///                                                         ISO_8859_1).unwrap();
    ///
    /// ```
    #[doc(hidden)]
    pub fn from_encoded_dbc_file<P, E>(path: P, encoding: &E) -> io::Result<Self>
    where
        P: AsRef<Path>,
        E: Encoding,
    {
        let mut lib = PgnLibrary::default();

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

    #[doc(hidden)]
    pub fn from_not_encoded_buffer(buffer: &[u8]) -> io::Result<Self>
    {
        let mut lib = PgnLibrary::default();

        let mut i: &str = "";
        let parsed_string_result = std::str::from_utf8(buffer);
        match parsed_string_result {
            Ok(parsed_string) => i = parsed_string,
            Err(_) => {
                // TODO: Handle from_utf8 error
            }
        }
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

    /// Converts/combines DBC `Entry` values into entries within `PgnLibrary`.  Different `Entry`
    /// variants can modify the same internal entry, hence the need for mutability.  This function
    /// is meant to be called when parsing lines in a `dbc` file.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::collections::HashMap;
    /// use std::io::BufRead;
    /// use std::str::FromStr;
    /// use canparse::dbc::Entry;
    /// use canparse::pgn::PgnLibrary;
    ///
    /// let mut lib = PgnLibrary::new( HashMap::default() );
    ///
    /// // File is ISO-8859-1 and needs to be converted before iterating
    /// // with `lines`. `PgnLibrary::from_dbc_file` does this for you.
    /// let data: String = include_bytes!("../tests/data/sample.dbc")
    ///     .iter().map(|b| *b as char).collect();
    ///
    /// for line in data.lines() {
    ///     if let Some(entry) = Entry::from_str(line).ok() {
    ///         lib.add_entry(entry).ok();
    ///     }
    /// }
    /// ```
    pub fn add_entry(&mut self, entry: Entry) -> Result<(), String> {
        use std::collections::hash_map::Entry as HashMapEntry;

        let _id: u32 = *match entry {
            Entry::MessageDefinition(MessageDefinition { ref id, .. }) => id,
            Entry::MessageDescription(MessageDescription { ref id, .. }) => id,
            Entry::MessageAttribute(MessageAttribute { ref id, .. }) => id,
            Entry::SignalDefinition(..) => {
                // no id, and by definition must follow MessageDefinition
                if self.last_id == 0 {
                    return Err("Tried to add SignalDefinition without last ID.".to_string());
                }
                &self.last_id
            }
            Entry::SignalDescription(SignalDescription { ref id, .. }) => id,
            Entry::SignalAttribute(SignalAttribute { ref id, .. }) => id,
            _ => {
                return Err(format!("Unsupported entry: {}.", entry));
            }
        };

        // CanId{ DP, PF, PS, SA } => Pgn{ PF, PS }
        let pgn = (_id >> 8) & 0x1FFFF;

        self.last_id = _id;
        match self.pgns.entry(pgn) {
            HashMapEntry::Occupied(mut existing) => {
                existing.get_mut().merge_entry(entry).unwrap();
            }
            HashMapEntry::Vacant(vacant) => {
                vacant.insert(PgnDefinition::from_entry(entry).unwrap());
            }
        }

        Ok(())
    }

    /// Returns a `PgnDefinition` entry reference, if it exists.
    pub fn get_pgn(&self, pgn: u32) -> Option<&PgnDefinition> {
        self.pgns.get(&pgn)
    }

    /// Returns a `SpnDefinition` entry reference, if it exists.
    pub fn get_spn(&self, name: &str) -> Option<&SpnDefinition> {
        self.pgns
            .iter()
            .filter_map(|pgn| pgn.1.spns.get(name))
            .next()
    }
}

impl Default for PgnLibrary {
    fn default() -> Self {
        PgnLibrary::new(HashMap::default())
    }
}

/// Parameter Group Number definition
#[derive(Debug, PartialEq, Clone)]
pub struct PgnDefinition {
    pgn: u32,
    pgn_long: u32,
    name_abbrev: String,
    description: String,
    length: u32,
    spns: HashMap<String, SpnDefinition>,
}

impl PgnDefinition {
    pub fn new(
        pgn: u32,
        pgn_long: u32,
        name_abbrev: String,
        description: String,
        length: u32,
        spns: HashMap<String, SpnDefinition>,
    ) -> Self {
        PgnDefinition {
            pgn: pgn,
            pgn_long: pgn_long,
            name_abbrev: name_abbrev,
            description: description,
            length: length,
            spns: spns,
        }
    }
}
// TODO: PgnDefinition Builder pattern

/// Error returned on failure to parse `*Definition` type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseDefinitionError {
    kind: DefinitionErrorKind,
}

impl ParseDefinitionError {
    #[doc(hidden)]
    pub fn __description(&self) -> &str {
        self.kind.__description()
    }

    #[doc(hidden)]
    pub fn __cause(&self) -> Option<&dyn Error> {
        self.kind.__cause()
    }
}

impl Display for ParseDefinitionError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.__description())
    }
}

impl Error for ParseDefinitionError {
    fn description(&self) -> &str {
        self.__description()
    }

    fn cause(&self) -> Option<&dyn Error> {
        self.__cause()
    }
}

/// Internal type for `*Definition` parsing errors.
#[derive(Debug, Clone, PartialEq, Eq)]
enum DefinitionErrorKind {
    /// Internal `Entry` parsing error
    Entry(super::dbc::ParseEntryError),
    /// `Entry` type not applicable in constructing Definition
    UnusedEntry(super::dbc::EntryType),
}

impl DefinitionErrorKind {
    #[doc(hidden)]
    pub fn __description(&self) -> &str {
        match self {
            DefinitionErrorKind::Entry(_) => "internal Entry parsing error",
            DefinitionErrorKind::UnusedEntry(_) => {
                "Entry type not applicable in constructing Definition"
            }
        }
    }

    #[doc(hidden)]
    pub fn __cause(&self) -> Option<&dyn Error> {
        match self {
            DefinitionErrorKind::Entry(e) => Some(e),
            DefinitionErrorKind::UnusedEntry(_e) => None,
        }
    }
}

impl Display for DefinitionErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = self.__description();
        write!(f, "{}", s)
    }
}

impl From<DefinitionErrorKind> for ParseDefinitionError {
    fn from(kind: DefinitionErrorKind) -> Self {
        ParseDefinitionError { kind }
    }
}

impl FromStr for PgnDefinition {
    type Err = ParseDefinitionError;

    /// `&str` -> `PgnDefinition` via `dbc::Entry` (though probably won't be used).
    fn from_str(line: &str) -> Result<Self, Self::Err>
    where
        Self: Sized + FromDbc,
    {
        Entry::from_str(line)
            .map_err(|e| DefinitionErrorKind::Entry(e).into())
            .and_then(Self::from_entry)
    }
}

impl FromDbc for PgnDefinition {
    type Err = ParseDefinitionError;

    fn from_entry(entry: Entry) -> Result<Self, Self::Err>
    where
        Self: Sized,
    {
        match entry {
            Entry::MessageDefinition(MessageDefinition { id, name, .. }) => {
                let pgn_long = id;
                let pgn = pgn_long & 0x1FFFF;
                Ok(PgnDefinition::new(
                    pgn,
                    pgn_long,
                    name,
                    "".to_string(),
                    0,
                    HashMap::new(),
                ))
            }
            Entry::MessageDescription(MessageDescription {
                id, description, ..
            }) => {
                let pgn_long = id;
                let pgn = pgn_long & 0x1FFFF;
                Ok(PgnDefinition::new(
                    pgn,
                    pgn_long,
                    "".to_string(),
                    description,
                    0,
                    HashMap::new(),
                ))
            }
            Entry::MessageAttribute(MessageAttribute { id, .. }) => {
                let pgn_long = id;
                let pgn = pgn_long & 0x1FFFF;
                Ok(PgnDefinition::new(
                    pgn,
                    pgn_long,
                    "".to_string(),
                    "".to_string(),
                    0,
                    HashMap::new(),
                ))
            }
            _ => Err(DefinitionErrorKind::UnusedEntry(entry.get_type()).into()),
        }
    }

    fn merge_entry(&mut self, entry: Entry) -> Result<(), Self::Err> {
        match entry {
            Entry::MessageDefinition(MessageDefinition {
                id, message_len, ..
            }) => {
                let pgn_long = id;
                let pgn = pgn_long & 0x1FFFF;
                self.pgn = pgn;
                self.pgn_long = pgn_long;
                self.length = message_len;
                Ok(())
            }
            Entry::MessageDescription(MessageDescription {
                id, description, ..
            }) => {
                let pgn_long = id;
                let pgn = pgn_long & 0x1FFFF;
                self.pgn = pgn;
                self.pgn_long = pgn_long;
                self.description = description;
                Ok(())
            }
            Entry::MessageAttribute(MessageAttribute { id, .. }) => {
                let pgn_long = id;
                let pgn = pgn_long & 0x1FFFF;
                self.pgn = pgn;
                self.pgn_long = pgn_long;
                Ok(())
            }
            Entry::SignalDefinition(wrapped) => {
                if self.spns.contains_key(&wrapped.name) {
                    (*self.spns.get_mut(&wrapped.name).unwrap())
                        .merge_entry(Entry::SignalDefinition(wrapped))
                        .unwrap();
                } else {
                    self.spns.insert(
                        wrapped.name.clone(),
                        SpnDefinition::from_entry(Entry::SignalDefinition(wrapped)).unwrap(),
                    );
                }
                Ok(())
            }
            Entry::SignalDescription(wrapped) => {
                if self.spns.contains_key(&wrapped.signal_name) {
                    (*self.spns.get_mut(&wrapped.signal_name).unwrap())
                        .merge_entry(Entry::SignalDescription(wrapped))
                        .unwrap();
                } else {
                    self.spns.insert(
                        wrapped.signal_name.clone(),
                        SpnDefinition::from_entry(Entry::SignalDescription(wrapped)).unwrap(),
                    );
                }
                Ok(())
            }
            Entry::SignalAttribute(wrapped) => {
                if wrapped.name != "SPN" {
                    // Skip non-SPN attributes
                    return Ok(());
                }
                if self.spns.contains_key(&wrapped.signal_name) {
                    (*self.spns.get_mut(&wrapped.signal_name).unwrap())
                        .merge_entry(Entry::SignalAttribute(wrapped))
                        .unwrap();
                } else {
                    self.spns.insert(
                        wrapped.signal_name.clone(),
                        SpnDefinition::from_entry(Entry::SignalAttribute(wrapped)).unwrap(),
                    );
                }
                Ok(())
            }
            _ => Err(DefinitionErrorKind::UnusedEntry(entry.get_type()).into()),
        }
    }
}

/// Suspect Parameter Number definition
#[derive(Debug, PartialEq, Clone)]
pub struct SpnDefinition {
    name: String,
    pub number: usize,
    id: u32,
    description: String,
    start_bit: usize,
    bit_len: usize,
    little_endian: bool,
    signed: bool,
    scale: f32,
    offset: f32,
    min_value: f32,
    max_value: f32,
    units: String,
}

/// Internal function for parsing CAN message arrays given the definition parameters.  This is where
/// the real calculations happen.
fn parse_array(
    bit_len: usize,
    start_bit: usize,
    little_endian: bool,
    scale: f32,
    offset: f32,
    msg: &[u8; 8],
) -> Option<f32> {
    let msg64: u64 = if little_endian {
        LittleEndian::read_u64(msg)
    } else {
        BigEndian::read_u64(msg)
    };

    let bit_mask: u64 = 2u64.pow(bit_len as u32) - 1;

    Some((((msg64 >> start_bit) & bit_mask) as f32) * scale + offset)
}

/// Internal function for parsing CAN message slices given the definition parameters.  This is where
/// the real calculations happen.
fn parse_message(
    bit_len: usize,
    start_bit: usize,
    little_endian: bool,
    scale: f32,
    offset: f32,
    msg: &[u8],
) -> Option<f32> {
    if msg.len() < 8 {
        return None;
    }
    let msg64: u64 = if little_endian {
        LittleEndian::read_u64(msg)
    } else {
        BigEndian::read_u64(msg)
    };

    let bit_mask: u64 = 2u64.pow(bit_len as u32) - 1;

    Some((((msg64 >> start_bit) & bit_mask) as f32) * scale + offset)
}

/// The collection of functions for parsing CAN messages `N` into their defined signal values.
pub trait ParseMessage<N> {
    /// Parses CAN message type `N` into generic `f32` signal value on success, or `None`
    /// on failure.
    fn parse_message(&self, msg: N) -> Option<f32>;

    /// Returns a closure which parses CAN message type `N` into generic `f32` signal value on
    /// success, or `None` on failure.
    fn parser(&self) -> Box<dyn Fn(N) -> Option<f32>>;
}

impl SpnDefinition {
    /// Return new `SpnDefinition` given the definition parameters.
    pub fn new(
        name: String,
        number: usize,
        id: u32,
        description: String,
        start_bit: usize,
        bit_len: usize,
        little_endian: bool,
        signed: bool,
        scale: f32,
        offset: f32,
        min_value: f32,
        max_value: f32,
        units: String,
    ) -> Self {
        SpnDefinition {
            name: name,
            number: number,
            id: id,
            description: description,
            start_bit: start_bit,
            bit_len: bit_len,
            little_endian: little_endian,
            signed: signed,
            scale: scale,
            offset: offset,
            min_value: min_value,
            max_value: max_value,
            units: units,
        }
    }
}

impl<'a> ParseMessage<&'a [u8; 8]> for SpnDefinition {
    fn parse_message(&self, msg: &[u8; 8]) -> Option<f32> {
        parse_array(
            self.bit_len,
            self.start_bit,
            self.little_endian,
            self.scale,
            self.offset,
            msg,
        )
    }

    fn parser(&self) -> Box<dyn Fn(&[u8; 8]) -> Option<f32>> {
        let bit_len = self.bit_len;
        let start_bit = self.start_bit;
        let scale = self.scale;
        let offset = self.offset;
        let little_endian = self.little_endian;

        let fun =
            move |msg: &[u8; 8]| parse_array(bit_len, start_bit, little_endian, scale, offset, msg);

        Box::new(fun)
    }
}

impl<'a> ParseMessage<&'a [u8]> for SpnDefinition {
    fn parse_message(&self, msg: &[u8]) -> Option<f32> {
        parse_message(
            self.bit_len,
            self.start_bit,
            self.little_endian,
            self.scale,
            self.offset,
            msg,
        )
    }

    fn parser(&self) -> Box<dyn Fn(&[u8]) -> Option<f32>> {
        let bit_len = self.bit_len;
        let start_bit = self.start_bit;
        let scale = self.scale;
        let offset = self.offset;
        let little_endian = self.little_endian;

        let fun =
            move |msg: &[u8]| parse_message(bit_len, start_bit, little_endian, scale, offset, msg);

        Box::new(fun)
    }
}

#[cfg(feature = "use-socketcan")]
impl<'a> ParseMessage<&'a CANFrame> for SpnDefinition {
    fn parse_message(&self, frame: &CANFrame) -> Option<f32> {
        let msg = &frame.data();
        parse_message(
            self.bit_len,
            self.start_bit,
            self.little_endian,
            self.scale,
            self.offset,
            msg,
        )
    }

    fn parser(&self) -> Box<dyn Fn(&CANFrame) -> Option<f32>> {
        let bit_len = self.bit_len;
        let start_bit = self.start_bit;
        let scale = self.scale;
        let offset = self.offset;
        let little_endian = self.little_endian;

        let fun = move |frame: &CANFrame| {
            let msg = &frame.data();
            parse_message(bit_len, start_bit, little_endian, scale, offset, msg)
        };

        Box::new(fun)
    }
}

impl FromStr for SpnDefinition {
    type Err = ParseDefinitionError;

    /// `&str` -> `SpnDefinition` via `dbc::Entry` (though probably won't be used).
    fn from_str(line: &str) -> Result<Self, Self::Err>
    where
        Self: Sized + FromDbc,
    {
        Entry::from_str(line)
            .map_err(|e| DefinitionErrorKind::Entry(e).into())
            .and_then(Self::from_entry)
    }
}

impl FromDbc for SpnDefinition {
    type Err = ParseDefinitionError;

    fn from_entry(entry: Entry) -> Result<Self, Self::Err>
    where
        Self: Sized,
    {
        match entry {
            Entry::SignalDefinition(signal_definition) => Ok(signal_definition.into()),
            Entry::SignalDescription(signal_description) => Ok(signal_description.into()),
            Entry::SignalAttribute(signal_attribute) => Ok(signal_attribute.into()),
            _ => Err(DefinitionErrorKind::UnusedEntry(entry.get_type()).into()),
        }
    }

    fn merge_entry(&mut self, entry: Entry) -> Result<(), Self::Err> {
        match entry {
            Entry::SignalDefinition(SignalDefinition {
                name,
                start_bit,
                bit_len,
                little_endian,
                signed,
                scale,
                offset,
                min_value,
                units,
                ..
            }) => {
                self.name = name;
                self.start_bit = start_bit;
                self.bit_len = bit_len;
                self.little_endian = little_endian;
                self.signed = signed;
                self.scale = scale;
                self.offset = offset;
                self.min_value = min_value;
                self.units = units;
                Ok(())
            }
            Entry::SignalDescription(SignalDescription {
                id,
                signal_name,
                description,
            }) => {
                self.name = signal_name;
                self.id = id;
                self.description = description;
                Ok(())
            }
            Entry::SignalAttribute(SignalAttribute {
                id,
                signal_name,
                value,
                ..
            }) => {
                self.name = signal_name;
                self.id = id;
                self.number = value.parse().unwrap();
                Ok(())
            }
            _ => Err(DefinitionErrorKind::UnusedEntry(entry.get_type()).into()),
        }
    }
}

impl From<SignalDefinition> for SpnDefinition {
    fn from(
        SignalDefinition {
            name,
            start_bit,
            bit_len,
            little_endian,
            signed,
            scale,
            offset,
            min_value,
            max_value,
            units,
            ..
        }: SignalDefinition,
    ) -> Self {
        SpnDefinition::new(
            name,
            0,
            0, // TODO: Some()?
            "".to_string(),
            start_bit,
            bit_len,
            little_endian,
            signed,
            scale,
            offset,
            min_value,
            max_value,
            units,
        )
    }
}
impl From<SignalDescription> for SpnDefinition {
    fn from(
        SignalDescription {
            id,
            signal_name,
            description,
        }: SignalDescription,
    ) -> Self {
        SpnDefinition::new(
            signal_name,
            0,
            id,
            description,
            0,
            0,
            true,
            false,
            0.0,
            0.0,
            0.0,
            0.0,
            "".to_string(),
        )
    }
}
impl From<SignalAttribute> for SpnDefinition {
    fn from(
        SignalAttribute {
            id,
            signal_name,
            value,
            ..
        }: SignalAttribute,
    ) -> Self {
        SpnDefinition::new(
            signal_name,
            value.parse().unwrap(),
            id,
            "".to_string(),
            0,
            0,
            true,
            false,
            0.0,
            0.0,
            0.0,
            0.0,
            "".to_string(),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::pgn::*;
    use approx::assert_relative_eq;

    lazy_static! {
        static ref PGNLIB_EMPTY: PgnLibrary = PgnLibrary::default();
        static ref PGNLIB_ONE: PgnLibrary = PgnLibrary::from_dbc_file("./tests/data/sample.dbc")
            .expect("Failed to create PgnLibrary from file");
        static ref SPNDEF: SpnDefinition = SpnDefinition::new(
            "Engine_Speed".to_string(),
            190,
            2364539904,
            "A description for Engine speed.".to_string(),
            24,
            16,
            true,
            false,
            0.125,
            0.0,
            0.0,
            8031.88,
            "rpm".to_string()
        );
        static ref SPNDEF_BE: SpnDefinition = {
            let mut _spndef = SPNDEF.clone();
            _spndef.little_endian = false;
            _spndef
        };
        static ref MSG: [u8; 8] = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
        static ref MSG_BE: [u8; 8] = [0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11];
    }

    #[test]
    fn default_pgnlibrary() {
        assert_eq!(PGNLIB_EMPTY.pgns.len(), 0);
    }

    #[test]
    fn get_spndefinition() {
        assert_eq!(
            *PGNLIB_ONE
                .get_pgn(0xF004)
                .expect("failed to get PgnDefinition from PgnLibrary")
                .spns
                .get("Engine_Speed")
                .expect("failed to get SpnDefinition from PgnDefinition"),
            *SPNDEF
        );
    }

    #[test]
    fn unsupported_entry() {
        let mut pgnlib: PgnLibrary = PgnLibrary::default();
        let unsupported = Entry::Version(Version("Don't care about version entry".to_string()));
        let res = pgnlib.add_entry(unsupported);

        assert!(res.is_err(), "Unsupported entry: Version");
    }

    #[test]
    fn test_parse_array() {
        assert_relative_eq!(SPNDEF.parse_message(&MSG as &[u8; 8]).unwrap(), 2728.5f32);
        assert_relative_eq!(
            SPNDEF_BE.parse_message(&MSG_BE as &[u8; 8]).unwrap(),
            2728.5
        );
    }

    #[test]
    fn test_parse_message() {
        assert_relative_eq!(SPNDEF.parse_message(&MSG[..]).unwrap(), 2728.5);
        assert_relative_eq!(SPNDEF_BE.parse_message(&MSG_BE[..]).unwrap(), 2728.5);
        assert!(SPNDEF.parse_message(&MSG[..7]).is_none());
        assert!(SPNDEF_BE.parse_message(&MSG_BE[..7]).is_none());
    }

    #[test]
    fn parse_message_closure() {
        assert_relative_eq!(SPNDEF.parser()(&MSG[..]).unwrap(), 2728.5);
        assert_relative_eq!(SPNDEF_BE.parser()(&MSG_BE[..]).unwrap(), 2728.5);
    }

    #[cfg(feature = "use-socketcan")]
    mod socketcan {
        extern crate socketcan;

        use super::*;

        use socketcan::CANFrame;

        lazy_static! {
            static ref FRAME: CANFrame = CANFrame::new(0, &MSG[..], false, false).unwrap();
            static ref FRAME_BE: CANFrame = CANFrame::new(0, &MSG_BE[..], false, false).unwrap();
        }

        #[test]
        fn parse_canframe_closure() {
            assert_relative_eq!(SPNDEF.parser()(&FRAME as &CANFrame).unwrap(), 2728.5);
            assert_relative_eq!(SPNDEF_BE.parser()(&FRAME_BE as &CANFrame).unwrap(), 2728.5);
        }

        #[test]
        fn test_parse_canframe() {
            assert_relative_eq!(SPNDEF.parse_message(&FRAME as &CANFrame).unwrap(), 2728.5);
            assert_relative_eq!(
                SPNDEF_BE.parse_message(&FRAME_BE as &CANFrame).unwrap(),
                2728.5
            );
        }
    }
}
