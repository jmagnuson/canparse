use regex::Regex;
use std::collections::HashMap;
use std::marker::Sized;
use std::str::FromStr;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use dbc::*;
use byteorder::{ByteOrder, BigEndian, LittleEndian};

#[cfg(feature = "use-socketcan")]
use socketcan::CANFrame;

/// Trait for converting `Entry` values into a library's own entries.
pub trait FromDbc {

    /// Converts an `Entity` value from scratch.
    fn from_entry(entry: Entry) -> Result<Self, String> where Self: Sized;

    /// Merges the given `Entity` with a `mut` version of the library's entity.  Useful for when
    /// multiple `Entry` types contribute to various attributes within the same destination.
    fn merge_entry(&mut self, entry: Entry) -> Result<(), String>;
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
        PgnLibrary { last_id:0, pgns:pgns }
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
    /// use canparse::{PgnLibrary, Entry};
    ///
    /// let mut lib = PgnLibrary::new( HashMap::default() );
    ///
    /// let br = include_bytes!("./tests/data/sample.dbc");
    ///
    /// for l in br.lines() {
    ///     let line = l.unwrap();
    ///     if let Some(entry) = Entry::from_str(line.as_str()).ok() {
    ///         lib.add_entry(entry).ok();
    ///
    ///     }
    /// }
    /// ```
    pub fn add_entry(&mut self, entry: Entry) -> Result<(), String> {
        let _id: u32 = match entry {
            Entry::MessageDefinition (MessageDefinition  { ref id, .. }) => id.parse(),
            Entry::MessageDescription(MessageDescription { ref id, .. }) => id.parse(),
            Entry::MessageAttribute( MessageAttribute  { ref id, .. }) => id.parse(),
            Entry::SignalDefinition ( .. ) => {

                 // no id, and by definition must follow MessageDefinition
                if self.last_id == 0 {
                    return Err("Tried to add SignalDefinition without last ID.".to_string());
                }
                Ok(self.last_id)
            },
            Entry::SignalDescription (SignalDescription{ ref id, .. }) => id.parse(),
            Entry::SignalAttribute (SignalAttribute { ref id, .. }) => id.parse(),
            _ => { return Err(format!("Unsupported entry: {}.", entry).to_string()); }
        }.unwrap();

        // CanId{ DP, PF, PS, SA } => Pgn{ PF, PS }
        let pgn = (_id >> 8) & 0x1FFFF;

        self.last_id = _id;
        if self.pgns.contains_key(&pgn) {
            (*self.pgns.get_mut(&pgn).unwrap()).merge_entry(entry).unwrap();
        } else {
            self.pgns.insert(pgn, PgnDefinition::from_entry(entry).unwrap());
        }
        Ok(())
    }

    /// Returns a `PgnDefinition` entry reference, if it exists.
    pub fn get_pgn(&self, pgn: u32) -> Option<&PgnDefinition> {
        self.pgns.get(&pgn)
    }

    /// Returns a `SpnDefinition` entry reference, if it exists.
    pub fn get_spn(&self, name: &String) -> Option<&SpnDefinition> {
        self.pgns.iter().filter_map(|pgn| {
            pgn.1.spns.get(name)
        }).next()
    }
}

impl Default for PgnLibrary {
    fn default() -> Self {
        PgnLibrary::new( HashMap::default() )
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
    spns: HashMap<String, SpnDefinition>
}

impl PgnDefinition {
    pub fn new( pgn: u32,
                pgn_long: u32,
                name_abbrev: String,
                description: String,
                length: u32,
                spns: HashMap<String,SpnDefinition> ) -> Self {

        PgnDefinition {
            pgn: pgn,
            pgn_long: pgn_long,
            name_abbrev: name_abbrev,
            description: description,
            length: length,
            spns: spns
        }
    }
}
// TODO: PgnDefinition Builder pattern

impl FromStr for PgnDefinition {
    type Err = String;

    /// `&str` -> `PgnDefinition` via `dbc::Entry` (though probably won't be used).
    fn from_str(line: &str) -> Result<Self, String> where Self: Sized + FromDbc {
        Entry::from_str(line).and_then(|entry| Self::from_entry(entry))
    }
}

impl FromDbc for PgnDefinition {
    fn from_entry(entry: Entry) -> Result<Self, String> where Self: Sized {
        match entry {
            Entry::MessageDefinition ( MessageDefinition { id, name, message_len, sending_node }) => {
                let pgn_long = id.parse::<u32>().unwrap();
                let pgn = pgn_long & 0x1FFFF;
                Ok(PgnDefinition::new(pgn, pgn_long, name, "".to_string(), 0, HashMap::new()))
            },
            Entry::MessageDescription ( MessageDescription { id, signal_name, description }) => {
                let pgn_long = id.parse::<u32>().unwrap();
                let pgn = pgn_long & 0x1FFFF;
                Ok(PgnDefinition::new(pgn, pgn_long, "".to_string(), description, 0, HashMap::new()))
            },
            Entry::MessageAttribute ( MessageAttribute { name, signal_name, id, value }) => {
                let pgn_long = id.parse::<u32>().unwrap();
                let pgn = pgn_long & 0x1FFFF;
                Ok(PgnDefinition::new(pgn, pgn_long, "".to_string(), "".to_string(), 0, HashMap::new()))
            }
            _ => Err("Could not map entry to PgnDefinition".to_string())
        }
    }

    fn merge_entry(&mut self, entry: Entry) -> Result<(), String> {
        match entry {
            Entry::MessageDefinition ( MessageDefinition { id, name, message_len, sending_node }) => {
                let pgn_long = id.parse::<u32>().unwrap();
                let pgn = pgn_long & 0x1FFFF;
                self.pgn = pgn; self.pgn_long = pgn_long; self.length = message_len;
                Ok(())
            },
            Entry::MessageDescription ( MessageDescription {id, signal_name, description }) => {
                let pgn_long = id.parse::<u32>().unwrap();
                let pgn = pgn_long & 0x1FFFF;
                self.pgn = pgn; self.pgn_long = pgn_long; self.description = description;
                Ok(())
            },
            Entry::MessageAttribute( MessageAttribute { name, signal_name, id, value }) => {
                let pgn_long = id.parse::<u32>().unwrap();
                let pgn = pgn_long & 0x1FFFF;
                self.pgn = pgn; self.pgn_long = pgn_long;
                Ok(())
            },
            Entry::SignalDefinition ( wrapped ) => {
                if self.spns.contains_key(&wrapped.name) {
                    (*self.spns.get_mut(&wrapped.name).unwrap())
                        .merge_entry(Entry::SignalDefinition(wrapped)).unwrap();
                } else {
                    self.spns.insert(wrapped.name.clone(),
                                     SpnDefinition::from_entry(Entry::SignalDefinition(wrapped)).unwrap());
                }
                Ok(())
            },
            Entry::SignalDescription ( wrapped ) => {
                if self.spns.contains_key(&wrapped.signal_name) {
                    (*self.spns.get_mut(&wrapped.signal_name).unwrap())
                        .merge_entry(Entry::SignalDescription(wrapped)).unwrap();
                } else {
                    self.spns.insert(wrapped.signal_name.clone(),
                                     SpnDefinition::from_entry(Entry::SignalDescription(wrapped)).unwrap());
                }
                Ok(())
            },
            Entry::SignalAttribute ( wrapped ) => {
                if self.spns.contains_key(&wrapped.signal_name) {
                    (*self.spns.get_mut(&wrapped.signal_name).unwrap())
                        .merge_entry(Entry::SignalAttribute(wrapped)).unwrap();
                } else {
                    self.spns.insert(wrapped.signal_name.clone(),
                                     SpnDefinition::from_entry(Entry::SignalAttribute(wrapped)).unwrap());
                }
                Ok(())
            },
            _ => Err("Could not map entry to PgnDefinition".to_string())
        }
    }
}

/*impl FromDbc for PgnDefinition {
    fn from_dbc(line: String) -> Result<Self, &'static str> {
        // TODO: Populate based on `processDbcLine` in `PgnLibrary`
    }
}*/

/// Suspect Parameter Number definition
#[derive(Debug, PartialEq, Clone)]
pub struct SpnDefinition {
    name: String,
    pub number: usize,
    id: String,
    description: String,
    start_bit: usize,
    bit_len: usize,
    little_endian: bool,
    signed: bool,
    scale: f32,
    offset: f32,
    min_value: f32,
    max_value: f32,
    units: String
}

const SHIFT_BYTE_LOOKUP: [u64; 8] = [ 1, 1<<(1*8), 1<<(2*8), 1<<(3*8), 1<<(4*8), 1<<(5*8), 1<<(6*8), 1<<(7*8) ];

/// Internal function for parsing CAN message arrays given the definition parameters.  This is where
/// the real calculations happen.
fn parse_array(bit_len: usize, start_bit: usize, little_endian: bool, scale: f32, offset: f32, msg: &[u8; 8]) -> Option<f32> {
    let msg64: u64 = match little_endian {
        true => LittleEndian::read_u64(msg),
        false => BigEndian::read_u64(msg)
    };

    let bit_mask: u64 = 2u64.pow(bit_len as u32) - 1;

    Some(
        ( ( ( msg64 >> start_bit ) & bit_mask ) as f32 ) * scale + offset
    )
}

/// Internal function for parsing CAN message slices given the definition parameters.  This is where
/// the real calculations happen.
fn parse_message(bit_len: usize, start_bit: usize, little_endian: bool, scale: f32, offset: f32, msg: &[u8]) -> Option<f32> {

    let num_bytes: usize = f32::ceil( (bit_len as f32)/8.0 ) as usize;
    let byte_pos: usize = f32::floor( (start_bit as f32)/8.0 ) as usize;
    let mut val32: u32 = 0;

    // TODO: There has to be a clean way to parameterize iter transforms
    if little_endian {
        for (i, n) in msg.iter().skip(byte_pos).take(num_bytes).enumerate() {
            val32 += ((n & 0xFF) as u32) * (SHIFT_BYTE_LOOKUP[i] as u32);
        }
    } else {
        for (i, n) in msg.iter().rev().skip(byte_pos).take(num_bytes).enumerate() {
            val32 += ((n & 0xFF) as u32) * (SHIFT_BYTE_LOOKUP[i] as u32);
        }
    }

    let bit_pos = start_bit % 8;
    val32 >>= bit_pos;

    let bit_mask = 2u32.pow(bit_len as u32) - 1;
    val32 &= bit_mask;

    Some(val32 as f32 * scale + offset)
}

/// The collection of functions for parsing CAN messages `N` into their defined signal values.
pub trait ParseMessage<N> {

    /// Parses CAN message type `N` into generic `f32` signal value on success, or `None`
    /// on failure.
    fn parse_message(&self, msg: N) -> Option<f32>;

    /// Returns a closure which parses CAN message type `N` into generic `f32` signal value on
    /// success, or `None` on failure.
    fn parser(&self) -> Box<Fn(N) -> Option<f32>>;
}

impl SpnDefinition {

    /// Return new `SpnDefinition` given the definition parameters.
    pub fn new(name: String,
               number: usize,
               id: String,
               description: String,
               start_bit: usize,
               bit_len: usize,
               little_endian: bool,
               signed: bool,
               scale: f32,
               offset: f32,
               min_value: f32,
               max_value: f32,
               units: String) -> Self {
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
            units: units
        }
    }
}

impl <'a> ParseMessage<&'a [u8; 8]> for SpnDefinition {

    fn parse_message(&self, msg: &[u8; 8]) -> Option<f32> {
        parse_array(self.bit_len, self.start_bit, self.little_endian, self.scale, self.offset, msg)
    }

    fn parser(&self) -> Box<Fn(&[u8; 8]) -> Option<f32>> {
        let bit_len = self.bit_len;
        let start_bit = self.start_bit;
        let scale = self.scale;
        let offset = self.offset;
        let little_endian = self.little_endian;

        let fun = move |msg: &[u8; 8]| {
            parse_array(bit_len, start_bit, little_endian, scale, offset, msg)
        };

        Box::new(fun)
    }
}

impl <'a> ParseMessage<&'a [u8]> for SpnDefinition {

    fn parse_message(&self, msg: &[u8]) -> Option<f32> {
        parse_message(self.bit_len, self.start_bit, self.little_endian, self.scale, self.offset, msg)
    }

    fn parser(&self) -> Box<Fn(&[u8]) -> Option<f32>> {
        let bit_len = self.bit_len;
        let start_bit = self.start_bit;
        let scale = self.scale;
        let offset = self.offset;
        let little_endian = self.little_endian;

        let fun = move |msg: &[u8]| {
            parse_message(bit_len, start_bit, little_endian, scale, offset, msg)
        };

        Box::new(fun)
    }
}

#[cfg(feature = "use-socketcan")]
impl <'a> ParseMessage<&'a CANFrame> for SpnDefinition {

    fn parse_message(&self, frame: &CANFrame) -> Option<f32> {
        let ref msg = frame.data();
        parse_message(self.bit_len, self.start_bit, self.little_endian, self.scale, self.offset, msg)
    }

    fn parser(&self) -> Box<Fn(&CANFrame) -> Option<f32>> {

        let bit_len = self.bit_len;
        let start_bit = self.start_bit;
        let scale = self.scale;
        let offset = self.offset;
        let little_endian = self.little_endian;

        let fun = move |frame: &CANFrame| {
            let ref msg = frame.data();
            parse_message(bit_len, start_bit, little_endian, scale, offset, msg)
        };

        Box::new(fun)
    }
}

impl FromStr for SpnDefinition {
    type Err = String;

    /// `&str` -> `SpnDefinition` via `dbc::Entry` (though probably won't be used).
    fn from_str(line: &str) -> Result<Self, String> where Self: Sized + FromDbc {
        Entry::from_str(line).and_then(|entry| Self::from_entry(entry))
    }
}

impl FromDbc for SpnDefinition {
    fn from_entry(entry: Entry) -> Result<Self, String> where Self: Sized {
        match entry {
            Entry::SignalDefinition ( signal_definition ) =>
                Ok(signal_definition.into()),
            Entry::SignalDescription ( signal_description ) =>
                Ok(signal_description.into()),
            Entry::SignalAttribute ( signal_attribute ) =>
                Ok(signal_attribute.into()),
            _ => Err("Could not map entry to SpnDefinition.".to_string())
        }
    }

    fn merge_entry(&mut self, entry: Entry) -> Result<(), String> {
        match entry {
            Entry::SignalDefinition ( SignalDefinition { name, start_bit, bit_len, little_endian, signed, scale, offset, min_value, max_value, units, .. }) => {
                self.name = name; self.start_bit = start_bit; self.bit_len = bit_len; self.little_endian = little_endian; self.signed = signed; self.scale = scale; self.offset = offset; self.min_value = min_value; self.units = units;
                Ok(())
            },
            Entry::SignalDescription ( SignalDescription {id, signal_name, description }) => {
                self.name = signal_name; self.id = id; self.description = description;
                Ok(())
            },
            Entry::SignalAttribute ( SignalAttribute { name, id, signal_name, value }) => {
                self.name = signal_name; self.id = id; self.number = value.parse().unwrap();
                Ok(())
            }
            _ => Err("Could not merge entry with SpnDefinition.".to_string())
        }
    }

}

impl From<SignalDefinition> for SpnDefinition {
    fn from(SignalDefinition { name, start_bit, bit_len, little_endian, signed, scale, offset, min_value, max_value, units, .. }: SignalDefinition) -> Self {
        SpnDefinition::new(name, 0, "".to_string(), "".to_string(), start_bit, bit_len, little_endian, signed, scale, offset, min_value, max_value, units)
    }
}
impl From<SignalDescription> for SpnDefinition {
    fn from(SignalDescription { id, signal_name, description }: SignalDescription) -> Self {
        SpnDefinition::new(signal_name, 0, id, description, 0, 0, true, false, 0.0, 0.0, 0.0, 0.0, "".to_string() )
    }
}
impl From<SignalAttribute> for SpnDefinition {
    fn from(SignalAttribute { name, id, signal_name, value }: SignalAttribute) -> Self {
        SpnDefinition::new(signal_name, value.parse().unwrap(), id, "".to_string(), 0, 0, true, false, 0.0, 0.0, 0.0, 0.0, "".to_string() )
    }
}

#[cfg(test)]
mod tests {
    //extern crate test;

    #[cfg(feature = "use-socketcan")]
    extern crate socketcan;

    use pgn::*;
    use dbc::*;
    //use test::Bencher;

    #[cfg(feature = "use-socketcan")]
    use socketcan::CANFrame;

    lazy_static!{
    static ref PGNLIB: PgnLibrary = PgnLibrary::default();

    static ref SPNDEF: SpnDefinition =
        SpnDefinition::new("Engine_Speed".to_string(), 190, "2364539904".to_string(),
            "A description for Engine speed.".to_string(),
            24, 16, true, false, 0.125, 0.0, 0.0, 8031.88, "rpm".to_string()
        );

    static ref SPNDEF_BE: SpnDefinition = {
        let mut _spndef = SPNDEF.clone();
        _spndef.little_endian = false;
        _spndef
    };

    static ref MSG: [u8; 8] = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
    static ref MSG_BE: [u8; 8] = [0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11];

    }

    #[cfg(feature = "use-socketcan")]
    lazy_static!{
    static ref FRAME: CANFrame = CANFrame::new(
        0,
        &MSG[..],
        false,
        false
    ).unwrap();
    }

    #[test]
    fn default_pgnlibrary() {
        assert_eq!(
            PGNLIB.pgns.len(),
            0
        );
    }

    // TODO: Compose a dbc file so PGNLIB can be properly tested.
    /*
    #[test]
    fn get_spndefinition() {
        assert_eq!(
            *PGNLIB.get_pgn(0xF004).unwrap().spns.get(&"Engine_Speed".to_string()).unwrap(),
            *SPNDEF
        );
    }
    */

    #[test]
    fn unsupported_entry() {
        let mut pgnlib: PgnLibrary = PgnLibrary::default();
        let unsupported = Entry::Version(Version("Don't care about version entry".to_string()));
        let res = pgnlib.add_entry(unsupported);

        assert!(res.is_err(), "Unsupported entry: Version".to_string());
    }

    #[test]
    fn test_parse_array() {
        assert_eq!(
        SPNDEF.parse_message(&MSG as &[u8; 8]).unwrap(),
        2728.5
        );
        assert_eq!(
        SPNDEF_BE.parse_message(&MSG_BE as &[u8; 8]).unwrap(),
        2728.5
        );
    }

    #[test]
    fn test_parse_message() {
        assert_eq!(
            SPNDEF.parse_message(&MSG[..]).unwrap(),
            2728.5
        );
    }

    #[test]
    fn parse_message_closure() {
        assert_eq!(
            SPNDEF.parser()(&MSG[..]).unwrap(),
            2728.5
        );
    }

    #[cfg(feature = "use-socketcan")]
    #[test]
    fn test_parse_canframe() {
        let frame = CANFrame::new(
            0,
            &MSG[..],
            false,
            false
        ).unwrap();

        assert_eq!(
            SPNDEF.parse_message(&frame).unwrap(),
            2728.5
        );
    }

    #[cfg(feature = "use-socketcan")]
    #[test]
    fn parse_canframe_closure() {
        assert_eq!(
            SPNDEF.parser()(&FRAME as &CANFrame).unwrap(),
            2728.5
        );
    }

}

