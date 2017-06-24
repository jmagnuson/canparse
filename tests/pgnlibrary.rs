
extern crate canparse;

use canparse::{PgnLibrary, SpnDefinition, Entry, ParseMessage};
use std::str::FromStr;
use std::collections::HashMap;
use std::io::BufRead;

#[test]
fn pgnlib_build_parse() {
    let mut lib = PgnLibrary::new( HashMap::default() );

    let br = include_bytes!("./data/sample.dbc");

    // Parse db lines into PgnLibrary
    for l in br.lines() {
        let line = l.unwrap();
        if let Some(entry) = Entry::from_str(line.as_str()).ok() {
            lib.add_entry(entry).ok();
        }
    }

    // Pull signal definition for engine speed
    let enginespeed_def: &SpnDefinition = lib
        .get_spn(&"Engine_Speed".to_string()).unwrap();

    // Parse frame containing engine speed
    let msg: [u8; 8] = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
    let engine_speed: f32 = enginespeed_def.parse_message(&msg).unwrap();

    assert_eq!(engine_speed, 2728.5);
}

