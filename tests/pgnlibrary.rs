
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
        if let Ok(entry) = Entry::from_str(line.as_str()) {
            lib.add_entry(entry).ok();
        }
    }

    // Pull signal definition for engine speed
    let enginespeed_def: &SpnDefinition = lib
        .get_spn(&"Engine_Speed".to_string()).unwrap();

    // Parse frame containing engine speed
    let msg: [u8; 8] = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
    let engine_speed: f32 = enginespeed_def.parse_message(&msg).unwrap();

    assert!(engine_speed - 2728.5 < std::f32::EPSILON);
}

#[test]
fn pgnlib_from_dbc_file() {

    let lib = PgnLibrary::from_dbc_file("./tests/data/sample.dbc");
    assert!(lib.is_ok(), "PgnLibrary should have built successfully.");

    let lib_fail = PgnLibrary::from_dbc_file("./tests/data/sample.dbc.fail");
    assert_eq!(lib_fail.map_err(|e| e.kind()), Err(std::io::ErrorKind::NotFound))
}
