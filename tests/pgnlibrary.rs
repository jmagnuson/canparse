extern crate canparse;

use approx::assert_relative_eq;
use canparse::pgn::{ParseMessage, PgnLibrary, SpnDefinition};

#[test]
fn pgnlib_build_parse() {
    let lib = PgnLibrary::from_dbc_file("./tests/data/sample.dbc").unwrap();

    // Pull signal definition for engine speed
    let enginespeed_def: &SpnDefinition = dbg!(lib.get_spn("Engine_Speed").unwrap());

    /*{
    // Parse frame containing engine speed
    let msg: [u8; 8] = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
    let engine_speed: f32 = enginespeed_def.parse_message(&msg).unwrap();

    assert_relative_eq!(engine_speed, 2728.5);
    }*/

    {
    let msg: [u8; 8] = [0x00, 0x01, 0x00, 0x00, 0xcb, 0xff, 0x02, 0x00];
    let engine_speed: f32 = enginespeed_def.parse_message(&msg).unwrap();

    assert_relative_eq!(engine_speed, -53.0);
    }
}

#[test]
fn pgnlib_from_dbc_file() {
    let lib = PgnLibrary::from_dbc_file("./tests/data/sample.dbc");
    assert!(lib.is_ok(), "PgnLibrary should have built successfully.");

    let lib_fail = PgnLibrary::from_dbc_file("./tests/data/sample.dbc.fail");
    assert_eq!(
        lib_fail.map_err(|e| e.kind()),
        Err(std::io::ErrorKind::NotFound)
    )
}
