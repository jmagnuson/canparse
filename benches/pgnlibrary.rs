#![feature(test)]

#[macro_use] extern crate lazy_static;
extern crate test;
extern crate canparse;

#[cfg(feature = "use-socketcan")]
extern crate socketcan;

use canparse::pgn::*;
use test::Bencher;

#[cfg(feature = "use-socketcan")]
use socketcan::CANFrame;

lazy_static!{

static ref SPNDEF: SpnDefinition =
    SpnDefinition::new("Engine_Speed".to_string(), 190, "2364539904".to_string(),
        "A description for Engine speed.".to_string(),
        24, 16, true, false, 0.125, 0.0, 0.0, 8031.88, "rpm".to_string()
    );

static ref MSG: [u8; 8] = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];

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

#[bench]
fn bench_parse_array(b: &mut Bencher) {

    b.iter(|| test::black_box(
        SPNDEF.parse_message(&MSG as &[u8; 8]).unwrap()
    ))
}
#[bench]
fn bench_parse_message(b: &mut Bencher) {

    b.iter(|| test::black_box(
        SPNDEF.parse_message(&MSG[..]).unwrap()
    ))
}
#[bench]
fn bench_parse_message_closure(b: &mut Bencher) {
    let parse = SPNDEF.parser();

    b.iter(|| test::black_box(
        parse(&MSG[..]).unwrap()
    ))
}

#[cfg(feature = "use-socketcan")]
#[bench]
fn bench_parse_canframe(b: &mut Bencher) {
    let frame = CANFrame::new(
        0,
        &MSG[..],
        false,
        false
    ).unwrap();

    b.iter(|| test::black_box(
        SPNDEF.parse_message(&frame).unwrap()
    ))
}

#[cfg(feature = "use-socketcan")]
#[bench]
fn bench_parse_canframe_closure(b: &mut Bencher) {
    let parse = SPNDEF.parser();

    b.iter(|| test::black_box(
        parse(&FRAME as &CANFrame).unwrap()
    ))
}

