#[macro_use]
extern crate lazy_static;
extern crate canparse;

extern crate socketcan;

use canparse::pgn::*;
use criterion::{black_box, criterion_group, criterion_main, Criterion as Bencher};

use socketcan::CANFrame;

lazy_static! {
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
    static ref MSG: [u8; 8] = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
}

lazy_static! {
    static ref FRAME: CANFrame = CANFrame::new(0, &MSG[..], false, false).unwrap();
}

fn bench_parse_canframe(b: &mut Bencher) {
    let frame = CANFrame::new(0, &MSG[..], false, false).unwrap();

    b.bench_function("bench_parse_canframe", move |b| {
        b.iter(|| black_box(SPNDEF.parse_message(&frame).unwrap()))
    });
}

fn bench_parse_canframe_closure(b: &mut Bencher) {
    let parse = SPNDEF.parser();

    b.bench_function("bench_parse_canframe_closure", move |b| {
        b.iter(|| black_box(parse(&FRAME as &CANFrame).unwrap()))
    });
}

criterion_group!(benches, bench_parse_canframe, bench_parse_canframe_closure,);

criterion_main!(benches);
