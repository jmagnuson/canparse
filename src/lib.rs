//! A CAN signal and definition parser, written in Rust.
//!
//! The goal of canparse is to provide a means of converting CAN frames into
//! pre-defined signals, via CANdb definition input ([DBC](https://vector.com/vi_candblib_en.html)).
//! One common application is the [J1939](https://en.wikipedia.org/wiki/SAE_J1939)
//! spec, which defines a set of common parameters for heavy-duty trucks and other vehicles.
//! `PgnLibrary` is also included as an application of DBC, to give first-class support for
//! the PGN/SPN schema.
//!
//! ## Example
//!
//! For a predefined DBC file, a simple program which utilizes `PgnLibrary` can be
//! implemented as folows:
//!
//! ```rust,no_run
//! extern crate canparse;
//!
//! use canparse::pgn::{PgnLibrary, SpnDefinition, ParseMessage};
//!
//! fn main() {
//!
//!     // Parse dbc file into PgnLibrary
//!     let lib = PgnLibrary::from_dbc_file("./j1939.dbc").unwrap();
//!
//!     // Pull signal definition for engine speed
//!     let enginespeed_def: &SpnDefinition = lib
//!         .get_spn(&"Engine_Speed").unwrap();
//!
//!     // Parse frame containing engine speed
//!     let msg: [u8; 8] = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
//!     let engine_speed: f32 = enginespeed_def.parse_message(&msg).unwrap();
//!
//!     println!("Engine speed: {}", engine_speed);
//! }
//! ```

#![cfg_attr(feature = "cargo-clippy", allow(redundant_field_names))]

#![allow(non_snake_case, non_camel_case_types)]
#![allow(unused_variables, dead_code)]
#![allow(unused_imports, unused_mut)]
#![allow(plugin_as_library)]

#![crate_name = "canparse"]

#[macro_use] extern crate enum_primitive;
#[macro_use] extern crate lazy_static;
extern crate num;
extern crate regex;
extern crate rustc_serialize;
extern crate byteorder;

#[cfg(feature = "use-socketcan")]
extern crate socketcan;

pub mod dbc;
pub mod pgn;
