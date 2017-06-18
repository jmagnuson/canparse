//! A CAN signal and definition parser, written in Rust.

#![allow(non_snake_case, non_camel_case_types)]
#![allow(unused_variables, dead_code)]
#![allow(unused_imports, unused_mut)]
#![allow(plugin_as_library)]
//#![feature(test)]

#![crate_name = "canparse"]

#[macro_use] extern crate enum_primitive;
#[macro_use] extern crate lazy_static;
extern crate num;
extern crate regex;
extern crate rustc_serialize;
extern crate byteorder;

//extern crate test;

#[cfg(feature = "use-socketcan")]
extern crate socketcan;

pub mod dbc;
pub mod pgn;

pub use dbc::{
    Entry,
    Version,
    BusConfiguration,
    MessageDefinition,
    MessageDescription,
    MessageAttribute,
    SignalDefinition,
    SignalDescription,
    SignalAttribute
};

pub use pgn::{
    PgnLibrary,
    SpnDefinition,
    PgnDefinition,
    FromDbc,
    ParseMessage
};

