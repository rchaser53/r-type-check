extern crate regex;
extern crate clap;
use crate::r_type_check::compile;

use regex::Regex;
use clap::{App, Arg};
use r_type_check;
use std::{fs, str};

fn main() {
    let matches = App::new("r-type-check")
        .version("0.0.1")
        .author("rchaser53 <tayoshizawa29@gmail.com>")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();
    let input_str = if let Some(input) = matches.value_of("INPUT") {
        let re = Regex::new(r"\.rtc$").unwrap();
        if re.is_match(input) {
            read_file(input)
        } else {
            println!("file extension should be .rtc");
            exit(1);
        }
    } else {
        panic!("should input file name")
    };
    match compile(&input_str) {
        Ok(_) => {}
        Err(err) => println!("{}", err),
    }
}

fn read_file<T: AsRef<str>>(input_path: T) -> String
where
    T: std::convert::AsRef<std::path::Path> + std::fmt::Display,
{
    match fs::read_to_string(&input_path) {
        Ok(result) => result,
        Err(err) => panic!("{}", err),
    }
}
