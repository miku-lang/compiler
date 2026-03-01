#![allow(unused)]
use std::path::Path;

use clap::Parser;

mod c;
mod java;
mod parser;

#[derive(Debug, Parser)]
struct Args {
    filename: String,

    #[arg(short, long)]
    output: String,
}

fn main() {
    let args = Args::parse();

    let code = parser::parse(&args.filename);
    let mut inferrer = parser::inferrer::Inferrer::new(code);
    inferrer.infer_main();

    if args.output.ends_with(".c") {
        c::generate(&args.output, inferrer).unwrap();
    } else if args.output.ends_with(".java") {
        java::generate(&args.output, inferrer).unwrap();
    } else {
        panic!(
            "Unknown file extension: {:?}",
            Path::new(&args.output).extension()
        );
    }
}
