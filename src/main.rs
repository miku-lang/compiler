use clap::Parser;

mod c;
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
    c::generate(&args.output, inferrer).unwrap();
}
