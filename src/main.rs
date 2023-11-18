use clap::Parser;
use codespan_reporting::{
    diagnostic::Diagnostic,
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use hexagn_two::typechk;

#[derive(Parser)]
struct Args {
    #[arg()]
    input_file: String,
    #[arg(short, long, default_value_t = String::from("out.s"))]
    output_file: String,
}

fn main() {
    let args = Args::parse();
    let mut files = SimpleFiles::new();
    let code = std::fs::read_to_string(&args.input_file).unwrap();
    files.add(&args.input_file, &code);
    let (ast, diags) = hexagn_two::ast::parse(&code, 0);
    println!("{:#?}", ast);
    print_diags(diags, files.clone());

    typechk::typecheck(&ast).unwrap_or_else(|err| {
        print_diags(vec![err], files);
        std::process::exit(1);
    });
}

fn print_diags(diags: Vec<Diagnostic<usize>>, file_map: SimpleFiles<&String, &String>) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    for diag in diags {
        term::emit(&mut writer.lock(), &config, &file_map, &diag).unwrap();
    }
}
