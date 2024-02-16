use anyhow::Context;
use rust::{ast::Program, lexer, parser, token::Token};
use std::io;
use strum::VariantNames;

// todo: We can use strum to give us the discriminate names for auto complete, but does lack auto
// creating a brace :think:
// fn get_names() {
//     Token::VARIANTS
// }
fn parse(src: &String) -> Result<Program, String> {
    let lexer = lexer::Lexer::new(&src);
    let mut parser = parser::Parser::new(lexer);
    match parser.parse_program() {
        Ok(success) => Ok(success),
        Err(err) => Result::Err(err.to_string()),
    }
}

fn main() -> io::Result<()> {
    let stdin = io::stdin(); // We get `Stdin` here.
    let mut input = String::new();
    while input != "exit".to_string() {
        print!(">");
        stdin.read_line(&mut input)?;
        match parse(&input) {
            Ok(parsed) => {
                println!("{:?}", parsed);
            }
            Err(error) => println!("parsing error\n{error}"),
        }
        input.clear();
    }
    println!("bye bye!");
    Ok(())
}
