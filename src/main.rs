use rust::object::*;
use rust::{ast::Program, eval::Eval, lexer, parser};
use std::io::{self, Write};

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
    let mut stdout = io::stdout(); // We get `Stdin` here.
    let mut input = String::new();
    while input != "exit".to_string() {
        print!(">> ");
        let _ = stdout.flush()?;
        stdin.read_line(&mut input)?;
        match parse(&input) {
            Ok(parsed) => {
                // println!("{parsed}");
                println!("{}", parsed.eval())
            }
            Err(error) => println!("parsing error\n{error}"),
        }
        input.clear();
    }
    println!("bye bye!");
    Ok(())
}
