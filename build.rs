use std::{env, path::Path, fs};
use regex::Regex;

const ENUM_START: &str = "/// BEAM \"generic opcodes\" generated at compile time
#[derive(Clone, Copy, PartialEq, Eq, FromRepr, Debug)]
pub enum Opcode {
";
const IMPL_START: &str = "impl Opcode {
    pub const fn arity(self) -> usize {
        match self {
";

fn snake_to_pascal(snake: &str) -> String {
    snake.split('_')
        .map(|p| {
            let (first, rest) = p.split_at(1);
            first.to_uppercase() + rest
        })
        .collect::<Vec<String>>()
        .join("")
}

fn main() {
    // generate genop.rs from genop.tab at compile time
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("genop.rs");

    // parse input file
    let insn_regex = Regex::new(r"^([0-9]+):\s*([a-z0-9_]+)/([0-9]+)").unwrap();
    let source = fs::read_to_string(Path::new("src/vm/genop.tab")).unwrap();
    let specifications: Vec<[&str; 3]> = source
        .split('\n')
        .filter(|l| !l.starts_with('#'))
        .map(|l| insn_regex.captures_iter(l).map(|c| c.extract()).next())
        .filter_map(|opt| opt.map(|t| t.1))
        .collect();

    let mut enum_str = ENUM_START.to_owned();
    let mut impl_str = IMPL_START.to_owned();

    // add enum variants and impl match arms
    for [opcode, name, arity] in specifications {
        let opcode = opcode.parse::<usize>().unwrap();
        let name = snake_to_pascal(name);
        let arity = arity.parse::<usize>().unwrap();
        enum_str.push_str(format!("    {name} = {opcode},\n").as_str());
        impl_str.push_str(format!("            Self::{name} => {arity},\n").as_str());
    }

    enum_str.push_str("}");
    impl_str.push_str("        }\n    }\n}");

    fs::write(dest_path, format!(
"use strum_macros::FromRepr;

{enum_str}

{impl_str}
")).unwrap();

    println!("cargo::rerun-if-changed=src/vm/genop.tab");
    println!("cargo::rerun-if-changed=build.rs");
}
