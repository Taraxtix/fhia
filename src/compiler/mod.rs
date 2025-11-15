// use std::fmt::Display;
//
// use crate::{
//     parser::Parser,
//     Args,
// };
//
// #[allow(dead_code)]
// #[derive(Debug, Clone)]
// enum Register {
//     Rax,
//     Rbx,
//     Rcx,
//     Rdx,
//     Rdi,
//     Rsi,
//     Rbp,
//     Rsp,
//     R8,
//     R9,
//     R10,
//     R11,
//     R12,
//     R13,
//     R14,
//     R15,
// }
//
// impl From<Register> for &'static str {
//     fn from(val: Register) -> Self {
//         match val {
//             Register::Rax => "rax",
//             Register::Rbx => "rbx",
//             Register::Rcx => "rcx",
//             Register::Rdx => "rdx",
//             Register::Rdi => "rdi",
//             Register::Rsi => "rsi",
//             Register::Rbp => "rbp",
//             Register::Rsp => "rsp",
//             Register::R8 => "r8",
//             Register::R9 => "r9",
//             Register::R10 => "r10",
//             Register::R11 => "r11",
//             Register::R12 => "r12",
//             Register::R13 => "r13",
//             Register::R14 => "r14",
//             Register::R15 => "r15",
//         }
//     }
// }
//
// // const STD_FILES: [(&str, &str); 6] = [
// //     ("../../std/io.fhia", include_str!("../../std/io.fhia")),
// //     (
// //         "../../std/option.fhia",
// //         include_str!("../../std/option.fhia"),
// //     ),
// //     ("../../std/panic.fhia", include_str!("../../std/panic.fhia")),
// //     (
// //         "../../std/process.fhia",
// //         include_str!("../../std/process.fhia"),
// //     ),
// //     (
// //         "../../std/result.fhia",
// //         include_str!("../../std/result.fhia"),
// //     ),
// //     ("../../std/str.fhia", include_str!("../../std/str.fhia")),
// // ];
//
// #[allow(dead_code)]
// pub struct Compiler<'a> {
//     generated: String,
//     included_files: Vec<&'a str>,
//     curr_path: String,
//     args: &'a Args,
//     // declarations: HashMap<String, Vec<(Ty, ProgExpr)>>,
// }
//
// impl<'a> Compiler<'a> {
//     pub fn new(args: &'a Args) -> Self {
//         Self {
//             generated: String::new(),
//             included_files: Vec::new(),
//             args,
//             // declarations: HashMap::new(),
//             curr_path: String::new(),
//         }
//     }
//
//     // fn add_std(&mut self, args: &Args) {
//     //     self.generated
//     //         .push_str(include_str!("../../static/primitives.h"));
//
//     //     for (path, source) in STD_FILES {
//     //         if !self.included_files.contains(&path) {
//     //             self.add(Parser::parse_user_program(
//     //                 Lexer::from_filename_and_source(path, source),
//     //                 args,
//     //             ));
//     //         }
//     //     }
//
//     //     self.generated
//     //         .push_str(include_str!("../../static/start.c"));
//     // }
//
//     #[allow(dead_code)]
//     fn add(&mut self, parser: Parser) {
//         _ = parser;
//         let _exprs = parser.collect::<Vec<_>>();
//         self.not_implemented_yet("add another parser")
//     }
//
//     #[allow(unreachable_code)]
//     pub fn compile(&mut self, program: Program) {
//         if !self.args.no_std {
//             self.not_implemented_yet("no std");
//         }
//
//         self.curr_path = program.path.clone();
//
//         self.generated
//             .push_str("format ELF64\nsection \".text\" executable");
//
//         todo!();
//
//         //DEBUG
//         for (name, variants) in &self.declarations {
//             for (ty, expr) in variants {
//                 println!("Declaration: {name}: {ty} =\n {expr}");
//             }
//         }
//
//         let (_, _main_function) = match self.declarations.remove("main") {
//             None => {
//                 self.report_error(
//                     &program.prog_exprs[0],
//                     "No 'main' function found in the program",
//                 );
//             }
//             Some(mut variants) => variants.remove(0),
//         };
//
//         let mut file = OpenOptions::new()
//             .write(true)
//             .create(true)
//             .truncate(true)
//             .open(&self.args.output)
//             .expect("Failed to create output file");
//
//         file.write_all(self.generated.as_bytes())
//             .expect("Failed to write to output file");
//     }
//
//     fn report_error(&self, expr: &ProgExpr, msg: impl Display) -> ! {
//         eprintln!(
//             "[ERROR]: {}:{}: Compiling Error: {}",
//             expr.path(),
//             expr.span(),
//             msg
//         );
//         std::process::exit(1);
//     }
//
//     fn compile_lit(&self, expr: Expr, _output: bool) {
//         match expr.kind {
//             ExprKind::U32(_) => self.not_implemented_yet("Compile_lit(U32)"),
//             ExprKind::U64(_) => self.not_implemented_yet("Compile_lit(U64)"),
//             ExprKind::U128(_) => self.not_implemented_yet("Compile_lit(U128)"),
//             ExprKind::F64(_) => self.not_implemented_yet("Compile_lit(F64)"),
//             ExprKind::Bool(_) => self.not_implemented_yet("Compile_lit(Bool)"),
//             ExprKind::Str(_) => self.not_implemented_yet("Compile_lit(Str)"),
//             ExprKind::Char(_) => self.not_implemented_yet("Compile_lit(Char)"),
//             ExprKind::Decla { .. }
//             | ExprKind::Ident(_)
//             | ExprKind::Unit(_)
//             | ExprKind::Sequence { .. }
//             | ExprKind::BinOp { .. }
//             | ExprKind::Application(_, _)
//             | ExprKind::Paren(_) => self.not_implemented_yet("Compile_lit(Non-literal)"),
//         }
//     }
//
//     fn not_implemented_yet(&self, feature: &str) -> ! {
//         for decla in &self.declarations {
//             println!("Declaration: {:?} => {:?}", decla.0, decla.1);
//         }
//
//         let mut file = OpenOptions::new()
//             .write(true)
//             .create(true)
//             .truncate(true)
//             .open(&self.args.output)
//             .expect("Failed to create output file");
//
//         file.write_all(self.generated.as_bytes())
//             .expect("Failed to write to output file");
//         panic!("Compiler feature '{feature}' not implemented yet");
//     }
// }
