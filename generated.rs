fn __internal_main__(argc: usize, argv: &[&str]) -> i32 {}

fn main() -> i32 {
    let args: Vec<String> = std::env::args().collect();
    __internal_main__(args.len(), args.as_slice())
}
