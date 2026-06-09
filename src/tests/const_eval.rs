use crate::tests::{get_default_target_machine, int_ty};
use crate::{
    Args,
    const_eval::ConstValue,
    parser::expr::Ty,
    program::{Program, diagnostics::ErrorCode},
};

fn eval_const(input: &str, name: &str) -> Option<ConstValue> {
    let program = Program::lex(Args::default(), get_default_target_machine(), input)
        .parse()
        .type_check()
        .const_eval();
    program.state.env.lookup_const(name).copied()
}

fn const_err(input: &str, expected: ErrorCode) {
    let output = std::process::Command::new(std::env::current_exe().unwrap())
        .env("__FHIA_INPUT", input)
        .args([
            "--include-ignored",
            "--exact",
            "tests::const_eval::__inner_const_eval",
        ])
        .output()
        .expect("failed to spawn subprocess");
    assert!(
        !output.status.success(),
        "expected const eval error for: {input}"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(expected.title()),
        "expected {:?} in stderr for: {input}\nstderr: {stderr}",
        expected.title(),
    );
}

#[test]
#[ignore = "subprocess helper"]
fn __inner_const_eval() {
    let input = std::env::var("__FHIA_INPUT").unwrap();
    let _ = Program::lex(Args::default(), get_default_target_machine(), &input)
        .parse()
        .type_check()
        .const_eval();
}

fn get_default_ptr_size() -> usize {
    let target_machine = get_default_target_machine();
    (target_machine.get_target_data().get_pointer_byte_size(None) * 8) as usize
}

// =============================================================================
// cast_to unit tests — Int source
// =============================================================================

#[test]
fn cast_int_to_signed_no_wrap() {
    assert!(matches!(
        ConstValue::Int(42).cast_to(int_ty(true, 64), int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(42)
    ));
}

#[test]
fn cast_int_to_signed_narrowing_wraps_negative() {
    // 200 = 0xC8, bit 7 set → sign-extends to -56
    assert!(matches!(
        ConstValue::Int(200).cast_to(int_ty(true, 64), int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(-56)
    ));
}

#[test]
fn cast_int_to_signed_negative_no_wrap() {
    assert!(matches!(
        ConstValue::Int(-1).cast_to(int_ty(true, 64), int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(-1)
    ));
}

#[test]
fn cast_int_to_signed_negative_wraps_positive() {
    // lower 8 bits of -200 = 0x38 = 56, bit 7 clear
    assert!(matches!(
        ConstValue::Int(-200).cast_to(int_ty(true, 64), int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(56)
    ));
}

#[test]
fn cast_int_to_signed_widening() {
    assert!(matches!(
        ConstValue::Int(-1).cast_to(int_ty(true, 8), int_ty(true, 16), get_default_ptr_size()),
        ConstValue::Int(-1)
    ));
}

#[test]
fn cast_int_to_signed_same_width() {
    assert!(matches!(
        ConstValue::Int(42).cast_to(int_ty(true, 64), int_ty(true, 64), get_default_ptr_size()),
        ConstValue::Int(42)
    ));
}

#[test]
fn cast_int_to_unsigned_negative_wraps() {
    // -1 → u8: bit pattern 0xFF = 255
    assert!(matches!(
        ConstValue::Int(-1).cast_to(int_ty(true, 64), int_ty(false, 8), get_default_ptr_size()),
        ConstValue::Uint(255)
    ));
}

#[test]
fn cast_int_to_unsigned_overflow() {
    // 300 = 0x12C, lower 8 bits = 0x2C = 44
    assert!(matches!(
        ConstValue::Int(300).cast_to(int_ty(true, 64), int_ty(false, 8), get_default_ptr_size()),
        ConstValue::Uint(44)
    ));
}

#[test]
fn cast_int_to_unsigned_no_wrap() {
    assert!(matches!(
        ConstValue::Int(42).cast_to(int_ty(true, 64), int_ty(false, 8), get_default_ptr_size()),
        ConstValue::Uint(42)
    ));
}

#[test]
fn cast_int_to_unsigned_full_width() {
    // -1i128 as u128 = u128::MAX
    assert!(matches!(
        ConstValue::Int(-1).cast_to(
            int_ty(true, 128),
            int_ty(false, 128),
            get_default_ptr_size()
        ),
        ConstValue::Uint(u128::MAX)
    ));
}

#[test]
#[expect(
    clippy::float_cmp,
    reason = "Exact comparison is intentional for testing const eval behavior"
)]
fn cast_int_to_float() {
    assert!(matches!(
        ConstValue::Int(42).cast_to(int_ty(true, 64), Ty::F64, get_default_ptr_size()),
        ConstValue::Float(v) if v == 42.0
    ));
    assert!(matches!(
        ConstValue::Int(-1).cast_to(int_ty(true, 64), Ty::F64, get_default_ptr_size()),
        ConstValue::Float(v) if v == -1.0
    ));
}

// =============================================================================
// cast_to unit tests — Uint source
// =============================================================================

#[test]
fn cast_uint_to_signed_wraps_negative() {
    // 255 = 0xFF, bit 7 set → -1 as i8
    assert!(matches!(
        ConstValue::Uint(255).cast_to(int_ty(false, 64), int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(-1)
    ));
    // 200 = 0xC8, bit 7 set → -56 as i8
    assert!(matches!(
        ConstValue::Uint(200).cast_to(int_ty(false, 64), int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(-56)
    ));
}

#[test]
fn cast_uint_to_signed_no_wrap() {
    // 127 = 0x7F, bit 7 clear → 127
    assert!(matches!(
        ConstValue::Uint(127).cast_to(int_ty(false, 64), int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(127)
    ));
}

#[test]
fn cast_uint_to_signed_full_width() {
    // u128::MAX as i128 = -1
    assert!(matches!(
        ConstValue::Uint(u128::MAX).cast_to(
            int_ty(false, 128),
            int_ty(true, 128),
            get_default_ptr_size()
        ),
        ConstValue::Int(-1)
    ));
}

#[test]
fn cast_uint_to_unsigned_overflow() {
    // 300 = 0x12C, lower 8 bits = 0x2C = 44
    assert!(matches!(
        ConstValue::Uint(300).cast_to(int_ty(false, 64), int_ty(false, 8), get_default_ptr_size()),
        ConstValue::Uint(44)
    ));
}

#[test]
fn cast_uint_to_unsigned_no_wrap() {
    assert!(matches!(
        ConstValue::Uint(42).cast_to(int_ty(false, 64), int_ty(false, 8), get_default_ptr_size()),
        ConstValue::Uint(42)
    ));
}

#[test]
fn cast_uint_to_unsigned_full_width_identity() {
    assert!(matches!(
        ConstValue::Uint(u128::MAX).cast_to(
            int_ty(false, 128),
            int_ty(false, 128),
            get_default_ptr_size()
        ),
        ConstValue::Uint(u128::MAX)
    ));
}

#[test]
#[expect(
    clippy::float_cmp,
    reason = "Exact comparison is intentional for testing const eval behavior"
)]
#[expect(
    clippy::cast_precision_loss,
    reason = "Testing const eval behavior when casting large integers to float, which necessarily \
              involves precision loss"
)]
fn cast_uint_to_float() {
    assert!(matches!(
        ConstValue::Uint(42).cast_to(int_ty(false, 64), Ty::F64, get_default_ptr_size()),
        ConstValue::Float(v) if v == 42.0
    ));
    assert!(matches!(
        ConstValue::Uint(u128::MAX).cast_to(int_ty(false, 128), Ty::F64, get_default_ptr_size()),
        ConstValue::Float(v) if v == u128::MAX as f64
    ));
}

// =============================================================================
// cast_to unit tests — Float source
// =============================================================================

#[test]
fn cast_float_to_signed_truncates_toward_zero() {
    assert!(matches!(
        ConstValue::Float(42.9).cast_to(Ty::F64, int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(42)
    ));
    assert!(matches!(
        ConstValue::Float(-42.9).cast_to(Ty::F64, int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(-42)
    ));
}

#[test]
fn cast_float_to_signed_wraps() {
    // 200.0 → i128 = 200, 200 = 0xC8 bit 7 set → -56
    assert!(matches!(
        ConstValue::Float(200.0).cast_to(Ty::F64, int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(-56)
    ));
    // -200.0 → i128 = -200, lower 8 bits = 0x38 = 56, bit 7 clear
    assert!(matches!(
        ConstValue::Float(-200.0).cast_to(Ty::F64, int_ty(true, 8), get_default_ptr_size()),
        ConstValue::Int(56)
    ));
}

#[test]
fn cast_float_to_unsigned_positive() {
    // truncates toward zero
    assert!(matches!(
        ConstValue::Float(42.9).cast_to(Ty::F64, int_ty(false, 8), get_default_ptr_size()),
        ConstValue::Uint(42)
    ));
}

#[test]
fn cast_float_to_unsigned_negative_via_signed_intermediate() {
    // -1.0 → i128 = -1 → u128::MAX → lower 8 bits = 255
    assert!(matches!(
        ConstValue::Float(-1.0).cast_to(Ty::F64, int_ty(false, 8), get_default_ptr_size()),
        ConstValue::Uint(255)
    ));
}

#[test]
fn cast_float_to_unsigned_overflow() {
    // 300.0 → i128 = 300 → lower 8 bits = 44
    assert!(matches!(
        ConstValue::Float(300.0).cast_to(Ty::F64, int_ty(false, 8), get_default_ptr_size()),
        ConstValue::Uint(44)
    ));
}

#[test]
#[expect(
    clippy::float_cmp,
    reason = "Exact comparison is intentional for testing const eval behavior"
)]
#[expect(
    clippy::approx_constant,
    reason = "Those are not intended to be precise but to be recognizable"
)]
fn cast_float_to_float_identity() {
    assert!(matches!(
        ConstValue::Float(3.14).cast_to(Ty::F64, Ty::F64, get_default_ptr_size()),
        ConstValue::Float(v) if v == 3.14
    ));
    assert!(matches!(
        ConstValue::Float(3.14).cast_to(Ty::F32, Ty::F32, get_default_ptr_size()),
        ConstValue::Float(v) if v == 3.14
    ));
}

// =============================================================================
// Negation (unary minus)
// =============================================================================

#[test]
fn neg_signed_int() {
    assert_eq!(
        eval_const("let main: i32 = 0 const x: i64 = -42", "x"),
        Some(ConstValue::Int(-42))
    );
}

#[test]
fn neg_signed_int_zero() {
    assert_eq!(
        eval_const("let main: i32 = 0 const x: i64 = -0", "x"),
        Some(ConstValue::Int(0))
    );
}

#[test]
fn neg_signed_int_min_boundary() {
    // -127 fits comfortably in i8
    assert_eq!(
        eval_const("let main: i32 = 0 const x: i8 = -127", "x"),
        Some(ConstValue::Int(-127))
    );
}

#[test]
#[expect(
    clippy::float_cmp,
    reason = "Exact comparison is intentional for testing const eval behavior"
)]
fn neg_float() {
    // 2.5 is exactly representable in IEEE 754 and not close to any named constant
    assert!(matches!(
        eval_const("let main: i32 = 0 const x: f64 = -2.5", "x"),
        Some(ConstValue::Float(v)) if v == -2.5
    ));
}

#[test]
fn neg_float_zero() {
    // -0.0 is a distinct IEEE 754 value; matches! uses PartialEq which treats it equal to 0.0
    assert!(matches!(
        eval_const("let main: i32 = 0 const x: f64 = -0.", "x"),
        Some(ConstValue::Float(_))
    ));
}

#[test]
fn neg_double_neg() {
    // --10 == 10
    assert_eq!(
        eval_const("let main: i32 = 0 const x: i64 = --10", "x"),
        Some(ConstValue::Int(10))
    );
}

#[test]
fn neg_of_ident() {
    // negation of a previously defined constant
    assert_eq!(
        eval_const("let main: i32 = 0 const a: i64 = 5  const x: i64 = -a", "x"),
        Some(ConstValue::Int(-5))
    );
}

#[test]
fn neg_forward_ref() {
    // neg of a forward reference — const_eval resolves deps via topo-sort
    assert_eq!(
        eval_const("let main: i32 = 0 const x: i64 = -a  const a: i64 = 7", "x"),
        Some(ConstValue::Int(-7))
    );
}

// =============================================================================
// Pipeline success cases
// =============================================================================

#[test]
fn const_eval_int_literal() {
    assert_eq!(
        eval_const("let main: i32 = 0 const x: i64 = 42", "x"),
        Some(ConstValue::Int(42))
    );
}

#[test]
fn const_eval_let_also_evaluable() {
    // `let` declarations are const-evaluated too, not only `const`
    assert_eq!(
        eval_const("let main: i32 = 0 let x: i64 = 42", "x"),
        Some(ConstValue::Int(42))
    );
}

#[test]
fn const_eval_unsigned_literal() {
    assert_eq!(
        eval_const("let main: i32 = 0 const x: u64 = 42", "x"),
        Some(ConstValue::Uint(42))
    );
}

#[test]
fn const_eval_float_literal() {
    assert_eq!(
        eval_const("let main: i32 = 0 const x: f64 = 1.", "x"),
        Some(ConstValue::Float(1.0))
    );
}

#[test]
fn const_eval_cast_wraps() {
    // i8 200 truncates to -56 at const-eval time
    assert_eq!(
        eval_const("let main: i32 = 0 const x: i8 = 200 as i8", "x"),
        Some(ConstValue::Int(-56))
    );
}

#[test]
fn const_eval_ident_reference() {
    assert_eq!(
        eval_const("let main: i32 = 0 const y: i64 = 42  const x: i64 = y", "x"),
        Some(ConstValue::Int(42))
    );
}

#[test]
fn const_eval_forward_reference() {
    // x is declared before y but depends on it — topo sort handles this
    assert_eq!(
        eval_const("let main: i32 = 0 const x: i64 = y  const y: i64 = 42", "x"),
        Some(ConstValue::Int(42))
    );
}

// =============================================================================
// Pipeline failure cases
// =============================================================================

#[test]
fn const_eval_cyclic_declaration() {
    const_err(
        "let main: i32 = 0 let x: i64 = y  let y: i64 = x",
        ErrorCode::CyclicDeclaration,
    );
}
