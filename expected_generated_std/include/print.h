#pragma once
#include "primitives.h"

#include "io.h"

unit print_0(_char c);

unit print_1(Ref_str str);

unit print_2(u8 val);
unit print_3(u16 val);
unit print_4(u32 val);
unit print_5(u64 val);

unit print_6(u128 val);

unit print_7(i8 val);
unit print_8(i16 val);
unit print_9(i32 val);
unit print_10(i64 val);

unit print_11(i128 val);

unit print_12(bool val);

unit print_13(f32 val);
unit print_14(f64 val);

unit print_15(f128 val);

unit println_0(Ref_str val);