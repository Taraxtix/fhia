#include "../include/print.h"

unit print_0(_char c) { putc_0(stdout, c); }

unit print_1(Ref_str str) { write_0(stdout, str); }

unit print_2(u8 val) { return print_6((u128)val); }
unit print_3(u16 val) { return print_6((u128)val); }
unit print_4(u32 val) { return print_6((u128)val); }
unit print_5(u64 val) { return print_6((u128)val); }

unit print_6__aux(u128 val) {
  if (val == 0) {
    return;
  } else {
    print_6__aux(val / 10);
    return print_0((_char)(val % 10) + '0');
  }
}
unit print_6(u128 val) {
  if (val == 0) {
    return print_0('0');
  } else {
    return print_6__aux(val);
  }
}

unit print_7(i8 val) { return print_11((i128)val); }
unit print_8(i16 val) { return print_11((i128)val); }
unit print_9(i32 val) { return print_11((i128)val); }
unit print_10(i64 val) { return print_11((i128)val); }

unit print_11(i128 val) {
  if (val >= 0) {
    return print_6((u128)val);
  } else {
    print_0('-');
    return print_6((u128)-val);
  }
}

unit print_12(bool val) {
  return print_1((val ? ((Ref_str){.data = (str) "true", .len = 4})
                      : ((Ref_str){.data = (str) "false", .len = 5})));
}

unit print_13(f32 val) { return print_15((f128)val); }
unit print_14(f64 val) { return print_15((f128)val); }

unit print_15__aux(f128 val) {
  if (val == 0.) {
    return;
  } else {
    f128 new_val = val * 10.;
    print_15__aux(new_val - (u128)new_val);
    print_6((u128)new_val);
  }
}

unit print_15(f128 val) {
  print_11((i128)val);
  print_0('.');
  f128 new_val = val - (val < 0 ? (-val * 2) : (val));
  print_15__aux(new_val);
}

unit println_0(Ref_str val) {
  print_1(val);
  return print_0('\n');
}