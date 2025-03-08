#pragma once
#include "primitives.h"

#include "print.h"
#include "process.h"
#include "str.h"

inline never panic_0(Ref_str msg) {
  print_1((Ref_str){.data = (str) "[PANIC]: ", .len = 9});
  print_1((Ref_str){.data = (str)__FILE__, .len = strlen((CString)__FILE__)});
  print_1((Ref_str){.data = (str) ":", .len = 1});
  print_4(__LINE__);
  print_1((Ref_str){.data = (str) ": ", .len = 2});
  println_0(msg);
  exit(1);
}