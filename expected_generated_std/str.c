#include "str.h"

Ref_str from_0(CString c_str) {
  return (Ref_str){.len = strlen(c_str), .data = c_str};
}