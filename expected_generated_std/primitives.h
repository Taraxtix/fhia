#pragma once

#include <inttypes.h>

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef __int128_t i128;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef __uint128_t u128;

typedef float f32;
typedef double f64;
typedef long double f128;

typedef _Bool bool;

typedef __SIZE_TYPE__ size;

typedef void never;
typedef void unit;

typedef unsigned char _char;

typedef u8 *str;
typedef struct Ref_str {
  size len;
  str data;
} Ref_str;

typedef struct Ref_Slice_Ref_str {
  size len;
  Ref_str *data;
} Ref_Slice_Ref_str;