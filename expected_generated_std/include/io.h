#pragma once
#include "primitives.h"

#include "str.h"

typedef i32 FileDescriptor;

extern FileDescriptor stdin;
extern FileDescriptor stdout;
extern FileDescriptor stderr;

extern size write(FileDescriptor fd, const CString buf, size len);

size write_0(FileDescriptor fd, Ref_str buf);

size putc_0(FileDescriptor fd, const _char c);
