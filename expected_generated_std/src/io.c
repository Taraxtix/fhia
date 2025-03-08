#include "../include/io.h"

size write_0(FileDescriptor fd, Ref_str buf) {
  return write(fd, buf.data, buf.len);
}

size putc_0(FileDescriptor fd, const _char c) { return write(fd, &c, 1); }