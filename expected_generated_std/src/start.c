#include "../include/primitives.h"
#include "../include/process.h"
#include "../include/str.h"

extern i32 main(Ref_Slice_Ref_str args);

never _start(void) {
  size argc;
  const char *raw_argv;
  __asm__ volatile("mov %0, 8(%%rbp)\n"
                   "mov %1, 16(%%rbp)\n"
                   : "=r"(argc), "=r"(raw_argv)
                   :);

  Ref_Slice_Ref_str args;
  args.len = argc;
  Ref_str argv[argc];

  size len = 0;
  for (size i = 0; i < argc; i++) {
    argv[i] = (Ref_str){.data = (str)raw_argv + len + i,
                        .len = strlen((str)raw_argv + len + i)};
  }
  // const char **argv = (const char **)raw_argv; // TODO: Separate the args

  exit(main(args));
}