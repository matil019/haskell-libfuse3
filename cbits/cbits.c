#include <fuse.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

// Wraps a call to `fuse_new`, which is a function macro since an unknown version.
//
// We can't use CApiFFI because fuse requires `FUSE_USE_VERSION` macro to be defined. Although we do
// define it with a `-DFUSE_USE_VERSION=...` option using `libfuse3.buildinfo.in` and `configure.ac`,
// we couldn't find out how for CApiFFI to respect the option.
struct fuse * hs_libfuse3_fuse_new(struct fuse_args *args, const struct fuse_operations *op, size_t op_size, void *private_data) {
  return fuse_new(args, op, op_size, private_data);
}
