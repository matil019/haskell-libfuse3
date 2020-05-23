// TODO filename

#include <fuse.h>
#include <string.h>

void
hsfuse3_fuse_args_init(int argc, char **argv, struct fuse_args *out) {
  struct fuse_args args = FUSE_ARGS_INIT(argc, argv);
  memcpy(out, &args, sizeof(struct fuse_args));
}
