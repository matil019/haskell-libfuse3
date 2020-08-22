AC_INIT([Haskell libfuse3 package], [0.1.0.0])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([libfuse3.cabal])

AC_CONFIG_HEADERS([include/config.h])

PKG_CHECK_MODULES([FUSE3], [fuse3 >= 3])
AC_SUBST([FUSE3_CFLAGS])

# The FUSE API version we use. Also output it to *.buildinfo (see AC_CONFIG_FILES) so that it is reused by cabal
FUSEVER=31
AC_SUBST([FUSEVER])

# Check for version-dependent fields
old_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -DFUSE_USE_VERSION=$FUSEVER $FUSE3_CFLAGS"
AC_CHECK_MEMBER([struct fuse_operations.lseek],,,[[#include <fuse.h>]])
CFLAGS="$old_CFLAGS"

if test "x$ac_cv_member_struct_fuse_operations_lseek" = xyes; then
  AC_DEFINE([FUSE_HAS_LSEEK], [1], [struct fuse_operations.lseek exists.])
fi

AC_CONFIG_FILES([libfuse3.buildinfo])
AC_OUTPUT
