# libfuse3: A Haskell binding for libfuse-3.x

## Known issues and limitations

- Can't stop the process and unmount with signals. You have to use `fusermount -u` to unmount.
- Not all Haskell-friendly bindings to the FUSE operations are implemented yet, including but not limited to:
  - `struct fuse_conn_info`. The availability of filesystem capabilities such as `FUSE_CAP_HANDLE_KILLPRIV` can't be checked.
  - Setting the fields of `struct fuse_file_info` from the certain fuse operations.
- Look for the `TODO` comments in the source tree for more specific topics.

If you are able to implement any of these, that would be very appreciated! Please open a PR to contribute.

## Related works

- [libfuse][libfuse]: The reference implementation, to which this package binds
- [HFuse][HFuse]: The bindings for libfuse-2.x
- [fuse-rs][fuse-rs]: The Rust implementation of FUSE. Unlike this package, `fuse-rs` implements the FUSE protocol itself (i.e. replaces `libfuse`). See [its README](https://github.com/zargony/fuse-rs) for overview.

[libfuse]: https://github.com/libfuse/libfuse
[HFuse]: https://github.com/m15k/hfuse
[fuse-rs]: https://github.com/zargony/fuse-rs
