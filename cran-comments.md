## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

On Windows only, I see 2 additional NOTEs:

1. Found the following sources/headers not terminated with a newline:
   src/rust/target/x86_64-pc-windows-gnu/release/build/tree-sitter-139dcded71f8f57b/out/flag_check.c
   
2. Files which contain pragma(s) suppressing diagnostics:
  'src/vendor/tree-sitter-r/src/parser.c'
  'src/vendor/tree-sitter-r/src/tree_sitter/array.h'
  
The three files mentioned in those NOTEs are from upstream dependencies. While
I have access to them in the vendored dependencies, I cannot modify them without
altering the checksums that were put in place by Cargo (Rust's package manager),
making the installation procedure fail.
  
Therefore, I don't see a way to remove those NOTEs.
