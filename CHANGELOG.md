## Unreleased

+ Fix bug where extracting an entry with fewer components than are stripped
  causes an error.
+ Fix bug where extracting files with non-ASCII characters would error. (I
  thought char was unsigned by default in C, but CFFI treats it as signed.

## v0.1.0 - September 22, 2021

First release!
