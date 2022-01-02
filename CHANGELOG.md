## Unreleased

+ All slots on ENTRY now have a default value. Use NIL for "not provided"
  instead of keeping the slot unbound.

## v0.1.1 - September 23, 2021

+ Fix bug where extracting an entry with fewer components than are stripped
  causes an error.
+ Fix bug where extracting files with non-ASCII characters would error. (I
  thought char was unsigned by default in C, but CFFI treats it as signed.
+ Fix bug reading archives with a PAX-GLOBAL-ATTRIBUTES-ENTRY. Currently, just
  ignore it.

## v0.1.0 - September 22, 2021

First release!
