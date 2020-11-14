Curry Libraries
===============

This repository contains the standard libraries of
Curry distributions like PAKCS or KiCS2.

The structure of the repository is similar to a Curry package
so that the dependency on this package can be specified
in other Curry packages.

*Important note:*
The file `VERSION` must contain the version number of this package
as specified in the `version` field of `package.json`.
This file is used during the build process of Curry systems
like PAKCS or KICS2 in order to avoid reading and parsing the
JSON file.
