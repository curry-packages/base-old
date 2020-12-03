Curry Base Libraries
====================

This repository contains the standard libraries of
Curry distributions like
[PAKCS](https://www.informatik.uni-kiel.de/~pakcs/) or
[KiCS2](https://www-ps.informatik.uni-kiel.de/kics2/).

The libraries in this package can be used in these Curry systems
without the use of the Curry Package Manager.
Since the structure of this base repository is similar to
other Curry packages, the dependency on this package
can be specified in other Curry packages.
This is reasonable to specify the dependency
on base libraries in a Curry package.

*Technical note:*
The file `VERSION` must contain the version number of this package
as specified in the `version` field of `package.json`.
This file is used during the build process of Curry systems,
like PAKCS or KICS2,
in order to avoid reading and parsing the JSON file.
