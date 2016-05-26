Checking Equivalence of Nondeterministic Finite Automata
========================================================

This project contains the re-implementations of the algorithms in Haskell, as
well as the technical report that can be generated out of it, in case somebody
wants to read it.

Building
--------

The project uses the `stack` infrastructure which ensures that there is a stable
set of packages available. Also, it ensures that there is a sandbox, ensuring that
globally installed packages do not disturb this project.

To build the code install `stack` first. For that, go to
()[the install instructions] and follow them. Then run

    $ cd NfaExperiments
    $ stack setup
    $ stack build

to get the right compiler version for the project and to build it. Stack will
download all the necessary packages and build them inside the sandbox.

All of this can also done by using the provided Makefile by just running `make` or `make build`.

Technical Report
----------------

The technical report is contained in the `doc` subirectory. Type `make` there
or `make doc` in the project directory to build it.

The build process uses the program `lhs2tex` on all Literate Haskell
source files in the `src` directory and below. Then they are combined by the
main file `doc/report.tex`.

`latex2hs` is installed into the sandbox if not in $PATH on your machine.
In any case the `lhs2tex` tool requires the CTAN packages `polytable` and `lazylist`.
If they are not present, or not recent enough, the installer of lhs2TeX will
try to install its own version of them into the system's TeX distribution,
which will most likely fail unless `make` is called with `sudo` rights.

The places where `lhs2TeX` will look for these files (`polytable.sty` and
`lazylist.sty`) can be looked up in the `lhs2TeX` documentation.
