Checking Equivalence of Nondeterministic Finite Automata
========================================================

[![CircleCI](https://circleci.com/gh/koflerdavid/nfa-equivalence-code/tree/master.svg?style=svg)](https://circleci.com/gh/koflerdavid/nfa-equivalence-code/tree/master)

This project contains the re-implementations of the algorithms in Haskell, as
well as the technical report that can be generated out of it, for those who
want to read it.


Building
--------

The project uses the `stack` infrastructure which ensures that there is a stable
set of packages available.
Also, it ensures that there is a sandbox, ensuring that globally
installed packages do not disturb this project.

To build the code install `stack` first.
For that, go to
[the install instructions](http://docs.haskellstack.org/en/stable/README/#how-to-install) and follow them.

Check the version with `stack --version`.
If it is `v1.3.0` or `v1.3.2`, either upgrade to another version
(with `stack upgrade`) or downgrade with
`stack upgrade --binary-version 1.2.0` to the previous version.
The reason is that the above versions deactivate line buffering for
stdin, that is, it would be difficult to terminate the input with
`Ctrl-D` or correct mistakes in the current line (with backspace).
See
[Stack exec incorrectly setting no buffering #2884](https://github.com/commercialhaskell/stack/issues/2884)
for details.

Execute

    $ stack setup
    $ stack build

to get the right compiler version for the project and to build it.
Stack will download all the necessary packages and build them inside the sandbox.

All of this can also done by using the provided Makefile by just running `make` or `make build`.


Running the tool
----------------

### Equality checking ###

The tool receives its input in the format used by Bonchi and Pous at
[the web=appendix of their paper](https://perso.ens-lyon.fr/damien.pous/hknt/).

Example for checking equality of two NFAs:

    $ stack exec automata-equivalence nfaequivalence
       x -a-> y
       y -a-> z
       z -a-> x y
       u -a-> w v
       v -a-> w
       w -a-> u
       accept: y v
       check: x = u
       ^D
       Checking equivalence of ["x"] and ["u"]
               ""    { x }    { u }
               "a"   { y }    { w, v }
               "aa"  { z }    { u, w }
       skipped "aaa" { x, y } { u, w, v }
    $ echo $?
       0

With `stack exec automata-equivalence nfaequivalence <filename>` the
input is read from `<filename>`.


### Regular expressions ###

Regular expressions use the following BNF grammar:

    regex            <- regex '|' regex
                      | regex regex
                      | regex postfix-operator
                      | primitiveRegex
                      | '(' regex ')'
    primitiveRegex   <- 'a' | 'b' | ...
                      | "\'a\'" |"\'b\'" | ...
                      | 0 | 1
    postfix-operator <- '*' | '?' | '+'

Single characters can be typed without clothes.
Special characters (like braces, operators, 0, 1) have to be surrounded by single quotes.
The precedence rules are as usual:

    alternative < sequence < zeroOrMore = zeroOrOne = oneOrMore


### Deriving regular expressions ###

Example for turning a regular expression into a DFA by using derivatives:

    $ stack exec regex-derivation -- --withoutSkeleton
      a* (b | c)
      ^D
      <LaTeX snippet for DFA table>

To quickly view the result, the option `--withoutSkeleton` can be
removed to produce a regular LaTeX document.

Example for deriving a regular expression by a word:

    $ stack exec regex-derivation -- regexderivation abc
      a (b | c) (c | de)* f
      ^D
      ('c' | 'd' 'e')* 'f'


### Checking regular expression equivalence ###

Example for checking two regular expression (on consecutive lines) for
equivalence:

    $ stack exec regex-derivation regexequivalence
      a b c*
      a c* b
      "acb"	∅	ε
      "abc"	'c'*	∅

The first two input lines contain the regular expressions to compare.
The program computes the equivalence and, in the case of nonequivalence,
prints out witnesses for that.
Each of those strings is accepted by only one of the regular expression,
but not by the other.
The second and third column contain the derivation of each input regular
expression by that word.


### Webservice ###

The subdirectory `webservice` contains a webservice implemented using the
Snap framework. 
It makes it possible to use the above tools via a REST-style interface.
By default it opens a HTTP server on `0.0.0.0:8000`.
At `/` there is a HTML page that makes it possible to try out the following 
endpoints:

 * `/regex/derivation`: Expects a POST request with content type 
   `application/x-www-form-urlencoded` and two parameters: 
    * `regex`, which is a valid UTF-8 string obeying the aforementioned syntax 
      for regular expressions, 
    * and `word`, a valid UTF-8 string.
    
   If parameters are missing, the regular expression has syntax errors or 
   if UTF-8 decoding errors occur, a `400 Bad Request` status message will
   be returned.
   In that case the response body will be a short UTF-8 text 
   describing the error.
   If derivation is successful, the resulting regular expression is returned.
    
 * `/regex/equivalence`: Expects a POST request with content type 
   `application/x-www-form-urlencoded` and two parameters: `regex1` and `regex2`.
   Both are expected to be valid UTF-8 strings of regular expressions according to 
   the aforementioned syntax.
   If parameters are missing or UTF-8 decoding errors or syntax errors occur,
   a `400 Bad Request` status message will be returned.
   The response body will be a short UTF-8 string describing the error.
   In case of success, a JSON document will be returned.
   It will be an object that always has at least the field `equivalent`.
   It is always a boolean stating whether the two regular expressions are equivalent.
   In case the regular expressions are not equivalent, 
   an additional field `witnesses`, containing an array of strings, will be present.
   Each of these strings would be accepted by one of the regular expressions, 
   but not by the other one.
   
 * `/regex/dfa_conversion`: Expects a POST request with content type 
   `application/x-www-form-urlencoded` and a parameter `regex`.
   The parameter is expected to be a valid UTF-8 string obeying the aforementioned
   syntax of regular expressions.
   The endpoint will return a HTML page describing a DFA that accepts the
   same language as the given regular expression.
   For the user's convenience, the states are named after regular expression
   derivatives whose language they accept.
   If the HTTP header `X-Embeddable` is specified, the output will be restricted
   to a HTML fragment since it looks like there is no MIME type for partial
   HTML documents.
   If the parameter is missing or if UTF-8 decoding errors or syntax errors occur,
   a `400 Bad Request` status message will be returned.
   The response body will be a short UTF-8 string describing the error. 


Technical Report
----------------

The technical report is contained in the `doc` subirectory. Type `make` there
or `make doc` in the project directory to build it.

The build process uses the program `lhs2tex` on all Literate Haskell
source files in the `src` directory and below.
Then they are combined by the main file `doc/report.tex`.

`latex2hs` is installed into the sandbox if not in $PATH on your machine.
In any case the `lhs2tex` tool requires the CTAN packages `polytable` and `lazylist`.
If they are not present, or not recent enough, the installer of lhs2TeX will
try to install its own version of them into the system's TeX distribution,
which will most likely fail unless `make` is called with `sudo` rights.

The places where `lhs2TeX` will look for these files (`polytable.sty` and
`lazylist.sty`) can be looked up in the `lhs2TeX` documentation.
