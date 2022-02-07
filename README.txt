
* Install using cabal:

To install the executable in the default location (usually ~/.cabal/bin/)
run:

  cabal install

in the this directory. To chose a different directory DIR use:

  cabal install --installdir=DIR --install-method=copy

When rebuilding/reinstalling you may need to add the
'--overwrite-policy=always' option to cabal.

* Usage

Usage: hilbert-encodings [-f | -c | -d] [OPTIONS] POLY

DESCRIPTION:

Encode variants of Hilbert's 10th problem as (incremental) polynomial
termination problems.

One of the options --func (-f), --coef (-c) or --deg (-d) must be
given. If multiple encoding or output format options are specified,
the respectively last encoding or output format is used.

OPTIONS:

  -h, -?  --help    print this message
  -f      --func    encode as constraint on interpretation functions
  -c      --coef    encode as constraint on coefficients
  -d      --deg     encode as constraint on degrees
          --pretty  standard output format
          --cops    output in COPS format, useful as input to other tools
                    (default: pretty)

INPUT FORMAT (POLY):

  <poly> :=  <monomial> ["+" | "-"] <poly> | <monomial>
  <monomial> := <powerproduct> | integer <powerproduct> | integer
  <powerproduct> := <exp> <powerproduct> | <exp>
  <exp> := var "^" integer | var

Here 'integer' is some integer and 'var' a sequence of
alpha-numerical symbols (letters are non case-sensitive).
