STLC-Redex
------------

Requirements: stack, python3

Build:
> stack build
Test known counterexamples:
> stack test --test-arguments --match=SanityCheck
Run tools against all mutations:
> ./test_all.sh
