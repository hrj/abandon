# These are tests which are checking various "input" handling cases

## glob01 is a test for glob-pattern as input.

It is using glob multiple times (even when single, more greedy glob 
could do same). Testcase is also use one normal file as input.

This is actually mimicking use case when there are multiple transactions per
month, each on own file,  and there is also one equity transaction 
at the begin of year.

