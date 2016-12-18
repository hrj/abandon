# Functional and System Tests

This folder contains functional tests for Abandon.

## Testrunner

Test framework implements automatic Testrunner framework.

Testrunner will look test cases under certain directories
based on following naming convention:

    sclT<ID>-<Subject>

where: 
   - sclT denotes tests which are run by Testrunner
   - ID is unique, test id (digits only)
   - Subject is very short mnemonic about test topic

Inside a test directory, there must be:

 - at least one conf-file, (e.g. bal01.conf)
 - reference output files, filenames MUST follow convention: 
   (e.g. bal01.ref.balance.xml and bal01.ref.journal.xml)

 - Optionally use extra command line arguments for current test case.
   These arguments are listed on "args"-file (e.g. bal01.args),
   one argument per line, and this file MUST NOT contain
   "-c", "conf-file" args.

   For example filter-arguments:
   ```
   --filter
   onOrAfter=2013-01-01
   before=2013-12-31
   ```
   Each argument line is trimmed, so whitespace is removed
   from begining and at end.

Abandon configuration MUST output one file per reference file
in following filename format:
   e.g. bal01.conf -> bal01.ref.balance.xml -> out.bal01.balance.xml

where:
   bal01.conf := Abandon configuration
   bal01.ref.balance.xml := one of reference files
   out.bal01.balance.xml := one of output files

