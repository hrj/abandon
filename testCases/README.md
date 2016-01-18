# Functional and System Tests

This folder contains functional tests for Abandon.

Test folders use following naming convention:

    <scl|cli>T<ID>-subject

where: 
   - scl denotes tests which are integrated and implemented in Scala code
   - cli denotes tests which are run by external program
   - ID is unique, test id (digits only)
   - subject is very short mnemonic about test topic

Inside a test directory, there is:

 - at least one conf-file, (e.g. bal01.conf)
 - reference output files, filenames MUST follow convention: 
   (e.g. bal01.ref.balance.xml and bal01.ref.journal.xml)

Configuration MUST output one file per reference file, in following filename format:
e.g. bal01.conf -> bal01.ref.balance.xml -> out.bal01.balance.xml


