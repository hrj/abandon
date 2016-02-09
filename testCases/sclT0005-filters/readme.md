# tests for filters

## testcases

 - date
   - [001] begin
   - [002] end
   - [003] span (e.g. begin - end)

 - payee
   - [010] Single txn
   - [011] multiple txn

 - [020] annotation

 - account
   - [030] single account
   - [031] multiple accounts

 - [04X] combinations
   - [040] txn:attr multiple times (e.g. time + annotation, payee)
   - txn:attr + post:attr
	- [041] txn:attr + post:account
	- [042] txn:time + post:account

 - filter definitions precedence + conf-file
   - filters from conf-file
     - [050] one test with combined filter (multiple filter definitions) should cover it for now

   - filters from conf-file + cli
     - [051] test that cli overrides conf

## Special things to test

 - [010, 020, 030] correct usage of txn:attr (e.g. two different txn, where payee and annotation would match)
 - [010, 030] txn:attr vs. post:account (e.g. two different txn, where txn:attr and post:attr would match)
 - [060] active filter notices (this is also testing combinations of all filters by conf-file)
