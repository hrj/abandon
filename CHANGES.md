## Release History

### version 0.3.0
Currently release-candidate level.

#### Update Note for 0.3.0
 - XML format has been changed
   - New container element "abandon:journal"
   - Renamed elemet "txn" to "post"
   - Renamed element "txnGroup" to "txn"
   - Transaction dates are in extended ISO 8601 format
   - Account balance export in XML "abandon:balance"

 - Clarified reporting and exporting (https://github.com/hrj/abandon/issues/71)
   - Clarified meaning of "type": it could be either "journal" or "balance"
   - New configuration keyword "format": which could be "ledger" or "xml"

#### New features

* New report type `balance` for XML format
* Compact notation for transactions. #57
* Support for [scopes](https://github.com/hrj/abandon/wiki/Scopes). #5
* Default accounts for balancing a transaction can be specified by defining `defaultAccount`.
* Support for comparison expressions, such as `<`, `>`, etc
* Support for conditional expressions using ternary operator: `condition ? x : y`
* ISO 8601 date format supported #67
* Support for regex and glob patters in `input` config option: #76
* Format of xml exports and the config options for them has changed.
* The runner script uses JAVA_HOME if set. #83

#### Fixes

* Fix for #72: output of xml export gets duplicated with stdout (6d5e539b)
