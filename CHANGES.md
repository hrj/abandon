## Release History

### Current Release

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

#### Fixes
 - Fix for #72: output of xml export gets duplicated with stdout (6d5e539b)
 
#### New features
 - New report type `balance` for XML format
 - In journal configuration `input` could be specified as glob or regex pattern. See `doc/abandon.md` for further info.

