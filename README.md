> **abandon** _noun_: freedom from inhibitions, restraint, concern, or worry.

**Abandon** is a text based, double-entry accounting system. Transactions are entered in plain text files. You can use your
favorite text editor to edit these files, and can use your favorite [VCS](http://en.wikipedia.org/wiki/Revision_control) for versioning
and collaboration.

From these input text files, Abandon can present textual reports
or graphical reports. The graphical reports are useful when you need to interactively explore the data.

Abandon is inspired by [Ledger](http://ledger-cli.org/) but is simpler to use, has a more regular syntax, has a GUI
and is cross-platform. Abandon tries to maintain syntax compatibility with Ledger whenever possible.

#### Sample Text report
![Abandon Text output Screenshot](http://i.imgur.com/3n3GmdE.png)

#### Sample Graphical report
![Abandon Screenshot](http://i.imgur.com/9mTthiH.png)

[(Screenshot Gallery)](http://imgur.com/a/GLhV5#0)

### Quick start
If we enter this into a text file:
```
2013/1/1
    Expense:Food                -200
    Assets:Cash
```

... and run it through `abandon`, the program will subtract `200` from the account `Expense:Food` and (automatically) balance the transaction by adding `200` to the account `Assets:Cash`.

`Expense:Food` will become an account nested under the account `Expense`. If we add another transaction like this:
```
2013/1/2
    Expense:Entertainment                -400
    Assets:Cash
```
then `Entertainment` will be another child account under the parent `Expense` account.

Hence, the balance report will look like this:

```
   600.00   Assets:Cash        
  -600.00   Expense            
  -400.00    ├╴Entertainment   
  -200.00    └╴Food            
─────────────────────────────────────────────
     0.00                         0.00 = Zero
```

The `Expense` account shows a value of `-600` which is the total of its own amount and its childrens'.

The last line shows the total of the top level accounts, which in this case is `0.00`.

> *Note:* The second `0.00` is for accounts that get printed on right. In this simple example there is nothing to show on the right side of the report.


### Features at a glance

* Double entry accounting
* Infinite precision arithmetic
* Input is through plain-text files. The syntax is well defined and yet human friendly, just like `ledger`'s.
* Portable across many operating systems; based on the Java platform.
* Reporting: supports both textual and interactive, graphical reports.
  The GUI watches for changes in input files and automatically refreshes when it detects a change.


### Differences from Ledger
(or why yet another fork)
* Cross-platform. This was a major consideration to be able to collaborate with external auditors, accountants, etc. While, in theory, the existing implementations of Ledger are cross-platform, they need to be compiled and packaged separately for each platform.
* Simpler and more regular syntax. Some of the simplicity is because of missing features. But, in general, I want to cut the flab and keep things simple and regular.
  For example, identifiers can have numbers in them. Although this might sound trivial, the language has to be carefully designed for this. In `ledger`, this doesn't work because it messes up with the syntax for currencies.
* An interactive GUI for viewing reports. 

### Installation
* Install **Oracle Java 7** or **Java 8 from any provider**.
  Other versions of Java will work fine for the CLI, but the GUI requires the above specific versions.
* Download and extract the `Abandon` binaries from [here](https://github.com/hrj/abandon/releases)
* Use the `*.sh` files to run on `*nix` and `Mac` or the `*.bat` files to run on `Windows`.

### Running

The command line options are:
```
   -c <config-file-path>        Specifies the path to a config file
   -i <input-file-path>         Specifies the path to an input file
```

The config file can specify which reports to generate and how. Some of these options are available as command line parameters too.

As of now, the preferred way of running the program is by specifying a config file. Look at `examples/simple/accounts.conf` for an example config file, and `doc/abandon.md` for further instructions and information about Abandon.

### Roadmap

Many features are still missing. Until version 0.5 the focus will be on adding basic features such as:

* Support for currencies
* Reporting options (sorting, grouping, time limits, etc)

Look at the issue list for a complete road map.


### License
Creative Commons [CC BY 3.0](http://creativecommons.org/licenses/by/3.0/)
(free to use, share, modify, but attribution is required)

### Contact
Join us in the chat room here: [![Gitter chat room](https://badges.gitter.im/hrj/abandon.png)](https://gitter.im/hrj/abandon).

Or simply raise an issue in GitHub.

### Build & Coverage status
[![Build Status](https://travis-ci.org/hrj/abandon.svg?branch=master)](https://travis-ci.org/hrj/abandon)
[![Coverage Status](https://img.shields.io/coveralls/hrj/abandon.svg)](https://coveralls.io/r/hrj/abandon?branch=master)
