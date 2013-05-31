## About

> **abandon** _noun_: freedom from inhibitions, restraint, concern, or worry.

Abandon is a text based, double-entry accounting system. It is heavily inspired by [Ledger](http://http://www.ledger-cli.org/) but leans towards simplicity.

Example entry:
```
2013/1/1
    Expense:Food                -200
    Assets:Cash
```

The above will subtract `200` from the account `Expense:Food` and (automatically) balance the transaction by add `200` to the account `Assets:Cash`.

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

Note: The second `0.00` is for accounts that get printed on right. In this simple example there is nothing to show on the right side of the report.

## Features at a glance

* Double entry accounting
* Input is through plain-text files. The syntax is well defined and yet human friendly, just like `ledger`'s.
* Portable across various operating systems; based on the Java platform.
* Reporting: supports both textual and interactive, graphical reports.

## Differences from Ledger
(or why yet another fork)
* Cross-platform. This was a major consideration to be able to collaborate with external auditors, accountants, etc. While, in theory, the existing implementations of Ledger are cross-platform, they need to compiled / packaged separately for each platform.
* Simpler and more regular syntax. Some of the simplicity is because many features haven't been implemented yet. But, in general, I want to cut the flab and keep things simple and regular.
  For examle, identifiers can have numbers in them. Although this might sound trivial, the language has to be carefully designed for this. In `ledger`, this doesn't work because it messes up with the syntax for currencies.
* An interactive GUI for viewing reports.

## Installation
TODO

## Roadmap

Many features are still missing. Until version 0.5 the focus will be on adding basic features such as:

* Support for currencies
* Reporting options (sorting, grouping, time limits, etc)

## License
Creative Commons [CC BY 3.0](http://creativecommons.org/licenses/by/3.0/)
(that is free to use, share, modify, but attribution is required)
