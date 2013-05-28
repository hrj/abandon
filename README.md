## Abandon

_abandon_
   n: freedom from inhibitions, restraint, concern, or worry.

Abandon is a text based, double-entry accounting system. It is heavily inspired by [Ledger](http://http://www.ledger-cli.org/), another similar system.

Example entry:
```
2013/1/1
    Expense:Food                -200
    Assets:Cash
```

## Features at a glance

* Double entry accounting
* Input is through plain-text files. The syntax is well defined and yet human friendly, just like `ledger`'s.
* Portable across various operating systems; based on the Java platform.
* Reporting: supports both textual and interactive, graphical reports.

## Differences from Ledger

(why yet another fork)
* Cross-platform. This was a major consideration to be able to collaborate with external auditors, accountants, etc. While, in theory, the existing implementations of Ledger are cross-platform, they need to compiled / packaged separately for each platform.
* Simpler and more regular syntax. Some of the simplicity is because many features haven't been implemented yet. But, in general, I do want to cut the flab and keep things simple and regular.
* An interactive GUI for viewing reports.

## Roadmap

Many basic features are still missing. Upto version 0.5 the focus will be on adding basic features such as:

* Support for currencies
* Reporting options (sorting, grouping, time limits, etc)

## License
Creative Commons [CC BY 3.0](http://creativecommons.org/licenses/by/3.0/)
(that is free to use, share, modify, but attribution is required)
