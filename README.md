> **abandon** _noun_: freedom from inhibitions, restraint, concern, or worry.

**Abandon** is a text based, double-entry accounting system. Transactions are entered in plain text files. You can use your
favorite text editor to edit these files, and can use your favorite [VCS](http://en.wikipedia.org/wiki/Revision_control) for versioning
and collaboration.

From these input text files, Abandon can present textual reports
or web-based graphical reports. The web reports are useful when you need to interactively explore the data.

In addition, PDF reports can be generated using [abandon-reports](https://github.com/hrj/abandon-reports). PDFs are useful
when you need to print the report or share it with someone by email, etc.

Abandon is inspired by [Ledger](http://ledger-cli.org/) but is simpler to use, has a more regular syntax, includes a Web UI
and is cross-platform. Abandon tries to maintain syntax compatibility with Ledger whenever possible.

#### Sample Text report
![Abandon Text output Screenshot](http://i.imgur.com/3n3GmdE.png)

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
* Reporting: supports both textual and web-based graphical reports.
* Includes a modern Web UI (`web-ui`) built with Svelte and Vite for interactively exploring financial reports. The web server monitors input files for changes and updates the Web UI automatically.


### Differences from Ledger
(or why yet another fork)
* Cross-platform. This was a major consideration to be able to collaborate with external auditors, accountants, etc. While, in theory, the existing implementations of Ledger are cross-platform, they need to be compiled and packaged separately for each platform.
* Simpler and more regular syntax. Some of the simplicity is because of missing features. But, in general, I want to cut the flab and keep things simple and regular.
  For example, identifiers can have numbers in them. Although this might sound trivial, the language has to be carefully designed for this. In `ledger`, this doesn't work because it messes up with the syntax for currencies.
* An interactive Web UI for viewing reports.

### Installation
* Install **Java 8** (or later) from any provider. OpenJDK works fine.
* Download and extract the `Abandon` binaries from [here](https://github.com/hrj/abandon/releases)
* Use the `*.sh` files to run on `*nix` and `Mac` or the `*.bat` files to run on `Windows`.

### Running

The command line options are:
```
   -c <config-file-path>        Specifies the path to a config file
   -i <input-file-path>         Specifies the path to an input file
   -w <start-date>              Starts the web server with specified start date (e.g., -w 2020/01/01)
```

To run the Web UI, use the `-w` / `--web-start-date` option when launching Abandon (e.g. `abandon -c accounts.conf -w 2020/01/01`). The Web UI is bundled directly with the Abandon binary and will be accessible at `http://localhost:9000/`.

The config file can specify which reports to generate and how. Some of these options are available as command line parameters too.

As of now, the preferred way of running the program is by specifying a config file. Look at `examples/simple/accounts.conf` for an example config file, and `doc/abandon.md` for further instructions and information about Abandon.

### Roadmap
Abandon works fine for single-currency accounting. In the future, we plan to add support for:

* Multiple currencies (or units or commodities).
* Reporting options (sorting, grouping, time limits, etc).

The issue list provides a glimpse into the immediate road map.

### Maven artifacts
If you need to use abandon as a library, you can use the following maven dependency information:

```
<dependency>
  <groupId>in.co.uproot</groupId>
  <artifactId>abandon-base_2.11</artifactId>
  <version>0.3.2</version>
</dependency>
```

The library jars can be downloaded from [ Sonatype](https://oss.sonatype.org/content/repositories/releases/in/co/uproot/).

### Mutation testing

The Scala code uses [Stryker4s](https://stryker-mutator.io/docs/stryker4s/getting-started) to check whether the test suite detects changes in production code.

Run the checked-in mutation-testing baseline for the `base` and `cli` modules:

```sh
sbt mutationTest
```

Each module writes timestamped HTML and JSON reports below its `target/` directory. The console summary and reports classify mutants as killed, surviving, timed out, or uncovered. Since `target/` is ignored, generated reports are not committed.

The baseline mutates the tested Scala sources in `base` except `FileWatcher.scala`, whose asynchronous test does not terminate cleanly in Stryker4s's reusable runner. In `cli`, it mutates `JsonUtils.scala`; the other CLI sources depend on the separately generated frontend bundle or do not yet have isolated mutation-test coverage. Method-expression and string-literal mutations are disabled because Stryker4s 1.1.1 cannot reliably roll back their Scala 3.8 compiler errors in a multi-file mutation batch.

To iterate on a smaller area, run one module and pass a source glob:

```sh
sbt 'base/stryker --mutate "src/main/scala/**/Parser.scala"'
```

The initial configuration reports the mutation score without enforcing a minimum. A full mutation run is intentionally separate from the normal `sbt test` and CI workflows because it is substantially slower.

### License
[Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0)

### Contact
Join us in the chat room here: [![Gitter chat room](https://badges.gitter.im/hrj/abandon.png)](https://gitter.im/hrj/abandon).

Or raise an issue in GitHub.

### Build & Coverage status
[![Build Status](https://travis-ci.org/hrj/abandon.svg?branch=master)](https://travis-ci.org/hrj/abandon)
[![Coverage Status](https://img.shields.io/coveralls/hrj/abandon.svg)](https://coveralls.io/r/hrj/abandon?branch=master)
