# Abandon

## Command line arguments

 - '-g' := Start graphical GUI
 - '-c' <conf-file> := path to journal's configuration file
 - '-i' <input-file> := input file, could be specified inside conf-file
 
## Configuration file


### Input -directive

Input directive defines primary inputs for this particular journal. 
There could be other "include" directives inside these primary inputs.

Input directive supports following formats:

	inputs += input

where input could be:
 
 - file-path, this path could be absolute, or relative to the conf-file
 - "glob:" + pattern, this is glob based pattern matching, see below for further info
 - "regex:" + pattern, this is regex based pattern matching, see below for further info
      
#### Wildcard support

With both wildcard inputs (`glob:` and `regex`) candidates files paths are matched 
against filenames which are full, absolute paths to those files.

Input files are unique set, so if a file gets included multiple times by wildcard pattern,
it will be used only once. 

##### Glob syntax

Glob syntax supported by Adandon is based on Java's NIO Glob based filename matching.
There is good tutorial about glob in 
[Java Tutorial: Glob](https://docs.oracle.com/javase/tutorial/essential/io/fileOps.html#glob)
 
Glob based path matching is activated by prefixing input path with "glob:".
  
 - '*' matches zero or multiple characters, but it will not gross directory boundary
 - '**' matches zero or multiple characters, and it will cross directory boundaries
 - '?' matches single character 
 - also sub-patterns with curly braces {foo, bar} and character classes [a-z] are supported.
  
 Adandon specific extension to glob syntax:
 
 - Cooked form of glob: if glob pattern starts with relatvie directory or filename, 
   without glob-pattern at the beging of path, glob-pattern will be prefixed 
   or "cooked" automatically with full path to directory where configuration 
   file is located.   This makes it easy and intuitive to match paths relative to 
   configuration file.  

   For example, let's assume a setup where there is common configuration file for journal, 
   and each transaction is kept in own file. These individual transactions reside directory 
   tree structure which is modeled after years and months.

   ```
	journal
	├── journal.conf
	└── transactions
	    ├── 2015
	    │   ├── 01
	    │   └── 02
	    └── 2016
	        ├── 00-equity.txn
	        ├── 99-closing.txn
	        ├── 01
	        │   ├── 2016-01-01.txn
	        │   └── 2016-01-15.txn
	        └── 02
   ```

   With above layout for example following glob patterns could be used:

   Find all transactions for January 2016:

   `input += "glob:transactions/2016/01/*.txn"`

   Find all transactions and equity for year 2016:

   ```
   input += transactions/2016/00-equity.txn
   input += "glob:transactions/2016/*/*.txn"
   ```

   Find all transactions (including equity and closing statements) 
   for year 2016:

   `input += "glob:transactions/2016/**.txn"`


##### Regex syntax

Regex syntax supported by Adandon is based on Java's regex library.
There is good tutorial about regex in 
[Java Tutorial: Regex](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)

Regex based path matching is activated by prefixing input path with "regex:". 

Abandon regex system supports also basepath cooked form of regex, please see 
documentation of glob syntax for further info how cooked mode works.

