---
name: Stryker4s on Scala 3.8
description: Compatibility constraint for reliable multi-file mutation testing with Stryker4s on Scala 3.8.
---

Exclude method-expression and string-literal mutators from multi-file Stryker4s runs on Scala 3.8 unless a newer plugin is verified to handle them.

**Why:** Stryker4s 1.1.1 generates invalid parser/operator mutations, then fails to roll them back because Scala 3.8 compiler diagnostics prevent its recovery pass from completing.

**How to apply:** When upgrading Stryker4s or Scala, test a full multi-file run with these mutators restored. Remove the exclusions only after the run completes and reports co
