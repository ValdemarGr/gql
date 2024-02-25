---
title: Structuring large applications
---
The documentation explores smaller examples.
To host larger graphs there are some considerations that must be addressed.
* What up-front work can be done to minimize the overhead in introducing new types.
* How is (mutual) recursion handled between different domains.

## Seperating domains
Partially applying all needed dependencies can be expressed with a class.
```scala mdoc
class DomainType
```