---
title: Digraph philosophy
---
When designing data structures that allow recursion some tradeoffs must be made.

One such choice is, how do nodes reference other nodes?
One of two choices can be made that have different implications.
## Refernce by id
In the reference by id approach, each node has a unique id, say, it's GraphQL name.
Nodes reference other nodes by their id, with no regard for the implementation of the referenced nodes.
This approach is simple and easy to implement, but it has a few drawbacks.
* There is no compile-time guarentee that every referenced node has a corresponding definition (at least not a simple one).
* Assuming structural information of a node is also forbidden, since the node implementation is not tied to it's id.
(for instance, like using an interface's fields in a subtype's implementation)
* Since the id is the only information a reference has, automatic discovery of the schema is impossible, which burdens the end-user with the task of explicitly supplying implementations for every node.
## Refernce by value
The reference by value choice is the one this library takes.
It imposes some differences that disallow ambiguity unless the user explicitly opts in to it.
:::note
Implementation wise, converting one model to the other is not difficult.
If your need for this arises, please open an issue.
:::
In the reference by value approach, each node references other nodes by their value; their implementation.
This approach has a few advantages over the reference by id approach.
* There is a compile-time guarentee that every referenced node has a corresponding definition.
* Since the implementation is tied to the node, structural information can be extracted and used.
* The user should only supply as little information as necessary.

Unfortunately, this approach has a few drawbacks.
The most noticeble drawback is that during construction of (mutually) recursive types, either the user must "tie the loop",
or opt out of safety.
Defining types with functions (`def`) instead of `lazy val` can cause a situation where the structure cannot be validated, 
since every time the type is referenced, it is a new instance.
Fortunately there are many ways to work around this limitation.
