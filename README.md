# CQRS for Haskell

## Introduction

This is a Haskell implementation of the CQRS+ES architectural pattern.
It combines two patterns which are each powerful in their own right, but
whose combination is exponentially more powerful yet.

The first pattern is **Command Query Responsibility Segregation**
(CQRS) which is about separating the portion of your application that
does writes or updates from the portion of your application that
performs queries against data. Martin Fowler has a [lovely
introduction to CQRS](http://martinfowler.com/bliki/CQRS.html) which I
recommend reading if you're new to CQRS.

The second pattern is **Event Sourcing** (ES) which is about
representing all state change in your application as a sequence of
semantic immutable *events* rather than modifying data in-place. A
simple analogy is to think of an accounting ledger: Every single
monetary transaction is recorded as a *new* line in the ledger rather
than by going in and changing any existing lines. When you want to
know the current balance you sum up all entries. Martin Fowler also
has an [introduction to Event
Sourcing](http://martinfowler.com/eaaDev/EventSourcing.html) which I
recommend reading if you're not already familiar with ES.

## Example Application

A simple example application is provided in the
[cqrs-example](https://github.com/BardurArantsson/cqrs/tree/master/cqrs-example)
directory. It's a basic TODO-list webapp which uses the `cqrs`
framework and [scotty](http://hackage.haskell.org/package/scotty) on
the backend and uses [React](https://facebook.github.io/react/) on the
frontend. The code should hopefully mostly be self-explanatory.

## API Stability and Planned/Missing Features

At *least* the following changes are currently planned:

* Migrations will probably be reworked and possibly moved to an
  external library. They are currently not flexible enough to handle
  using an arbitrary SCHEMA, or e.g. using a different database for
  snapshots.
* Aggregate root IDs may be reworked to allow for arbitrary types
  instead of being limited to UUIDs as they are currently. This is to
  allow for more flexibility, for example to allow using hashes as
  aggregate IDs to guarantee uniqueness.
* There's currently no built-in support for persistent query views
  based on the event streams. Such support is planned.
