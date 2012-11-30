ltc
===

> A distributed, disconnected database for the relativistic universe

Relativistic?
-------------

Imagine you have copies of the same database stored on two different
computers.  Changes are made to each of the two copies and now, you
want to sync them.  But there's a catch: the round-trip time is 8 min
(Earth-to-Mars and back).

The `ltc` project aims to build an eventually consistent, distributed,
disconnected database.  It is distributed in that copies of the data
are stored at multiple sites.  It is disconnected in that the running
nodes are *not* connected to one another.  By eventually consistent,
we mean that updates cause forward progress, and, in the absence of
changes to the copies, and given enough updates, all sites will
converge to the same data.

Data layout
------------

`ltc` is a key-value store, where both keys and values are opaque
binary blobs.

Implicitly, every entry is associated with a version that can be used
to distinguish states between updates.

The `Ltc.Store` module exports the low-level key-value store's
interface.  Normal users should not have to deal with this.

Sites
-----

`ltc` stores copies of the entire dataset at each site.  A site is
identified by an unique randomly generated id, and a human-friendly
name.  Sites with different ids may have the same name, though this
may be confusing for humans inspecting the change history.

Why?
----

`ltc` solves two main problems: simple storage and synchronization
over *bad* connections:

- `ltc` is a key-value data-store; additionally, it takes some
  features from version control systems, and stores some history for
  each recorded field;

- `ltc` allows synchronization over very-high latency, lossy, and
  possibly one-way connections; the versioning plays a key role here,
  and allows `ltc` to resolve more conflicts than similar systems.

Operations
----------

Conceptually, `ltc` supports the following commands:

  * `set k v` - set the entry for key `k` to the value `v`; any
    previous value is overwritten; the entry's version for the current
    site is incremented;

  * `get k vsn` - get the value associated with the key `k` at version
    `vsn`.

Distribution
------------

`ltc` sites communicate with each other through a variety of ways.
All of these are, at least conceptually, asynchronous.  Additionally,
some protocols (e.g. UDP) may not guarantee the integrity of a
communication.

An `ltc` site may `push` changes to a remote site, or may `pull`
changes from it.

In order to `push` changes, `ltc` conceptually does the following:

  1. the local site iterates over all key-value pairs and sends them,
  along with their versions, to the remote site; if versions for
  entries are known for the remote site, the local site only sends
  newer entries;

  2. upon the receiving the entries, the remote site proceeds as if it
  had requested the changes by `pull`.

In order to `pull` changes, `ltc` conceptually does the following:

  1. the local site iterates over all key-value pairs and sends only
  their versions to the remote site; if versions for entries are known
  for the remote site, the local site only sends newer versions;

  2. upon receiving the versions, the remote sites sends all necessary
  updates to the local site;

  3. upon receiving the entries from the remote site, `ltc` proceeds
  to add them one-by-one into the local site: new entries or entries
  that are strictly newer than the local ones (as indicated by their
  versions) are simply inserted; entries that were changed both
  remotely and locally (we call these conflicts) are resolved.

The name
--------

The acronym `ltc` stands for "less than c", because, for the
foreseeable future, communication speeds will be bounded by the speed
of light.

Here, we normally talk about the time necessary to perform a
round-trip.  E.g. the shortest packet round-trip time to the Moon is
about 3 seconds and round-trip time to Mars is about 8 minutes.  A
less exotic example would be communicating with a phone, which may be
capable of receiving communications (e.g. GPS), but may not be able to
reply (e.g. there is no cell coverage); in this case, sending data to
the phone would be possible, but a reply from it may take long to
arrive, or may not arrive at all.
