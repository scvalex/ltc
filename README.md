ltc
===

> A distributed, disconnected database for the relativistic universe

Relativistic?
-------------

Imagine you have copies of the same database stored on two different
computers.  Changes are made to each of the two copies and now, you
want to sync them.  But there's a catch: the round-trip time is 8 min
(Earth-to-Mars and back).

`ltc` strives to be an eventually consistent distributed, disconnected
database.  It is distributed in that copies of the data are stored at
multiple sites.  It is disconnected in that the running nodes are
*not* connected to one another.  By eventually consistent, we mean
that updates cause forward progress, and, in the absence of changes to
the copies, and given enough updates, all sites will converge to the
same data.

Data layout
------------

`ltc` is a key-value store, where both keys and values are opaque
binary blobs.

Implicitly, every entry is associated with a vector clock that keeps
track of its version across updates.

Sites
-----

`ltc` stores copies of the entire dataset at each site.  A site is
identified by an unique randomly generated id, and a human-friendly
name.  Sites with different ids may have the same name, though this
may be confusing for humans inspecting the change history.

Operations
----------

`ltc` supports the following commands:

  * `set k v` - set the entry for key `k` to the value `v`; any
    previous value is overwritten; the entry's vector clock entry for
    the current site is incremented;

  * `get k` - get the value associated with the key `k`

Distribution
------------

`ltc` sites communicate with each other over `UDP`.

An `ltc` site may `push` changes to a remote site, or may `pull`
changes from it.

In order to `push` changes, `ltc` conceptually does the following:

  1. the local site iterates over all key-value pairs and sends them,
  along with their vector clocks, to the remote site; if vector clocks
  for entries are known for the remote site, only send newer entries;

  2. upon the receiving the entries, the remote site proceeds as if it
  had requested the changes by `pull`.

In order to `pull` changes, `ltc` conceptually does the following:

  1. the local site iterates over all key-value pairs and sends only
  their vector clocks to the remote site;

  2. upon receiving the entries from the remote site, `ltc` proceeds
  to add them one-by-one into the local site: new entries or entries
  that are strictly newer than the local ones (as indicated by their
  vector clocks) are simply inserted; entries that were changed both
  remotely and locally are resolved by application-specific functions.


The name
--------

The acronym `ltc` stands for "less than c", because, for the
foreseeable future, communication speeds will be bounded by the speed
of light.  Concretely, this means that the packet round-trip time to
the Moon is about 3 s and round-trip time to Mars is about 8 min.
