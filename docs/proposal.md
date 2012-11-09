Objective
---------

The ultimate goal of LTc[^ltc] is to develop an
eventually-consistent[^cap], replicated key-value store optimised for
the case where the nodes are connected by an intermittent,
high-bandwidth, high-latency link.

By intermittent, we mean that the link between nodes will be down
regularly for hours at a time.

By high-latency, we mean that the round-trip time between nodes is
measured in minutes, rather than in milliseconds.

Additionally, LTc should be able to store terabytes of data[^wikisize].

[^ltc]: As in, less than the speed of light in vacuum, which is the
upper bound on the speed of communications in our universe.

[^cap]: We are aiming for the Availability and Partitioning-tolerance
parts of CAP.

[^wikisize]: For comparison, English Wikipedia takes 38 GB
uncompressed.

Motivation
----------

The recent advent of NoSQL databases show that there are uses for
distributed databases that are not relational or fully consistent, and
that do not support ACID transactions.  Given the relative infancy of
the field, I suspect there is room for improvement in how current
systems propagate updates between nodes (especially if you relax the
consistency guarantee between nodes).

There are quite a number of similar projects (Mnesia, CouchDB,
MongoDB, Membase, Yahoo Sherpa, Project Voldemort), but none solve
quite the same problem as LTc.  Specifically, none will work when the
links connecting the nodes are intermittent and high-latency.

Implementation
--------------

### Phase 0

> Deadline: end of October

Research and document exactly what issues are likely to be encountered
when two communicating systems are physically separated by large
distances.  Specifically, determine why TCP would not work (and at
what point in breaks down), whether UDP or another existing protocol
would work, and how "intermittent" the connection is likely to be.

### Phase 1

> Deadline: end of November

Develop an on-disk key-value data-store designed for a modern Linux
system running on modern hardware.  By modern Linux system, we mean
that the data-store should make use of features such as the
file-system cache, the IO scheduler's elevator lifting, and automatic
memory paging.

Since this is only a key-value store, without support for
transactions, it should not be too difficult to implement.  For
instance, CouchDB's on-disk format is an append-only B+-tree; if all
else fails, that should be enough for LTc, and it could probably be
written and tested in a week.

Additionally, develop a benchmark to compare different implementations
of the data-store.

### Phase 2

> Deadline: end of January

Find out how NASA and ESA are currently getting information to and
from the Mars orbiters and rovers.  Invent and implement a
synchronization protocol for the replicated nodes.  This will probably
involve sending large sets of updates at once.  Entries will probably
need to be versioned with a scheme like vector-clocks.

### Phase 3

> Deadline: mid March

Since we are unlikely to get access to a computer far enough to test
LTc's synchronization, develop a Linux kernel module that creates a
loopback network device that is high-latency and intermittent.  Using
this, test and tune the synchronization algorithm.

### Phase 4

> Deadline: mid May

Develop an easy-to-use monitoring and statistics interface for LTc.
Use the statistics to automatically tune the synchronization algorithm
(e.g. we probably want different behaviours if the other node is 100ms
away, and if it is 30min away).  Polish everything and come up with a
convincing demo.
