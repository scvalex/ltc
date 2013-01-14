References
==========

Background Reading
------------------

### Subversion's Delta Editor: Interface As Ontology \cite{Fogel12}

Subversion has a client/server model.  The server holds all the
history for a repository, while clients only have "working
directories" checked out at particular revisions, and possibly local,
uncommitted changes.

The server sees a repository as a functional tree.  That is, every
time an update occurs, it creates new nodes on the path from the
change to the root, linking back to the old revision when possible.

Furthermore, the interface to changing the tree on the server, and the
working directory on the client, uses a functional stateless approach.
Concretely, updates must happen on a node-by-node basis, starting with
a root, and descending in the tree.  This is enforced by requiring
that, in order to change a node, one must have a reference ("baton")
to its parent, which can only be obtained by starting a change
operation of the parent.

Although it's probably not implemented as such, Subversion's update,
and commit operations can be done through a single RPC call.

### Git Internals -- Transfer Protocols \cite{Chacon09}

Git separates the task of downloading/uploading the changes, and
updating the working tree.  Only the transfer part is interesting to
us.

In order to exchange changes, Git queries the remote for its
references, then walks through the commits, downloading or uploading
changes.  This is all done with many separate requests.

### Two Cons against NoSQL \cite{Zicari12}

The author notes two current issues with NoSQL databases:

 * it's hard to transfer data from one NoSQL product into another, and

 * there's no standard way to access a NoSQL data-store.

The author then proceeds to interview several people who explain how
his issues are fictive, or only true in some narrow sense.

### On Eventual Consistency – An interview with Justin Sheehy \cite{Sheehy12}

An interview with the CTO of Basho Technologies, where he talks about
Riak, eventual consistency, and uses of document-stores.

### On Eventual Consistency - An interview with Monty Widenius \cite{Widenius12}

An interview with the author of MySQL (who forked MariaDB), where he
explains how eventual consistency is not what he'd want in many
circumstances.  This is mostly a response to the Basho interview
above.

### Riak SmartMachine Benchmark: The Technical Details \cite{badnima10}

Joyent bencharked Riak under a variety of loads.  The found that:
"Riak behaves predictably under high loads", "Riak demonstrates
stability under high loads", "Riak demonstrates linear scalability".

### Delay-tolerant networking \cite{wiki:DTN}

DTN is basically normal networking will lots of store-and-forward hops.

### Mercurial WireProtocol \cite{mercurial}

The mercurial wire protocol is a set of RPC methods that are used to
discover what changes one side is aware of and the other is unaware
of, and then to transfer said changes.  The RPC calls are either done
over HTTP or over an SSH pipe.

### Bazaar

It's over-complicated and over-engineered as hell.  Comparing the way
it works to git is really an eye-opening experience.

### Darcs Optimized HTTP

> [darcs-http.pdf](https://github.com/scvalex/ltc/blob/master/reference/darcs-http.pdf)

In order to minimize the number of round-trips, the current "pristine"
repo is packed into a tarball, and all the patches are packed into a
different tarball.  This way, to get cracking you only need the first
tarball, and to get everything you need both tarballs.

### Darcs Patch Theory \cite{wiki:darcs}

Patch theory is an algebra for patches.  It talks about patches as an
almost-group (patch application is not a closed operation).  It uses
commutation (which is not to say patches are commutative) to re-order
the patch history before doing some operations.

### Rsync Algorithm \cite{wiki:rsync}

In order to synchronize a local file with an older version of the file
on a remote server, `rsync` does the following: first, the local
machine splits the file into fixed-size chunks and computes two
checksums for them (an MD5 and a rolling checksum); these checksums
are sent to the remote; the remote computes *all* the rolling
checksums for its version of the file, and checks if any blocks match
with what the local sent; it tells the local what pieces it's missing;
the local then sends the missing pieces over.

### Diff O(ND) Algorithm \cite{Myers86ano(nd)}

Diffing is related to the Longest Common Subsequence problem.  The
general idea is to find the LCS of two strings, then work backwards,
and compute the edit script.

Interestingly, the way the algorithms above are explained is different
from the standard LCS dynamic programming solution.  The define the
edit graph of two strings in the obvious way with edges going to the
nodes underneath, to the right, and on the diagonal. They then define
a D-path of length `k` as a path through the edit-graph that has
exactly `k` non-diagonal edges.  Their algorithms then work by
computing D-paths of increasing lengths.

### Bundle Protocol Specification \cite{rfc5050}

"This document describes version 6 of the Delay Tolerant Networking
(DTN) "bundle" protocol (BP).  Delay Tolerant Networking is an end-
to-end architecture providing communications in and/or through
**highly stressed environments**.  Stressed networking environments
include those with intermittent connectivity, large and/or variable
delays, and high bit error rates." (emphasis ours)

BP is an overlay store-and-forward network.  It sits at the
application layer and is built on top of existing networks.

They use a dictionary to "compress" IDs in each bundle.  All IDs are
included once at the beginning of each bundle, and references to these
are used in the rest of the bundle; so, every ID appears only once in
the binary representation.

### Interplanetary Internet \cite{wiki:IPN}

"While the Internet as it is known today tends to be a busy network of
networks with high traffic, negligible delay and errors, and a wired
backbone, the Interplanetary Internet is a store-and-forward network
of internets that is often disconnected, has a wireless backbone
fraught with error-prone links and delays ranging from tens of minutes
to even hours, even when there is a connection."

### Delay Tolerant Networking Research Group \cite{dtnrg}

"Said another way, we are concerned with interconnecting highly
heterogeneous networks together even if end-to-end connectivity may
never be available. Examples of such environments include spacecraft,
military/tactical, some forms of disaster response, underwater, and
some forms of ad-hoc sensor/actuator networks. It may also include
Internet connectivity in places where performance may suffer such as
developing parts of the world."

### Routing in delay-tolerant networking \cite{wiki:dtn-routing}

"There are many characteristics DTN protocols, including routing, must
take into consideration. A first consideration is if information about
future contacts is readily available. For example, in interplanetary
communications, many times a planet or moon is the cause of contact
disruption, and large distance is the cause of communication
delay. However, due to the laws of physics, it is possible to predict
the future in terms of the times contacts will be available, and how
long they will last. These types of contacts are known as scheduled or
predictable contacts.[7] On the contrary, in disaster recovery
networks the future location of communicating entities, such as
emergency responders, may not be known. These types of contacts are
known as intermittent or opportunistic contacts."

### Delay-Tolerant Networking Architecture \cite{rfc4838}

"In a sense, the DTN architecture provides a common method for
interconnecting heterogeneous gateways or proxies that employ store-
and-forward message routing to overcome communication disruptions.  It
provides services similar to electronic mail, but with enhanced
naming, routing, and security capabilities.

The existing Internet protocols do not work well for some
environments, due to some fundamental assumptions built into the
Internet architecture:

 - that an end-to-end path between source and destination exists for
   the duration of a communication session

 - (for reliable communication) that retransmissions based on timely
   and stable feedback from data receivers is an effective means for
   repairing errors

 - that end-to-end loss is relatively small

 - that all routers and end stations support the TCP/IP protocols

 - that applications need not worry about communication performance

 - that endpoint-based security mechanisms are sufficient for meeting
   most security concerns

 - that packet switching is the most appropriate abstraction for
   interoperability and performance

 - that selecting a single route between sender and receiver is
   sufficient for achieving acceptable communication performance

The DTN architecture is conceived to relax most of these assumptions,
based on a number of design principles that are summarized here:

..."

"The use of the bundle layer is guided not only by its own design
principles, but also by a few application design principles:

 - Applications should minimize the number of round-trip exchanges.

 - Applications should cope with restarts after failure while network
   transactions remain pending.

 - Applications should inform the network of the useful life and
   relative importance of data to be delivered."

### DRBD \cite{drbd}

DRBD just sends write operations from the master node to the slaves.
The write either blocks until all the slaves have finished the write
(Protocol C), or it's done in an asynchronous fashion which only
blocks when the TCP send buffer is full (Protocol A).

It currently uses TCP.  UDP multicast is on their
roadmap. \cite{drbd-roadmap}

### Globally Distributed Content Delivery \cite{akamai}

This is an overview of what Akamai does -- it serves static and
dynamic content from the "Internet's Edge".  It focuses on the user's
perspective so it doesn't contain anything useful to us.

### Redis replication \cite{redis-replication}

"If you set up a slave, upon connection it sends a SYNC command. And
it doesn't matter if it's the first time it has connected or if it's a
reconnection.

The master then starts background saving, and collects all new
commands received that will modify the dataset. When the background
saving is complete, the master transfers the database file to the
slave, which saves it on disk, and then loads it into memory. The
master will then send to the slave all accumulated commands, and all
new commands received from clients that will modify the dataset. This
is done as a stream of commands and is in the same format of the Redis
protocol itself."
