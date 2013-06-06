\begin{abstract}

One of the difficulties of engineering a distributed system is keeping
data synchronized across different nodes.  Current solutions include
distributed version control systems such as Git, traditional databases
such as MySQL, and modern NoSQL data stores such as Redis.  But all
these make tacit assumptions about the communication medium: that the
round-trip time between nodes is relatively short, and that the
channel is mostly error-free.  Although these assumptions hold within
data centers and sometimes on the Internet, they do not hold for some
extreme situations such as disaster-stricken areas and interplanetary
communications.  We have implemented LTc, a replicated data store that
remains synchronized over the high-latency lossy network channels
typically found in such situations.

\end{abstract}

\tableofcontents

\clearpage

Introduction
============

LTc\footnote{the name is an acronym that stands for "less than c", and
refers to the fact that all communication happens at sub-light speeds
in our universe} is a key-value data store designed so that replicated
data sets can be synchronized over lossy connections where end-to-end
connectivity may not be available.  Examples of environments where
this is the case "include spacecraft, military/tactical, some forms of
disaster response, underwater, some forms of ad-hoc sensor/actuator
networks, and Internet connectivity in places where performance may
suffer such as developing parts of the world." \citep{dtnrg}

## Motivation

\label{sec:motivation}

Data replication is a problem tackled by distributed version control
systems, traditional databases, and modern NoSQL data stores.  Current
systems are unsuitable for some situations because they make tacit
assumptions about the communications medium.  We discuss these
assumptions in detail in Sections \ref{sec:other-data-stores} and
\ref{sec:dtn}.  Broadly speaking, these systems assume that network
channels between nodes\footnote{when speaking of nodes, we mean
machines that hold (possibly different versions of) the same data set}
are always available, and use a protocol with strong reliability and
ordering guarantees such as TCP/IP.

Unlike other systems, LTc makes only the following explicit
assumptions.  First, nodes may send messages to some other nodes
occasionally.  Secondly, it is possible for all nodes to communicate
with each other, perhaps indirectly.

In other words, an LTc node does *not* expect to be able to send
messages to any other node at any time, it does *not* expect all sent
messages to reach the other nodes, and it does *not* expect immediate
replies to sent messages.  Additionally, if we construct a graph,
where nodes are vertices, and the asynchronous network channels
between nodes are directed edges, then the graph is strongly
connected.

We note that these assumptions hold for all the environments mentioned
above and discuss this in the following section.

## Usage Scenarios

\label{sec:scenarios}

There are several environments in which current systems will not work
effectively or at all, but for which LTc is designed.  In this
section, we look at what issues arise in each environment, and how
they impact network channels.

The following issues can usually be alleviated by better
communications infrastructure.  That said, unless an inexpensive
instantaneous communication system is discovered, these issues are
here to stay, and will only get worse as time goes on and the
transferred data increases in size.

### Interplanetary Internet

Consider interplanetary communications.  In contrast to the Internet,
which "tends to be a busy network of networks with high traffic,
negligible delay and errors, and a wired backbone, the Interplanetary
Internet is a store-and-forward network of internets that is often
disconnected, has a wireless backbone fraught with error-prone links
and delays ranging from tens of minutes to even hours, even when there
is a connection." \citep{Bur03} In other words, due to obstacles and
the energy-efficient nature of the machines involved, there is
significant packet-loss on any interplanetary communication channel.
Furthermore, because of the distances involved, the round-trip times
for messages are too large for packet retransmission to be a viable
solution to the problem.

Take, for instance, an idealized case of communicating with NASA's
Mars Reconnaissance Orbiter.  When they are closest together, Earth
and Mars are $4$ light minutes apart.  This means that any message
sent from one to the other will take *at least* $4$ minutes.

~~~~ {.sourceCode}
    The Sun             Earth     Mars
     /---\
     |   | <------------> o <----> o
     \---/     8 min         4 min
~~~~

When they are furthest apart, Earth and Mars are $20$ light minutes
apart.  Even worse, the Sun is between them at this point, making
communications impossible.

~~~~ {.sourceCode}
    Earth             The Sun                   Mars
                       /---\
      0 <------------> |   | <------------------> o
            8 min      \---/         12 min
~~~~

Given that the packet round-trip to Mars is at least $1200$ times
greater than the longest packet round-trip in today's Internet, it is
not hard to see how an acknowledgment/retransmission-based protocol
such as TCP would not work effectively.  Similarly, because of the
length of time in which communications are impossible, any system not
designed with severe partitioning in mind will probably not work.

### Disaster-Stricken Areas

Consider a disaster stricken area: a city hit by an earthquake or by a
storm.  Although many parts of the infrastructure can collapse after a
catastrophic event, "the breakdown of essential communications is one
of the most widely shared characteristics of all disasters".
\citep{Tow05} Interestingly enough, cell towers and other elements of
the centralized communications infrastructure fail, but individual
devices such as mobile phones or portable computers continue to
function.

The result of this sort of centralized communications failure is a
large number of nodes that can set up occasional connections between
themselves, but are disconnected from any global network.  For
instance, these connections could be set up via Bluetooth, NFC, or
ad-hoc WiFi networks.  We call these connections occasional because no
two nodes will be permanently connected, and there is no way of
predicting when two nodes will connect.

Because connections cannot be established between any two nodes
automatically, and because connection establishment between nodes is
effectively random, current replicated data store systems will not be
able to handle this case automatically.

### Developing and Rural Areas

Finally, consider developing countries or rural areas.  In both cases,
the ground-based communications infrastructure is either incomplete or
non-existing, and the only alternative is satellite communications.
Unfortunately, consumer-grade satellite links tend to be expensive,
lossy, and unpredictable.

~~~~ {.sourceCode}
       +--------+     +--------+
       |  |  |  |--0--|  |  |  |
       +--------+     +--------+
                   |
             /     |     \
            o      o      o

       A satellite communicating
       with three nodes.
~~~~

Although a traditional database system could function in such an
environment, it would also have to handle the inevitable network
partitions gracefully.  This is not often the case, as we will see in
Section \ref{sec:other-data-stores}.

\clearpage

Background
==========

<!-- What am I trying to achieve? -->

LTc is, first and foremost, a data store.  As such, we need to answer
a few questions about its design: how its data can be accessed, what
sort of data it can hold, what guarantees it makes, and over what
kinds of networks it can synchronize.

We begin this section by discussing how other data stores keep
replicated copies of the data synchronized.  We then move on to
answering the above questions, and discuss why each approach was
chosen in favour of its alternatives.

## Replicating Other Data Stores

\label{sec:other-data-stores}

As mentioned in Section \ref{sec:motivation}, our main reason for
writing LTc is because other data stores make certain strong
assumptions about the communications medium which make
synchronization over high-latency lossy channels hard.  We will now
look at how several data stores synchronize and see why exactly their
approach is problematic.

<!-- FIXME Mention The Fallacies of Distributed Computing:

https://en.wikipedia.org/wiki/Fallacies_of_Distributed_Computing -->

### Traditional Databases

Traditional relational databases such as
MySQL\footnote{\url{https://www.mysql.com/}},
PostgreSQL\footnote{\url{http://www.postgresql.org/}}, and Oracle
Database\footnote{\url{http://www.oracle.com/uk/products/database/overview/index.html}}
generally offer two different kinds of replication: master-slave
replication, where changes are only made on one master node, which
then propagates them to slave nodes, and clustering, where changes can
be made on any node, which are then propagated to every other node.

MySQL describes its replication mechanism as "asynchronous"
\citep{mysql:replication}, by which it means that the master node is
not permanently connected to the slave nodes; instead, the master
holds a log of all the changes made, which the slaves get at their
leisure, and then apply the same changes to their local copies of the
data \citep{mysql:replication-implementation}.  Apart from the obvious
downside that slaves cannot initiate changes, slaves also need a
complete log of the changes made by the master: if there were any gaps
in the log, they would not be able to guarantee data consistency.  So,
although MySQL replication would be suitable for high-latency
connections, it would not work over lossy ones.

MySQL clustering works by replicating the data across multiple nodes,
and making sure that changes on any node happen on all the other nodes
as well.  To achieve this, the clustered nodes need access to each
other to ensure that none commit changes unless all of them do
\citep{mysql:clustering-overview}.  So, all the nodes need to be
connected to the same low-latency network
\citep{mysql:clustering-requirements}.

PostgreSQL provides much the same options as MySQL, albeit with
different names \citep{postgresql:replication}.  "Transaction log
shipping" is PostgreSQL's counterpart of MySQL's replication, and
works by having the master send a stream of changes to the slaves.  It
suffers from the same problem in that it requires lossless connections
between master and slaves.  PostgreSQL's "synchronous multimaster
replication" works exactly like MySQL's clustering and shares the
requirement that nodes must be connected to a low-latency network.

Additionally, PostgreSQL supports "asynchronous multimaster
replication" through an external program:
Bucarado\footnote{\url{http://bucardo.org/wiki/Bucardo}} attempts to
keep several PostgreSQL databases in sync by propagating changes
between them.  On each node, on seeing a change to the database,
Bucarado connects to the other nodes, figures out the differences
between datasets, and tries to reconcile them.  Since changes can be
made on any node without having to wait for the other nodes, this
scheme is asynchronous.  Unfortunately, the actual reconciliation of
datasets is done through PostgreSQL transactions, which require
temporary synchronous connections between nodes.

Oracle Database's approach to replication is similar to messaging
systems such as JMS\footnote{Java Message Service} or
AMQP\footnote{Advanced Message Queuing Protocol}, and is a
generalization of the approaches of MySQL and PostgreSQL.  Oracle
Streams provides a mechanism for Oracle databases to publish and to
subscribe to events \citep{oracle:streams}.  Although this scheme
could work in an asynchronous fashion, it still requires stable
lossless connections between databases.

Oracle Database's clustering
solution\footnote{\url{http://www.oracle.com/technetwork/products/clustering/overview/index.html}}
is conceptually the same as MySQL's and PostgreSQL's, and requires a
low-latency network.

### NoSQL Data Stores

Unlike traditional SQL databases which are similar in many regards,
the various NoSQL data stores have very different approaches to
solving the problem of data storage and retrieval.  Despite this, they
use same replication schemes as traditional databases.

Consider \href{http://redis.io/}{Redis}, a widely used in-memory
key-value store.  Unlike traditional databases, it does not store any
persistent on-disk data, it only supports "tables" with two columns,
the key and the value, it does not store any relational information
about the data, but it instead supports storing more varied data
structures such as lists and sets.  Although its interface implies
much weaker consistency guarantees, Redis's only replication mechanism
is essentially the same as that used by traditional databases: when a
slave node is attached to the master, it first requests all the
current entries in the data store, and then gets a stream of changes
to that data \citep{redis-replication}.  In addition to requiring a
lossless connection for the stream of changes, this approach is
wasteful since it requires the slave to re-get the whole data set if
the stream of changes from the master is interrupted.

\href{https://couchdb.apache.org/}{CouchDB} is a widely used document
store.  Like Redis, it is conceptually a key-value store which holds
no relational information about the data , but, unlike Redis, it is
persistent, and the values are arbitrary queryable JSON *documents*.
Replication works the same as with traditional databases: a CouchDB
node connects to another and requests a stream of changes from it
\citep{couchdb:replication}.  Since CouchDB has conflict resolution
built-in, it is possible to do two-way replication, and thus have
multiple writable nodes.  This achieves the same effect as clustering
in traditional databases.  Additionally, since CouchDB versions the
documents inserted into it, it avoids the wasteful retransmission that
Redis does.  Unfortunately, getting the stream of changes requires
synchronous lossless connections between nodes.  That said, the
authors do not believe that this requirement is essential to the
replication scheme, and LTc's replication is similar, but designed to
work over lossy asynchronous connections.

### Distributed Version Control Systems

## Data Interface

<!-- FIXME Discuss the various interfaces LTc could expose and why we
chose KV. -->

When people talk about "relational", "NoSQL", or "logical" databases,
they refer to the data interface exposed to user.  The two main ways
to access data are through SQL interfaces, and various key-value
interfaces.  We explain how these are usually a tradeoff between
richness/complexity/difficulty-to-write/difficulty-to-reason-about,
and
simplicity/generality/scalability/ease-to-write/ease-to-reason-about.

~~~~ {.sourceCode}
  Simple ------------------------------------------ Complex
~~~~

### Relational

### Key-Value

\label{sec:kv-store}

Conceptually, LTc is a key-value store.  Its API has only three core
commands: `set <key> <value>`, `get <key>`, and `delete <key>`.  The
main reason behind this decision is simplicity: experience has shown
that key-value stores such as Cassandra, Dynamo, and Riak are the
simplest data stores that are still useful. \citep{wiki:NoSQL}

The main alternative designs are relational databases such as
PostgreSQL, and document stores such as CouchDB.  Implementing either
of these would have made LTc more useful, but it would have made
writing the data store itself much more difficult, and that is not the
focus of this project.  So, although, LTc is *not* designed as a
drop-in replacement for any existing piece of software, we believe
that the plugable architecture described in Section \ref{sec:plugable}
makes writing adapters a simple order of business.

~~~~ {.sourceCode}
    +----------------+----------------+
    | Key            | Value          |
    +================+================+
    | users          | alex,bob       |
    +----------------+----------------+
    + alex:karma     + 42             +
    +----------------+----------------+
    + alex:email     + alex@email.com +
    +----------------+----------------+
    + bob:karma      + 240            +
    +----------------+----------------+
    + bob:email      + bob@bob.me     +
    +----------------+----------------+

    An example key-value table populated with
    data for a social news website.
~~~~

Although, key-value store APIs are much less featureful than the SQL
APIs exposed by relational databases, we note that a recent trend has
been to use the latter strictly as the former.  For example,
\citet{Hof10} mentions that \href{http://www.reddit.com}{Reddit}, a
site with 270 million page views per month, achieves its performance
by using MySQL as a key-value store.

### Other

## Field Types

### No Types

<!-- File Systems, BerkleyDB?, Almost Redis. -->

### Primitive Types

<!-- SQL Databases, though the relational structures can be used to
build more complex types. -->

<!-- Document Stores, though documents themselves are richer types.
It's all Javascript, so meh. -->

### Rich Types

<!-- OO Databases -->

## Guarantees

### The CAP Problem

~~~~ {.sourceCode}
  +---------------+  +----------------+  +-----------------------+
  |  Consistency  |  |  Availability  |  |  Partition-Tolerance  |
  +---------------+  +----------------+  +-----------------------+

                          Pick TWO
~~~~

### CAp

### CaP

### ecAP

LTc is a distributed data store and one of the ways to characterize it
is in terms of Eric Brewer's CAP Theorem \citep{Gil02}.  The CAP
theorem simply states that a distributed service cannot provide all
three of the following guarantees: consistency, availability, and
partition tolerance.

In other words, when designing a distributed service, we must relax at
least one of the above guarantees.  We note that since network
partitions happen, we cannot relax the last guarantee. \citep{Vog08}
LTc takes this observation further: not only do network partitions
happen, the network *is* partitioned.

Since communication between nodes can be very slow, guaranteeing
consistency would mean that most operations have to be equally slow.
So, we relax the strict consistency guarantee and use *eventual
consistency*.  In other words, LTc nodes may have conflicting data
sets, but, given enough time, all the nodes will converge to The One
True Data Set.

The downside to using eventually consistent semantics is that the user
has to take into account that they may be operating on "old" data.

## Atomicity

### Atomic Read/Write

### MSET/MGET

### ACID

Orthogonal to which of the CAP guarantees LTc makes, we ask whether it
supports ACID transactions. \citet{wiki:ACID} defines ACID as the
following four characteristics which transactions need to have:
atomicity, consistency, isolation, durability.

LTc does *not* support any transactions at the moment, but they would
be a very useful future development.  Although this makes LTc less
useful, it does allow us to focus on the data store, and on the
synchronization protocols, both of which would be greatly complicated
by the addition of transactions.

So, ACID-wise, LTc only supports atomic, isolated, durable write
operations, but this is not as limiting as it first seems: widely used
data stores such as MongoDB and CouchDB have similar limitations.
\citep{mongodb-transactions} \citep{couchdb-transactions}

## Delay-Tolerant Network Model

\label{sec:dtn}

As mentioned in Section \ref{sec:motivation}, LTc's synchronization
mechanism is meant to work even on networks where packet round-trip
times are prohibitively large, and end-to-end connectivity may be
impossible.  The approach LTc takes with regards to communication is
not new, and is called Delay-Tolerant Networking\footnote{or,
\emph{Disruption}-Tolerant Networking as DARPA likes to call it}
(DTN).  Although research in DTN has been done since the first
computer networks were built, the pace has increased greatly in the
last decade.  More relevant to LTc, the
\href{https://www.ietf.org/}{IETF} has now published two RFCs about
the architecture of DTN systems, and the protocol used by them.

RFC 4838 \citep{rfc4838} outlines the reasons for developing DTN and
describes the high-level architecture of such networks.  Usefully, it
identifies the tacit assumptions made by most networked programs:

> "The existing Internet protocols do not work well for some
> environments, due to some fundamental assumptions built into the
> Internet architecture:
>
> - that an end-to-end path between source and destination exists for
>   the duration of a communication session
>
> - (for reliable communication) that retransmissions based on timely
>   and stable feedback from data receivers is an effective means for
>   repairing errors
>
> - that end-to-end loss is relatively small
>
> - that all routers and end stations support the TCP/IP protocols
>
> - that applications need not worry about communication performance
>
> - that endpoint-based security mechanisms are sufficient for meeting
>   most security concerns
>
> - that packet switching is the most appropriate abstraction for
>   interoperability and performance
>
> - that selecting a single route between sender and receiver is
>   sufficient for achieving acceptable communication performance"

As described in mentioned in Section \ref{sec:motivation}, and
described in Section \ref{sec:scenarios}, there are several
environments where the previous assumptions do not hold.  Protocols
that make these assumptions will not function as expected in such
environments, and programs relying on them will fail.  In particular,
this is the case for distributed data stores which perform many
network operations during synchronization.

### Bundle Protocol

Taking these assumptions into account and relaxing them, RFC 5050
\citep{rfc5050} describes the Bundle Protocol (BP), a protocol for DTN
networks.  \citet{rfc4838} again lists the guiding principles behind
BP.

> "The DTN architecture is conceived to relax most of these
> assumptions, based on a number of design principles that are
> summarized here:
>
> - Use variable-length (possibly long) messages (not streams or
>   limited-sized packets) as the communication abstraction to help
>   enhance the ability of the network to make good scheduling/path
>   selection decisions when possible.
>
> - Use a naming syntax that supports a wide range of naming and
>   addressing conventions to enhance interoperability.
>
> - Use storage within the network to support store-and-forward
>   operation over multiple paths, and over potentially long timescales
>   (i.e., to support operation in environments where many and/or no
>   end-to-end paths may ever exist); do not require end-to-end
>   reliability.
>
> - Provide security mechanisms that protect the infrastructure from
>   unauthorized use by discarding traffic as quickly as possible.
>
> - Provide coarse-grained classes of service, delivery options, and a
>   way to express the useful lifetime of data to allow the network to
>   better deliver data in serving the needs of applications.

As mentioned in Section \ref{sec:udp}, LTc uses UDP instead of BP, but
this decision was made because of the obscurity of BP, and not based
on the relative technical merits of the two protocols.  Despite this,
LTc is internally written as if it were using BP; for example, LTc
node names follow the same convention as BP endpoint ids.  Given this
policy, and because UDP has strictly fewer features than BP, adapting
LTc to BP in the future should not be difficult.

### UDP

\label{sec:udp}

As previously mentioned in Section \ref{sec:scenarios}, LTc cannot use
a transport protocol such as TCP, which assumes end-to-end
connectivity, and short round-trips between nodes.  Starting from the
first assumption outlined in Section \ref{sec:motivation}, LTc only
requires a protocol that uniquely identifies nodes, and allows at
least one-way communication between them.

Interestingly, the only widely used protocol that completely avoids
round-trips is UDP, which "provides a minimal, unreliable,
best-effort, message-passing transport". \citep{rfc5405}

The only alternative we could find is the Bundle Protocol (BP), which
is the subject of Section \ref{sec:dtn}.  Unfortunately, although it
is a better fit for LTc's requirements, BP is not widely deployed, and
relying on it would only make LTc more difficult to use and test.
However, because of the plugable architecture described in Section
\ref{sec:plugable} which we adopted, we believe that adding support
for BP would be a straightforward future extension.

Defining the LTc's synchronization protocol on top of UDP poses
significant problems.  First, UDP makes no guarantee that a sent
packet will be received by the destination.  More problematically,
there is no way for the source to tell if a packet was lost or not.
Worse yet, even assuming a perfect connection that does not lose
packets, if the destination does not process them quickly enough, its
UDP buffer will spill over causing lost packets.  So, LTc's
synchronization mechanism must be able to make forward progress even <!-- FIXME: Susan: Explain incomplete updates -->
if only incomplete updates are available.  Second, UDP makes no
guarantee that sent packets will be received in order.  This is
problematic because the order of updates is important.  Finally, UDP
makes no guarantee that a sent packet will not be received multiple
times.  Again, this is problematic because updates should only be
applied once.  Because BP was designed with systems like LTc in mind,
if we used it, it would solve or alleviate all these problems.

### Other Protocols

Needless to say, LTc would work well on models that offer more
guarantees.

\clearpage

<!-- How am I achieving it? -->

# Design and Implementation

## Outside View

<!-- FIXME Say how an LTc systems looks like from the outside: on the
network (nodes, UDP connections, etc.), on the machine (processes,
threads, directories). -->

## Haskell

## Component Architecture

### The Static View: Plugable Architecture

\label{sec:plugable}

<!-- FIXME Mention its a choice between complexity/flexibility, and
simplicity/staticity -->

<!-- FIXME Break down by plugable component: explain what each
plugable enables. -->

Because LTc is a research project, we are not entirely sure what it
will look like in the end.  In particular, we do not know exactly what
specific technologies and algorithms it will use, and the way they
will work together.  In light of this, we have opted for a decoupled
architecture, where components interact with one another only through
well-defined APIs.  This architecture was heavily influenced by the
design of \href{http://xmonad.org/}{XMonad} and
\href{http://jaspervdj.be/hakyll/}{Hakyll}.

Fundamentally, there are only a few kinds of components in LTc:
stores, clients, and proxies.  Because LTc is written in Haskell, the
APIs for these components is specified with type-classes.

Stores expose the key-value store API described in Section
\ref{sec:kv-store}.  They are responsible for storing entries
persistently, and for managing the change history for each value.  For
instance, the "Ltc.Reference" store is the straightforward
implementation of a key-value store on top of a file system, where
each entry is represented by a file.  Although it works, it is grossly
inefficient in terms of disk-space.  Thanks to the store abstraction,
different implementation could be tested without affecting the rest of
LTc's code.

Clients serve as high-level "handles" for working with stores.  They
are short lived, usually created in response to some external event,
and may expose a different API than underlying store.  For instance,
in addition to the basic client which just re-exposes the store API,
we could have a Redis client, which exposes Redis's API and is
responsible for translating Redis API calls to store API calls.

Proxies are abstractions around some external interface; they are both
the only way for an external entity to interact with an LTc node, and
the only way for an LTc node to interact with the outside world.  For
instance, the UDP proxy is responsible for listening on an UDP port,
spawning clients in response to received messages, and sending
messages in response to internal events.  Other proxies would offer
support for different protocols such as BP, or Redis.

~~~~ {.sourceCode}

    +------+  +------+
    | LTc  |  | LTc  |
    | Node |  | Node |   ...
    +------+  +------+
       |        |

    +-- LTc node ---------------------------+
    |                                       |
    |   |  |  |     | | |           |       |
    | +--------+  +-------+     +-------+   |
    | |   UDP  |  |  BP   |     | Redis |   |
    | |  Proxy |  | Proxy |     | Proxy |   |
    | +--------+  +-------+     +-------+   |
    |         |    |               |||      |
    |          \  /             +--------+  |
    |           ||              | Redis  |  |
    |       +-------+      /--- | Client |  |
    |       | Store | ----/     +--------+  |
    |       +-------+                       |
    +---------------------------------------+

       An LTc node, connected to other LTc nodes
       through UDP and BP proxies, and which
       exposes a Redis-like interface through
       a Redis proxy.
~~~~

We take an unusual approach to configuring LTc nodes: we use an
XMonad-like DSL to choose which components are active in a node; a
more orthodox choice would have been to use XML.  The downside of our
approach is that users need to know a bit of Haskell's syntax in order
to configure LTc.  On the other hand, it makes the system much easier
to tweak by advanced users, and it is the Haskell way of approaching
this problem.

### The Dynamic View: Actor-Model Concurrency

\clearpage

Type Safety
===========

## Types in Database Interfaces

## A Strongly Typed Database Interface

<!-- Mention Redis's stringly typed interface -->

### GADTs

### Phantom Types

\clearpage

Changes
=======

## Patches

## Conflict Resolution

\label{sec:conflicts}

We expect communication between LTc nodes to be difficult, and since
updates can occur on each node, we expect the data sets to diverge in
conflicting ways.  As such, conflict resolution will play a key role
in the operation of LTc.

Because conflict resolution must happen automatically in most cases,
we make two design choices meant to increase the amount of information
available during resolution.  First, we timestamp events with vector
clocks, which allows us to establish causal relationships between
them; this is the subject of Section \ref{sec:vector-clocks}.  Second,
we store at least some change history for every field in the database,
much like a distributed version control system.  This should allow us
solve some conflicting changes by "merging"; this is the subject of
Section \ref{sec:patch-theory}.

~~~~ {.sourceCode}
    Other key-value stores          LTc key-value store with changes

    +---------------+-----------+   +---------------+--------------+
    | Key           | Value     |   | Key           | Value        |
    +===============+===========+   +===============+==============+
    | alex:ballance |  120$     |   | alex:ballance |    0$        |
    +---------------+-----------+   |               |  v | +200$   |
                                    |               |    200$      |
                                    |               |  v | -80$    |
                                    |               |    120$      |
                                    +---------------+--------------+
~~~~

The use of vector clocks and storing changes are complications due to
the scope of the problem.  Conflict resolution mechanisms employed by
other data stores such as consensus and majority voting \citep{Vog08}
cannot be used by LTc because of the large delays in communications
between nodes.

## Patch Theory

\label{sec:patch-theory}

As mentioned in Section \ref{sec:conflicts}, we expect parallel
updates to the data sets on different nodes to cause conflicts.
Relational databases data stores usually solve this issue by
coordinating the nodes such that conflicts cannot appear in the first
place.  Because of the disconnected nature of LTc, this is not an
option, so LTc nodes have to deal with conflicts as they appear.
DVCSs such as git face a similar problem, which they solve by
attempting a textual merge of the conflicting changes, and falling
back to manual intervention.  NoSQL data stores usually adopt an
automated version of previous scheme; for instance, CouchDB detects
conflicts, but lets the user application decide how the merging should
be done. \citep{And10} Because both these approaches work well in
practice, in LTc, we first attempt to solve conflict through automatic
merging, and fallback to application specific conflict resolution.

Among DVCSs, \href{http://darcs.net/}{Darcs} is the only one with a
mathematical formalism underlying its behaviour.  This formalism is
known as Patch Theory, and is a way of reasoning about what happens
when two repositories exchange patches.  It begins by defining basic
concepts such as patches, and changes, and the properties these may
have.  For instance, some patches have inverses, and some pairs of
patches are commutative; if all the patches participating in a
conflict have these two properties, it is always possible to perform
an automatic merge.  As other DVCSs have shown, it is possible to
write a working system without any formalization, but for an
experimental system such as LTc, we believe that a formalism will be
indispensable.

Unfortunately, Patch Theory is very much an open area of research;
there are no published works on the topic, but the
\href{http://darcs.net/Theory}{Darcs Theory} page links to several
talks and unfinished articles.

## Versioning with Vector Clocks

\label{sec:vector-clocks}

Many distributed systems need to create globally unique identifiers
for events, and to determine a global logical ordering for them.  This
is definitely the case for LTc, which attempts to store historic
changes in addition to just the values of entries, and needs an
unambiguous way of referring to these changes.  There are many ways of
generating these identifiers, and in this section, we look at vector
clocks, which are a general way of achieving this while preserving
enough information to determine causal relationships among events.

Consider the following problem: We have two nodes, A and B.  There are
events that occur internally in each node, and nodes may send messages
to each other, causing events to occur in the destination node.
Uniquely identifying events inside a node is simple: we just keep a
counter that is incremented every time an event occurs.  In order to
uniquely identify events globally, we just have to include the node's
id inside the event id.  So, events are identified by pairs `(node-id,
counter)`.

Now that we have unique identifiers, we need a way to order them.  It
is interesting to discuss what "ordering" means in this case.  It
cannot mean chronological ordering, because clocks in distributed
systems are rarely synchronized well enough to allow this.
Furthermore, according to Special Relativity, time passes at different
rates for observers moving at different speeds, so it does not make
sense to talk about a "global" time. \citep{wiki:SR} Because
chronological ordering is not an option, we must use a logical
ordering.

With our identification scheme, we have a total ordering for events
within each node, but no global ordering for events.  \citet{Lam78}
describes an algorithm by which it is possible to establish a partial
global ordering among events in a distributed system.  In addition to
the previous identification scheme, we only need the following rule:
"When a node receives a message, it updates its counter to be the
maximum of the its current value, and the counter of the received
event".  Given this, we can state that an event cannot happen before
another, unless the counter of the first one is less than the counter
of the second.  Unfortunately, this does not provide us with enough
information to state anything more about the order of two events,
given only their ids.

~~~~ {.sourceCode}
    Event timeline for Node A: (A, 1); (A, 2)
    Event timeline for Node B: (B, 199); (B, 200)

    B sends "(B, 200)" to A.

    Event timeline for Node A: (A, 1); (A, 2); (B, 200); (A, 201)
    Event timeline for Node B: (B, 199); (B, 200)

       After Node A receives B's message, it updates its own
       counter to be 200, so the next event will have a counter
       value of 201.
~~~~

In order to get more ordering relations we need to increase the amount
of information in the event ids.  \citet{Bal02} describe vector
clocks, a scheme by which event ids contain a vector of the counters
for all the nodes the event has "traveled through".  Additionally,
each node uses a vector clock to generate ids for new events.  Upon
receiving an event, a node updates its vector clock entry-by-entry
using the Lamport clock rule for entries present in both clocks, and
by copying over entries present only in the event's clock.  Thus, a
node's vector clock is conceptually the list of all the events the
node is aware of and which may have affected its behaviour.

~~~~ {.sourceCode}
    Event timeline for Node A: [(A, 1)]; [(A, 2)]
    Event timeline for Node B: [(B, 199)]; [(B, 200)]

    B sends "[(B, 200)]" to A.

    Event timeline for Node A: [(A, 1)]; [(A, 2)];
                                 [(A, 2), (B, 200)];
                                 [(A, 3), (B, 200)]
    Event timeline for Node B: [(B, 199)]; [(B, 200)]

        After Node A receives B's message, it updates its own vector
        clock to be [(A, 2), (B, 200)], so the next event will have a
        vector clock of [(A, 3), (B, 200)].
~~~~

With vector clocks, we can finally get the ordering relations we
wanted.  Given two events, $e$, and $f$, where $\text{VC}(e)$ is the
event's vector clock, and $\text{VC}(e)[k]$ is the $k^\text{th}$ entry
of that vector clock, we have:
\vspace{-0.6cm}
\begin{align*}
  &\forall e, f.\ e \neq f \Rightarrow\\
  &\qquad\Big( (e < f) \Leftrightarrow (\forall k.\ \text{VC}(e)[k] \leq \text{VC}(f)[k]) \land (\exists k.\ \text{VC}(e)[k] < \text{VC}(f)[k]) \Big)
\end{align*}

By adding vector clocks to events in LTc, we gain the ability to
sometimes determine causal relationships among them.  This is very
important because, in the event of a conflict, using the "most recent"
value is a probably a good enough resolution \citep{Vog08}, and we
would need the causal relationships to determine which that is.

The downside of using vector clocks is that they add considerable
overhead.  As is obvious from the vector clock update rule, they grow
by one entry every time an event passes thorough a new node.  If our
assumption that the network of LTc nodes is strongly connected holds,
eventually, every event will carry a vector clock with entries for all
the nodes.  Ultimately, vector clocks are on a sliding scale tradeoffs
between overhead and scope.  If we wanted less overhead, we would use
Lamport timestamps, and if we wanted more causal relations to be
detected, we would use Matrix Clocks \citep{Bal02}.

## Epidemic Updating

In the previous sections, we mentioned some of the issues surrounding
the synchronization of two nodes, but we did not discuss the way
updates are propagated through the network of LTc nodes.  In other
words, when a node sees an update to one of its entries, how does it
propagate the update to all the other nodes that hold a copy of the
data set?

Because of the disconnected nature of the network, we do not have much
choice in the matter, and updates can only be propagated on a
node-by-node basis.  We note that this is the same problem routers
that are in partially connected have when updating their routing
tables.  "When instantaneous end-to-end paths are difficult or
impossible to establish, routing protocols must take to a 'store and
forward' approach, where data is incrementally moved and stored
throughout the network in hopes that it will eventually reach its
destination."  \citep{wiki:dtn-routing}

Our update propagation algorithm will be a variant of epidemic
routing: When a node becomes "infected" by an update, it seeks out
uninfected nodes, and infects them.  After some time has passed, an
update is assumed to have propagated through the network, and ceases
to be infectious. \citep{Vah00} Thus, the update executes a
breadth-first walk of network graph.

~~~~ {.sourceCode}
    A    B    C           Day 1: None of the three nodes is
    o    o    o                  infected.

    A    B    C           Day 2: B is "infected" by an update.
    o    I    o

    A    B    C           Day 3: C connects to B and is infected
    o    I -- I                  by the update.

    A    B    C           Day 4: B does not consider the update
    o    i    I                  infections any more.

    A    B    C           Day 5: A connects to B, but does not
    o -- i    I                  receive the update.

       The propagation of an update through a partially
       connected network of three nodes.  Ultimately, the
       update does not fully propagate due to it ceasing to
       be infectious too soon in B.
~~~~

The algorithm outlined above is rumor mongering, and it is what LTc
will initially use.  By changing the way a nodes selects other nodes
to infect, and the time until an update is no longer considered
infections, several variations of the basic algorithm
arise. \citep{wiki:dtn-routing} Exploring which of these is best
suited for LTc is a future path for development.

\clearpage

Correctness
===========

## Empirical Correctness

### Unit Testing

### Random Testing

### Exhaustive Testing

## Provable Correctness

### Global Properties

<!-- This is what we want to prove. -->

### Local Properties

<!-- This is what we konw. -->

### Proof

\clearpage

Evaluation Plan
===============

We evaluate LTc in terms of what it can do (Section
\ref{sec:functionality}), and how well it does it (Section
\ref{sec:performance}).  Broadly speaking, LTc should have the
interface and behaviour of a persistent map/hashtable/dictionary, it
should be possible to synchronize entries from one LTc node to
another, and the synchronization should work over bad connections.

If all goes well, we will have developed and benchmarked novel
synchronization methods for replicated data stores over bad
connections.

## Functionality

\label{sec:functionality}

In terms of functionality, LTc should expose at least the following
commands: `set <key> <value>`, `get <key>`, and `delete <key>`.  These
commands should have the obvious meanings, and should be atomic and
persistent.  By persistent we mean that once they return, the changes
have been recorded to persistent storage.  For testing, we can use a
random tester that runs the commands with dummy data, occasionally
killing the program, all the while checking invariants.

Additionally, LTc nodes should be able to push updates to other nodes.
For this, LTc should expose at least a `sync-to <node-id>
<node-location>` command that performs the to for the specified peer.
For testing, we can use a random tester that creates two nodes,
connects them through a channel that loses the majority of packets,
inserts some dummy data into each node, then runs the `push` command
repeatedly, expecting the two data sets to eventually become
consistent.  If we complete Phase X, we would need to perform the
above test for networks of nodes, and not just for pairs of nodes.

### Almost-Drop-In Replacement for Redis

### Writing a Address Book Example

<!-- Emphasis on how hard it would be to write with something
else. -->

### Writing a Collaborative Editor Example

<!-- Emphasis on how hard it would be to write with something
else. -->

### Large Amounts of Data

<!-- The perils thereof. -->

## Performance

\label{sec:performance}

### Read/Write Throughput

In terms of performance, we are interested in several measures.
First, we need to know the throughput of the key-value store.
Specifically, we need to find the number of reads and writes per
second it can sustain on magnetic disks and solid-state drives.
Although this is not strictly the project's focus, it seems a waste
not to measure, and it may be useful in interpreting the following
measures.

### Round Trips vs. Deteriorating Links *

Second, we need to determine the number of times two nodes need to
synchronize in order to reach consistency, when connected over
channels of decreasing quality.

### Protocol Overhead

Third, we need to measure the synchronization protocol's overhead,
defined as the size of the data sent, and possibly lost, divided by
the total size of the entries that needed synchronization.

\clearpage

Conclusions
===========

<!-- What I achieved. -->
<!-- What I learned. -->
<!-- What I found easy. -->
<!-- What I found hard. -->

\clearpage

Future Work
===========

<!-- What future does this program have? -->
<!-- Will I continue working on it? -->
<!-- Will anyone use it? -->
<!-- What future research opportunities there are? -->

\clearpage
\appendix

User Guide
==========

<!-- README: How to setup and run? -->
<!-- Haddock docs -->
