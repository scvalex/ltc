\begin{abstract}

A frequent problem that appears in computing is keeping replicated
data sets synchronized.  Solutions to this problem include distributed
version control systems such as git, traditional databases such as
MySQL, and modern NoSQL data stores such as Redis.  All these make
tacit assumptions about the communication channel; for instance, all
assume that the round-trip times between nodes is relatively short.
Although these assumptions hold withing data centers, and sometimes,
on the Internet, they do not hold for some extreme situations such as
interplanetary communications.  LTc aims to define and implement
synchronization protocols for such situations.

\end{abstract}

\tableofcontents

\clearpage

Introduction
============

LTc is a key-value store designed so that replicated data sets can be
synchronized over lossy connections where end-to-end connectivity may
not be available.  Examples of environments where this is the case
"include spacecraft, military/tactical, some forms of disaster
response, underwater, some forms of ad-hoc sensor/actuator networks,
and Internet connectivity in places where performance may suffer such
as developing parts of the world." \citep{dtnrg}

<!-- The final report should include a discussion of how each of the above -->
<!-- environments is bad for communications. -->

## Motivation

Current systems such as distributed version control systems (DVCS),
traditional databases (DBMS), and modern NoSQL data stores are
unsuitable for such situations because they make tacit assumptions
about the communications medium.  We discuss these assumptions in
detail in Section \ref{sec:dtn}, but, broadly speaking, these systems
assume that the communication channel uses a protocol, with strong
reliability and ordering guarantees, such as TCP/IP.

Unlike other systems, LTc makes only the following explicit
assumptions.  First of all, nodes\footnote{when speaking of nodes, we
mean machines that hold (possibly different versions of) the same data
set} may send messages to other nodes occasionally.  This means that
an LTc node does *not* expect to be able to send messages to another
at any time, it does *not* expect that all the sent messages will
reach the other node, and it does *not* expect an immediate reply to
the sent messages.  Secondly, it is possible for all nodes to
communicate with each other, perhaps indirectly.  In other words, if
we construct a directed graph, where the LTc nodes are vertices, and
the communication channels between nodes are directed edges, the graph
is strongly connected.  We note that these assumptions hold for all
the environments mentioned above, and discuss this in Section
\ref{sec:scenarios}.

## Usage Scenarios

\label{sec:scenarios}

There are several environments in which current systems will not work
effectively or at all, but for which LTc is designed.  In this
section, we look at what issues arise in each environment, and how
they impact communication channels.

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
for messages are too large for packet retransmission to a viable
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

~~~~ {.sourceCode}
      o              o
     /    o -- o     |
    o  o             o
~~~~

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
~~~~

Although a traditional database system could function in such an
environment, it would also have to handle the inevitable network
partitions gracefully.  This is not often the case.  For instance,
Redis requires nodes to perform a re-synchronization of all the data
once the connection has been re-established \citep{redis-replication}.

Design Decisions
================

## Key-Value Store

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

## ecAP

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

## ACID?

## DVCS Semantics

conflict resolution through Patch Theory (because communication
between nodes is very difficult, so the usual consensus or voting
algorithms are not feasible)

## UDP

## Epidemic Updating

## Plugable Internal Architecture

Background
==========

> The background section of the report should set the project into
> context by relating it to existing published work which you read at
> the start of the project when your approach and methods were being
> considered. There are usually many ways of solving a given problem,
> and you shouldn't just pick one at random. Describe and evaluate as
> many alternative approaches as possible. The published work may be
> in the form of research papers, articles, text books, technical
> manuals, or even existing software or hardware of which you have had
> hands-on experience. Your must acknowledge the sources of your
> inspiration. You are expected to have seen and thought about other
> people's ideas; your contribution will be putting them into practice
> in some other context. However, avoid plagiarism: if you take
> another person's work as your own and do not cite your sources of
> information/inspiration you are being dishonest; in other words you
> are cheating. When referring to other pieces of work, cite the
> sources where they are referred to or used, rather than just listing
> them at the end. Make sure you read and digest the Department's
> plagiarism document .

> In writing the Background chapter you must demonstrate your
> capability of analysis, synthesis and critical judgement. Analysis
> is shown by explaining how the proposed solution operates in your
> own words as well as its benefits and consequences. Synthesis is
> shown through the organisation of your Related Work section and
> through identifying and generalising common aspects across different
> solutions. Critical judgement is shown by discussing the limitations
> of the solutions proposed both in terms of their disadvantages and
> limits of applicability.

## DTN

\label{sec:dtn}

## Patch Theory

## Vector Clocks

Project Plan
============

## Phase 0

> Deadline: end of October

Research and document exactly what issues are likely to be encountered
when two communicating systems are physically separated by large
distances.  Specifically, determine why TCP would not work (and at
what point in breaks down), whether UDP or another existing protocol
would work, and how "intermittent" the connection is likely to be.

## Phase 1

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

## Phase 2

> Deadline: end of January

Find out how NASA and ESA are currently getting information to and
from the Mars orbiters and rovers.  Invent and implement a
synchronization protocol for the replicated nodes.  This will probably
involve sending large sets of updates at once.  Entries will probably
need to be versioned with a scheme like vector-clocks.

## Phase 3

> Deadline: mid March

Since we are unlikely to get access to a computer far enough to test
LTc's synchronization, develop a Linux kernel module that creates a
loopback network device that is high-latency and intermittent.  Using
this, test and tune the synchronization algorithm.

## Phase 4

> Deadline: mid May

Develop an easy-to-use monitoring and statistics interface for LTc.
Use the statistics to automatically tune the synchronization algorithm
(e.g. we probably want different behaviours if the other node is 100ms
away, and if it is 30min away).  Polish everything and come up with a
convincing demo.

Evaluation Plan
===============
