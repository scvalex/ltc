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

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
The Sun             Earth     Mars
 /---\
 |   | <------------> o <----> o
 \---/     8 min         4 min
\end{lstlisting}
\end{tabular}
\end{center}

When they are furthest apart, Earth and Mars are $20$ light minutes
apart.  Even worse, the Sun is between them at this point, making
communications impossible.

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
Earth             The Sun                   Mars
                   /---\
  0 <------------> |   | <------------------> o
        8 min      \---/         12 min
\end{lstlisting}
\end{tabular}
\end{center}

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

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
+--------+     +--------+
|  |  |  |--0--|  |  |  |
+--------+     +--------+
            |
      /     |     \
     o      o      o

   A satellite communicating
   with three nodes.
\end{lstlisting}
\end{tabular}
\end{center}

Although a traditional database system could function in such an
environment, it would also have to handle the inevitable network
partitions gracefully.  This is not often the case, as we will see in
Section \ref{sec:other-data-stores}.

\clearpage

Design
======

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

### Traditional Databases

\label{sec:other-traditional-databases}

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

For completeness, we also mention
\href{http://www.mongodb.org/}{MongoDB}, the other widely used
document store.  From a usage point of view, it provides roughly the
same interface and features as CouchDB, and handles replication using
a similar scheme fraught with the same problems
\citep{mongodb:replication}.

### Distributed Version Control Systems

### Conclusion

In this section, we have looked at how some traditional SQL databases,
NoSQL data stores, and distributed version control systems handle
replication of data across nodes.  We have seen that they generally
follow one of two schemes: the first is based around the idea of
propagating changes to the data between nodes such that they all apply
the same operations, and the second is based around the idea of
blocking write operations until all nodes commit to making the change.
Although only the latter scheme is inherently synchronous, in
practice, either due to assumptions about the network medium, or as an
artifact of the environment in which they were written, both schemes
tend to be implemented in synchronous ways.  Furthermore, all
implementations invariably assume the existence of lossless
connections between nodes.  These two characteristics make them
unsuitable for the environments describe in Section
\ref{sec:scenarios}.

## Data Interface

Now that we have seen what problems existing data storage systems
present, we explain just what sort of data store LTc is, and how these
choices relate to data replication.  We begin with the data interface
LTc exposes.

When people talk about "relational", "NoSQL", or "logical" databases,
they are referring to the ways users can access stored data.
Traditionally, the main paradigm has been relational, but with the
advent of the Web over the past decade, key-value stores have been
gaining increased popularity.  In the following two subsections, we
briefly present the two paradigms, and explain why we chose key-value
for LTc.

### Relational

Relational databases conceptually store data of a few basic types into
arbitrary tables connected by constraints.  Consider the following
\href{https://sqlite.org/}{SQLite} commands which define two tables:

~~~~ {.sourceCode}
sqlite> CREATE TABLE users ( id INTEGER PRIMARY KEY
                           , name STRING );
sqlite> CREATE TABLE languages ( id INTEGER PRIMARY KEY
                               , language STRING
                               , user INTEGER
                               , FOREIGN KEY (user) REFERENCES users(id) );
~~~~

The `users` table has two rows: `id` which holds integers, and `name`
which holds strings.  Furthermore, `id` is a primary key, which
effectively means that all rows must have different values on the `id`
column.

The `languages` table has three rows: `id` which holds integers and is
the primary key, `language` which holds strings, and `user` which
holds integers.  Furthermore, we define `user` as a foreign key into
`users(id)`, which effectively means that all rows must have some value
on the `user` column that is present on the `id` column of some row in
the `users` table.

We see the primary key and foreign key constraints in actions by
inserting some values into the tables:

~~~~ {.sourceCode}
sqlite> INSERT INTO users VALUES (1, "alex");
sqlite> INSERT INTO users VALUES (1, "mike");
Error: PRIMARY KEY must be unique
sqlite> INSERT INTO users VALUES (2, "mike");
sqlite> INSERT INTO languages VALUES (1, "english", 1);
sqlite> INSERT INTO languages VALUES (2, "english", 2);
sqlite> INSERT INTO languages VALUES (3, "french", 2);
sqlite> INSERT INTO languages VALUES (4, "french", 3);
Error: foreign key constraint failed
~~~~

The second insertion into `users` fails because the value for the `id`
column, $1$, has already been used on the other row.  The last
insertion into `languages` fails because the value for the `user`
column, $3$, has not been used as the `id` of any row in `users`.

Having populated the tables with some values, we display them:

~~~~ {.sourceCode}
sqlite> SELECT * FROM users;
1|alex
2|mike
sqlite> SELECT * FROM languages;
1|english|1
2|english|2
3|french |2
sqlite> SELECT name, language
        FROM users JOIN languages
        WHERE users.id = languages.user;
alex|english
mike|english
mike|french
~~~~

The first two commands are unsurprising: they merely display the
`users` and `languages` table.  The third command, however,
illustrates a key feature of relational databases, the ability to
combine tables in queries.  Here, we combine (or join) the two tables
on the `id` column in `users` and the `user` column in `languages`.
Since the foreign key constraint holds, we know that all rows in the
`languages` table will be matched with some row of the `users` table.

A difficulty in implementing relational databases is ensuring that
constraints are not broken by changes to the data.  Although this is
not particularly difficult to guarantee if the entire database is on a
single machine, it becomes much harder in a distributed context.

We have seen in Section \ref{sec:other-traditional-databases} that, in
practice, relational databases generally have two data replication
mechanisms.  Of these, simple replication is the only asynchronous
scheme and involves sending a stream of changes from the master node
to the slaves.  It is easy to see now why this stream must be
lossless: if the master applies any change which the slaves do not
receive, they will end up checking constraints against a different
data set than the master, and may find failures where there are none;
so, it would be impossible to check consistency on the slaves.

Since LTc's replication needs to work over lossy connections, LTc
cannot support the kinds of constraints that are central to relational
databases.  So, a true relational data interface is out of the
question.  With this in mind, we look at another interface to data
which has gained much traction in recent years: key-value stores.

### Key-Value

\label{sec:kv-store}

Unlike relational databases which store tables of interconnected data,
key-value stores only associate keys with values, and the individual
values are entirely independent.  In terms of their interface, they
are more like the map or dictionary data structures present in every
programming language's standard library than relational databases.

Key-value stores have gained prominence in recent years and are now
used throughout the industry.  To list a few examples: Redis is used
by Twitter, GitHub, StackOverflow \citep{redis:who-uses}; CouchDB is
used by Engine Yard, and Credit Suisse \citep{couchdb:in-the-wild};
MongoDB is used by SAP, MTV, and Disney
\citep{mongodb:production-deployments}.

Although, the interfaces of key-value stores are much less featureful
than those exposed by relational databases, we note that a recent
trend has been to use the latter strictly as the former.  For example,
\citet{Hof10} mentions that \href{http://www.reddit.com}{Reddit}, a
site with $270$ million page views per month, achieves its performance
by using MySQL as a key-value store.

By transliterating the previous section's example into Redis, we see
the main characteristics of key-value stores.  Since there is no
schema to define, we skip the "create tables" step, and simply insert
the data.

~~~~ {.sourceCode}
redis> set user:1:name alex
redis> sadd users 1
redis> set user:2:name mike
redis> sadd users 2
redis> sadd languages:1 english
redis> sadd languages:2 english
redis> sadd languages:2 french
~~~~

All the above operations insert data into the data store: `set`
associates the key in its first argument with the value in its second
argument; `sadd` adds its second argument to the set keyed by its
first argument.  Note that the key "user:1:name" is just string; the
logic that it means "the field name of user 1" is handled solely by
the application logic.  The resulting data store looks like this:

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
+----------------+----------------------+
| Key            | Value                |
+================+======================+
| users          | SET(1, 2)            |
+----------------+----------------------+
| user:1:name    | alex                 |
+----------------+----------------------+
| user:2:name    | mike                 |
+----------------+----------------------+
| languages:1    | SET(english)         |
+----------------+----------------------+
| languages:2    | SET(english, french) |
+----------------+----------------------+
\end{lstlisting}
\end{tabular}
\end{center}

We have adopted a similar key-value interface for LTc.  Conceptually,
its API has only two core commands: `set <key> <value>`, and `get
<key>`.  The main reason behind this decision is simplicity: the
key-value interface is the simplest one that is still useful in
practice.

One complication introduced by using a key-value interface is that
there is no standardized one \citep{Zicari12}: every key-value store
exposes a different interface.  The issues with this situation is that
it makes transitioning from one data store to another difficult and it
introduces a learning curve which slows down adoption.  LTc is no
different in this regard, but attempts to alleviate the problem by
exposing multiple interfaces compatible with existing systems; more
specifically, LTc supports a subset of Redis's protocol and commands;
we discuss the details in Section \ref{sec:plugable}.

### Other

## Field Types

Now that we know that LTc is a key-value store, the question arises:
what sort of values can it hold?  The answer lies somewhere along the
spectrum from "only strings", to "any data type".  In this section, we
examine what other systems have done, and use them to put LTc's choice
into perspective.

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
             ...              \
             ...              |
 |  \ /  |    x   | \ /  |    > Any Types:
 |   x   |   / \  |  x   |    |  LTc
 |  / \  |  /   \ | / \  |    /  Object Databases
 | /   \ | /     \|/   \ |
 |/     \|/       |     \|
int   string    float   ...   - Primitive Types:
   \     \       /     /         SQL Databases
     \     \   /     /           NoSQL Document Stores
       \     |     /
         \   |   /
           \ | /
             *                - No Types:
                                 Most Filesystems
                                 Redis
\end{lstlisting}
\end{tabular}
\end{center}

### No Types

The most common storage systems are file systems, and they also tend
to be simplest in terms of what data types they can store.  POSIX file
systems \footnote{This is not the case for other file systems like the
Be File System, or HFS+ which store metadata along with file
contents}, in particular, only store binary strings \footnote{Note
that permissions and ownership data do not refer to the contents of
the file, but rather to how the file can be used.}.  The advantage of
viewing values as opaque binary blobs is that it makes the storage
system as general as possible: anything that can be serialized can be
stored and no modification to the storage system is required.  The
downside is that it puts the burden of type safety squarely on the
user's shoulders; the end result is as expected: many programs use
heuristics to determine the types of the contents of files, and the
heuristics often give wrong results\footnote{The author cannot count
the number of times he has had to manually tell his text editor that
it is looking at a Prolog file, and not a Perl file, even though they
both use the same extension}.

Redis takes a similar approach: it can store strings, sets of strings,
and lists of strings \citep{redis:types}.  Additionally, it sometimes
interprets some strings as integers, but there is nothing to stop a
misbehaving application from setting what is obviously an integer
field to some arbitrary string.  Again, the issue with this approach
is that it provides no safety and places the entire responsibility of
type checking on the user.

### Primitive Types

\label{sec:primitive-types}

The next step along the spectrum is to support storing some primitive
types in addition to strings.

Relational databases are like this and those that adhere to the SQL
'92 standard are capable of storing at least strings, bit strings,
several kinds of integer and floating point numbers, arbitrary
precision numbers, times, and dates \citep{sql-language}.

Modern document stores also support multiple primitive data types.
CouchDB stores data as JSON\footnote{"A lightweight data-interchange
format ... based on a subset of the JavaScript Programming Language";
\url{http://www.json.org/}}, and supports integer and floating point
numbers, dates, timestamps, arrays, dictionaries, and JavaScript
\citep{couchdb:json}.  MongoDB stores data as BSON\footnote{"A
binary-encoded serialization for JSON-like documents";
\url{http://bsonspec.org/}}, and supports roughly the same types as
CouchDB \citep{mongodb:bson-types}.

From a type-safety point of view, supporting some primitive types is
better than nothing, and indeed, it is often "good enough" since it
covers the most common use cases, but it still requires users to
manually disassemble their types into primitive components before
inserting them into the data store, and to manually reassemble them
when extracting.

### Any Types

Finally, some systems support storing any data types.  The fine print
is that only data types with some specific characteristics are
supported: they usually have to be serializable, and their
representations have to be finite\footnote{this usually rules out
storing functions and infinite data types}.

Object databases are the most famous examples of systems that support
storing any data type.  For instance,
ZODB\footnote{\url{http://zodb.org/}}, "a native object database for
Python", is capable of storing almost any Python object on disk, but
only if it inherits the `persistent.Persistent` class.

Since such systems can store arbitrary data types with little
additional programming effort, they are often used as a persistence
layer for applications.  ZODB, mentioned above, is the persistence
layer for Zope\footnote{\url{http://zope.org/}} and several associated
content management systems widely used on the Internet.

Since this is the most general approach, is also the approach LTc
takes.  We discuss the implications of this decision in detail in
Section \ref{sec:strongly-typed}, but, roughly speaking, any Haskell
value that can be serialized and diffed can be stored in LTc.

## Consistency Guarantees

So far, we have seen that LTc is a distributed key-value data store
that can hold any serializable Haskell value.  We now consider what
guarantees it makes about its replicated data when multiple nodes are
involved.

### The CAP Problem

In general, one of the ways to characterize a distributed data store
is in terms of Eric Brewer's CAP Theorem \citep{Gil02}.  The CAP
theorem roughly states that a distributed service cannot provide all
three of the following guarantees: consistency, availability, and
partition tolerance.  By consistency, we mean that all nodes have the
same data.  By availability, we mean that all nodes are capable of
responding to requests at any time.  By partition tolerance, we mean
that the system continues to function even if some nodes become
unreachable.

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
+-------------+  +--------------+  +---------------------+
| Consistency |  | Availability |  | Partition Tolerance |
+-------------+  +--------------+  +---------------------+

                     Pick TWO
\end{lstlisting}
\end{tabular}
\end{center}

In other words, when designing a distributed data store, we must relax
at least one of the above guarantees.  Various systems have, in the
past, relaxed each of the guarantees, and we now give a brief overview
of them.

### CP

The simplest guarantee we can relax is Availability.  This gives us a
system that is Consistent and Partition Tolerant.

Recall how clustering works in relational databases: one of the nodes
initiates a transaction, but before applying it, the node waits for
the other nodes to confirm that they are willing to make the
change\footnote{We limit our discussion here to the two-phase commit
algorithm, but other more sophisticated algorithms
exist. \url{https://en.wikipedia.org/w/index.php?title=Special:Cite&page=Two-phase_commit_protocol&id=559848750}}.
This implies that if even one of the other nodes is unreachable, no
changes can be made.  In other words, the clustering mechanism for
relational databases is not partition tolerant.

Although we described the above problem in terms of a clustered
database, it effectively boils down to a decision problem: when one
node wishes to make a decision, the other nodes must agree with the
decision.  In other words, this is basically the
\href{https://en.wikipedia.org/w/index.php?title=Consensus_\%28computer_science\%29&oldid=555475271}{distributed
consensus problem}, and the more sophisticated algorithms for solving
it would work for clustered databases as well.  For instance, by using
Paxos \citep{revisiting-paxos}, we could lift the constraint that all
the nodes be reachable, and limit it to requiring that only a majority
of nodes be reachable\footnote{Generally speaking, since there can
only ever be one majority of nodes, conflicting changes cannot be
made, so consistency is still guaranteed.  Paxos handles the details
of whether ``a majority of nodes are reachable'' or not.}.

### CA

The next guarantee we can relax is Partition Tolerance\footnote{In
fact, supposing that the network is always connected is one the most
common fallacies of distributed
computing. \url{http://en.wikipedia.org/w/index.php?title=Fallacies_of_Distributed_Computing&oldid=549312262}}.
This gives us a system that is Consistent and Available.

We note that, since network partitions do happen, we cannot generally
relax this guarantee \citep{Vog08}.  On the other hand, if we
distinguish between read and write nodes, we can relax this guarantee
to a certain extent.

For instance, consider how replication works with relational databases
or with NoSQL data stores: there is usually one or more master nodes
which accept both write and read requests, one or more slave nodes
which only accept read requests, and the master nodes forward write
requests to the slaves.  If a network partition occurs between slave
and master nodes, it is usually safe for both to continue operation,
with the caveat that the slaves may be serving stale data.

### ecAP

Finally, we can relax the Consistency guarantee.  This gives us a
system that is Available and Partition Tolerant.

Of course, a truly inconsistent system would not be very useful.  A
simple way to build such a system would be to setup several completely
independent nodes: each node would always be available, and since no
node is aware of any other nodes, network partitions would not affect
them; the downside would be that any advantage we might have been
trying to gain by distributing the data would be lost.  But suppose we
could guarantee that the system were *eventually* consistent.  That
is, suppose we could guarantee that, in the absence of writes, the
system will eventually reach a state where all the nodes have the same
data.  Then the system would be useful again.

This is the approach taken by CouchDB and its user manual illustrates
many of the difficulties that arise.  The biggest of them is the
existence of conflicts.  Note that, when relaxing any of the other
guarantees, it would not have been possible for nodes to make
conflicting changes to the database, but if we guarantee availability
and partition tolerance, it must be possible for nodes to make changes
independently of each other.  This introduces the need for conflict
resolution in the data store semantics, which in turn introduces the
need to keep track of previous versions of the data.

We previously mentioned that network partitions occur in practice.
With LTc, we take this observation further: not only do network
partitions happen, the network *is* partitioned.  As such, must
support partition tolerance.

Since communication between nodes can be very slow, guaranteeing
consistency would mean that most operations have to be equally slow.
For LTc, we relax the strict consistency guarantee and use *eventual
consistency*.

So, LTc is an eventually consistent, always available, and partition
tolerant system.  In this sense, LTc has more in common with
distributed version control systems and NoSQL data stores, than with
relational databases.

## Atomicity of Operations

So far, we have seen that LTc is a an ecAP distributed data store
which can hold any serializable Haskell value.  Now we describe the
levels of atomicity operations on individual data stores may have, and
then define the level LTc uses.

On one end of the spectrum, operations could have no atomicity
whatsoever: writes may be interrupted halfway through, reads may
happen on partially written values, and reads and writes may
interleave arbitrarily.  On the other end of the spectrum, the data
store may support full ACID transactions.

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
No Atomicity           MGet/MSet           ACID
   |-----------------------|----------------|
 Posix Filesystems        LTc         SQL Databases
                          Redis
\end{lstlisting}
\end{tabular}
\end{center}

### No Atomicity

The simplest data stores are those that make no guarantees about
read/write atomicity.  Famously, Posix file systems such as those used
by Linux are like this\footnote{Other file systems provide more
guarantees.  For instance, NTFS, used by Windows, has full support for
transactions:
\url{http://msdn.microsoft.com/en-us/library/windows/desktop/aa365695\%28v=vs.85\%29.aspx}}.
Although some operations like file creation, deletion, and renaming
are atomic, the key operations of read and write are not.  In other
words, when a process writes a string to a file, the operation can be
interrupted, perhaps by the process dying or the machine shutting
down, resulting in a partially written file.  Similarly, it is
possible for second process to be reading the file while the first is
writing to it, resulting in the second reading a mixture of the
original file contents and the modified ones.

The situation is worse if the network is involved in any way.  For
instance, the Network File System, which "provides transparent remote
access to shared files across networks" \citep{rfc1094}, actually has
slightly different semantics than most local file systems: when
appending to a file, it first enlarges the file by zeroing out the new
space, and only afterwards writes the new data.  This means that, if
you are tailing a file over NFS, you see spurious zeros which are
never part of the file itself.

Although file systems usually provide few atomicity guarantees, the
few that they do provide are enough to build upon.  For instance, we
can simulate atomic file writes by writing to an anonymous temporary
file, and then renaming it to the destination.  LTc relies heavily on
tricks like this to ensure its atomicity guarantees.

### MGet/MSet

The next step along the spectrum would be to support atomic single
reads and writes.  In other words, writing a single item to and
reading a single item from the data store is guaranteed to be an
uninterruptible atomic operation.  In practice, data stores usually go
one step further and support atomically writing and reading multiple
items at once.

Most NoSQL data stores usually make these or similar guarantees.  For
instance, Redis supports exactly this through two commands: `mset`
sets multiple values atomically, and `mget` retrieves multiple values
atomically.

CouchDB has an interesting take on this approach: it technically only
supports atomically reading and writing single values, but provides
the building blocks for full blown transactions
\citep{couchdb-transactions}.  In CouchDB, every stored value has a
version, and, in order to update a value, one must specify the
previous version of the value.  If the specified previous version and
the real version do not match, the update operation fails.  This is
enough to manually implement Software Transactional Memory
\citep{stm}, if desired.

For LTc, we provide both the ability to atomically set and get
multiple values, and versions for values.  Since both Redis and
CouchDB are widely used in production, we believe that these
guarantees and features are enough for a wide range of applications.

### ACID

For completeness, we also mention ACID transactions, which
\citep{ACID} defines as having the following four characteristics:
atomicity, consistency, isolation, durability.

LTc does *not* support any transactions.  In particular, it does not
support atomically reading a value, processing it, and writing it
back.  Although this makes LTc less useful, it does allow us to focus
on the data store, and on the synchronization protocols, both of which
would be greatly complicated by the addition of transactions.

## Delay-Tolerant Network Model

\label{sec:dtn}

So far, we have seen that LTc is a ecAP distributed data store which
can hold any serializable Haskell value and which supports atomically
writing and reading multiple values at once.  We now discuss the
network conditions under which LTc needs to function.

As mentioned in Section \ref{sec:motivation}, LTc's synchronization
mechanism is meant to work even on networks where packet round-trip
times are prohibitively large, where nodes are not always reachable,
and where end-to-end connectivity may be impossible.  The approach LTc
takes with regards to communication is not new, and is called
Delay-Tolerant Networking\footnote{or, \emph{Disruption}-Tolerant
Networking as DARPA likes to call it} (DTN).  Although research in DTN
has been done since the first computer networks were built, the pace
has increased greatly in the last decade.  More relevant to LTc, the
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

LTc's replication is designed to work over BP, but does not actually
do so.  The main reason for this is that BP is currently obscure, and
the necessary infrastructure is not widely deployed; relying on it now
would just make development and testing more difficult.  Instead, LTc
uses UDP, a widely deployed protocol which has strictly fewer features
than BP.  Given that UDP is less general than BP, and that the
network-facing side of LTc is abstracted, we believe that it will
would be easy to add support for BP at a later date.

### UDP

\label{sec:udp}

As previously mentioned, LTc cannot use a transport protocol such as
TCP, which assumes end-to-end connectivity and short round-trips
between nodes.  Starting from the first assumption outlined in Section
\ref{sec:motivation}, LTc only requires a protocol that uniquely
identifies nodes, and allows at least one-way communication between
them.

Interestingly, the only widely used protocol that completely avoids
round-trips is UDP\footnote{``I have this awesome joke about UDP, but
I don't care if you get it or not.''}, which "provides a minimal,
unreliable, best-effort, message-passing transport". \citep{rfc5405}

Defining the LTc's synchronization protocol on top of UDP poses
significant problems.  First, UDP makes no guarantee that a sent
packet will be received by the destination.  More problematically,
there is no way for the source to tell if a packet was lost or not.
Worse yet, even assuming a perfect connection that does not lose
packets, if the destination does not process them quickly enough, its
UDP buffer will spill over causing lost packets.  So, LTc's
synchronization mechanism must be able to make forward progress even
if only partial updates are available.  Second, UDP makes no guarantee
that sent packets will be received in order.  This is problematic
because the order of updates is important.  Finally, UDP makes no
guarantee that a sent packet will not be received multiple times.
Again, this is problematic because updates should only be applied
once.  If we were using BP, which was designed with systems like LTc
in mind, it would solve or alleviate all these problems.  Since we
have little choice in the matter, we have worked around the
limitations of UDP as discussed in Section \ref{sec:changes}.

\clearpage

<!-- How am I achieving it? -->

# Implementation and Architecture

So far, we have seen what our reasons for writing LTc were, and what
design decisions we have made.  We will now discuss LTc is actually
implemented.  We begin with an outside view that treats an LTc system
as a blackbox, we briefly mention our choice of programming language,
we describe LTc's internal architecture, and we finish by discussing a
few design issues that cropped up during implementation.

## Outside View

\label{sec:outside-view}

In this section, we describe what a running LTc system looks like from
the outside.

LTc is designed to run as an embedded data store.  In other words,
unlike most "big" SQL databases, but like SQLite, LTc runs in the same
process as the application that is using it.  For instance, an LTc
node started by the debugging tool `ltc` looks like:

~~~~ {.sourceCode}
% ps -aux | grep ltc
scvalex   6807  2.0  0.4  55956 15948 pts/4    S+   10:19   0:00 ./ltc node
~~~~

"Big" databases usually run as separate server processes.  This is
advantageous, since it allows several applications to use a single
shared database.  The downside of this approach is that it requires
additional effort to setup a database, and more programming effort
because a client/server protocol is needed.  We believe that the costs
of a separate server process outweigh the benefits in LTc's case, so
we used the simpler approach.

On disk, an LTc database looks like an opaque directory.  By opaque,
we mean that we do not expect anyone other than an LTc process to have
any need to look into it.  For instance, after running a test node
with the file system backend for some time with `ltc node`, we get an
on-disk structure like the following:

~~~~ {.sourceCode}
% tree node-store-0
node-store-0
|-- clean-shutdown
|-- clock
|-- format
|-- keys
|   |-- 27067ff31175f604acdf4dcb804036e48ef050dd
|   |-- 3b19bc2b8b75edd0ee7d243100e7772c9feeb59c
|   |-- 788a88b84869f85cb5c09b6ea61a8319a04b5bad
|   |-- a689532a6a4af42dbc8e4610c784854e177a3dde
|   |-- c30bd16026732cc5e4147276f5b8e7fe990eba35
|   \-- fc98c6c9be0fcd6d389119906ad9da8a9e80bb8b
|-- nodeName
|-- tmp
|-- values
|   |-- 1b6453892473a467d07372d45eb05abc2031647a
|   |-- 356a192b7913b04c54574d18c28d46e6395428ab
|   |-- 77de68daecd823babbb58edb1c8e14d7106e83bb
|   \-- da4b9237bacccdf19c0760cab7aec4a8359010b0
\-- version
~~~~

The individual files each have their use; for instance,
`node-store-0/format` and `version` contain the format and the version
of the on-disk data store, which would enable us to perform automatic
upgrades whenever the on-disk structure needs changing; the
`node-store-0/keys/*` files hold records containing type information
and the changes made to each key in the data store.  For a full
description of these files, see the documentation for `Ltc.Store` in
Appendix \ref{sec:user-guide}.

From a network point of view, LTc usually listens on two ports: an UDP
port around $3582$ for replication, and an HTTP port around $5000$ for
the administrative web interface.  We say "around" because multiple
nodes may be running on the same machine, in which case the ports will
be offset by the node index (e.g. first node uses $3582$, the second
uses $3583$).  For instance, the test node started by `ltc node` looks
like:

~~~~ {.sourceCode}
% netstat -lp | grep ltc
tcp     0   0 *:5000        *:*         LISTEN      8522/./ltc
udp     0   0 *:3582        *:*                     8522/./ltc
~~~~

Since LTc uses UDP for replication, it never actually establishes
persistent outgoing connections, but instead sends discrete messages
to other nodes.

Additionally, if LTc's Redis adapter is enabled, it will also listen
on the standard Redis port for incoming connections over the Redis
wire protocol.

~~~~ {.sourceCode}
tcp     0   0 *:6379        *:*         LISTEN      19172/./ltc
~~~~

A bunch\footnote{We intentionally avoid the use of the word "cluster"
which is usually associated with tight synchronous coupling of nodes.}
of LTc nodes that hold the same data does not require any particular
network layout, and would not have any distinctive features.  The only
condition is for every node to be addressable and reachable by some
other node via UDP.

## Haskell

\label{sec:haskell}

We have seen what LTc looks like from the outside; we now delve into
LTc's internals.  The most obvious idiosyncrasy is that it is written
in \href{http://www.haskell.org/}{Haskell}; in this section, we
motivate the choice, and explain how choosing another language might
have affected development.

In addition to the author's familiarity with the language, we had
several other reasons for choosing Haskell.  The first is that it is
possible to write very terse code in some cases.  For instance,
consider the following predicate which asserts that a list is sorted
in non-decreasing order:

~~~~ {.haskell}
isSorted :: Ord b => [b] -> Bool
isSorted xs = all id (zipWith (<=) xs (tail xs))
~~~~

If we were to write it in Java, it would be considerably more verbose,
and, in the author's opinion, more obscure:

~~~~ {.java}
<T extends Comparable> boolean isSorted(List<T> xs) {
    for (int i = 0; i < xs.size() - 1; i++) {
        if (xs.get(i).compareTo(xs.get(i+1)) > 0) {
            return false;
        }
    }
    return true;
}
~~~~

Recent extensions to the Haskell compiler also helped in this regard.
The
\href{http://www.haskell.org/ghc/docs/7.6.2/html/users_guide/generic-programming.html}{GHC
Generics} generic programming framework enabled us to generate
serializers and deserializers for data types automatically, which
meant that we could avoid the considerable boilerplate associated with
such tasks.
\href{http://www.haskell.org/ghc/docs/7.6.2/html/users_guide/type-families.html}{Indexed
type families}, and, in particular, associated data types
\citep{type-families}, allowed us to express the dependencies between
some types clearly, and thus avoid the somewhat obscure type-class
machinery that were traditionally required.  We discuss the impact of
using these extensions in more detail in Section
\ref{sec:type-safety}, where we focus on type safety.

The fact that Haskell treats functions as first class objects was of
particular help when writing tests: almost all of the unit tests for
LTc are generated from tables of data.  More specifically, we usually
have a list of `(test name, input, expected output)` tuples, a test
generation function that takes an entry from this list and returns a
test function that actually runs a test, and we apply the test
generation function to the list of tuples.  In other words, we
generate the tests at runtime.  This is usually hard to do in other,
especially non-functional, languages\footnote{Interestingly enough, it
is hard to do even in some functional languages as well, if they lack
enough abstraction features.  For instance, Erlang's standard unit
testing framework, \texttt{etest}, is based entirely on macros which
get very confused if they are used as part of automatic test
generation.}.

Point in case, the data store part of LTc weights in at less than
$3000$ SLOC\footnote{Source Lines of Code $\approx$ total lines -
whitespace lines - comment lines} of Haskell.  Unfortunately, since
LTc is not *manture* software by any standard, we cannot compare its
size with other similar pieces of software.

Another reason for our choosing Haskell is its good library support.
At the time of writing, Hackage, the canonical repository of Haskell
packages, offers libraries for more-or-less every need imaginable.
For LTc, we have used an embedded web server, a WebSockets library, a
couple of serialization libraries, several testing frameworks, and
several libraries for writing command line programs.  Indeed, if there
is a problem with the current Haskell package ecosystem, it is that it
offers too much choice: for all the above cases, we have had to choose
between multiple alternatives\footnote{We usually chose the
alternative we were most familiar with.  While this heuristic may not
always lead to the best choice, it is fast, and, in our case, it led
to good enough choices}.

Our last reason for choosing Haskell was that we expected that some
features of its type system to come in handy.  We were particularly
hopeful that we would be able to use
\href{http://www.haskell.org/ghc/docs/7.6.2/html/users_guide/data-type-extensions.html\#gadt}{Generalised
Algebraic Data Types} to encode a type system for the values storable
in LTc.  There were a couple of problems with our attempt both caused
by limitation of the compiler.  The first was that the compiler could
not perform an exhaustiveness check when defining type class instances
over the GADT; in other words, defining instances for all individual
sub-types of the GADT would not automatically imply an instance for
the whole GADT.  This meant that we would have to include a large
number of type class constraints when defining even the simplest
functions, and that we would have to occasionally annotate values with
their types.  The second and larger problem was that the compiler
could not automatically derive common instances for GADTs.  This made
working with them very difficult, and meant we would lose most of the
advantages mentioned here.  We present the approach we ultimately
adopted in Section \ref{sec:type-safety}.

On the other hand, we had moderate success with using
\href{http://www.cs.ox.ac.uk/ralf.hinze/talks/FOP.pdf}{phantom types}
to encode extra properties about our data and have the type checker
enforce them automatically.  As a simple example, consider the
following type class, which describes an abstract network interface:

~~~~ {.haskell}
data Sending
data Receiving

class NetworkInterface a where
    serve :: NetworkLocation -> IO (a Receiving)
    receive :: a Receiving -> IO ByteString

    connect :: NetworkLocation -> IO (a Sending)
    send :: a Sending -> ByteString -> IO ()

    close :: a b -> IO ()
~~~~

One peculiarity of the way LTc handles network communications is that
it makes a sharp distinction between interfaces on which it sends
data, and interfaces on which it receives data.  In particular,
attempting to receive on a sending interface and vice versa is an
error.  Due to the above type class, making this error is impossible,
since an erroneous program would not type check.  For instance, note
that `receive` takes an `a Receiving` as its first parameter, but this
can only obtained from `serve`; the `a Sending` obtained from
`connect` is simply not the right type.

We end this section by mentioning a few other languages we considered.
Since LTc is a distributed program, it would have been reasonable to
write it in Erlang, and indeed, this would have given us inter-node
communication for free; unfortunately, Erlang's lack of static
typing\footnote{The author did not appreciate the importance of static
typing until he had to track down a malformed three-tuple as it was
passed through many modules and processes in a large Erlang project he
previously worked on.}  and its poor library support ruled it out
immediately.  Other similar key-values stores are written in C
(Redis), and C++ (MongoDB); we believe that following suit would have
made the code too long for a single person to develop effectively.  Of
the other languages the author is familiar with, OCaml would have
probably worked as well as Haskell, but its packaging ecosystem is far
inferior to the Haskell one.

## Component Architecture

### The Static View: Plugable Architecture

\label{sec:plugable}

<!-- FIXME Mention its a choice between complexity/flexibility, and
simplicity/staticity -->

<!-- FIXME Break down by plugable component: explain what each
plugable enables. -->

<!-- Mention the Redis adapter. -->
<!-- Mention the SQLite backend. -->

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

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
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
\end{lstlisting}
\end{tabular}
\end{center}

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

\label{sec:type-safety}

LTc is written in Haskell, and the we have discussed the advantages of
this in Section \ref{sec:haskell}.  We will not repeat the points
here, but we will expand on one of them, namely type safety.  In
particular, we now focus on type safety in the context of
serialization and, especially, deserialization of data.

Consider the archetypal serialization function, `show`:

~~~~ {.haskell}
show :: Show a => a -> String
~~~~

Its type describes it well: `show` is a function that takes a value of
some type which can be "shown" to a string, and gives you back the
value's string representation.  So far, so good, but consider its
inverse, `read`:

~~~~ {.haskell}
read :: Read a => String -> a
~~~~

In other words, `read` is a function that takes a string, and converts
it to a value of some type which can be "read" from a string.  The
problem is that `read` can make up values of effectively any
type\footnote{Technically, \texttt{read} can only make up values of
types which have \texttt{Read} instances, but that covers all the
types in the standard libraries, and the compiler can generate the
instances automatically for any user defined types.  So, for the
purpose of the discussion, we assume that all types have \texttt{Read}
instances.}.  This effectively throws type safety out the window, as
can be seen from the following simple example:

~~~~ {.sourceCode}
λ > let n = 42 :: Int
λ > read (show n) :: Int
42
λ > read (show n) :: String
"*** Exception: Prelude.read: no parse
λ > read (show n) :: Float
42.0
~~~~

All three of the above calls to `read` are valid Haskell, but the
second will throw an exception *at runtime*, and the third implicitly
casts a value between types.  Ideally, we would like the compiler to
spot both these misuses.  Although the mistakes are obvious in such a
simple example, they become much less visible when the code is part of
some larger application\footnote{The author has seen and written code
that uses \texttt{show} and \texttt{read} to store persistent
application state, and can attest that the bugs that appear in such
code are extremely annoying, especially if the deserialization
succeeds erroneously.}.

Since LTc is a data store, it was designed in such a way as to prevent
the more common bugs that can arise in such situations.

## Types in Database Interfaces

Consider the following Haskell data type that encodes some basic
information about a person.

~~~~ {.haskell}
data Person = Person
    { name   :: String
    , age    :: Int
    , height :: Double
    }
~~~~

We will now show how we would insert records of this type into a
database with \href{http://hackage.haskell.org/package/HDBC}{HDBC}, a
widely used Haskell SQL database connector.  First, we need to define
the schema for the `persons` table.  We note that it duplicates the
information in the `Person` data type declaration, the only difference
being that it is in the form of string.

~~~~ {.haskell}
setupTables :: (IConnection c) => c -> IO ()
setupTables conn = do
    tables <- getTables conn
    when (not ("persons" `elem` tables)) $ do
        stmt <- prepare conn ("CREATE TABLE persons ( name STRING, \
                              \                       age INT, \
                              \                       height DOUBLE )"
        _ <- executeRaw stmt
        return ()
~~~~

Next, we have a function that inserts values of type `Person` into the
`persons` table.  Here, we have to manually unfold the `Person`
record's fields.

~~~~ {.haskell}
insertPerson :: (IConnection c) => c -> Person -> IO ()
insertPerson conn person = do
    stmt <- prepare conn "INSERT INTO persons VALUES (?, ?, ?)"
    _ <- execute stmt [ toSql (name person)
                      , toSql (age person)
                      , toSql (height person)
                      ]
    return ()
~~~~

Finally, we have a function that extracts values of type `Person` from
the `persons` table.  This time, have to manually reconstruct the
`Person` record from its fields.

~~~~ {.haskell}
getPerson :: (IConnection c) => c -> String -> IO (Maybe Person)
getPerson conn personName = do
    stmt <- prepare conn "SELECT * FROM persons WHERE name = ?"
    _ <- execute stmt [toSql personName]
    values <- fetchRow stmt
    case values of
        Just [nameV, ageV, heightV] ->
            return (Just (Person { name   = fromSql nameV
                                 , age    = fromSql ageV
                                 , height = fromSql heightV
                                 }))
        _ ->
            return Nothing
~~~~

There are quite a few issues with the above code.  First, it ignores
the returns of the calls to `execute`, so we have no guarantee that
the database insertions actually succeeded.

Then, there is an undocumented requirement to call `commit` on the
database connection after each `insertPerson`; if this is omitted,
with some databases, the changes are not guaranteed to be persistent.

Note the above calls to `toSql` in `insertPerson`, and `fromSql` in
`getPerson`; these almost introduce the `show`/`read` problem we
mentioned in the previous section.  The problem in this case is not
quite so bad because, as we mentioned in Section
\ref{sec:primitive-types}, SQL databases support a few primitive
types.

In fact, the calls to `toSql`/`fromSql` are indicative of a larger
problem: since the database layer has no knowledge of the `Person`
type, we have to manually handle translations from `Person` to SQL
table columns and back.  Any change to the `Person` data declaration
would require slightly different changes to three places in the code.
Failure to make these changes would cause a warning in `getPerson`,
and would succeed silently for `setupTables` and `insertPerson`.  It
is easy to see how these pieces of code could easily get out of sync.

The last problem is somewhat less obvious: if we were to change the
declaration of `Person`, we would probably also need to change the
values *already present* in the database.  In other words, the version
of the code in the executable is now coupled with the values in the
database; using the wrong code for the database may work fine, it may
succeed while silently corrupting data, or it may fail.

We illustrated the above problems with SQL databases, but it could be
much worse.  If we were to rewrite the examples for Redis with
\href{http://hackage.haskell.org/package/redis-hs-0.1.2}{redis-hs}, we
would have to use the following primitives to insert and extract data
from the data store:

~~~~ {.haskell}
itemSet :: Handle -> String -> String -> IO (Maybe RedisReply)
itemGet :: Handle -> String -> IO (Maybe RedisReply)
~~~~

In other words, we would lose what little type safety
`toSql`/`fromSql` afford us, and would have to manually serialize to
and deserialize from strings.

While on the subject of bad database interface design, consider the
return types of the various functions we have seen so far.  HDBC's
`execute` returns an integer, which represents the number of rows
altered; so, in order to check if the insertions succeeded, we would
have to check whether the return is $1$ or not.  The return from
redis-hs's `itemSet` is `Maybe RedisReply`, where `RedisReply` is a
sum type with four alternatives which can be used to encode, among
other things, ordered lists of strings.  So, as far as the type
`itemSet` is concerned, it is reasonable to expect it to return an
ordered list of strings.  This is further inducement for the user of
the library to ignore the returns of functions.

Needless to say, all of these problems can be solved by careful
coding, but, in the author's experience, the above is typical of code
that works with databases.  As we will see, LTc's interface is
designed to prevent such problems from arising in the first place, or,
if they arise, to signal the failures as loudly and clearly as
possible.

## A Strongly Typed Database Interface

\label{sec:strongly-typed}

We now write the above example with LTc.  First off, LTc requires the
types of values inserted into it to have certain instances.

~~~~ {.haskell}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

data Person = Person
    { name   :: String
    , age    :: Int
    , height :: Double
    } deriving ( Eq, Generic, Ord, Typeable, Show )

instance Serialize Person
instance Sexpable Person

instance Diffable Person where
    data Diff Person = ReplaceDiff Person Person
                       deriving ( Eq, Generic, Show )

    diffFromTo = ReplaceDiff

    applyDiff x1 (ReplaceDiff x2 y) =
        if x1 == x2 then y else error "cannot apply diff to Person"

    reverseDiff (ReplaceDiff x y) = ReplaceDiff y x

instance Serialize (Diff Person)
instance Sexpable (Diff Person)

instance Storable Person
~~~~

And that is all.  With this boilerplate code in place, we can insert
and retrieve values of type `Person` from an LTc data store:

~~~~ {.haskell}
set :: (Store b, Storable b) => a -> Key -> b -> IO Version
get :: (Store a, Storable b) => a -> Key -> Version -> IO (Maybe b)
~~~~

The type for `set` says, given something that is an LTc data store, a
key, and some value that is storable, insert the key-value pair into
the store, and return the version of the value just inserted.  The
type for `get` says, given something that is an LTc data store, a key,
and a version, return the value that is associated with the key at the
version, if it exists.

Conceptually, an LTc data store looks like the following:

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
              Keys                    Values
+======+=====================+      +========+
| key1 | - TypeRep           |      | value1 |
|      | - Versions:         |      | value2 |
|      | |-- vsn1 -----------+----> |        |
|      | |   ...             |      |        |
|      | \-- vsnN -----------+----> |        |
+------+---------------------+      |        |
|       ...                  |      |   ...  |
+------+---------------------+      |        |
| keyN | - TypeRep           |      |        |
|      | - Version:          |      |        |
|      | |-- vsn1 -----------+----> |        |
|      | |   ...             |      |        |
|      | \-- vsnN -----------+----> | valueN |
+===============+============+      +========+
\end{lstlisting}
\end{tabular}
\end{center}

We will now go through the complaints and problems mentioned in the
previous section, explain why LTc addresses them, and how it achieves
this.

We begin with the complaint about the return types of `set` and `get`.
When we retrieve a value, we would expect `get` to either return it,
or let us know that it does not exist.  When inserting a value, we
would expect `set`, at least in the context of LTc, to give us some
proof that the value was inserted.  In both cases, the types match our
expectations.  Note also that because `get` wraps the retrieved value
in a `Maybe`, we are effectively forcing the user to pattern match
against the result, and consider the case where the value is missing.
If `set` fails for whatever reason, it throws an exception; so, there
is no chance of \emph{silent} data loss\footnote{There is still a
chance that \emph{noisy} data loss can occur, but that will make it
immediately clear that \emph{something} bad is happening.}.

Next, we consider the situation where the definition for `Person` were
changed, but the values in the data store were not updated to match
the new definition.  In LTc, *the type of the value is stored
alongside the value*, and *changing the type associated with a key is
not permitted*.  Concretely, when we insert a value of type `Person`
in the data store, the
\href{http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Typeable.html\#t:TypeRep}{\texttt{TypeRep}}
of `Person` is also stored with the value; when we try a key already
associated with the `Person` type with a value with the modified
`Person`, we get an error.  There are still problems with this
approach: first, these errors can only be signaled at runtime, since
they require accessing the on-disk database; second, although we
cannot update a key with the wrong type, we can still insert a new key
with the wrong type.  That said, we do not believe these problems are
fixable, unless we declare the types associated with all the keys in
advance, which would be far to cumbersome.

We briefly mentioned the `TypeRep` of a type above, but it deserves
further mention.  We can get the `TypeRep` of `Person` because
`Person` has a `Typeable` instance which is generated automatically by
the compiler \citep{syb}.  Despite its name, the `TypeRep` is more of
a hash of the type, than an actual representation of it; we know that
if two `TypeRep`s are different, they originated from different types.
We only use `TypeRep`s to ensure that the types associated with keys
do not change.

The next issue that we mentioned was the manual deconstruction and
reconstruction of the `Person` records when inserting and getting
values.  For LTc, we require that values have a `Generic` instance.
This type-class is part of the
\href{http://www.haskell.org/ghc/docs/7.6.2/html/users_guide/generic-programming.html}{GHC
Generics} framework, and allows us to get a "universal" representation
of any value.  We can then use this representation to automatically
generate the serializers and deserializers which effectively replace
the `insertPerson` and `getPerson` functions from the original
example.  As an added bonus, since the type of the values is always
implied by the type of the accessor functions, we can drop the
needless "Person" suffix, and just use `get` and `set`.

Our final complaint about HDBC's interface was that are hidden
requirements to ensuring persistence of data.  For LTc's data stores,
if a `set` returns, the value has been stored successfully.

We end this section by mentioning that LTc is not the first Haskell
data store to attempt a nicer type-safe interface.  In particular, the
interface of
\href{http://hackage.haskell.org/package/acid-state}{acid-state} seems
to have been guided by the same principles as that of LTc.  The main
difference is that, whereas LTc attempts to make all of its
requirements explicit by requiring the numerous `instance`
declarations, `acid-state` attempts to keep everything "under the
hood" by wrapping the boilerplate in
\href{http://www.haskell.org/ghc/docs/7.4.2/html/users_guide/template-haskell.html}{Template
Haskell} macros.  On the other hand, this difference could just be an
artifact of the times, since when `acid-state` was originally written,
GHC Generics were not yet available.

\clearpage

Handling Changes
================

\label{sec:changes}

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

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
Other key-value stores     LTc key-value store with changes

+--------------+--------+  +--------------+--------------+
| Key          | Value  |  | Key          | Value        |
+==============+========+  +==============+==============+
| alex:balance |  120$  |  | alex:balance |    0$        |
+--------------+--------+  |              |  v | +200$   |
                           |              |    200$      |
                           |              |  v | -80$    |
                           |              |    120$      |
                           +--------------+--------------+
\end{lstlisting}
\end{tabular}
\end{center}

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

\begin{center}
\begin{tabular}{c}
\begin{lstlisting}
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
\end{lstlisting}
\end{tabular}
\end{center}

The algorithm outlined above is rumor mongering, and it is what LTc
will initially use.  By changing the way a nodes selects other nodes
to infect, and the time until an update is no longer considered
infections, several variations of the basic algorithm
arise. \citep{wiki:dtn-routing} Exploring which of these is best
suited for LTc is a future path for development.

\clearpage

Correctness
===========

We have seen that LTc is a data store that can be replicated over
multiple nodes.  Now, we claim that it is *correct*.  In this section,
we discuss what it means for a data store to be correct, we explain
why we believe LTc fits this description, and give details to the
steps we took to convince ourselves of this.

To begin, what would make a data store *worse* than useless?
Arguably, this is the case if the data store loses data, or if its
replication mechanism corrupts data.  We say that a data store loses
data if some write to it that succeeds later becomes undetectable.  We
say that the replication mechanism corrupts data if, after
replication, the values in the data store are somehow unexpected.

Note that the first of the two items we would like to prove is a
negative---we are attempting to prove that something does not happen.
Since this is hard to do in practice, we will settle for tests that
show that it does not happen in the common cases.  On the other hand,
the second item is a positive, so we will give a mathematical proof
for it.

## Empirical Correctness

We now show that LTc does not lose data in the common cases.  We use
three testing methodologies for this: the first is unit testing as
commonly used in industry; the second and third are random testing and
exhaustive testing, respectively, which although widely used in the
Haskell ecosystem, are less widespread outside.  With all three
methodologies, we will seek to show the same thing, namely that data
inserted into LTc *somehow* is accessible later *somehow*.

### Unit Testing

When running the tests for LTc, the first batch of tests are the unit
tests.  We do this because they tend to finish relatively
quickly\footnote{In fact, since the unit tests usually finish in just
a few seconds, the author likes to run them automatically after
\emph{every} build.}, and because, unlike the random and exhaustive
tests, when they fail, the reason for failure is usually easy to
determine.

These tests check the simplest use cases for LTc: open an LTc data
store, write some value to it, check that we can read the value back,
write some other value, and check that we can get both values back.
Once these test finish, we can be confident that LTc is usable, if not
correct.

One place where unit testing proved a particularly good fit was
testing conformance with Redis: we extracted the examples from Redis's
documentation, and converted them to unit tests for the Redis adapter.
Since these tests pass, we can be somewhat confident that we have
correctly implemented our subset of Redis.  Unfortunately, since Redis
does not define any formal semantics, we have no way to say for
certain.

### Random Testing

The problem with unit tests is that crafting complicated scenarios is
long and boring process which the author would rather avoid.  This
explains why the majority of LTc tests are not unit tests.  Instead,
we have focused our efforts on random testing.

By random testing, we mean that "properties are described as Haskell
functions, and can be automatically tested on random input"
\citep{quickcheck}.  We used Haskell's
\href{http://hackage.haskell.org/package/QuickCheck}{QuickCheck}
framework to write these tests.

For each random test, we usually take a new LTc data store, and we
then apply a random sequence of set commands to it, all the while
checking various properties.  As with unit testing, we are usually
interested that data written to the data store is later accessible,
even if it is superseded by newer data.  Once these tests are done,
and by virtue of our running them thousands of times, we can be
reasonably confident that LTc does not lose data in the usual cases.

We make a point of running roughly the same tests both with LTc's
normal interface, and over the Redis wire protocol.  By doing this, we
gain confidence that the two interfaces do not have different
semantics.

### Exhaustive Testing

Although random testing forms the backbone of LTc tests, and the
author expects it to catch the most bugs, there are a few deficiencies
in the approach.  A large list is included in the SmallCheck paper
\citep{smallcheck}, but we are interested in one in particular: most
errors are revealed by small test cases, but random testing does not
favour them, and instead wastes time on unnecessarily large test
cases.  This is a problem for LTc particularly, since the tests make
changes to the on-disk data store which, in turn, are time-expensive.

We attempt to remedy this problem by using
\href{http://hackage.haskell.org/package/smallcheck}{SmallCheck}, an
exhaustive testing framework for Haskell.  Unlike our random tests
which generate random sequences of commands to run on a data store,
the exhaustive tests generate *all* command sequences up to a certain
depth.  This gives us more breadth on the set of tested commands, and
we gain certainty that LTc is not doing anything surprising.

In addition to checking variants of the properties previously
mentioned, we also use exhaustive testing for the encoders and
decoders for the network protocols we use: LTc's inter-node protocol,
and the Redis wire protocol.  Of course, in Redis's case, this does
not mean we are actually handling it correctly, but at least we know
that we are handling it consistently.

## Provable Correctness

We now prove that LTc's replication mechanism does not corrupt data.

<!-- Explain the structure of our proof: global properties/local
properties -->

### Global Properties

<!-- This is what we want to prove. -->

### Local Properties

<!-- This is what we konw. -->

### Proof

\clearpage

Evaluation
==========

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

<!-- FW: Expire values. -->
<!-- FW: Expire history. -->
<!-- FW: Namespaces for keys; sort of like databases in SQL lingo. -->
<!-- FW: Upgradeable values (store the full representation of the type) -->
<!-- FW: More ACID -->
