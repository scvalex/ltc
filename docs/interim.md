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

Introduction
============

\nocite{*}

> For the interim report this section should be a short, succinct,
> summary of the project’s main objectives.

> What is the problem,

> why is it interesting

> what’s your main idea for solving it?

What is LTc?

What assumptions do current datastores make?

What assumptions does LTc make?  Mention that c+ communications are
currently in the realm of SF.

Where would LTc be used?

How is LTc implemented? (cAP (in fact, we usually can't choose C, and
take this case to its logical extreme), key-value store with DVCS
semantics, communicates over UDP, conflict resolution through Patch
Theory (because communication between nodes is very difficult, so the
usual consensus or voting algorithms are not feasible), not ACID,
decoupled internal architecture, updates are propagated via some lazy
epidemic technique, vector clocks)

What are the obvious downsides?

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

### DTN

### Patch Theory

### Vector Clocks

Project Plan
============

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

Evaluation Plan
===============
