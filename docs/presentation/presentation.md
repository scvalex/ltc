#

\begin{center}
  \textsc{\LARGE LTc}\\[0.5cm]
  \textsc{\large A replicated data store for high-latency disconnected environments}
\end{center}

\note{

Good afternoon.  I'm Alexandru, and for the next few minutes, we're
going to talk about Mars, data replication, and type safety.

}

#

![](space1.png)

\note{

\begin{itemize}

\item Space!

\end{itemize}

}

#

![](space2.jpg)

\note{

\begin{itemize}

\item This is Earth.  As you can see, this is where we are.

\end{itemize}

}

#

![](space3.jpg)

\note{

\begin{itemize}

\item And this is Mars, and we want to send a message to the Mars
rover.

\item What sort of problems would we encounter communicating with
Mars?

\end{itemize}

}

#

![](space4.jpg)

\note{

\begin{itemize}

\item The first problem is that Mars is very very far away.

\item Since communications are limited by the speed of light, sending
a message would take between 4 and 20 minutes.

\item Sending a message and receiving a reply would take between 8 and
40 minutes.  This pretty much means that any sort of synchronous
communications protocol breaks down at such distances.

\item Point in case, if you tried to open a TCP connection to Mars,
the handshake alone would take at least 12 minutes.  So, any sort of
network program that uses TCP would just not work under such
circumstances.

\end{itemize}

}

#

\begin{center}
  \Large Problem 1: High latency
\end{center}

\note{

\begin{itemize}

\item This brings us to our first problem: high latency.

\item Under some circumstances, network channels can be very high
latency and network programs that don't take this into account will
just not work.

\end{itemize}

}

#

![](space3.jpg)

\note{

\begin{itemize}

\item Let's go back a bit.  What other problems do we have
communicating with Mars?

\end{itemize}

}

#

![](space5.jpg)

\note{

\begin{itemize}

\item Sometimes our line of sight to Mars is obstructed; there's
something in the way preventing any direct communications.  For
instance, this year, Mars swung behind the Sun in late April, and is
only becoming visible again now; that's two months of no connectivity.

\item So, if a network program expects to have a permanent connection
to Mars, it's not going to work.

\end{itemize}

}

#

\begin{center}
  \Large Problem 2: Intermittent connectivity
\end{center}

\note{

\begin{itemize}

\item And this brings us to our second problem: intermittent
connectivity.

\item Under some circumstances, you cannot have a permanent network
channel to some remote location.

\end{itemize}

}

#

\begin{center}
  \large (smaller) \Large Problem 3: Lossy channels
\end{center}

\note{

\begin{itemize}

\item There's also a smaller third problem in that network channels
are probably going to be lossy.

\item This isn't so much of a new problem, it's just that high latency
and intermittent connectivity prevent the usual solutions from
working.

\item For instance, TCP is a lossless protocol: it achieves this by
acking every packet received.  So, when you send a packet from Earth
to Mars, Mars replies that it's received it.  But we've already seen
that this will take at least 8 minutes, so it's not a workable scheme
in practice.

\end{itemize}

}

# Three Problems

* Problem 1: High latency

* Problem 2: Intermittent connectivity

* Problem 3: Lossy channels

\note{

\tiny

\begin{itemize}

\item So, these are the three big problems that occur in
inter-planetary communications: very high latency, intermittent
connectivity, and lossy channels.

\item But they also happen in more mundane circumstances.  Strictly
speaking, any environment that's not a data centre has these problems,
but there are a few situations where they stick out.

\item For instance, in many rural or developing areas, the
ground-based communications infrastructure is either incomplete or
non-existing.  For reference, about two thirds of all people are in
this situation.

\item In this case, the only traditional option, is satellite
communications, which tends to be expensive, unpredictable, and lossy.
There's also Project Loon now, which uses low orbit balloons to
provide internet access.  The idea is that you use a special antenna
to connect to the nearest balloon, and your traffic is forwarded
through the network of balloons until it reaches the Internet.  So,
this has the added problem that you may be able to contact your own
region, but not outside, if the balloons that were forwarding the
traffic fail.

\item The same problems also happen in disaster stricken areas.  When
something catastrophic happens, like an earthquake, the communications
infrastructure tends to go down.  Sometimes, this happens because the
actual infrastructure is damaged, or simply because it cannot handle
the unusually high traffic.  What's interesting in this case is that
the individual nodes in network, so laptops and phones, generally
continue to work.  This means that you have a bunch of devices that
can form temporary connections of varying quality with one another,
but are not permanently connected.

\end{itemize}

}

# Data replication

\tikzset{state/.style={rectangle, draw, text centered}}

\centering

\begin{tikzpicture}

\node (A1) {Node 1};
\node (A2) [state, below of=A1] {A, B};
\node (A3) [state, below=1.4cm of A2] {\dots};
\node (A4) [state, below=1.4cm of A3] {A, B, C};

\node (B1) [right=2cm of A1] {Node 2};
\node (B2) [state, below of=B1] {A, C};
\node (B3) [state, below=1.4cm of B2] {\dots};
\node (B4) [state, below=1.4cm of B3] {A, B, C};

\path[->]
    (A2) edge (A3)
    (A3) edge (A4)
    (B2) edge (B3)
    (B3) edge (B4);

\path[->,dashed,font=\scriptsize]
    (A2) edge (B3)
    (B2) edge (A3)
    (B3) edge (A4)
    (A3) edge (B4);

\end{tikzpicture}

\note{

\tiny

\begin{itemize}

\item Those three problems affect basically every networked program,
but we're not going to treat the general case.  Instead, we will focus
on data replication.

\item Here's what we mean by ``data replication''.  We've got one data
store, but it's on multiple nodes.  To be clear, the data isn't
fragmented, sharded or anything like that.  It's the same data on
every node.  Or rather, that's what we'd like to happen.

\item In this diagram, we have two nodes.  The boxes represent states
the nodes are in, and time flows downward.  So, Node 1 starts in this
state, goes through some other states, and ends up in this state.  The
same goes for Node 2: it starts here, it goes through some
intermediary states, and ends here.

\item Here's what we mean by data replication.  The two nodes hold the
same data, sort of.  At the beginning, both have A, but each also has
something extra: Node 1 has B, and Node 2 has C.  By data replication,
or synchronization, we'll be using the terms interchangeably, we mean
that the two nodes start like this, they exchange a bunch of messages,
and they end in states where they agree on the data.

\item We're first going to briefly look at how other data stores
handle replication, and why it doesn't quite work in the environments
we mentioned before.

\end{itemize}

}

# Clustering

\tikzset{state/.style={rectangle, draw, text centered}}

\centering

\begin{tikzpicture}

\node (A1) {Node 1};
\node (A2) [state, below of=A1] {A};
\node (A3) [state, below=0.6cm of A2] {A};
\node (A4) [state, below=0.6cm of A3] {A};
\node (A5) [state, below=0.6cm of A4] {A};
\node (A6) [state, below=0.6cm of A5] {A, B};

\node (B1) [right=2cm of A1] {Node 2};
\node (B2) [state, below of=B1] {A};
\node (B3) [state, below=0.6cm of B2] {A};
\node (B4) [state, below=0.6cm of B3] {A};
\node (B5) [state, below=0.6cm of B4] {A, B};
\node (B6) [state, below=0.6cm of B5] {A, B};

\path[->]
    (A2) edge (A3)
    (A3) edge (A4)
    (A4) edge (A5)
    (A5) edge (A6)
    (B2) edge (B3)
    (B3) edge (B4)
    (B4) edge (B5)
    (B5) edge (B6);

\path[->,dashed,font=\scriptsize]
    (A2) edge node [above] {B?} (B3)
    (B3) edge node [above] {B? ok} (A4)
    (A4) edge node [above] {B!} (B5)
    (B5) edge node [above] {B! ok} (A6);

\end{tikzpicture}

\note{

\tiny

\begin{itemize}

\item Other data stores generally use of two mechanisms for data
replication.  They first is usually called ``clustering''.

\item The idea is that you start both nodes in the same state.  Here,
Node 1 wants to make a change, call it B.  So, Node 1 tells the other
nodes that it wants to make the change.  If everything is ok, the
other nodes reply that they are willing to make the change.  So Node 1
does so, and tells the other nodes to make the change.

\item This is basically two phase commit.  The are more sophisticated
schemes available, but two phase commit is enough to illustrate our
point.

\item Note that clustering is a blocking synchronous process.
Remember, 4 minutes, 4 minutes, 4 minutes, 4 minutes.  All the while
this is happening, neither node can process writes.  Of course, it
could be worse: it could be that the satellite that connects Node 1 to
Node 2 is not visible, and you have to wait until tomorrow to do this.

\item And remember, this entire process has to happen for \emph{every}
change.  Clearly, this isn't an option for the environments we're
considering.

\end{itemize}

}

# Master-slave replication

\tikzset{state/.style={rectangle, draw, text centered}}

\centering

\begin{tikzpicture}

\node (A1) {Node 1};
\node (A2) [state, below of=A1] {A B};
\node (A3) [state, below=0.6cm of A2] {A B C};
\node (A4) [state, below=0.6cm of A3] {A B C D};
\node (A5) [state, below=0.6cm of A4] {A B C D E};
\node (A6) [state, below=0.6cm of A5] {A B C D E};

\node (B1) [right=2cm of A1] {Node 2};
\node (B2) [state, below of=B1] {A};
\node (B3) [state, below=0.6cm of B2] {A B};
\node (B4) [state, below=0.6cm of B3] {A B C};
\node (B5) [state, below=0.6cm of B4] {A B C};
\node (B6) [state, below=0.6cm of B5] {A B C D E};

\path[->]
    (A2) edge (A3)
    (A3) edge (A4)
    (A4) edge (A5)
    (A5) edge (A6)
    (B2) edge (B3)
    (B3) edge (B4)
    (B4) edge (B5)
    (B5) edge (B6);

\path[->,dashed,font=\scriptsize]
    (A2) edge node [above] {B} (B3)
    (A3) edge node [above] {C} (B4)
    (A5) edge node [above] {D E} (B6);

\end{tikzpicture}

\note{

\tiny

\begin{itemize}

\item The other way mechanisms data stores usually use for data
replication is usually called master-slave replication.

\item The idea here is that there's one node called the master which
makes changes to the data, and other nodes called slaves, which just
follow the changes the master makes.  Here, only Node 1 initiates
changes.  Every once in a while, Node 2 gets the changes Node 1 has
made and makes them as well.

\item Note that this is an asynchronous process.  The reason we can
avoid the whole back-and-forth from before is because Node 2
\emph{never} initiates changes to the data.  So, it's perfectly safe
for Node 1 to tell it exactly what to do.

\item Unlike before, this is a non-blocking process.  Node 1 is free
to make changes even if Node 2 isn't up to date.  All that matters is
that Node 2 will be able to catch up at some point in the future.

\item Because it's asynchronous and non-blocking, it's the sort of
mechanism that would work well in the environments we're considering.
Unfortunately, the way every data store implements this in practice is
over TCP, so none of them could actually be used without modification.

\item Furthermore, they usually assume that lossless channels are
available, which is not usually the case.  It would be pretty bad if
an update where lost between Node 1 and Node 2.  Node 2 might then end
up in an inconsistent state.

\item Despite these two practical issues, this is basically what we
want, with one exception.  With master-slave replication, there's only
one node that can initiate changes.  So, we can have Earth Wikipedia,
and replicate it to Mars, but then martians can't edit Wikipedia,
which is not nice at all.

\end{itemize}

}

# Master-master replication?

\tikzset{state/.style={rectangle, draw, text centered}}

\centering

\begin{tikzpicture}

\node (A1) {Node 1};
\node (A2) [state, below of=A1] {A, B};
\node (A3) [state, below=1.4cm of A2] {A, B};
\node (A4) [state, below=1.4cm of A3] {A, B};

\node (B1) [right=2cm of A1] {Node 2};
\node (B2) [state, below of=B1] {A, $\lnot$ B};
\node (B3) [state, below=1.4cm of B2] {A, $\lnot$ B};
\node (B4) [state, below=1.4cm of B3] {A, $\lnot$ B};

\path[->]
    (A2) edge (A3)
    (A3) edge (A4)
    (B2) edge (B3)
    (B3) edge (B4);

\path[->,dashed,font=\scriptsize]
    (A2) edge node [right] {B} (B3)
    (B2) edge node [left] {$\lnot$ B} (A3)
    (B3) edge node [left] {$\lnot$ B} (A4)
    (A3) edge node [right] {B} (B4);

\end{tikzpicture}

\note{

\tiny

\begin{itemize}

\item We want both earthlings and martians to be able to edit
Wikipedia at the same time.  In other words, we want to be able to
make changes to all of the nodes.

\item Since master-master is strictly more general than master-slave,
and also nicer to have, the question that comes to mind is, why is it
that other data stores generally don't offer this?  What's difficult
about master-master?

\item The big problem is diverging changes.  If the nodes can make
changes independently of one another, they can make different changes
to the exact same item.  Here, both nodes agree on A, but they've both
made changes regarding B.  Node 1 says the result should be B, while
Node 2 says the result should be not B.

\item Note that both of them have already committed to making the
respective changes on behalf of their users.  It's not like
clustering, where it's ok for the transaction to fail.  This is a
situation where each node knows that it has the correct data, but it's
getting conflicting changes from on of its peers.  Since we're talking
about peers, rather than masters and slaves, it doesn't have some
simple heuristic like, ignore the other node's changes, or accept the
other node's changes.

\item Since the nodes have made diverging changes to the data, we need
to reconcile them somehow.  But to do this, we'd like to have a bit
more information about the state of the world and we need to be a bit
careful about what changes to the data we can make.

\end{itemize}

}

#

\begin{center}
  \textsc{\large Introducing} \textsc{\LARGE LTc}\\[0.5cm]
  \textsc{\large A replicated data store for high-latency disconnected environments}
\end{center}

\note{

\begin{itemize}

\item We wrote LTc to solve this problem in the aforementioned
environments.  LTc is a replicated data store for high-latency
disconnected environments.  We're now going to talk about LTc's
replication mechanism and how it tries to achieve master-master
replication.

\end{itemize}

}

# What we know

\tikzset{state/.style={rectangle, draw, text centered}}

\centering

\begin{tikzpicture}

\node (A1) {Other data stores};
\node (A2) [state, below of=A1] {\dots};
\node (A3) [state, below=1cm of A2] {\dots};
\node (A4) [state, below=1cm of A3] {\dots};

\node (B1) [right=2cm of A1] {LTc};
\node (B2) [state, below of=B1] {\dots};
\node (B3) [state, below=1cm of B2] {\dots};
\node (B4) [state, below=1cm of B3] {\dots};

\path[->]
    (A2) edge (A3)
    (A3) edge (A4)
    (B2) edge (B3)
    (B3) edge (B4);

\draw[thick,dotted] ($(A4.north west)+(-0.5,0.15)$) rectangle ($(A4.south east)+(0.5,-0.15)$);
\draw[thick,dotted] ($(B2.north west)+(-0.5,0.15)$) rectangle ($(B4.south east)+(0.5,-0.15)$);

\end{tikzpicture}

\note{

\tiny

\begin{itemize}

\item How much data does a data store actually hold?  Most just store
the current value of the data and nothing else.  So, when they get a
write request, they're probably going to do some things in
preparation, then they're going to perform the write, and then they're
going to forget about it.  Once a write has been done, conceptually,
it's not important anymore.  In other words, the data store only know
about the very latest state.

\item LTc takes a hint from version control systems and keeps track of
all the previous states in addition to the current states.  In other
words, LTc knows about the entire chain of states that led to the
latest state.  This turns out to be a big help when we reconcile
diverging changes.

\end{itemize}

}

# Key-value store

\tikzset{state/.style={rectangle, draw, text centered}}

\centering

\begin{tikzpicture}

\node (B1) [right=2cm of A1] {\texttt{[(Key, Value)]}};
\node (B2) [state, below of=B1] {\texttt{[("foo", 23)]}};
\node (B3) [state, below=1cm of B2] {\texttt{[("foo", 23),("bar", 41)]}};
\node (B4) [state, below=1cm of B3] {\texttt{[("foo", 23),("bar", 42)]}};

\path[->]
    (B2) edge (B3)
    (B3) edge (B4);

\end{tikzpicture}

\note{

\tiny

\begin{itemize}

\item What is the layout of the data inside of a data store?  For SQL
databases, it's a bunch of tables, interconnected by constraints.  For
object databases, it's a bunch of nested objects.  For NoSQL data
stores, it's usually some variant of key-value store.

\item Although key-value stores are less featureful than the
alternatives, they are widely used, which implies that they're good
enough for a wide range of applications.  Furthermore, since they're
also easier to reason about, we've chosen to make LTc a key-value
store.

\item So, in LTc, we can represent a state by a set of key-value
pairs.  The basic ways to change a the data store are then to insert a
new key into it, or to update the value of an existing key.

\end{itemize}

}

# Changes

\tikzset{state/.style={rectangle, draw, text centered}}

\centering

\begin{tikzpicture}

\node (B1) [right=2cm of A1] {\texttt{[(Key, Value)]}};
\node (B2) [state, below of=B1] {\texttt{[("foo", 23)]}};
\node (B3) [state, below=1cm of B2] {\texttt{[("foo", 23),("bar", 41)]}};
\node (B4) [state, below=1cm of B3] {\texttt{[("foo", 23),("bar", 42)]}};

\path[->]
    (B2) edge node [right] {\texttt{[("bar", +41)]}} (B3)
    (B3) edge node [right] {\texttt{[("bar", +1)]}} (B4);

\end{tikzpicture}

\note{

\tiny

\begin{itemize}

\item We've said that LTc stores the previous states as well as the
current one.  We can now explain how it does this.

\item First off, like any data store, it stores the current state
as-is.  Then, like version control systems, it also stores the changes
that led to the current state.  Since LTc's data is key-value map, the
changes are key-diff maps.

\item This is easy to see in our example.  The changes that
transformed the first state into the second was the addition of the
"bar" key.  The changes that transformed the second state into the
third was the update of the bar key.  If we had changed more keys at
once, these changes would contain multiple entries.

\item So, LTc stores the latest state, and the chain of changes that
led to it.  Since the changes are reversible, we can also infer any
previous state.  The way you get back to the second state from the
third is by decrementing "bar" by 1.

\end{itemize}

}

# Handling updates --- the setup

\tikzset{state/.style={rectangle, draw, text centered}}

\centering

\begin{tikzpicture}

\node (A1) {Node 1};
\node (A2) [state, below of=A1] {\texttt{[("foo", 23)]}};
\node (A3) [state, below=1cm of A2] {\texttt{[("foo", 24)]}};

\node (B1) [right=4cm of A1] {Node 2};
\node (B2) [state, below of=B1] {\texttt{[("foo", 23)]}};
\node (B3) [state, below=1cm of B2] {\texttt{[("foo", 25)]}};
\node (B4) [state, below=1cm of B3] {\dots};

\path[->]
    (A2) edge node [right] {\texttt{[("foo", +1)]}} (A3)
    (B2) edge node [right] {\texttt{[("foo", +2)]}} (B3)
    (B3) edge (B4);

\path[->,dashed,font=\scriptsize]
    (A3) edge node [below, xshift=-0.5cm] {\texttt{[("foo", +1)]}} (B4);

\end{tikzpicture}

\note{

\tiny

\begin{itemize}

\item So, we've said what LTc does differently: it's a key value store
that keeps track of both current and previous states and the changes
between them.  But how does this help us to reconcile diverging
changes?

\item Suppose we have two nodes that made the following diverging
changes: Node 1 increases "foo" by 1, and Node 2 increases it by 3.
Now suppose Node 1 managed to send its change to Node 2.

\end{itemize}

}

# Handling updates --- the merge

\tikzset{state/.style={rectangle, draw, text centered}}

\centering

\begin{tikzpicture}

\node (B1) {Node 2};
\node (B2) [state, below of=B1] {\texttt{[("foo", 23)]}};
\node (B3) [state, below=1cm of B2, xshift=2cm] {\texttt{[("foo", 25)]}};
\node (B4) [state, below=1cm of B2, xshift=-2cm] {\texttt{[("foo", 24)]}};
\node (B5) [state, below=3cm of B2] {\texttt{["foo", 26]}};

\path[->]
    (B2) edge node [left] {\texttt{[("foo", +1)]}} (B4)
    (B2) edge node [right] {\texttt{[("foo", +2)]}} (B3)
    (B2) edge node [right, yshift=-0.75cm] {\texttt{[("foo", +3)]}} (B5);

\path[->,dashed,font=\scriptsize]
    (B3) edge (B5)
    (B4) edge (B5);

\end{tikzpicture}

\note{

\tiny

\begin{itemize}

\item From Node 2's POV, the situation looks like this.  It knows two
diverging changes have been made to "foo".

\item If we weren't keeping track of the entire history, we have very
few options now.  We would only know the two current states, so we
would be forced to choose between them.  Sometimes, this is enough,
but we can be more general if we use the history.

\item We can take the two current states, we can walk back up the
graph and find their most recent common ancestor, we can take the
differences between this state and the two, we can merge the
difference, and finally apply it to that node.  This way, we can get a
state that's truly a combination of the two diverging states.

\item I should mention that although this kind of merge is LTc's
default behaviour, the user can change it easily.

\end{itemize}

}

# Thank you

\begin{center}
  \Huge Questions?
\end{center}

# Images Sources

\tiny

* Space background: \url{http://dawn.jpl.nasa.gov/feature_stories/images/field_of_stars_full.jpg}

* Earth: \url{http://eoimages.gsfc.nasa.gov/images/imagerecords/57000/57723/globe_west_2048.jpg}

* Queen's Tower: \url{http://www2.imperial.ac.uk/blog/videoarchive/files/2010/02/tower-224x300.jpg}

* Mars: \url{http://nssdc.gsfc.nasa.gov/image/planetary/mars/marsglobe1.jpg}

* Wall-E: \url{http://files.gamebanana.com/bitpit/walle.jpg}

* The Sun: \url{http://www.nasa.gov/images/content/174603main_Image-1A-RIGHT.jpg}
