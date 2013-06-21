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

\begin{figure}[h!]
\centering

\tikzset{state/.style={rectangle, draw, text centered}}

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

\end{figure}

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
and they end in a states where they agree on the data.

\item We're first going to briefly look at how other data stores
handle replication, and why it doesn't quite work in the environments
we mentioned before.

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
