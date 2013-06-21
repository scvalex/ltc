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
