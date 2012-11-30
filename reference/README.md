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

### Darcs Patch Theory

> <https://en.wikibooks.org/w/index.php?title=Understanding_Darcs/Patch_theory&oldid=2216663>

Patch theory is an algebra for patches.  It talks about patches as an
almost-group (patch application is not a closed operation).  It uses
commutation (which is not to say patches are commutative) to re-order
the patch history before doing some operations.

### Rsync Algorithm

> <https://en.wikipedia.org/wiki/Rsync>

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
