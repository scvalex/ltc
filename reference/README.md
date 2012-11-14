References
==========

Background Reading
------------------

### Subversion's Delta Editor: Interface As Ontology

> [subversion-delta-editor.pdf](https://github.com/scvalex/ltc/blob/master/reference/subversion-delta-editor.pdf)

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

### Git Internals -- Transfer Protocols

> [git-transfer.pdf](https://github.com/scvalex/ltc/blob/master/reference/git-transfer.pdf)

Git separates the task of downloading/uploading the changes, and
updating the working tree.  Only the transfer part is interesting to
us.

In order to exchange changes, Git queries the remote for its
references, then walks through the commits, downloading or uploading
changes.  This is all done with many separate requests.

### Two Cons against NoSQL

> [two-cons-nosql.pdf](https://github.com/scvalex/ltc/blob/master/reference/two-cons-nosql.pdf)

The author notes two current issues with NoSQL databases:

 * it's hard to transfer data from one NoSQL product into another, and

 * there's no standard way to access a NoSQL data-store.

The author then proceeds to interview several people who explain how
his issues are fictive, or only true in some narrow sense.
