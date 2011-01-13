Copyright 2011 Erlang Solutions Ltd
Author: ulf.wiger@erlang-solutions.com

DEQC - Distributed EQC Testing
==============================

This is a collection of utilities for facilitating the use of 
Erlang QuickCheck (EQC) in testing distributed applications.

deqc_proxy
----------
A simple module to start, stop and talk to a slave node.
This module uses gproc and slave and provides a small API:

`start(Alias, Options)`
  Starts a node

`stop(Alias, How)`
  Stops a node. How :: shutdown | halt | kill

`disconnect(Alias)`
  Disconnects the node from all other nodes, except the master node.

`rpc(Alias, M, F, Args)`
  Performs an rpc call to the node.

Support for various options will follow.