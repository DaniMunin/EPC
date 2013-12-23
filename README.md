EPC
===

EPC is a Erlang-based parallel crawler for an image indexation engine.


Install
-------

Similarity search is based on pHash, which is used as a
[port](http://www.erlang.org/doc/tutorial/c_port.html),
so it must be compiled like this:

    unix> g++ -o priv/phash_port include/phash_port.c $PATH_TO_PHASH/src/pHash.o -I$PATH_TO_PHASH/ -I$PATH_TO_PHASH/src/  -lpng -lpthread -ljpeg


Mnesia DB must be installed by calling epc_dba:install(list_of_nodes), where list_of_nodes can be simply [node()].

Start up
--------

Compile the files

    unix> erl -make

Start the interpreter on the `ebin/` directory.

    unix> erl -pa ebin/ 

And start the services

    1> master:init().
    true
    2> indexer:start().
    ok
    3> phash:start().
    <0.66.0>`
