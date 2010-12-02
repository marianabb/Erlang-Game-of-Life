Files

 - frame.erl

   interface to + representation of a table.

 - ehtml.erl

   code for the representation of html pages, borrowed from Yaws.

 - dummy.erl

   A very simple test applications. Randomly changes all cells to purple.


To test run, start the frame interface by the call

    frame:init().

and the dummy application with

    dummy:init().

and point a browser to this address:

    http://localhost:8088/
