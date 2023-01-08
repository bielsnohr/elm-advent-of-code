# Notes for elm-advent-of-code

## Year 2022

### Day 5

- This was quite tricky because handling array or list data when then these data types
  are not mutable is a different paradigm of processing.
- And a common theme has been my difficulty using the `Parser` libraray of Elm.
  It really isn't terribly intuitive. The introductory documentation could be
  much better.

### Day 6

- Just parsing a string using sets.
- Took a bit of time to think how to do this without a loop, and in this case I
  decided on recursion.

### General

- Debugging is not a great experience in Elm. From what I can tell, there is no
  dedicated debugger like pdb or gdb. Instead, one is relegated to using
  `Debug.log` statements, which then pop up in the Debug console of the browser
  in which the program is run. *It doesn't come out of the console output!* Took
  me a while to figure that out.
