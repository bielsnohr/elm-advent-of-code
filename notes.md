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

### Deployment

Very little guidance out there on how to actually deploy an Elm application. Nor
any indication that the most standard Elm architecture is a SPA (Single Page
Application).

After a lot of stumbling around, I eventually ended up with what I think is a
way forward, involving these components.

- The way to build with just elm is `elm make src/Main.elm --optimize
  --output=elm.js`
- [rtfeldman-elm-example] adds another step involving Uglify, but I don't this
  is particularly necessary for my simple use case (???)
- then, I found the simple `index.html` that loads this from the base template
  that `elm-spa` produces
- finally, I will need to connect this up to Netlify using the guidance at [elm-land]

This takes inspiration mostly from these sources:

- [rtfeldman-elm-example]
- [elm-spa-tool]
- [elm-land]


[rtfeldman-elm-example]: https://github.com/rtfeldman/elm-spa-example#building
[elm-spa-tool]: https://www.elm-spa.dev/guide
[elm-land]: https://elm.land/guide/deploying.html
