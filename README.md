# Clo-Toe - A Twisted Game of Tic-Tac-Toe

See the game live at [kosmik.kapsi.fi/clotoe](http://kosmik.kapsi.fi/clotoe/).

This a personal project for practising Clojure and Reagent.

## Build

Run "`lein figwheel`" in a terminal to compile the app, and then open example.html.

Any changes to ClojureScript source files (in `src`) will be reflected in the running page immediately
(while "`lein figwheel`" is running).

Run "`lein clean; lein with-profile prod compile`" to compile an optimized version.
