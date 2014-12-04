# gigamesh

## About

The program is inspired by a chapter within Richard Dawkins' book "The blind Watchmaker". 

Within this chapter, Dawkins visualizes the power behind evolutionary theory by developing a computer program drawing "biomorphs".
A biomorph's look only depends on a handful of genes.
These genes are all inherited from the "elder", the previous generation, except for one mutation in one particular gene. 

In each round, the user chooses which mutation should become the elder for the following round. 
This way, the user is in responsibility of the natural selection, a process inherent in evolution. 

## Naming

The name of this Haskell implementation goes back to the Epic of Gilgamesh, the first known piece of literature descibing how mankind was created by the gods.
It was taken since in early versions of the program, many biomorphs tended to look like gigantic meshes. 

## Installation

Warning: This program bases on Gtk, which seems to be a pita to install on Windows.

If you do not already have a running Haskell environment, the easiest way to get this working is probably to install a current Haskell platform [1], follow the steps described within [2] and type "cabal install" in the root directory.

[1] https://www.haskell.org/platform/

[2] https://www.haskell.org/haskellwiki/Gtk2Hs/Installation
