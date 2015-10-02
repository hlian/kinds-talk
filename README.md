HAO LIAN

gave a talk

VARIED & DIVERSE

about type-level programming

IT WAS SORT OF ABOUT KINDS

but not really

TO GET STARTED (OS X users)

* Go to [this page](https://github.com/commercialhaskell/stack/releases/) and download the latest release.
* As of writing, that is [this .tar.gz](https://github.com/commercialhaskell/stack/releases/download/v0.1.5.0/stack-0.1.5.0-x86_64-osx.tar.gz).
* Extract to a directory. (Say, `foo`.)
* Open iTerm (or Terminal).
* `mkdir -p ~/.local/bin`
* `mv foo/stack ~/.local/bin/stack`
* `export PATH=~/.local/bin:$PATH`

You'll want to put that last statement in your `.bashrc` or `.zshrc` so that it runs automatically every time a shell starts. Now go to somewhere on your computer where you clone the code strange and mysterious strangers.

* `git clone git@github.com:hlian/kinds-talk.git`
* `cd kinds-talk`
* `stack setup`
* `stack build`

It should just work (?????)

TO GET STARTED (Windows/Linux users)

Same instructions, except [you'll have to read this page on installing Stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md).

TO GET A REPL

`stack ghci`

ONCE YOU ARE IN THE REPL

* `:r` (`:reload`) to reload all code
* `:bro` (`:browse`) to list all code
* `:t` (`:type`) to look at the type of a value
* `:k` (`:kind`) to look at the type of a type (we call this a "kind")
* `:i` (`:info)` to look at the type of a type and all its instances and other goodies
