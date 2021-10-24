# Cosmic Conquest 1982

This is a re-creation of a [realtime strategy game published in BYTE Magazine in 1982](https://archive.org/details/byte-magazine-1982-12/page/n131/mode/1up?q=cosmic+conquest). It is credited as one of the first realtime strategy computer games.

The code is available as `cosmic_conquest.fs`. It's runability in modern times is still unknown.

# Goals

1. ~Create a human-verified reference copy of the original source code.~ DONE.
2. If possible, modify the original source code so that it can run on a modern, standards compliant Forth (I am currently targetting GForth).

# How to Help

If you would like to help, please reach out to me on Reddit, LinkedIn or Mastodon.

# How to Run

Work in progress. It appears that the game was written in some flavor of FIGForth for the Apple II (possibly FIGForth '78?)

# Research and Resources

 * The game appears to have been [re-written to run on the Cosmac ELF](http://cosmacelf.com/publications/newsletters/ipso-facto/ipso-facto-42.pdf) rather than an Apple II (FIG-Forth).

# Quirks

Ongoing log of issues:

 * The word `DELAY` is processor dependent - it will need to be updated to run on modern hardware (game will move too fast otherwise).

# Issues

 * I am still unsure which Forth system this was suposed to run on. The magazine does not specifically say (?).
 * VHTAB, character graphics, etc. will need to implemented on a modern system. [Some old PDFs shed light on how they worked](http://www.cosmacelf.com/publications/newsletters/ipso-facto/ipso-facto-37.pdf).
 * `CASE`, `OF`, `ENDOF`, `ENDCASE` are included for historical reasons but probably need to be removed from playable version.
