# Cosmic Conquest 1982

This is a re-creation of a [realtime strategy game published in BYTE Magazine in 1982](https://archive.org/details/byte-magazine-1982-12/page/n131/mode/1up?q=cosmic+conquest). It is credited as one of the first realtime strategy computer games.

I am going to try to get it running in GForth on modern hardware.

Wish me luck.

# Goals

I have two goals:

1. Create a human-verified reference copy of the original source code.
2. If possible, modify the original source code so that it can run on a modern, standards compliant Forth (I am currently targetting GForth).

# How to Help

If you would like to help, please reach out to me on Reddit, LinkedIn or Mastodon.

# How to Run

**NOT COMPLETE- WORK IN PROGRESS**

Assuming you have installed `gforth` installed:

```
gforth main.fs
```

I don't have a preferred GForth version yet. **I have no certainty that GForth will work and may change this decision later. Some discussion on Reddit seems to indicate it will either not work or require source modification.**

# Research and Resources

 * The game appears to have been [re-written to run on the Cosmac ELF](http://cosmacelf.com/publications/newsletters/ipso-facto/ipso-facto-42.pdf) rather than an Apple II (FIG-Forth).

# Quirks

Ongoing log of issues:

 * Variables `COMP-START` and `COMPUTER` have identical documentation comments. Did the technical editor miss this?
 * The word `DELAY` is processor dependent - it will need to be updated to run on modern hardware (game will move too fast otherwise).
 * Certain

# Issues

 * I am still unsure which Forth system this was suposed to run on. The magazine does not specifically say (?).
 * VHTAB, character graphics, etc. will need to implemented on a modern system. [Some old PDFs shed light on how they worked](http://www.cosmacelf.com/publications/newsletters/ipso-facto/ipso-facto-37.pdf).
 * Probably typo in `SET-UP-GALAXY`: /u/erroneousbosh thinks it should read `NO-OF-STARS 0 DO 2 RANDOM1 RANDOM2 GALAXY C!`.
