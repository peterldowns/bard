# bard: a system for poem creation

### Theory
Poems are generally sequences of words that fit some pattern, most often based
on rhyming structure or number of syllables. For example, a “haiku” poem is a
series of 5 syllables, followed by a series of 7 syllables, followed by a
series of 5 syllables. Other types of poems are defined by their rhyme
structure – a couplet is a pair of end-rhymed lines. We propose to implement a
means of expressing these types of constraints and then automatically solving
them to end up with a poem.


### Usage
An example use of the library is included in `demo.scm`. You can load this demo
from the command line:

```bash
$ mit-scheme --load demo.scm
```

and then interpret an example haiku as follows:

```scheme
1 ]=> (print-poem (interpreter test-haiku))
anemones reside
souffle inoperable
souvenir cleveland
```

If you'd like to hack on or learn more about the code, all of the code is
(theoretically) nicely formatted and reasonably well-commented. Read the source
for more details, or email one of the authors -- they'd be happy to hear from
you!


### Installation
Once you have Scheme and a copy of this repository, that's it! Simply `(load
"interpreter.scm")` to have access to all of our code. See usage above for more
information.

##### Rebuilding the dictionary

If you'd like to re-build the source dictionary, you'll need Python and some
libraries.  Once you've installed Python and
[`pip`](https://pip.pypa.io/en/latest/installing.html) (a Python dependency
manager interface), you can install these libraries like so:

```bash
$ cd vocabulary
$ pip install -r requirements.txt
```

You'll also need to install the [`rhyme`](http://rhyme.sourceforge.net/) tool,
whose modified source is included in the `rhyme-0.9` directory. The
installation instructions are located in the `INSTALL` file in that directory.

Once that's done, you can re-run the scraper and re-build the dictionary like
this:

```bash
$ cd vocabulary
$ make clean
$ make
```

This will take about 4-5 hours, so unless you have some particular reason to do
so we advise against it.
