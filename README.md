# bard: a system for poem creation

### Theory

### Installation
Once you have Scheme and a copy of this repository, that's it! Simply `(load
"interpreter.scm")` to have access to all of our code. See usage below for more
information.

---

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
