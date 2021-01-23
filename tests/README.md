TESTS
=====


## Directory structure

There are three directories: **ada**, **c** and **py**. But maybe others will
be added in the future. Each of them will have the following directory
structure:

```
bin/
   test-lang
analyzer.awk
run-tests.sh
lang/
   obj/
   src/
   compile.sh
```

In the [bin](bin) directory the executables will be stored ready to be
executed. Each of the files inside it are test-_lang_.
[analyzer.awk](analyzer.awk) will take input from Standard Error from the tests
and anlyze it and make some kind of readable report.
[run-tests.sh](run-tests.sh) will run each of the tests, it takes as arguments
the languages or **all** by default.
_lang_ is the language where the tests will be taken, _lang_ will be replaced
by each of the directories listed above.
In the _lang/src/_ directory the source files will be stored.
In the _lang/obj/_ directory temporary files will be stored.
The _lang/compile.sh_ file will be used to compile all the tests found under
_lang/src/_, it may call an external command like _gprbuild_ in ada.


## Tests structure

In the _lang/src_ directory there are various files with the extension of their
respective programming language. There are also some shared functions for
input/output/delay...

There is one test for each child package of Malef. For example:
**test_malef.ada** or **test_malef_colors.ada**. Each of them will test several
things, there might be somethings popping into the terminal, but once it
finishes it prints into Standard Error (**stderr**) the information about the
tests results.
**NOTE**: Output cannot be written into stdout because it might collide with
          the Malef.

Each of those files, will contain every function from each package and test it
with different inputs.

### Special tests

There are some kind of tests that requiere a real person to be around to check
them and give approval. These are the visual ones.


## Tests output format

Each of the lines output by each of the programmes will be divided by TABS or
better known as character number 0x09. The output will be passed to an awk
script that will analyze it and prepare it for the user to see it. It will keep
track of every error found. Each of the items separated by tabs will be called
**words** in this document from now on.

### First word
The first word will tell which kind of line it is (case-sensitive), these are
the available options each of them followed by the number of words that can be
followed by it:

 * language <language=ada,c,python3> (The language)
 * package <name> (The name of the package that will be tested)
 * failed <function/procedure name> <info>
 * passed <function/procedure name> <time>
 * fatal <name> (A fatal error in a package that can't be recovered)
 * skipped <name> A procedure that has been skipped.


### Info
The info is a word passed by both **failed** and **passed** words which
tells what has happened there.

#### Failed
For **failed**, it has two words: <expected> <got>
The expected is the result that was **expected** to have and **got** is what
it had got. What had been got can be an _error_ or a wrong value.
If the **expected** string starts with a _$_, then it failed but it's not
important, it's called "known_to_fail" in the source files.
If the **expected** string starts with a _@_, then it may be user's fault.
