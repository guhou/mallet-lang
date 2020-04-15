# Mallet

[![License](https://img.shields.io/github/license/guhou/mallet-lang)][License]
[![Issues](https://img.shields.io/github/issues/guhou/mallet-lang)][GitHub Issues]
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)][Code of Conduct]

Mallet is an experimental multi-paradigm programming language.

At the moment, Mallet is a hobby project, and is probably not usable for
anything other than exploring type systems. The experimental goal of Mallet is
to explore the contribution of dependent type systems to the developer
experience of programming languages. This experiment is inspired by projects
such as [Idris][], which encourage the principle of 'Type-Driven Development',
as well as projects such as [TypeScript][], which also provides extensive
tooling support to improve developer experience. In particular, the design of
TypeScript as a typed superset of JavaScript means that the ergonomics of
programming with it can 'feel' like a dynamic language, but with the tooling
support associated with static type systems.

## Installation

As this project is currently in its infancy, only source distributions are
provided at present. There are currently no stable versions, so installations
may have bugs or missing features.  Mallet is developed using [Haskell][], and
uses the [Stack][] tools for build. The following installation instructions
assume that Stack is already installed- follow Stack's own installation
instructions first if this is not the case.

Check out Mallet from the Git repository:

```sh
git clone git://github.com/guhou/mallet-lang
cd mallet-lang
```

The build is performed using the Stack build tool. This step may take several
minutes on first build, as Stack will install a package-local compiler
toolchain if a compatible tool version is not currently installed on the system.

```sh
stack build
```

To build a specific target, append the target name to the build command.

```sh
# Build the Mallet interactive environment
stack build mallet-interactive
```

The default build will build the Mallet library and all executables, but all
artifacts will be output into an isolated project directory. To run the built
executable, Stack provides a `run` command to build and run a particular target.

```sh
# Run the Mallet interactive environment
stack run mallet-interactive
```

To install a locally-built executable globally, add the `--copy-bins` flag to
the build step.

```sh
stack build --copy-bins
```

Mallet includes a test suite that describes the behaviour of the Mallet
library. Tests are written using [Hspec][] and can be found in the `tests`
directory of the package. Tests can be run through the `stack` command:

```sh
stack test
```

## Usage

At the moment, there are no practical ways to use Mallet. However, the are
several planned features in this project, including:

### Mallet Language Library

Mallet provides all its language features as a Haskell library. The library is
not currently available on Hackage but a version will be uploaded when initial
features are available.

Planned library features:

- Parsing of text into a rich representation of Mallet expressions
- Type-checking of Mallet expressions
- Code refactoring, such as renaming, finding definitions, etc.
- Translation of the Mallet language to Mallet Core (IR)
- Optimisation transforms on Mallet Core
- Code generation of Mallet Core into an executable form

### Mallet Compiler

Mallet will include a traditional batch-mode compiler, `mallet-compile`. The
batch-mode compiler is an essential tool for building programs, as well as
integrating into other software systems.

### Mallet Interactive

Mallet will include an interactive interface, `mallet-interactive`, for
'REPL'-style development and code querying/exploration. Interactive
environments allow developers to rapidly explore new libraries and design
software.

### Mallet Language Server

Mallet will include a language server, `mallet-language-server`, implementing
the [Language Server Protocol][] for integration into code editors or IDEs.
This enables rich language editing, which greatly improves the developer
experience when writing new programs or editing existing programs. Language
servers are currently a popular approach for rich editing, as it provides a
standardised interface for many editors using a single protocol.

### Mallet Package Manager

Mallet will include a package manager, `mallet-package`, for acquiring and
building Mallet packages and their dependencies.

The planned scope for the package manager is not yet determined, but anecdotal
observations of contemporary language projects indicate that integrating
package management tools into the compiler infrastructure may yield more
efficient and robust development lifecycles.

### Mallet Standard Library

A standard library for Mallet programs. The standard library will include
intrinsic functions and data structures that are implemented as part of the
Mallet runtime, as well as a selection of modules that the majority of programs
are expected to require. The scope of the standard library is yet to be
determined.

Standard library:

- Integer mathematics for machine types
- Floating-point mathematics
- Unicode-aware strings, characters, string processing
- Binary data- byte buffers, encoding, decoding
- Efficient contiguous collections, e.g. array or vector trie
- Efficient keyed collections, e.g. hashtable or HAMT
- Time library for measuring time
- Operating System access for reading and writing files, processes, etc.
- Networking library for opening and manipulating network sockets

## Support

Mallet is currently an experiment developed as a solo hobby project, so support
can only be provided on a 'best-effort' basis. The best way to get support
right now is via [GitHub Issues][]. Please report any bugs found! Support is
provided under the expectation that you will abide by the project's
[Code of Conduct][].

## Contributing

Mallet is currently a solo hobby project in an experimental stage. The best way
to contribute is currently by trying out Mallet and providing feedback, and
reporting any bugs! Contributors are expected to abide by the project's
[Code of Conduct][].

## Code of Conduct

This project is released with a [Contributor Code of Conduct][Code of Conduct].
By participating in this project you agree to abide by its terms.

## License

This project is released under the GNU General Public License version 3 or
later. Full terms of this license are included in [LICENSE.md][License].

[License]: <https://github.com/guhou/mallet-lang/blob/master/LICENSE.md>
[GitHub Issues]: <https://github.com/guhou/mallet-lang/issues>
[Code of Conduct]: <https://github.com/guhou/mallet-lang/blob/master/CODE_OF_CONDUCT.md>
[Idris]: <https://www.idris-lang.org/>
[TypeScript]: <https://www.typescriptlang.org/>
[Haskell]: <https://www.haskell.org/>
[Stack]: <http://haskellstack.org/>
[Hspec]: <https://hspec.github.io/>
[Language Server Protocol]: <https://microsoft.github.io/language-server-protocol/>

