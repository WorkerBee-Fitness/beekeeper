# Beekeeper
The purpose of Beekeeper is to relieve anxiety by remembering things using a
simple command-line interface. Currently, the things it can remember for you are
bookmarks to locations of files + directories and aliases of commands. Then you
can easily recall your bookmarks or run your aliases by using the label you
create for them. The following video illustrates Beekeeper's features:

![Beekeeper Demi](doc/README/assets/bk-demo.gif)

# Installation

You will need [Stack](https://docs.haskellstack.org/en/stable/) a project
management tool for the [Haskell](https://www.haskell.org/) programming
language. Then clone this repository and run the following to install Beekeeper:

```.bash
> stack build
> stack install
```

After installation there is no setup necessary. Beekeeper saves all of it's data
to `~/.bk` by default. We strive to keep all of Beekeeper's data human readable
and not dependent on any heavy external libraries or tools. This is the reason
we chose [CSV](https://en.wikipedia.org/wiki/Comma-separated_values) as
Beekeeper's underlying data format.

# Usage

The following sections give examples of every feature Beekeeper supports.

## Adding a bookmark or alias
To add a bookmark use the following command:

```.bash
> bk add bookmark LABEL=TARGET
```

the string `LABEL` is the name of the bookmark and its value is `TARGET`. For
example, `bk add bookmark math-proj="path/to/math/project"` will add a bookmark
with label `math-proj` whose target is `path/to/math/project`.


