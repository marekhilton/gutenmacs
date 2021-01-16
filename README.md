# Gutenmacs

An Emacs package for reading books from project Gutenberg in plaintext.

## Dependencies

Below are the dependencies for this package. Over time I intend to implement fallbacks for the various system dependencies.

### Emacs

- Hydra
- Term
- Dash

### System

- sed
- fzf
- sponge

## Installation

To install this package, clone this repository to a directory of your
choosing, possibly `~/.emacs.d/`. Then add the following lines to your
.emacs or init.el files:

```
(add-to-list 'load-path "path/to/repository")
(require 'gutenmacs)
```

## How to use

Before first use you need to initialise the index used by
gutenmacs. To do this, run `M-x gutenmacs-get-index`. This will take a
little time as emacs downloads the plaintext Gutenberg index and
strips it of non-machine-readable content.

Subsequently a book can be viewed by running the command `M-x
gutenmacs`. This will bring up an fzf process. Type in the name of the
book and use the arrow keys to select the book you want and press
enter. A new hydra dialogue will appear asking you which text file you
want to view (choose any, it usually doesn't matter).

## To Do

This package is functional but there are more things to change and
more things to add. Here are some of the things on my to do list:

- Fallbacks for all system dependencies
- Caching for downloaded books
- Make fzf process more robust to unexpected user inputs
- Favourite list to store books you read often
