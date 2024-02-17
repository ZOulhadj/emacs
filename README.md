# Emacs
This is my personal programming-focused Emacs configuration. Feel free to copy
anything to want from my configuration.

## Prerequisites
- rg (Ripgrep search)
- aspell (Spelling)
- aspell-en (English spelling dictionary)
- mbsync (Mail fetching and sending)
- mu4e (Mail indexing)
- mpv (Music Player)


## Building
If building Emacs from source, below are dependencies which are required:
- tree-sitter (Tree sitter)
- librsvg2-dev (SVG support)
- libgccjit0 (Just-in-time compilation)

Compiling from source command:
``./configure --with-native-compilation --with-json --with-pgtk --with-tree-sitter --with-rsvg``

