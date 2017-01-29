# rjsx-mode.el

[![Build Status](https://secure.travis-ci.org/cjfuller/rjsx-mode.png)](https://travis-ci.org/cjfuller/rjsx-mode)

rjsx-mode.el is an emacs major mode for editing react JSX (and javascript)
files.  It's based off of [web-mode.el](http://web-mode.org), but
slimmed down and refactored for editing just jsx files.

`web-mode.el` is excellent for editing files with mixed languages, but I spend
most of my web programming time in files that are jsx-only and wanted something
easier to understand and modify and focused on that usecase.

## Style

The goal is for the indentation to follow
the [Khan Academy style guidelines for jsx](https://github.com/Khan/style-guides/blob/master/style/react.md).

The simplifying refactor from web-mode.el is still in progress, but indentation
should be up to spec with the style guide.  If not, feel free to add a test and
submit a pull request!

## Installation

Requires the `seq.el` library, which is included in emacs 25+ and is otherwise available on ELPA.
Requires emacs >= 24.

- Put rjsx-mode.el on your load path.
- `(require 'rjsx-mode)`

You might also want to associate rjsx-mode with jsx files automatically.  In your `init.el`:
`(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))`
