## inf-crystal.el
*Run a Inferior-Crystal process in a buffer*

---
[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
[![MELPA](https://melpa.org/packages/inf-crystal-badge.svg)](https://melpa.org/#/inf-crystal)
[![MELPA Stable](https://stable.melpa.org/packages/inf-crystal-badge.svg)](https://stable.melpa.org/#/inf-crystal)

inf-crystal provides a REPL buffer connected
to a [icr](https://github.com/crystal-community/icr) subprocess.
It's based on ideas from the popular `inferior-lisp` package.

`inf-crystal` has two components - a basic crystal REPL
and a minor mode (`inf-crystal-minor-mode`), which
extends `crystal-mode` with commands to evaluate forms directly in the
REPL.

`inf-crystal` provides a set of essential features for interactive
Crystal development:

* REPL
* Interactive code evaluation

### ICR

To be able to connect to [inf-crystal](https://github.com/brantou/inf-crystal.el),
you need to make sure that [icr](https://github.com/crystal-community/icr) is installed.
Installation instructions can be found on
the main page of [icr](https://github.com/crystal-community/icr#installation).

### Installation

#### Via package.el

Available on all major ~package.el~ community maintained repos -
[MELPA Stable](https://stable.melpa.org/#/) and [MELPA](https://melpa.org/#/) repos.

MELPA Stable is recommended as it has the latest stable version.
MELPA has a development snapshot for users who don't mind breakage but
don't want to run from a git checkout.

You can install `inf-crystal` using the following command:

```elisp
M-x package-install [RET] inf-crystal [RET]
```

or if you'd rather keep it in your dotfiles:

```elisp
  (unless (package-installed-p 'inf-crystal)
    (package-refresh-contents)
    (package-install 'inf-crystal))
```

If the installation doesn't work try refreshing the package list:

```elisp
M-x package-refresh-contents
```

#### Manual

If you're installing manually, you'll need to:
* drop the file somewhere on your load path (perhaps ~/.emacs.d)
* Add the following lines to your .emacs file:

```elisp
   (autoload 'inf-crystal "inf-crystal" "Run an inferior Crystal process" t)
   (add-hook 'crystal-mode-hook 'inf-crystal-minor-mode)
```

### Usage

Run one of the predefined interactive functions.

See [Function Documentation](#function-documentation) for details.


### Function Documentation


#### `(inf-crystal-reset)`

Clear out all of the accumulated commands.

#### `(inf-crystal-toggle-debug-mode)`

Toggle debug mode off and on.
In debug mode icr will print the code before executing it.

#### `(inf-crystal-enable-paste-mode)`

Enable paste mode.

#### `(inf-crystal-disable-paste-mode)`

Disable paste mode.

#### `(inf-crystal-clear-repl-buffer)`

Clear the REPL buffer.

#### `(inf-crystal-quit &optional BUFFER)`

Kill the REPL buffer and its underlying process.

You can pass the target BUFFER as an optional parameter
to suppress the usage of the target buffer discovery logic.

#### `(inf-crystal-restart &optional BUFFER)`

Restart the REPL buffer and its underlying process.

You can pass the target BUFFER as an optional parameter
to suppress the usage of the target buffer discovery logic.

#### `(inf-crystal CMD)`

Launch a crystal interpreter in a buffer.
using ‘inf-crystal-interpreter’as an inferior mode.

Argument CMD defaults to ‘inf-crystal-interpreter’.
When called interactively with ‘prefix-arg’, it allows
the user to edit such value.

#### `(crystal-switch-to-inf EOB-P)`

  Switch to the inf-crystal process buffer.
With argument, positions cursor at end of buffer.

#### `(crystal-send-last-sexp)`

Send the previous sexp to the inferior crystal process.

#### `(crystal-send-line)`

Send the current line to the inferior crystal process.

#### `(crystal-send-definition)`

Send the current definition to the inferior crystal process.

#### `(crystal-send-definition-and-go)`

Send the current definition to the inferior Crystal.
Then switch to the process buffer.

#### `(crystal-send-region START END)`

Send the region delimited by START and END to inferior crystal process.

#### `(crystal-send-region-and-go START END)`

Send the region delimited by START and END to inferior crystal process.
Then switch to the process buffer.

#### `(crystal-send-buffer)`

Send the current buffer to the inferior crystal process.

#### `(crystal-send-buffer-and-go)`

Send the current buffer to the inferior crystal process.
Then switch to the process buffer.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
