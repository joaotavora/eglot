# Eglot manual

* [Introduction](#what-is-eglot)
* [Setting up](#setting-up-eglot)
  - [Choosing an LSP server](#choosing-an-lsp-server)
  - [Connecting to a server](#connecting-to-a-server)
  - [Project organization](#project-organization)
  - [Eglot-managed buffers](#eglot-managed-buffers)
  - [Shutting down a server](#shutting-down-a-server)
  - [Troubleshooting](#troubleshooting)

* [Using it](#using-eglot)
  - [Commands](#commands)
  - [Keybindings](#keybindings)
  - [Opt-in features](#opt-in-features)

* [Configuration](#configuring-eglot)
  - [Overriding Eglot's choices](#overriding-eglots-choices)
  - [Summary of Eglot variables](#summary-of-eglot-variables)
  - [`eglot-workspace-configuration`](#eglot-workspace-configuration)
  - [Handling quirky servers](#handling-quirky-servers)

## What is Eglot?

Eglot -- *E*macs Poly*glot* -- is the Emacs LSP client that stays out
of your way.

The [Language Server Protocol (LSP)][language-server-protocol] defines
standardized communication between source code editors and language
server programs, which analyze that source code on behalf of the
editors.

Using Eglot, Emacs may be enriched with a language server and its
findings, such as the location of functions calls, types of variables,
class definitions or syntactic mistakes.  This enables new editing
features and significantly enhances Emacs existing features such as
[automatic code completion, go-to definition, at-point documentation,
automatic refactoring, diagnostics, and more][readme-pretty-gifs].

As the user edits, creates and deletes source files, clients
continuously inform the server of this, so that the server can update
its analysis.

LSP enables a single language server to be useful to many different
editors and a single editor to speak to many different language
servers.

As long as there is a server language to connect to, Eglot doesn't
require Emacs to know anything about a particular programming language
to enable these features.

## Setting up Eglot

<a name="choosing-lsp-server"></a>
### Choosing an LSP server

For Eglot to be useful, it must first be combined with a suitable LSP
server.

Frequently, that means running a given program locally as a [child
process][inferior-process] locally and "talking" to it via the
standard input and output streams.

The LSP server program must be installed separately by the user.
Doing so is currently outside the scope of this manual, but [see
here][servers] for a comprehensive list of servers.

Eglot must be made aware of the programming languages servers purport
to understand.  Coupling servers to file name extensions directly is
brittle, so Eglot uses a more standard Emacs way to identify
programming languages, the [major mode][major-mode].

<a name="eglot-server-programs"></a> The variable
`eglot-server-programs` is an [alist][alist] pairing Emacs [major
modes][major-mode] with server program invocations.  A fairly complete
set of associations to popular language servers is predefined in this
variable.  Except for this information, Eglot is completely
language-agnostic, just as prescribed by the LSP protocol.  There is
no need to load any special `<language-mode>-eglot` Elisp packages.


In their simplest form, elements of this variable look like
`(MAJOR-MODE . CONTACT)`.  `MAJOR-MODE` is a symbol or a list of
symbols naming major modes.  `CONTACT` is the invocation strategy for
the server, typically a list of strings designating an executable
program and its command line arguments.

Adding new server associations to `eglot-server-programs` is generally
straightforward.  Say there is an hypothetical `fools` language server
program for the language _Foo_, which is handled by Emacs's
`foo-mode`.  To add it to the alist:

```lisp
(add-to-list 'eglot-server-programs
             '(foo-mode . ("fools" "--stdio")))
```

Here, `MAJOR-MODE` is the symbol `foo-mode` and `CONTACT` is the list
of strings containing the executable name `fools` and a single
hypothetical command line argument, `--stdio`, asking it to
communicate via the standard streams.  The executable `fools` should
be in the [`exec-path`][exec-path] variable or in the [PATH
environment variable][exec-path], otherwise Eglot won't be able to
find it when it needs to (see [Starting Eglot](#starting)).

The forms for `MAJOR-MODE` and `CONTACT` take other more complex
forms, for example to handle multiple major modes with one server or
to connect to running server processes over the network.  Read the
[docstring][docstring] of `eglot-server-programs` for more
information.

<a name="starting"></a>
### Connecting to a server

The most common way to start Eglot is to simply visit a source file of
a given language and use the command `M-x eglot`.

If the connection to the language server is successful, the user sees
an `[eglot:<server>]` indicator pop up in the mode line.  Otherwise,
if the server program couldn't be started or somehow didn't behave, an
error message is printed.  The user may attempt to
[troubleshoot](#troubleshoot) the situation.

Rarely there is the need to type `M-x eglot` more than a few times in
each Emacs session to start new server connections.  This is because
Eglot understands project groupings (see [Project
organization](#project-organization)) and uses re-uses the same
connection for profiles files.  Thus, after explicitly starting a
connection for some buffer, that connection is automatically selected
when visiting files in the same project (see [Eglot-managed
buffers](#eglot-managed-buffers)).

If the user is absolutely confident that Eglot can be started smoothly
for any given file of any given project, a special `eglot-ensure` hook
can be added to the major-mode.  The following line can appear in the
user's configuration file.

```lisp
(add-hook 'foo-mode-hook 'eglot-ensure)
```

As the name implies, this will start a new server (if one isn't found)
for any new `foo-mode` file visited.

When Eglot connects to a given server for the first time, it runs the
hook [`eglot-connect-hook`](#summary-of-eglot-variables).

<a name="project-organization"></a>
### Project organization

Eglot works in tandem with Emacs's project management infrastructure,
the [Project][project.el] package.  The file where the `M-x eglot`
invocation takes place is required to belong to a project which groups
it with other files.

Fortunately, this requirement is rarely problematic: most often there
is nothing to configure if the project grouping is implicit or can be
divined by the Project package.

This happens, for example:

* in the common case of "one off" scripts contained in a single file.

* in the case of multiple files all living under the same directory.

* in the case of multiple files in a directory hierarchy under the
  same version control.

In other situations, it is necessary to use the [Project][project.el]
facilities to configure a more complex project.

When Eglot needs to initiate a server program, it does so in the the
project's root directory.  This is usually ensures
the language server has an the same overarching vision of the project's files.

So, if the user visits the file `~/projects/fooey/lib/x.foo` and
`x.foo` belongs to a project rooted in `~/projects/fooey` (perhaps
because a `.git` directory exists there), then `M-x eglot` causes the
server program to start with that root as the current working
directory.  The server then undertakes to analyze not only the
`lib/x.foo` file just visited, but likely also the all `*.foo` files
that live under `.../projects/fooey`.

In some cases, additional information specific to a given project
(which in LSP parlance is called a "workspace") may be communicated to
the server.  See the variable
[`eglot-workspace-configuration`](#eglot-workspace-configuration).

<a name="eglot-managed-buffers"></a>
### Eglot-managed buffers

While Eglot is active, it is said to be _managing_ a subset of project
files of a specific major-mode under the same server connection.  As
evidence of this, an indicator `[eglot:<server/project-name>]` can be
seen in the mode line (mouse-clicking it brings up some
server-specific help).

For the duration of Eglot's tenure over a buffer, a special minor mode
(see [minor modes][minor-mode]) is active.  This mode ensures that:

* Modifying the buffer by adding or removing some text (even if done
  automatically by another package) causes Eglot to communicate this
  change to the LSP server.  The file needn't be saved for this to
  happen.

* Various configuration variables that govern some [Emacs
  facilities](#features) are set to LSP-specific versions whose job is
  query the active LSP server for the needed data and transform this
  data into whatever format the feature expects;

* Finding or creating a new file under the same project automatically
  adds that file to the set of files being managed by the server, and
  activates the minor mode.

  Visiting the file `/home/joe/projects/fooey/lib/y.foo` will search
  for a server managing `foo-mode` files in that project.  If found,
  Eglot will automatically inform the language server that it started
  managing `y.foo`.

Unlike other minor-modes, the special Eglot minor-mode is not
activated manually by the user.  However, the minor mode hook
[`eglot-managed-mode-hook`](#summary-of-eglot-variables) may be
manipulated by the user just like any other minor mode hook.

### Shutting down a server

When the command `M-x eglot-shutdown` is invoked (or if the server
process unexpectedly terminates), Eglot ceases to manage all the
buffers of the current server connection (see
[Commands](#commands)). The special minor mode is deactivated and the
pre-Eglot configuration of Emacs variables is restored.

Note that unless the user variable
[`eglot-autoshutdown`](#summary-of-eglot-variables) is true, killing
the last buffer managed by a given server process is not enough to
shut the process down.  This is so that visiting a new eligible file
in a buffer-less project will again bring up Eglot.

<a name="troubleshoot"></a>
### Troubleshooting

The commands `M-x eglot-events-server` and `M-x eglot-stderr-buffer`
pop a special buffer that can be used to inspect the communication
between the LSP client and server.

A common and easy-to-fix cause of performance problems is the length
of these log buffers.  If Eglot is operating correctly but slowly, the
variable [`eglot-events-buffer-size`](#summary-of-eglot-variables) can
be used to restrict logging and speed things up.

#### Reporting bugs

Having trouble connecting to a server?  Expected to have a certain
feature supported by it (e.g. completion) but nothing happens?  Or
do you get spurious and annoying errors in an otherwise smooth
operation?

The Eglot maintainers may have help, but they need the best bug report
possible.

Because there are so many variables involved, Eglot's reality is that
it is generally both very **difficult** and **absolutely essential**
to reproduce bugs exactly as they happened to the user.  Every bug
report should include:

1. The invaluable events transcript obtained with `M-x
   eglot-events-buffer`.  If the transcript can be narrowed down to
   show the problematic exchange, so much the better.

2. If Emacs errored (an error message was seen or heard), make sure to
   repeat the process after toggling `debug-on-error` on (via `M-x
   toggle-debug-on-error`).  This normally produces a backtrace of the
   error that should also be attached to the bug report.

3. An explanation how to obtain and install the language server used.  If
   possible, try to replicate the problem with the C/C++
   [Clangd][clangd] or Python [Pylsp][pylsp] servers, as these are
   very easy to install.

4. A description of how to setup a **minimal** project (one or two
   files and their contents) where the problem happens.

5. A recipe to replicate the problem with **a clean Emacs run**.
   **PLEASE**, this means an `emacs -Q` invocation or a very minimal
   (max 10 lines) `.emacs` initialization file.  `eglot-ensure` and
   `use-package` are generally *not* needed.

6. Make sure to double check all the above elements and re-run the
   recipe.

#### Some bugs are server-specific or package-specific

Since Eglot is the user-facing front-end of Emacs LSP functionality, it
is understandable that bugs are first reported as Eglot problems.

However, it is often the case that the underlying cause of the problem
lies with the LSP server, and in this case the Eglot maintainers will
eventually ask you to report the problem to those developers.

Other times, the problem lies somewhere between Eglot and an auxiliary
package such as [ElDoc][eldoc], [Xref][xref], [Project][project.el] or
[Flymake][flymake].  It's sometimes tempting to suggest an quick
Eglot-specific solution to a problem, but frequently the problem must
be handled in tandem with these packages.

Please be aware that Eglot developers have limited resources and no
way to test all the possible server combinations.  This means you'll
sometimes have to do most of the testing.

<a name="features"></a>
<a name="auxiliary packages"></a>
## Using Eglot

Eglot uses LSP to activate modern IDE features in Emacs.  These
features are provided via a number of auxiliary packages:

* at-point documentation, via the [ElDoc][eldoc] package;
* on-the-fly diagnostic annotations with server-suggested fixes, via
  the [Flymake][flymake] package;
* definition chasing/cross-referencing, via the [Xref][xref] package;
* in-file symbolic navigation, via the [Imenu][imenu] package;
* completion (via [Emacs built-in front-ends][symbol-completion],
  [Company][company-mode], or other front-ends)

The working principles of these packages are described in the linked
documentation.  For convenience, the most important user-facing
commands relevant to Eglot are summarized
[here](#package-specific-commands).

Experienced users will notice that these auxiliary facilities aren't
exactly new in Emacs.  For lack of adequate configuration, they are
sometimes inactive by default.  Traditionally, each major-mode tries
to configure a subset of them, at least partially.  Users commonly
fill in the gaps in their personal configurations.  In general, it is
time-consuming to achieve some kind of unifying consistency across
different packages in
the same major mode or across different major modes in the same package.

This is the main problem solved by LSP and Eglot: Eglot links up Emacs
packages to LSP features (also known as [LSP
capabilities][lsp-capabilities]) by temporarily configuring the
auxiliary packages while it is active in some buffer (see
[Eglot-managed buffers](#eglot-managed-buffers)).

Not all servers support the full set of LSP capability, but most of
them support enough to enable the basic set of features enumerated
above.  Conversely, some servers offer capability for which an Emacs
equivalent package doesn't yet exist, and so Eglot can't link up the
two.

There are some cases where Eglot itself offers a user-facing facility
for a given capability.  Examples are the refactoring commands
`eglot-rename` and `eglot-code-action-organize-imports` (see
[Commands](#commands)).

<a name="commands"></a>
### Commands

Here is a list of the most commonly used Eglot commands:

- `M-x eglot`, as described [above](#starting);

- `M-x eglot-reconnect` reconnects to current server;

- `M-x eglot-shutdown` says bye-bye to a server of the user'sr choice;

- `M-x eglot-shutdown-all` says bye-bye to every server;

- `M-x eglot-rename` ask the server to rename the symbol at point;

- `M-x eglot-format` asks the server to format buffer or the active
  region;

- `M-x eglot-code-actions` asks the server for any "code actions" at
  point. Can also be invoked by `mouse-1`-clicking some diagnostics.
  Also `M-x eglot-code-action-<TAB>` for shortcuts to specific actions.

<a name="package-specific-commands"></a>

The following commands of auxiliary packages are very commonly used in
[Eglot-managed buffers](#eglot-managed-buffers) as a way to access its
main features:

- `M-x eldoc` asks the ElDoc system for help at point;

- `M-x flymake-show-buffer-diagnostics` asks [Flymake][flymake] system to
  display diagnostics for the current buffer;

- `M-x flymake-show-project-diagnostics` asks [Flymake][flymake] to list
  diagnostics for all files in the current project;

- `M-x xref-find-definitions` asks [Xref][xref] to go the definition
  of the thing at point;

- `M-x imenu` lets the user navigate to elements of the program,
  categorizing them by syntactic class (class, method, variable, etc.)
  and offering completion;

- `M-x completion-at-point` requests completion of the current thing
  at point.

The following are less commonly used Eglot commands:

- `M-x eglot-events-buffer` jumps to the events buffer for
  [troubleshooting](#troubleshooting) communication with the server.

- `M-x eglot-stderr-buffer` if the LSP server is printing useful debug
information in stderr, jumps to a buffer with these contents.

- `M-x eglot-signal-didChangeConfiguration` updates the LSP server
configuration according to the value of the variable
[`eglot-workspace-configuration`](#eglot-workspace-configuration).

### Keybindings

There are *no keybindings* specific to Eglot, but the user can bind
stuff in [`eglot-mode-map`](#summary-of-eglot-variables), which is
active as long as Eglot is managing a file in a project.  The commands
don't need to be Eglot-specific:

```lisp
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
(define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)
```

### Opt-in features

Eglot is designed to work with a simple vanilla installation of Emacs
and its built-in packages.  However it will automatically detect and
use a small set of more exotic extra package:

* If [Yasnippet][yasnippet] is installed, some completions provided by
  the LSP server expand into interactive snippets;

* If [Markdown Mode][markdown-mode] is installed, at-point
  documentation supplied by the server will be formatted accordingly
  before being displayed;

* If [Company][company-mode] is installed, it is configured to access
  LSP completions exclusively.  This can be changed by [overriding
  Eglot's defaults](#overriding-defaults).

* If a remote file is being visited via [TRAMP][tramp], `M-x eglot`
  will attempt to start the server in the remote machine.  Things
  should mostly "just work", though it is recommended to first try
  using Emacs directly on the machine where the project and language
  exist.

  This is only supported on Emacs 27.1 or later.  Emacs 27 users may
  find some language servers [fail to start up over
  TRAMP](https://github.com/joaotavora/eglot/issues/662).  This issue
  can be solved by updating TRAMP to 2.5.0.4 or later.


## Configuring Eglot

As seen in section [Using Eglot](#using-eglot), Eglot largely plays an
intermediary role between existing auxiliary Emacs packages and LSP
functionality.

There are [relatively few Eglot-specific
variables](#summary-of-eglot-variables) to study.  This is by design:
if Eglot had these variables, it could be duplicating configuration
found elsewhere, bloating itself up, and making it generally hard to
integrate with the ever growing set of LSP features and Emacs
packages.

This means that fine-tuning a the Eglot LSP experience is done by
either tweaking the small set of Eglot variables or configuring the
variables of said auxiliary packages.  For example:

* To configure the face used for LSP errors and warnings, the Flymake
  faces `flymake-error` and `flymake-error` should be used;

* To configure the amount of space taken up by documentation in the
  echo area, the ElDoc variable `eldoc-echo-area-use-multiline-p`
  should be used;

* To completely change how the at-point documentation destination is
  displayed, the ElDoc variable `eldoc-display-functions` can be used;

* To use a different completion front end, like
  [company][company-mode] for presenting LSP completions, simply
  install that front-end.  Eglot's use of [Emacs's
  completion-generating API][capf] should be sufficient as long as the
  front-end also abides by that API;

* To instruct the LSP server to never a specific capability, like
  formatting-as-you-type, add the symbol
  `:documentOnTypeFormattingProvider` to the customization variable
  [`eglot-ignored-server-capabilities`](#summary-of-eglot-variables).

<a name="overriding-defaults"></a>
### Overriding Eglot's choices

When linking up an LSP capability to a given Emacs package, Eglot
tries to provide sensible good default for beginner users that doesn't
clash with Emacs's defaults.

Often, more demanding users want to tweak these decisions to better
fit their ways of working.

For example, a common case is controlling the sources of LSP-generated
information that reaches the [ElDoc][eldoc] at-point documentation
front-end.

ElDoc's variable `eldoc-documentation-functions` is set by default by
Eglot in a manner similar to:

```lisp
(setq-local eldoc-documentation-functions
            ;; function signature, followed by function documentation
            '(eglot-signature-eldoc-function
              eglot-hover-eldoc-function))
```

This can be changed so that any error or warning diagnostics at point
are emitted to the ElDoc display, taking precedence.  A good way to do
this is to use `eglot-managed-mode-hook`:

```lisp
(defun my/setup-eglot-eldoc ()
  (push 'flymake-eldoc-function eldoc-documentation-functions))
(add-hook 'eglot-managed-mode-hook 'my/setup-eglot-eldoc)
```

Another way to override Eglot's defaults is to ask it to refrain from
configuring an auxiliary package entirely. The variable
`eglot-stay-out-of` can be used for this:

```lisp
(add-to-list 'eglot-stay-out-of 'eldoc)
(add-to-list 'eglot-stay-out-of 'flymake)
```

In this case, it is up to the user that added these lines to configure
[ElDoc][eldoc] and [Flymake][flymake] manually, using
`eglot-managed-mode-hook` or some other variable-setting mechanism.

### Summary of Eglot variables

Here's a quick summary of the customization options.  In Eglot's
customization group (`M-x customize-group`) there is more
documentation on what these do.

- `eglot-autoreconnect` controls the ability to reconnect automatically to
  the LSP server;

- `eglot-connect-timeout` is the number of seconds before timing out LSP
  connection attempts;

- `eglot-sync-connect` controls blocking of slow LSP connection
  attempts;

- `eglot-events-buffer-size` controls the size of the Eglot events
  buffer;

- `eglot-ignored-server-capabilities` lists [LSP server
  capabilities][lsp-capabilities] that Eglot could use, but won't.

- `eglot-confirm-server-initiated-edits`: If non-nil, ask for confirmation
  before allowing server to edit the source buffer's text;

There are some more variables that can be customized via Emacs lisp:

- `eglot-server-programs`: as described [above](#connecting);

- `eglot-strict-mode`: Set to `nil` by default, meaning Eglot is
  generally lenient about non-conforming servers.  Set this to
  `(disallow-non-standard-keys enforce-required-keys)` when debugging
  servers.

- `eglot-server-initialized-hook`: Hook run after the server object is
  successfully initialized;

- `eglot-connect-hook`: Hook run after connection to the server is
  successfully established;

- `eglot-managed-mode-hook`: Hook run after Eglot started or stopped
  managing a buffer.  Use `eglot-managed-p` to tell if current buffer
  is still being managed.

- `eglot-stay-out-of`: List of Emacs features that Eglot shouldn't
  automatically try to manage on users' behalf.  Useful when you need
  non-LSP Flymake or Company back-ends.  See docstring for examples.

- `eglot-extend-to-xref`: If non-nil and `xref-find-definitions` lands
  you in a file outside your project -- like a system-installed
  library or header file -- transiently consider it managed by the
  same LSP server.  That file is still outside your project
  (i.e. `project-find-file` won't find it).

- `eglot-workspace-configuration`: a "directory variable" for
  per-project settings [described separately
  here](#eglot-workspace-configuration).

### `eglot-workspace-configuration`

Many servers can guess good defaults and operate nicely
out-of-the-box, but some need to know project-specific settings, which
LSP calls "workspace configuration".

Within Eglot, these per-project settings are realized with the Elisp
variable `eglot-workspace-configuration`.  They are sent over to the
server:

* initially, as a [`didChangeConfiguration` notification][did-change-configuration];
* as the response to [configuration request][configuration-request] from the server.

##### How to set (and whether to set it at all)

Before considering what to set the variable to, one must understand
how to set it and whether to set it at all.

Most servers can be configured globally using some kind of global file
in the user's home directory or in the project directory --
[Pylsp][pylsp] reads `~/.config/pycodestyle` and [Clangd][clangd]
reads `.clangd` anywhere up the current project tree.

This type of configuration is done completely independently from Eglot
and Emacs and has the advantage that it'll work with other LSP clients.

On the other hand, one may find this not flexible enough or wish to
consolidate all configuration within Emacs.

In that case, the [directory  variable][dir-locals-emacs-manual]
`eglot-workspace-configuration` should be used.

Note that while it is possible to set this variable globally or
buffer-locally, doing so makes little sense.  It is usually set via
`.dir-locals.el` or [special-purpose Elisp
functions][dir-locals-emacs-manual].

##### Format of the value

The recommended format for this variable's value is a [_property
list_][plist]:

```lisp
(SECTION-1 PARAM-OBJECT-1 ... SECTION-N PARAM-OBJECT-N)
```

(Yes, earlier it used to be an association list, a format that is
still supported, but discouraged.)

Each `SECTION-N` is an Elisp keyword naming a parameter section
relevant to an LSP server.

`PARAM-OBJECT-N` contains one or more settings pertaining to the
server that is interested in `SECTION-N`.  Its value is an Elisp
object serialized to JSON by [`json-serialize`][json-serialize].  The
recommended format is again a [plist][plist], though `json-serialize`
also accepts other formats.

In any case, the JSON values `true`, `false` and `{}` are represented
by the Elisp values `t`, `:json-false` and `nil`, respectively.

When experimenting with settings, one may use `M-x
eglot-show-workspace-configuration` to inspect/debug the definite JSON
value sent over to the server.  This helper function works even before
actually connecting to the server.

##### Simple example

To make a particular Python project always enable [Pylsp][pylsp]'s
snippet support, put a file named `.dir-locals.el` in the project's
root:

```lisp
((python-mode
  . ((eglot-workspace-configuration
      .
      ;; the value in the format described above starts here
      (:pylsp (:plugins (:jedi_completion (:include_params t
                                           :fuzzy t)
                         :pylint (:enabled :json-false))))
      ;; and ends here
      ))))
```

This tells Emacs that any `python-mode` buffers in that directory
should have a particular value of `eglot-workspace-configuration`.

Here, the value in question associates the parameter section `:pylsp`
with a parameter object that is a plist of plists.  It is converted to
JSON before being sent to the server:

```json
{
  "pylsp": {
    "plugins": {
      "jedi_completion": { "include_params": true, "fuzzy": true },
      "pylint": { "enabled": false }
    }
  }
}
```

##### Multiple servers

Suppose one also has some Go code in the very same project, the
[Gopls][gopls] server can be configured in the same `.dir-locals.el`
file.  Adding a section for `go-mode`, the file's contents now become:

```lisp
((python-mode
  . ((eglot-workspace-configuration
      . (:pylsp (:plugins (:jedi_completion (:include_params t
                                             :fuzzy t)
                           :pylint (:enabled :json-false)))))))
 (go-mode
  . ((eglot-workspace-configuration
      . (:gopls (:usePlaceholders t))))))
```

Alternatively, as a matter of taste, one may choose to lay out
`.dir-locals.el` like so:

```lisp
((nil
  . ((eglot-workspace-configuration
      . (:pylsp (:plugins (:jedi_completion (:include_params t
                                             :fuzzy t)
                           :pylint (:enabled :json-false)))
         :gopls (:usePlaceholders t))))))
```

This is an equivalent setup which sets the value in all major-modes
inside the project: the major-mode specification is unneeded because
the LSP server will retrieve only the parameter section it is
interested in.

##### Setting the value without `.dir-locals.el`

If adding a `.dir-locals.el` file isn't suitable, or if managing this
file becomes cumbersome, the [Emacs manual][dir-locals-emacs-manual]
teaches you programmatic ways to leverage per-directory local
variables.  Look for the functions `dir-locals-set-directory-class`
and `dir-locals-set-class-variables`.

##### Dynamically setting the value

If one needs to determine the workspace configuration based on some
dynamic context, `eglot-workspace-configuration` can be set to a
function.  It is passed the `eglot-lsp-server` instance of the
connected server (if any) and runs with `default-directory` set to the
root of your project.  The function should return a value of the same
form as described in the previous paragraphs.

### Handling quirky servers

Some servers need special hand-holding to operate correctly.  If your
server has some quirk or non-conformity, it's possible to extend Eglot
via Elisp to adapt to it.  Here's an example on how to get
[cquery][cquery] working:

```lisp
(add-to-list 'eglot-server-programs '((c++ mode c-mode) . (eglot-cquery "cquery")))

(defclass eglot-cquery (eglot-lsp-server) ()
  :documentation "A custom class for cquery's C/C++ langserver.")

(cl-defmethod eglot-initialization-options ((server eglot-cquery))
  "Passes through required cquery initialization options"
  (let* ((root (car (project-roots (eglot--project server))))
         (cache (expand-file-name ".cquery_cached_index/" root)))
    (list :cacheDirectory (file-name-as-directory cache)
          :progressReportFrequencyMs -1)))
```

Similarly, some servers require the language identifier strings they
are sent by `eglot` to match the exact strings used by VSCode. `eglot`
usually guesses these identifiers from the major mode name
(e.g. `elm-mode` → `"elm"`), but the mapping can be overridden using
the `:LANGUAGE-ID` element in the syntax of `eglot-server-programs` if
necessary.

[language-server-protocol]: https://microsoft.github.io/language-server-protocol/
[major-mode]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Major-Modes.html
[inferior-process]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Processes.html
[alist]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html
[exec-path]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Subprocess-Creation.html
[docstring]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Variables.html
[project.el]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html
[minor-mode]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Minor-Modes.html
[servers]: https://github.com/joaotavora/eglot/blob/master/README.md#connecting-to-a-server
[readme-pretty-gifs]: https://github.com/joaotavora/eglot/blob/master/README.md#animated_gifs
[company-mode]: https://github.com/company-mode/company-mode
[lsp-capabilities]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#capabilities
[eldoc]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Doc.html
[flymake]: https://www.gnu.org/software/emacs/manual/html_mono/flymake.html
[xref]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html
[imenu]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html
[symbol-completion]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Symbol-Completion.html#Symbol-Completion
[capf]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html
[markdown-mode]: https://jblevins.org/projects/markdown-mode/
[yasnippet]: https://github.com/joaotavora/yasnippet
[tramp]: https://www.gnu.org/software/tramp/
[pylsp]: https://github.com/python-lsp/python-lsp-server
[clangd]: https://clang.llvm.org/extra/clangd.html
[dir-locals-emacs-manual]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
[json-serialize]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-JSON.html
[plist]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html
[gopls]: https://github.com/golang/tools/tree/master/gopls
[cquery]: https://github.com/cquery-project/cquery
[configuration-request]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_configuration
[did-change-configuration]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeConfiguration
