[![Build status](https://github.com/joaotavora/eglot/actions/workflows/test.yml/badge.svg)](https://github.com/joaotavora/eglot/actions/workflows/test.yml)
[![GNU ELPA](https://elpa.gnu.org/packages/eglot.svg)](https://elpa.gnu.org/packages/eglot.html)
[![MELPA](https://melpa.org/packages/eglot-badge.svg)](https://melpa.org/#/eglot)

# M-x Eglot

*E*macs Poly*glot*: an Emacs [LSP][lsp] client that stays out of your
way:

* 📽 Scroll down this README for some [pretty gifs](#animated_gifs)
* 📚 Read about:
**    [Connecting to a server](#connecting)
**    [Commands and keybindings](#commands)
**    [Workspace configuration](#workspace-configuration)
**    [Customization](#customization)
* 📣 Read the [NEWS][news] file
* 🏆 Folks over at Google [seem to like it][gospb].  Thanks!

# _1-2-3_

Install from [GNU ELPA][gnuelpa] or [MELPA][melpa].  Just type `M-x
package-install RET eglot RET` into Emacs 26.1+.

Now find some source file, any source file, and type `M-x eglot`.

*That's it*. If you're lucky, this guesses the LSP program to start
for the language you're using. Otherwise, it prompts you to enter one.

### _1-2-3-pitfall!_

By design, Eglot doesn't depend on anything but Emacs.  But there
_are_ ELPA dependencies to newer versions of so-called "core packages"
developed _in the Emacs mainline_.  So unless you're using a
bleeding-edge Emacs, where loading `eglot.el` is all you'd need to do,
make sure your package system pulls in and loads the newest
`project.el`, `xref.el`, `eldoc.el`, etc...  In case of trouble `M-x
find-library` can help you tell if that happened.

<a name="connecting"></a>
# Connecting to a server

`M-x eglot` can guess and work out-of-the-box with these servers:

* Ada's [ada_language_server][ada_language_server]
* Bash's [bash-language-server][bash-language-server]
* C/C++'s [clangd][clangd] or [ccls][ccls]
* C#'s [omnisharp][omnisharp]
* Clojure's [clojure-lsp][clojure-lsp]
* CMake's [cmake-language-server][cmake-language-server]
* CSS's [css-languageserver][css-languageserver]
* Dart's [analysis_server][dart-analysis-server]
* Dockerfile's [docker-langserver][docker-langserver]
* Elixir's [elixir-ls][elixir-ls]
* Elm's [elm-language-server][elm-language-server]
* Erlang's [erlang_ls][erlang_ls]
* Fortran's [fortls][fortls]
* Futhark's [futhark lsp][futhark-lsp]
* Go's [gopls][gopls]
* Godot Engine's [built-in LSP][godot]
* HTML [html-languageserver][html-languageserver]
* Haskell's [haskell-language-server][haskell-language-server]
* JSON's [vscode-json-languageserver][vscode-json-languageserver]
* Java's [Eclipse JDT Language Server][eclipse-jdt]
* Javascript's [TS & JS Language Server][typescript-language-server]
* Kotlin's [kotlin-language-server][kotlin-language-server]
* Lua's [lua-lsp][lua-lsp]
* Mint's [mint-ls][mint-ls]
* Nix's [rnix-lsp][rnix-lsp]
* Ocaml's [ocaml-lsp][ocaml-lsp]
* PHP's [php-language-server][php-language-server]
* PureScript's [purescript-language-server][purescript-language-server]
* Python's [pylsp][pylsp], [pyls][pyls] or [pyright][pyright]
* R's [languageserver][r-languageserver]
* Racket's [racket-langserver][racket-langserver]
* Ruby's [solargraph][solargraph]
* Rust's [rust-analyzer][rust-analyzer]
* Scala's [metals][metals]
* TeX/LaTeX's [Digestif][digestif]
* VimScript's [vim-language-server][vim-language-server]
* YAML's [yaml-language-server][yaml-language-server]
* Zig's [zls][zls]

I'll add to this list as I test more servers. In the meantime you can
customize `eglot-server-programs`:

```lisp
(add-to-list 'eglot-server-programs '(foo-mode . ("foo-language-server" "--args")))
```

Let me know how well it works and we can add it to the list.  

To skip the guess and always be prompted use `C-u M-x eglot`.

## Connecting automatically

You can also do:

```lisp
  (add-hook 'foo-mode-hook 'eglot-ensure)
```

, to attempt to start an eglot session automatically every time a
`foo-mode` buffer is visited.

## Connecting via TCP

The examples above use a "pipe" to talk to the server, which works
fine on Linux and OSX but in some cases
[*may not work on Windows*][windows-subprocess-hang].

To circumvent this limitation, or if the server doesn't like pipes,
you can use `C-u M-x eglot` and give it `server:port` pattern to
connect to a previously started TCP server serving LSP information.

If you don't want to start it manually every time, you can configure
Eglot to start it and immediately connect to it.  Ruby's
[solargraph][solargraph] server already works this way out-of-the-box.

For another example, suppose you also wanted start Python's `pyls`
this way:

```lisp
(add-to-list 'eglot-server-programs
             `(python-mode . ("pyls" "-v" "--tcp" "--host"
                              "localhost" "--port" :autoport)))
```

You can see that the element associated with `python-mode` is now a
more complicated invocation of the `pyls` program, which requests that
it be started as a server.  Notice the `:autoport` symbol in there: it
is replaced dynamically by a local port believed to be vacant, so that
the ensuing TCP connection finds a listening server.

<a name="workspace-configuration"></a>
## Workspace configuration

Many servers can guess good defaults and operate nicely
out-of-the-box, but some need to be configured via a special LSP
`workspace/configuration` RPC call to work at all.  Additionally, you
may also want a particular server to operate differently across
different projects.

Per-project settings are realized with the Elisp variable
`eglot-workspace-configuration`. 

Before considering what to set the variable to, one must understand
_how_ to set it.  `eglot-workspace-configuration` is a
"directory-local" variable, and setting its variable globally or
buffer-locally likely makes no sense. It should be set via
`.dir-locals.el` or equivalent mechanisms.

The variable's value is an _association list_ of _parameter sections_
to _parameter objects_.  Names and formats of section and parameter
objects are server specific.

#### Simple `eglot-workspace-configuration`

To make a particular Python project always enable Pyls's snippet
support, put a file named `.dir-locals.el` in the project's root:

```lisp
((python-mode
  . ((eglot-workspace-configuration
      . ((:pyls . (:plugins (:jedi_completion (:include_params t)))))))))
```

This tells Emacs that any `python-mode` buffers in that directory
should have a particular value of `eglot-workspace-configuration`.

Here, the value in question associates section `:pyls` with parameters
`(:plugins (:jedi_completion (:include_params t)))`.  The parameter
object is a plist converted to JSON before being sent to the server.

#### Multiple servers in `eglot-workspace-configuration`

Suppose you also had some Go code in the very same project, you can
configure the Gopls server in the same `.dir-locals.el` file.  Adding
a section for `go-mode`, the file's contents now become:

```lisp
((python-mode
  . ((eglot-workspace-configuration
      . ((:pyls . (:plugins (:jedi_completion (:include_params t))))))))
 (go-mode
  . ((eglot-workspace-configuration
      . ((:gopls . (:usePlaceholders t)))))))
```

Alternatively, as a matter of taste, you may choose this equivalent
setup, which sets the variables value in all all major modes of all
buffers of a given project.

```lisp
((nil
  . ((eglot-workspace-configuration
      . ((:pyls . (:plugins (:jedi_completion (:include_params t))))
         (:gopls . (:usePlaceholders t)))))))
```

#### `eglot-workspace-configuration` without `.dir-locals.el`

If you can't afford an actual `.dir-locals.el` file, or if managing
this file becomes cumbersome, the [Emacs
manual][dir-locals-emacs-manual] teaches you programmatic ways to
leverage per-directory local variables.  Look for the functions
`dir-locals-set-directory-class` and `dir-locals-set-class-variables`.

#### Dynamic `eglot-workspace-configuration` as a function

If you need to determine the workspace configuration base on some
dynamic context, make `eglot-workspace-configuration` a function.  It
is passed the `eglot-lsp-server` instance and runs with
`default-directory` set to the root of your project.  The function
should return a value of the same form as described in the previous
paragraphs.

## Handling quirky servers

Some servers need even more special hand-holding to operate correctly.
If your server has some quirk or non-conformity, it's possible to
extend Eglot via Elisp to adapt to it.  Here's an example on how to
get [cquery][cquery] working:

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

## TRAMP support

Should just work.  Try `M-x eglot` in a buffer visiting a remote file
on a server where you've also installed the language server.  Only
supported on Emacs 27.1 or later.

Emacs 27 users may find some language servers [fail to start up over
TRAMP](https://github.com/joaotavora/eglot/issues/662).  If you experience this
issue, update TRAMP to 2.5.0.4 or later.

<a name="reporting bugs"></a>
# Reporting bugs

Having trouble connecting to a server?  Expected to have a certain
capability supported by it (e.g. completion) but nothing happens?  Or
do you get spurious and annoying errors in an otherwise smooth
operation?  We may have help, so open a [new
issue](https://github.com/joaotavora/eglot/issues) and try to be as
precise and objective about the problem as you can:

1. Include the invaluable **events transcript**.  You can display that
   buffer with `M-x eglot-events-buffer`.  It contains the JSONRPC
   messages exchanged between client and server, as well as the
   messages the server prints to stderr.
    
2. If Emacs errored (you saw -- and possibly heard -- an error
   message), make sure you repeat the process using `M-x
   toggle-debug-on-error` so you **get a backtrace** of the error that
   you should also attach to the bug report.
   
3. Try to replicate the problem with **as clean an Emacs run as
   possible**.  This means an empty `.emacs` init file or close to it
   (just loading `eglot.el`, `company.el` and `yasnippet.el` for
   example, and you don't even need `use-package.el` to do that).
       
Some more notes: it is often the case the you will have to report the
problem to the LSP server's developers, too, though it's
understandable that you report it Eglot first, since it is the
user-facing frontend first.  If the problem is indeed on Eglot's side,
we _do_ want to fix it, but because Eglot's developers have limited
resources and no way to test all the possible server combinations,
you'll sometimes have to do most of the testing.

<a name="commands"></a>
# Commands and keybindings

Here's a summary of available commands:

- `M-x eglot`, as described above;

- `M-x eglot-reconnect` reconnects to current server;

- `M-x eglot-shutdown` says bye-bye to server of your choice;

- `M-x eglot-shutdown-all` says bye-bye to every server;

- `M-x eglot-rename` ask the server to rename the symbol at point;

- `M-x eglot-format` asks the server to format buffer or the active
  region;

- `M-x eglot-code-actions` asks the server for any "code actions" at
  point. Can also be invoked by `mouse-1`-clicking some diagnostics.
  Also `M-x eglot-code-action-<TAB>` for shortcuts to specific actions.

- `M-x eldoc` asks the Eldoc system for help at point (this command
  isn't specific to Eglot, by the way, it works in other contexts).

- `M-x eglot-events-buffer` jumps to the events buffer for debugging
  communication with the server.

- `M-x eglot-stderr-buffer` if the LSP server is printing useful debug
information in stderr, jumps to a buffer with these contents.

- `M-x eglot-signal-didChangeConfiguration` updates the LSP server
configuration according to the value of the variable
`eglot-workspace-configuration`, which you may be set in a
`.dir-locals` file, for example.

There are *no keybindings* specific to Eglot, but you can bind stuff
in `eglot-mode-map`, which is active as long as Eglot is managing a
file in your project. The commands don't need to be Eglot-specific,
either:

```lisp
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
(define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)
```

<a name="customization"></a>
# Customization

Here's a quick summary of the customization options.  In Eglot's
customization group (`M-x customize-group`) there is more
documentation on what these do.

- `eglot-autoreconnect`: Control ability to reconnect automatically to
  the LSP server;

- `eglot-connect-timeout`: Number of seconds before timing out LSP
  connection attempts;

- `eglot-sync-connect`: Control blocking of LSP connection attempts;

- `eglot-events-buffer-size`: Control the size of the Eglot events
  buffer;

- `eglot-ignored-server-capabilities`: LSP server capabilities that
  Eglot could use, but won't;

- `eglot-confirm-server-initiated-edits`: If non-nil, ask for confirmation 
  before allowing server to edit the source buffer's text;

There are a couple more variables that you can customize via Emacs
lisp:

- `eglot-server-programs`: as described [above](#connecting);

- `eglot-strict-mode`: Set to `nil` by default, meaning Eglot is
  generally lenient about non-conforming servers.  Set this to
  `(disallow-non-standard-keys enforce-required-keys)` when debugging
  servers.

- `eglot-server-initialized-hook`: Hook run after server is
  successfully initialized;

- `eglot-managed-mode-hook`: Hook run after Eglot started or stopped
  managing a buffer.  Use `eglot-managed-p` to tell if current buffer
  is still being managed.

- `eglot-stay-out-of`: List of Emacs features that Eglot shouldn't
  automatically try to manage on users' behalf.  Useful when you need
  non-LSP Flymake or Company backends.  See docstring for examples.
  
- `eglot-extend-to-xref`: If non-nil and `xref-find-definitions` lands
  you in a file outside your project -- like a system-installed
  library or header file -- transiently consider it managed by the
  same LSP server.  That file is still outside your project
  (i.e. `project-find-file` won't find it).

# How does Eglot work?

`M-x eglot` starts a server via a shell-command guessed from
`eglot-server-programs`, using the current major-mode (for whatever
language you're programming in) as a hint.

If the connection is successful, you see an `[eglot:<server>]`
indicator pop up in your mode-line.  More importantly, this means
current *and future* file buffers of that major mode *inside your
current project* automatically become \"managed\" by the LSP server,
This means that information about these file's contents is exchanged
periodically to provide enhanced coding assistance.  Eglot works
primarily with Emacs' built-in libraries and _not_ with third-party
replacements for those facilities.

* definitions can be found via `xref-find-definitions`;
* on-the-fly diagnostics for the buffer or project are given by
  `flymake-mode`;
* function signature hints are given by `eldoc-mode`;
* completion can be summoned with `completion-at-point`.
* projects are discovered via `project.el`'s API;

Some extra features are provided if certain libraries are installed
and enabled, such as:

* completion dropdowns via [company];
* snippet completions via [yasnippet];
* marked-up documentation via [markdown].

Eglot doesn't _require_ these libraries to work effectively, but will
use them automatically if they are found to be active.

To "unmanage" a project's buffers, shutdown the server with `M-x
eglot-shutdown`.

<a name="animated_gifs"></a>
# _Obligatory animated gif section_

## Completion
![eglot-completions](./gif-examples/eglot-completions.gif)

The animation shows [company-mode][company] presenting the completion
candidates to the user, but Eglot works with the built-in
`completion-at-point` function as well, which is usually bound to
`C-M-i`.

## Snippet completion
![eglot-snippets-on-completion](./gif-examples/eglot-snippets-on-completion.gif)

Eglot provides template based completion if the server supports
snippet completion and [yasnippet][yasnippet] is enabled _before_
Eglot connects to the server.  The animation shows
[company-mode][company], but `completion-at-point` also works with
snippets.

## Diagnostics
![eglot-diagnostics](./gif-examples/eglot-diagnostics.gif)

Eglot relays the diagnostics information received from the LSP server
to Emacs's [Flymake][flymake], which annotates/underlines the
problematic parts of the buffer.  The information is shared with the
[ElDoc][eldoc] system, meaning that the commands `eldoc` and
`eldoc-doc-buffer` (the latter bound to `C-h-.` for convenience) show
diagnostics along with other documentation under point.

[Flymake][flymake] provides other convenient ways to view and manage
diagnostic errors.  These are described in its [manual][flymake].

When Eglot manages a buffer, it disables pre-existing Flymake
backends.  See variable `eglot-stay-out-of` to change that.

## Code Actions
![eglot-code-actions](./gif-examples/eglot-code-actions.gif)

The LSP server may provide code actions, for example, to fix a
diagnostic error or to suggest refactoring edits.  The commands are
frequently associating with Flymake diagnostic annotations, so that
left-clicking them shows a menu.  Additionally, the command
`eglot-code-actions` asks the server for any code spanning a given
region.

Sometimes, these code actions are initiated by the server.  See
`eglot-confirm-server-initiated-edits` to control that behaviour.

## Hover on symbol /function signature
![eglot-hover-on-symbol](./gif-examples/eglot-hover-on-symbol.gif)

Here, too, the LSP server's view of a given symbol or function
signature is relayed to the [ElDoc][eldoc] system.  The commands
`eldoc` and `eldoc-doc-buffer` commands access that information.

There are customization variables to help adjust [ElDoc][eldoc]'s
liberal use of the lower "echo area", among other options.  If you
still find the solicitous nature of this LSP feature too distracting,
you can use `eglot-ignored-server-capabilities` to turn it off.

## Rename
![eglot-rename](./gif-examples/eglot-rename.gif)

Type `M-x eglot-rename RET` to rename the symbol at point.

## Find definition
![eglot-xref-find-definition](./gif-examples/eglot-xref-find-definition.gif)

To jump to the definition of a symbol, use the built-in
`xref-find-definitions` command, which is bound to `M-.`.

## Find references
![eglot-xref-find-references](./gif-examples/eglot-xref-find-references.gif)

Eglot here relies on Emacs' built-in functionality as well.
`xref-find-references` is bound to `M-?`.  Additionally, Eglot
provides the following similar commands: `eglot-find-declaration`,
`eglot-find-implementation`, `eglot-find-typeDefinition`.

# Historical differences to lsp-mode.el

Around May 2018, I wrote a comparison of Eglot to `lsp-mode.el`, and
was discussed with its then-maintainer.  That mode has since been
refactored/rewritten and now
[purports to support](https://github.com/joaotavora/eglot/issues/180)
a lot of features that differentiated Eglot from it.  It may now be
very different or very similar to Eglot, or even sing with the birds
in the trees, so [go check it out][emacs-lsp].  That said, here's the
original comparison, which I will not be updating any more.

"Eglot is considerably less code and hassle than lsp-mode.el.  In most
cases, there's nothing to configure.  It's a minimalist approach
focused on user experience and performance.

User-visible differences:

- The single most visible difference is the friendly entry point `M-x
  eglot`, not `M-x eglot-<language>`.  Also, there are no
  `eglot-<language>` extra packages.

- There's no "whitelisting" or "blacklisting" directories to
  languages.  `M-x eglot` starts servers to handle file of a major
  mode inside a specific project, using Emacs's built-in `project.el`
  library to discover projects.  Then it automatically detects current
  and future opened files under that project and syncs with server;

- Easy way to quit/restart a server, just middle/right click on the
  connection name;
- Pretty interactive mode-line section for live tracking of server
  communication;
- Automatically restarts frequently crashing servers;
- Slow-to-start servers start asynchronously in the background;
- Server-initiated edits are confirmed with the user;
- Diagnostics work out-of-the-box (no `flycheck.el` needed);
- Smoother/more responsive (read below).
   
Under the hood:

- Message parser is much simpler.
- Defers signature requests like `textDocument/hover` until server is
  ready.
- Sends `textDocument/didChange` for groups of edits, not
  one per each tiny change.
- Easier to read and maintain elisp. Yeah I know, *very subjective*,
  so judge for yourself.
- Doesn't *require* anything other than Emacs, but will automatically
  upgrade to work with stuff outside Emacs, like `company`,
  `markdown-mode`, if you happen to have these installed.
- Has automated tests that check against actual LSP servers."

# Copyright Assignment

`Eglot` is subject to the same [copyright assignment][copyright-assignment]
policy as `GNU Emacs`.

Any [legally significant][legally-significant] contributions can only
be merged after the author has completed their paperwork.  Please ask
for the request form, and we'll send it to you.

<!-- Language servers -->
[ada_language_server]: https://github.com/AdaCore/ada_language_server
[bash-language-server]: https://github.com/mads-hartmann/bash-language-server
[clangd]: https://clang.llvm.org/extra/clangd.html
[omnisharp]: https://github.com/OmniSharp/omnisharp-roslyn
[clojure-lsp]: https://clojure-lsp.io
[cmake-language-server]: https://github.com/regen100/cmake-language-server
[css-languageserver]: https://github.com/hrsh7th/vscode-langservers-extracted
[dart-analysis-server]: https://github.com/dart-lang/sdk/blob/master/pkg/analysis_server/tool/lsp_spec/README.md
[elixir-ls]: https://github.com/elixir-lsp/elixir-ls
[elm-language-server]: https://github.com/elm-tooling/elm-language-server
[fortls]: https://github.com/hansec/fortran-language-server
[futhark-lsp]: https://futhark-lang.org
[gopls]: https://github.com/golang/tools/tree/master/gopls
[godot]: https://godotengine.org
[html-languageserver]: https://github.com/hrsh7th/vscode-langservers-extracted
[haskell-language-server]: https://github.com/haskell/haskell-language-server
[vscode-json-languageserver]: https://github.com/hrsh7th/vscode-langservers-extracted
[eclipse-jdt]: https://github.com/eclipse/eclipse.jdt.ls
[typescript-language-server]: https://github.com/theia-ide/typescript-language-server
[kotlin-language-server]: https://github.com/fwcd/KotlinLanguageServer
[lua-lsp]: https://github.com/Alloyed/lua-lsp
[mint-ls]: https://www.mint-lang.com/
[rnix-lsp]: https://github.com/nix-community/rnix-lsp
[ocaml-lsp]: https://github.com/ocaml/ocaml-lsp/
[php-language-server]: https://github.com/felixfbecker/php-language-server
[purescript-language-server]: https://github.com/nwolverson/purescript-language-server
[pyls]: https://github.com/palantir/python-language-server
[pylsp]: https://github.com/python-lsp/python-lsp-server
[pyright]: https://github.com/microsoft/pyright
[r-languageserver]: https://cran.r-project.org/package=languageserver
[racket-langserver]: https://github.com/jeapostrophe/racket-langserver
[solargraph]: https://github.com/castwide/solargraph
[rust-analyzer]: https://github.com/rust-analyzer/rust-analyzer
[metals]: https://scalameta.org/metals/
[digestif]: https://github.com/astoff/digestif
[vim-language-server]: https://github.com/iamcco/vim-language-server
[yaml-language-server]: https://github.com/redhat-developer/yaml-language-server
[zls]: https://github.com/zigtools/zls

<!-- Other references -->
[lsp]: https://microsoft.github.io/language-server-protocol/
[company-mode]: https://github.com/company-mode/company-mode
[ccls]: https://github.com/MaskRay/ccls
[cquery]: https://github.com/cquery-project/cquery
[docker-langserver]: https://github.com/rcjsuen/dockerfile-language-server-nodejs
[emacs-lsp-plugins]: https://github.com/emacs-lsp
[emacs-lsp]: https://github.com/emacs-lsp/lsp-mode
[erlang_ls]: https://github.com/erlang-ls/erlang_ls
[gnuelpa]: https://elpa.gnu.org/packages/eglot.html
[melpa]: https://melpa.org/#/eglot
[news]: https://github.com/joaotavora/eglot/blob/master/NEWS.md
[windows-subprocess-hang]: https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Subprocess-hang.html
[company]: https://elpa.gnu.org/packages/company.html
[flymake]: https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html#Top
[eldoc]: https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/eldoc.el
[yasnippet]: https://elpa.gnu.org/packages/yasnippet.html
[markdown]: https://github.com/defunkt/markdown-mode
[gospb]: https://opensource.googleblog.com/2020/10/announcing-latest-google-open-source.html
[copyright-assignment]: https://www.fsf.org/licensing/contributor-faq
[legally-significant]: https://www.gnu.org/prep/maintain/html_node/Legally-Significant.html#Legally-Significant
[dir-locals-emacs-manual]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
