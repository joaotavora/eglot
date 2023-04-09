[![Build status](https://github.com/joaotavora/eglot/actions/workflows/test.yml/badge.svg)][build-status]
[![GNU ELPA](https://elpa.gnu.org/packages/eglot.svg)](https://elpa.gnu.org/packages/eglot.html)
[![MELPA](https://melpa.org/packages/eglot-badge.svg)](https://melpa.org/#/eglot)

# M-x Eglot

*E*macs Poly*glot* is the Emacs [LSP][lsp] client that stays out of
your way:

* 📽 Scroll down this README for some [pretty gifs](#animated_gifs)
* 📚 Read Eglot's [manual][manual] and [release notes][release-notes]
* 🏆 Folks over at Google [seem to like it][gospb].  Thanks!
* 👾 Eglot now lives in [Emacs itself](#emacscore)!

# Get stable [GNU ELPA][gnuelpa] version

Just type `M-x package-install RET eglot RET` into Emacs 26.3+.

Now find some source file, any source file, and type `M-x eglot`.

*That's it*. If you're lucky, this guesses the LSP program to start
for the language you're using.  Otherwise, it prompts you to enter
one.

# Get latest development version from [GNU-Devel ELPA][gnudevelelpa]

First, configure this repository.
```lisp
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
```

Then, use `M-x package-install` or `M-x package-update` to install
an ELPA package from the latest upstream.

<a name=emacscore></a>
# Contribute to Eglot's development

_**Eglot is now in Emacs's core!**_ Upcoming Emacs 29 will have `M-x
eglot` built-in.

The recommended way to experiment with changes to the latest Eglot is
to [compile][compile-emacs1] [Emacs][compile-emacs2]
[yourself][compile-emacs3-official].  

From a development perspective, moving to core allows us to work on
Eglot in tandem with other related packages already in Emacs, such as
[Flymake][flymake], [ElDoc][eldoc], [Xref][xref], [Project][project].

This means adding or tweaking an Emacs LSP feature is a matter of
submitting a single patch targetting multiple relevant packages, not
just Eglot.

These `:core` packages (Eglot included) are then released periodically
to GNU ELPA, so users of other Emacs's versions can get them via
`M-x package-install`.

# Status of this GitHub repository

This repository is **not the development upstream anymore**, but it's
**not** dead (yet):

* It may be used to start [discussions][discussion].

  Sometimes, it's possible the discussion or bug report will be moved
  to [Emacs's bug tracker][emacs-bug-tracker-eglot].  You may take the
  initiative and start discussion there using `M-x report-emacs-bug`
  or simply sending mail to `bug-gnu-emacs@gnu.org`.
  
  Please the [Eglot-specific bug-reporting instructions][bug-reporting].
  
* The [`eglot.el`][eglot.el] file here is periodically updated to mirror
  the [Emacs upstream][upstream-eglot.el]

* The existing tests of [`eglot-tests.el`][eglot-tests.el], also
  periodically updated, may be used to rehearse and validate patches
  using [GitHub CI infrastructure][build-status].

<a name="connecting"></a>
# Connecting to a server

These are just some of the servers that `M-x eglot` can use out of the
box.  The full list can be consulted in the `eglot-server-programs`
variable, where you can [easily add your own servers][manual].

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
* Markdown's [marksman][marksman]
* Mint's [mint-ls][mint-ls]
* Nix's [rnix-lsp][rnix-lsp]
* Ocaml's [ocaml-lsp][ocaml-lsp]
* Perl's [Perl::LanguageServer][perl-language-server]
* PHP's [php-language-server][php-language-server]
* PureScript's [purescript-language-server][purescript-language-server]
* Python's [pylsp][pylsp], [pyls][pyls] [pyright][pyright], or [jedi-language-server][jedi-language-server]
* R's [languageserver][r-languageserver]
* Racket's [racket-langserver][racket-langserver]
* Ruby's [solargraph][solargraph]
* Rust's [rust-analyzer][rust-analyzer]
* Scala's [metals][metals]
* TeX/LaTeX's [Digestif][digestif] ot [texlab][texlab]
* VimScript's [vim-language-server][vim-language-server]
* YAML's [yaml-language-server][yaml-language-server]
* Zig's [zls][zls]

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
still find the solicitous nature of this LSP feature too distracing,
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
[jedi-language-server]: https://github.com/pappasam/jedi-language-server
[vscode-json-languageserver]: https://github.com/hrsh7th/vscode-langservers-extracted
[eclipse-jdt]: https://github.com/eclipse/eclipse.jdt.ls
[typescript-language-server]: https://github.com/theia-ide/typescript-language-server
[kotlin-language-server]: https://github.com/fwcd/KotlinLanguageServer
[lua-lsp]: https://github.com/Alloyed/lua-lsp
[marksman]: https://github.com/artempyanykh/marksman
[mint-ls]: https://www.mint-lang.com/
[rnix-lsp]: https://github.com/nix-community/rnix-lsp
[ocaml-lsp]: https://github.com/ocaml/ocaml-lsp/
[perl-language-server]: https://github.com/richterger/Perl-LanguageServer
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
[texlab]: https://github.com/latex-lsp/texlab
[vim-language-server]: https://github.com/iamcco/vim-language-server
[yaml-language-server]: https://github.com/redhat-developer/yaml-language-server
[zls]: https://github.com/zigtools/zls

<!-- Other references -->
[manual]: https://joaotavora.github.io/eglot
[lsp]: https://microsoft.github.io/language-server-protocol/
[company-mode]: https://github.com/company-mode/company-mode
[ccls]: https://github.com/MaskRay/ccls
[cquery]: https://github.com/cquery-project/cquery
[docker-langserver]: https://github.com/rcjsuen/dockerfile-language-server-nodejs
[emacs-lsp-plugins]: https://github.com/emacs-lsp
[emacs-lsp]: https://github.com/emacs-lsp/lsp-mode
[erlang_ls]: https://github.com/erlang-ls/erlang_ls
[gnuelpa]: https://elpa.gnu.org/packages/eglot.html
[gnudevelelpa]: https://elpa.gnu.org/devel/eglot.html
[melpa]: https://melpa.org/#/eglot
[news]: https://github.com/joaotavora/eglot/blob/master/NEWS.md
[windows-subprocess-hang]: https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Subprocess-hang.html
[company]: https://elpa.gnu.org/packages/company.html
[flymake]: https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html#Top
[xref]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html
[imenu]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html
[eldoc]: https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/eldoc.el
[yasnippet]: https://elpa.gnu.org/packages/yasnippet.html
[markdown]: https://github.com/defunkt/markdown-mode
[gospb]: https://opensource.googleblog.com/2020/10/announcing-latest-google-open-source.html
[copyright-assignment]: https://www.fsf.org/licensing/contributor-faq
[legally-significant]: https://www.gnu.org/prep/maintain/html_node/Legally-Significant.html#Legally-Significant
[dir-locals-emacs-manual]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
[configuration-request]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_configuration
[did-change-configuration]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeConfiguration
[json-serialize]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-JSON.html
[plist]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html
[discussion]: https://github.com/joaotavora/eglot/discussions
[upstream-eglot.el]: https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/eglot.el
[eglot.el]: https://github.com/joaotavora/eglot/blob/master/eglot.el
[eglot-tests.el]: https://github.com/joaotavora/eglot/blob/master/eglot-tests.el
[announcement]: https://github.com/joaotavora/eglot/discussions
[compile-emacs1]: https://lars.ingebrigtsen.no/2014/11/13/welcome-new-emacs-developers/
[compile-emacs2]: https://batsov.com/articles/2021/12/19/building-emacs-from-source-with-pgtk/
[compile-emacs3-official]: https://github.com/emacs-mirror/emacs/blob/master/INSTALL
[emacs-bug-tracker-eglot]: https://debbugs.gnu.org/cgi/pkgreport.cgi?include=subject%3Aeglot;package=emacs
[bug-reporting]: https://joaotavora.github.io/eglot/#Troubleshooting-Eglot
[project]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html
[emacs-upstream]: https://github.com/emacs-mirror/emacs
[release-notes]: https://github.com/emacs-mirror/emacs/blob/master/etc/EGLOT-NEWS
[build-status]: https://github.com/joaotavora/eglot/actions/workflows/test.yml
