[![Build Status](https://travis-ci.org/joaotavora/eglot.png)](https://travis-ci.org/joaotavora/eglot)
Eglot
-----

*E*macs Poly*glot*. An Emacs client to [Language Server Protocol][lsp] servers.

Eglot is [in ELPA][gnuelpa]. Installation is straightforward:

```
(package-install 'eglot) ; Requires Emacs 26!
;; Now find some source file, any source file
M-x eglot
```

*That's it*. If you're lucky, this guesses the LSP executable to start
for the language of your choice. Otherwise, it prompts you to enter one:

`M-x eglot` can guess and work out-of-the-box with these servers:

* Javascript's [javascript-typescript-stdio][javascript-typescript-langserver]
* Rust's [rls][rls]
* Python's [pyls][pyls]
* Bash's [bash-language-server][bash-language-server]
* PHP's [php-language-server][php-language-server]

I'll add to this list as I test more servers. In the meantime you can
customize `eglot-server-programs`:

```lisp
(add-to-list 'eglot-server-programs '(fancy-mode . ("fancy-language-server" "--args"")))
```

Let me know how well it works and we can add it to the list.  You can
also enter a `server:port` pattern to connect to an LSP server. To
skip the guess and always be prompted use `C-u M-x eglot`.

# Commands and keybindings

Here's a summary of available commands:

- `M-x eglot`, as described above;

- `M-x eglot-reconnect` reconnects to the server;

- `M-x eglot-shutdown` says bye-bye to the server;

- `M-x eglot-rename` asks the server to rename the symbol at point;

- `M-x eglot-help-at-point` asks the server for help for symbol at
  point. Currently this is what `eldoc-mode` displays in the echo
  area;

- `M-x eglot-events-buffer` jumps to the events buffer for debugging
  communication with the server.

There are *no keybindings* specific to Eglot, but you can bind stuff
in `eglot-mode-map`, which is active as long as Eglot is managing a
file in your project. The commands don't need to be Eglot-specific,
either:

```
(define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)
(define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)
```

# How does this work exactly?

`M-x eglot` starts a server via a shell-command guessed from
`eglot-server-programs`, using the current major-mode (for whatever
language you're programming in) as a hint.

If the connection is successful, you see an `[eglot:<server>]`
indicator pop up in your mode-line.  More importantly, this means
current *and future* file buffers of that major mode *inside your
current project* automatically become \"managed\" by the LSP server,
i.e.  information about their contents is exchanged periodically to
provide enhanced code analysis via `xref-find-definitions`,
`flymake-mode`, `eldoc-mode`, `completion-at-point`, among others.

To "unmanage" these buffers, shutdown the server with `M-x
eglot-shutdown`.

# Supported Protocol features (3.6)

## General
- [x] initialize
- [x] initalized
- [x] shutdown
- [x] exit
- [ ] $/cancelRequest

## Window
- [x] window/showMessage
- [x] window/showMessageRequest
- [x] window/logMessage
- [x] telemetry/event

## Client
- [x] client/registerCapability (but only
  `workspace/didChangeWatchedFiles`, like RLS asks)
- [x] client/unregisterCapability  (ditto)

## Workspace
- [ ] workspace/workspaceFolders (3.6.0)
- [ ] workspace/didChangeWorkspaceFolders (3.6.0)
- [ ] workspace/didChangeConfiguration
- [ ] workspace/configuration (3.6.0)
- [x] workspace/didChangeWatchedFiles
- [x] workspace/symbol is
- [x] workspace/applyEdit

## Text Synchronization
- [x] textDocument/didOpen
- [x] textDocument/didChange (incremental or full)
- [x] textDocument/willSave
- [x] textDocument/willSaveWaitUntil
- [x] textDocument/didSave
- [x] textDocument/didClose

## Diagnostics
- [x] textDocument/publishDiagnostics

## Language features
- [x] textDocument/completion
- [x] completionItem/resolve (works quite well with [company-mode][company-mode])
- [x] textDocument/hover
- [x] textDocument/signatureHelp (fancy stuff with Python's [pyls][pyls])
- [x] textDocument/definition
- [ ] textDocument/typeDefinition (3.6.0)
- [ ] textDocument/implementation (3.6.0)
- [x] textDocument/references
- [x] textDocument/documentHighlight
- [x] textDocument/documentSymbol
- [ ] textDocument/codeAction
- [ ] textDocument/codeLens
- [ ] codeLens/resolve
- [ ] textDocument/documentLink
- [ ] documentLink/resolve
- [ ] textDocument/documentColor
- [ ] textDocument/colorPresentation (3.6.0)
- [ ] textDocument/formatting 
- [ ] textDocument/rangeFormatting
- [ ] textDocument/onTypeFormatting
- [x] textDocument/rename

# Differences to lsp-mode.el

Eglot is **beta**. It may currently underperform
[lsp-mode.el][emacs-lsp], both in functionality and correctness. That
other extension is much more mature and has a host of
[plugins][emacs-lsp-plugins] for bells and whistles.  If you don't
like the minimalist approach of `eglot.el`, you could be better served
with `lsp-mode.el` for now.

User-visible differences:

- Single and friendly entry point `M-x eglot`, not `M-x
  eglot-<language>`. Also no `eglot-<language>` extra packages.
- No "whitelisting" or "blacklisting" directories to languages. `M-x
  eglot` starts servers to handle major modes inside a specific
  project. Uses Emacs's built-in `project.el` library to discover
  projects. Automatically detects current and future opened files
  under that project and syncs with server.
- Easy way to quit/restart a server, just middle/right click on the
  connection name.
- Pretty interactive mode-line section for live tracking of server
  communication.
- Automatically restarts frequently crashing servers (like RLS).
- Server-initiated edits are confirmed with the user.
- Diagnostics work out-of-the-box (no `flycheck.el` needed).
- Smoother/more responsive (read below).
   
Under the hood:

- Message parser is much simpler.
- Defers signature requests like `textDocument/hover` until server is
  ready. Also sends `textDocument/didChange` for groups of edits, not
  one per each tiny change.
- Easier to read and maintain elisp. Yeah I know, *very subjective*,
  so judge for yourself.
- About 1k LOC lighter.
- Development doesn't require Cask, just Emacs.
- Project support doesn't need `projectile.el`, uses Emacs's `project.el`
- Requires the upcoming Emacs 26
- Contained in one file
- Has automated tests that check against actual LSP servers
  

[lsp]: https://microsoft.github.io/language-server-protocol/
[rls]: https://github.com/rust-lang-nursery/rls
[pyls]: https://github.com/palantir/python-language-server
[gnuelpa]: https://elpa.gnu.org/packages/eglot.html
[javascript-typescript-langserver]: https://github.com/sourcegraph/javascript-typescript-langserver
[emacs-lsp]: https://github.com/emacs-lsp/lsp-mode
[emacs-lsp-plugins]: https://github.com/emacs-lsp
[bash-language-server]: https://github.com/mads-hartmann/bash-language-server
[php-language-server]: https://github.com/felixfbecker/php-language-server
[company-mode]: https://github.com/company-mode/company-mode

   
