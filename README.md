[![Build Status](https://travis-ci.org/joaotavora/eglot.png)](https://travis-ci.org/joaotavora/eglot)
Eglot
-----

*E*macs Poly*glot*. An Emacs client to Language Server Protocol servers.

```
(add-to-list 'load-path "/path/to/eglot")
(require 'eglot)

M-x eglot
```

*That's it*. Either this guesses the local LSP program to start for
the language of your choice or it prompts you for such a
thing. Currently, if you have these programs installed, it works
out-of-the-box for:

* Javascript's [javascript-typescript-stdio][javascript-typescript-langserver]
* Rust's [rls][rls]
* Python's [pyls][pyls]

You can also enter a `server:port` pattern to connect to an LSP
server. To skip the guess and always be prompted use `C-u M-x eglot`.

# Supported Protocol features

- [x] textDocument/didChange (incremental)
- [x] textDocument/didClose
- [x] textDocument/didOpen
- [x] textDocument/didSave

- [ ] textDocument/codeAction
- [x] textDocument/completion
- [ ] completionItem/resolve
- [x] textDocument/definition
- [ ] textDocument/documentHighlight
- [x] textDocument/documentSymbol
- [ ] textDocument/executeCommand
- [ ] textDocument/format
- [x] textDocument/hover
- [ ] textDocument/rename
- [x] textDocument/references
- [ ] textDocument/signatureHelp
- [x] workspace/symbol

# Differences to lsp-mode.el

This is really beta and currently does less than
[lsp-mode.el][emacs-lsp] which is more
mature. Though I think `eglot.el` will eventually beat it, you could
be better served with `lsp-mode.el` for now.

User-visible differences:

- Single entry point, `M-x eglot` to enable LSP in a project.
  Automatically detects current and future opened files under that
  project and syncs with server.
- Easy way to restart a server
- Pretty interactive mode-line section for live tracking of server
  communication.
   
Differences under the hood:

- Message parser is much much simpler
- Easier to read and maintain elisp. Yeah I know, subjective... But
  judge for yourself.
- No external dependencies apart from Emacs (no `flycheck.el`, no
  `projectile.el`, no Cask, etc).
- Uses project.el, flymake.el
- Requires the upcoming emacs 26
- Contained in one file
- send `textDocument/didChange` for groups of edits, not one per each
  tiny change. 
- Its missing tests! This is *not good*

[rls]: https://github.com/rust-lang-nursery/rls
[pyls]: https://github.com/palantir/python-language-server
[javascript-typescript-langserver]: https://github.com/sourcegraph/javascript-typescript-langserver
[emacs-lsp]: https://github.com/emacs-lsp/lsp-mode


   
