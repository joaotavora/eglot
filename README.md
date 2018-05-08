[![Build Status](https://travis-ci.org/joaotavora/eglot.png)](https://travis-ci.org/joaotavora/eglot)
Eglot
-----

*E*macs Poly*glot*. An Emacs client to [Language Server Protocol][lsp] servers.

```
(add-to-list 'load-path "/path/to/eglot")
(require 'eglot) ; Requires emacs 26!

;; Now find some project file inside some Git-controlled dir
M-x eglot
```

*That's it*. Either this guesses the LSP executable to start for the
language of your choice, or it prompts you to enter the program.

If you have these programs installed, `M-x eglot` works out-of-the-box
with:

* Javascript's [javascript-typescript-stdio][javascript-typescript-langserver]
* Rust's [rls][rls]
* Python's [pyls][pyls]

You can also enter a `server:port` pattern to connect to an LSP
server. To skip the guess and always be prompted use `C-u M-x eglot`.

# Supported Protocol features

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
- [ ] client/registerCapability
- [ ] client/unregisterCapability

## Workspace
- [ ] workspace/workspaceFolders (3.6.0)
- [ ] workspace/didChangeWorkspaceFolders (3.6.0)
- [ ] workspace/didChangeConfiguration
- [ ] workspace/configuration (3.6.0)
- [ ] workspace/didChangeWatchedFiles
- [x] workspace/symbol
- [x] workspace/applyEdit

## Text Synchronization
- [x] textDocument/didOpen
- [x] textDocument/didChange (incremental or full)
- [x] textDocument/willSave
- [ ] textDocument/willSaveWaitUntil
- [x] textDocument/didSave
- [x] textDocument/didClose

## Diagnostics
- [x] textDocument/publishDiagnostics

## Language features
- [x] textDocument/completion
- [ ] completionItem/resolve
- [x] textDocument/hover
- [ ] textDocument/signatureHelp
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

Eglot is **really beta** and may currently underperform
[lsp-mode.el][emacs-lsp], which is more mature and has a host of
[plugins][emacs-lsp-plugins] for bells and whistles.  If you think the minimalist approach
of `eglot.el` is not for you, you could be better served with
`lsp-mode.el` for now.

User-visible differences:

- Single entry point `M-x eglot`, not `M-x eglot-<language>`. Also no
  `eglot-<language>` extra packages.
- No "whitelisting" or "blacklisting" directories to languages. `M-x
  eglot` starts servers to handle major modes inside a specific
  project. Uses Emacs's built-in `project.el` library to discover
  projects. Automatically detects current and future opened files under that
  project and syncs with server.
- Easy way to quit/restart a server, just middle/right click on the
  connection name.
- Pretty interactive mode-line section for live tracking of server
  communication.
- Automatically restarts frequently crashing servers (like RLS).
- Server-initiated edits are confirmed with the user.
- Diagnostics work out-of-the-box (no `flycheck.el` needed).
   
Under the hood:

- Message parser is much much simpler.
- Easier to read and maintain elisp. Yeah I know, *extremely
  subjective*, so judge for yourself.
- About 1k LOC lighter.
- Development doesn't require Cask, just Emacs.
- Project support doesn't need `projectile.el`, uses Emacs's `project.el`
- Requires the upcoming Emacs 26
- Contained in one file
- Sends `textDocument/didChange` for groups of edits, not one per each
  tiny change.
- Its missing tests! This is *not good*

[lsp]: https://microsoft.github.io/language-server-protocol/
[rls]: https://github.com/rust-lang-nursery/rls
[pyls]: https://github.com/palantir/python-language-server
[javascript-typescript-langserver]: https://github.com/sourcegraph/javascript-typescript-langserver
[emacs-lsp]: https://github.com/emacs-lsp/lsp-mode
[emacs-lsp-plugins]: https://github.com/emacs-lsp


   
