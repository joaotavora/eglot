# 1.5 (upcoming)

##### Prevent Eldoc flicker when moving around ([#198][github#198])

##### Show large docs in help buffer instead of echo area by default ([#198][github#198])

Also add two new customization variables
`eglot-put-doc-in-help-buffer` and `eglot-auto-display-help-buffer`.

# 1.4 (5/1/2019)

##### Correct param highlighting in the first line of signature

##### Display documentation strings with `gfm-view-mode`

##### Remove hard dependencies on `flymake-mode`

You can turn it off now in buffers managed by Eglot.

##### Run connection hooks with proper directory local variables ([#196][github#196])

This fixes issues with suspiciously empty `didChangeConfiguration`
messages that are supposed to communicate parameters from a
directory-set `eglot-workspace-configuration`.

##### Fix completion sorting ([#190][github#190])

##### Take over Flymake and Eldoc completely while managing buffers

No longer try to add Eglot's facilities to existing facilities in
these two domains.

# 1.3 (10/12/2018)

##### Control strictness towards incoming LSP messages ([#144][github#144], [#156][github#156])

##### Add brief context after `xref-find-references` when available ([#52][github#52])

##### Support `completionContext` to help servers like `ccls`.

##### Use Flymake from GNU ELPA ([#178][github#178])

# 1.2 (23/11/2018)

##### Support snippet completions ([#50][github#50])

Use `yasnippet.el` for this, if it is installed.

##### Implement `workspace/didChangeConfiguration` ([#29][github#29])

##### Handle experimental/unknown server methods gracefully ([#39][github#39])

##### Accept functions as entries in `eglot-server-programs` ([#63][github#63])

`CONTACT` in the `(MAJOR-MODE . CONTACT)` association in
`eglot-server-programs` can now be a function of no arguments
producing any value previously valid for contact.  Functions can be
interactive on non-interactive.

##### Snappier completions that don't hinder typing ([#61][github#61])

##### Consider `:triggerCharacters` in company completion ([#80][github#80])

##### Add support for `TextEdit`s in completion

##### Prefer ccls over cquery for C/C++ ([#94][github#94])

##### `eglot-ignored-server-capabilites` is more user-friendly ([#126][github#126])

##### Implement asynchronous server connection ([#68][github#68])

A new defcustom `eglot-sync-connect` controls this feature.

##### Add a generic `eglot-execute-command` API

Work by Michał K.

##### Prompt for server in `eglot-shutdown` ([#73][github#73])

##### Add support for code action literals

##### Add support for the Eclipse JDT language server ([#63][github#63])

##### Add out-of-the-box support for Haskell, Kotlin, Go, Ocaml, R

##### Add the ability to move to LSP-precise columns ([#124][github#124])

Some servers like `clangd` follow the spec very closely here.

##### Fix a potential security issue fontifying LSP doc ([#154][github#154])

##### Fix many, many bugs

[#44][github#44], [#48][github#48], [#54][github#54], [#58][github#58], [#64][github#64], [#74][github#74], [#81][github#81], [#82][github#82], [#86][github#86], [#87][github#87], [#83][github#83], [#93][github#93], [#100][github#100], [#115][github#115], [#120][github#120], [#121][github#121], [#126][github#126], [#138][github#138], [#144][github#144], [#158][github#158], [#160][github#160], [#167][github#167]

# 1.1 (9/7/2018)

##### Implement TCP autostart/autoconnect (and support Ruby's Solargraph)

The `:autoport` symbol in the server incovation is replaced
dynamically by a local port believed to be vacant, so that the ensuing
TCP connection finds a listening server.

##### Eglot now depends on Emacs library `jsonrpc.el`.

##### Assorted bugfixes

<!-- Now a million references -->

[github#198]: https://github.com/joaotavora/eglot/issues/198
[github#198]: https://github.com/joaotavora/eglot/issues/198
[github#196]: https://github.com/joaotavora/eglot/issues/196
[github#190]: https://github.com/joaotavora/eglot/issues/190
[github#144]: https://github.com/joaotavora/eglot/issues/144
[github#156]: https://github.com/joaotavora/eglot/issues/156
[github#52]: https://github.com/joaotavora/eglot/issues/52
[github#178]: https://github.com/joaotavora/eglot/issues/178
[github#50]: https://github.com/joaotavora/eglot/issues/50
[github#29]: https://github.com/joaotavora/eglot/issues/29
[github#39]: https://github.com/joaotavora/eglot/issues/39
[github#63]: https://github.com/joaotavora/eglot/issues/63
[github#61]: https://github.com/joaotavora/eglot/issues/61
[github#80]: https://github.com/joaotavora/eglot/issues/80
[github#94]: https://github.com/joaotavora/eglot/issues/94
[github#126]: https://github.com/joaotavora/eglot/issues/126
[github#68]: https://github.com/joaotavora/eglot/issues/68
[github#73]: https://github.com/joaotavora/eglot/issues/73
[github#63]: https://github.com/joaotavora/eglot/issues/63
[github#124]: https://github.com/joaotavora/eglot/issues/124
[github#154]: https://github.com/joaotavora/eglot/issues/154
[github#44]: https://github.com/joaotavora/eglot/issues/44
[github#48]: https://github.com/joaotavora/eglot/issues/48
[github#54]: https://github.com/joaotavora/eglot/issues/54
[github#58]: https://github.com/joaotavora/eglot/issues/58
[github#64]: https://github.com/joaotavora/eglot/issues/64
[github#74]: https://github.com/joaotavora/eglot/issues/74
[github#81]: https://github.com/joaotavora/eglot/issues/81
[github#82]: https://github.com/joaotavora/eglot/issues/82
[github#86]: https://github.com/joaotavora/eglot/issues/86
[github#87]: https://github.com/joaotavora/eglot/issues/87
[github#83]: https://github.com/joaotavora/eglot/issues/83
[github#93]: https://github.com/joaotavora/eglot/issues/93
[github#100]: https://github.com/joaotavora/eglot/issues/100
[github#115]: https://github.com/joaotavora/eglot/issues/115
[github#120]: https://github.com/joaotavora/eglot/issues/120
[github#121]: https://github.com/joaotavora/eglot/issues/121
[github#126]: https://github.com/joaotavora/eglot/issues/126
[github#138]: https://github.com/joaotavora/eglot/issues/138
[github#144]: https://github.com/joaotavora/eglot/issues/144
[github#158]: https://github.com/joaotavora/eglot/issues/158
[github#160]: https://github.com/joaotavora/eglot/issues/160
[github#167]: https://github.com/joaotavora/eglot/issues/167

