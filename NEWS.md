# (upcoming)

##### Rework mode-line menus ([#792][github#792])

The new menus help discovering Egolot's features and shows which of
them are supported by the current server.  Additionally, customizing
variable `eglot-mode-line-string` can leave more space on the
mode-line.

##### Easier to use LSP initialize.initializationOptions ([#901][github#901], [#845][github#845])
In `eglot-server-programs` a plist may be appended to the usual list
of strings passed as command line arguments.  The value of its
`:initializationOptions` key constructs the corresponding LSP JSON
object.  This may be easier than creating a `defclass` for a specific
server and specializing `eglot-initialization-options` to that class.

##### Support on-type-formatting ([#899][github#899])

##### Provide basic workspace-folders support ([#893][github#893])
Eglot now advertises `project-root` and `project-external-roots` as
workspace-folders.  (Configuring `tags-table-list` sets the external
roots of a simple git project.)

##### Show project wide diagnosics ([#810][github#810])
Some LSP servers report diagnostics for all files in the current
workspace.  Flymake has as of version 1.2.1 the option to show
diagnostics from buffers other than the currently visited one.  The
command `M-x flymake-show-project-diagnostics` will now show all
diagnostics relevant to a workspace.

##### Support optional completion tags ([#797][github#797])
A [completion-item tag][completiontag] can be used to tell the editor
how to render a completion.  Presently, one kind of tag exists,
denoting its corresponding completion as obsolete.

##### Support optional diagnostic tags ([#794][github#794])
A [diagnostic tag][diagnostictag] can indicate either "unused or
unnecessary code" or "deprecated or obsolete code".  Following the
rendering suggestions in the protocol, we fade out unnecessary code
and strike-through deprecated code.

##### The Rust language server is now rust-analyzer by default ([#803][github#803])
Eglot will now prefer starting "rust-analyzer" to "rls" when it is
available.  The special support code for RLS has been removed.

##### New servers have been added to `eglot-server-programs`
- clojure-lsp ([#813][github#813])
- racket-langserver ([#694][github#694])

# 1.8 (12/1/2022)

##### Multiple servers supported out-of-box for same major mode ([#688][github#688])

In practice, this removes the need for Eglot to "officially" bless one
server over another.  Thanks to Felicián Németh for the original idea.

##### TRAMP support ([#637][github#637], [#463][github#463], [#84][github#84])

Thanks to Brian Cully for the minimalist approach.

(also thanks to Felipe Lema who conducted many early experiments in
[#463][github#463])

##### `eglot-ignored-server-capabilities` now correctly spelled ([#724][github#724])

This user-visible variable used to be spelled
`eglot-ignored-server-capabilites`, which is still a valid but
obsolete name.

##### Manage cross-referenced files outside project ([#76][github#76], [#686][github#686], [#695][github#695])

This is activated by a new customization option
`eglot-extend-to-xref`, which defaults to nil.

Thanks to Michael Livshin for the investigation an elegant solution.

##### Code action shortcuts ([#411][github#411])

`M-x eglot-code-actions` accepts an optional `action-kind` argument,
specified interactively with `C-u`.  Other shortcuts call specific
actions directly (`eglot-code-action-inline`,
`eglot-code-action-extract`, `eglot-code-action-rewrite`,
`eglot-code-action-organize-imports` and
`eglot-code-action-quickfix`).  One can create own shortcuts for code
actions with specific a kind by calling `eglot-code-actions` from
elisp.

##### New command `eglot-shutdown-server` ([#643][github#643])

##### New variable `eglot-withhold-process-id` ([#722][github#722])
If non-nil, Eglot will not send the Emacs process id to the language server.
This can be useful when using docker to run a language server.

##### Several new servers have been added to `eglot-server-programs`
- cmake-language-server ([#787][github#787])
- css-languageserver ([#204][github#204], [#769][github#769])
- fortls ([#603][github#603])
- html-languageserver ([#204][github#204], [#769][github#769])
- json-languageserver ([#204][github#204], [#769][github#769])
- lua-lsp ([#721][github#721])
- mint ls ([#750][github#750])
- pyright ([#742][github#742])
- vim-language-server ([#787][github#787])
- yaml-language-server ([#751][github#751])
- zls ([#646][github#646])

# 1.7 (16/12/2020)

##### Support hierarchical symbols in Imenu ([#303][github#303])

Thanks to Ingo Lohmar for the original implementation.

##### Handle multiple "documentation at point" sources ([#439][github#439], [#494][github#494], [#481][github#481], [#454][github#454])

Such sources include as LSP's signature, hover and also the Flymake
diagnostic messages.  They can all be presented in the echo area
(space permitting), or via `C-h .`.  For now, composition of different
sources can be customized using `eldoc-documentation-strategy`,
`eldoc-echo-area-use-multiline-p` and `eldoc-prefer-doc-buffer`.

The variables `eglot-put-doc-in-help-buffer` and
`eglot-auto-display-help-buffer` have been removed.

# 1.6 (16/04/2020)

##### Column offset calculation is now LSP-conform ([#361][github#361])

It seems the majority of servers now comply with the language server
specification when it comes to handling non-ASCII texts.  Therefore
the default values of `eglot-move-to-column-function` and
`eglot-current-column-function` have been changed.  The documentations
of these variables help to restore the old behavior.

##### Support workspace/configuration requests ([#326][github#326])

Also a new section "Per-project server configuration" in the README.md
should answer some faq's in this regard.

# 1.5 (20/10/2019)

Thanks a lot to Felicián Németh, Ingo Lohmar, and everyone else who
helped out!

##### Take over Company configuration ([#324][github#324])

Similar to what was already the case with Flymake, Eldoc and Xref, use
just the backend that can do something useful in Eglot,
`company-capf`.  See `eglot-stay-out-of` to opt out of this.

##### New option `eglot-autoshutdown` to disconnect after last buffer killed ([#217][github#217], [#270][github#270])

##### Fix completion support ([#235][github#235], [#313][github#313], [#311][github#311], [#279][github#279])

Among other things, consider LSP's "filterText" cookies, which enables
a kind of poor-man's flex-matching for some backends.

##### Support LSP's "goto declaration/implementation/typeDefinition" ([#302][github#302])

##### New option `eglot-send-changes-idle-time` ([#258][github#258])

##### Prevent Eldoc flicker when moving around ([#198][github#198])

##### Show large docs in help buffer instead of echo area by default ([#198][github#198])

Also add two new customization variables
`eglot-put-doc-in-help-buffer` and `eglot-auto-display-help-buffer`.

##### Add built-in support for Go, Elixir and Ada ([#304][github#304], [#264][github#264], [#316][github#316])

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

[diagnostictag]: https://microsoft.github.io/language-server-protocol/specifications/specification-3-17/#diagnosticTag
[completiontag]: https://microsoft.github.io/language-server-protocol/specifications/specification-3-17/#completionItemTag
<!--- Now a bunch of references that I auto-generate with

(cl-loop
   with pivot
   initially
   (goto-char (point-min))
   (search-forward-regexp "and now said bunch of references...\n")
   (setq pivot (point))
   (goto-char (point-min))
   while (and (search-forward-regexp "github#\\([0-9]+\\)" nil t)
              (< (point) pivot))
   collect (string-to-number (match-string 1)) into refs
   finally (setq refs (delete-dups refs))
   (goto-char pivot)
   (delete-region pivot (point-max))
   (cl-loop for ref in (sort refs #'<)
            do (insert (format "[github#%d]: https://github.com/joaotavora/eglot/issues/%d\n" ref ref))))

and now said bunch of references-->
[github#29]: https://github.com/joaotavora/eglot/issues/29
[github#39]: https://github.com/joaotavora/eglot/issues/39
[github#44]: https://github.com/joaotavora/eglot/issues/44
[github#48]: https://github.com/joaotavora/eglot/issues/48
[github#50]: https://github.com/joaotavora/eglot/issues/50
[github#52]: https://github.com/joaotavora/eglot/issues/52
[github#54]: https://github.com/joaotavora/eglot/issues/54
[github#58]: https://github.com/joaotavora/eglot/issues/58
[github#61]: https://github.com/joaotavora/eglot/issues/61
[github#63]: https://github.com/joaotavora/eglot/issues/63
[github#64]: https://github.com/joaotavora/eglot/issues/64
[github#68]: https://github.com/joaotavora/eglot/issues/68
[github#73]: https://github.com/joaotavora/eglot/issues/73
[github#74]: https://github.com/joaotavora/eglot/issues/74
[github#76]: https://github.com/joaotavora/eglot/issues/76
[github#80]: https://github.com/joaotavora/eglot/issues/80
[github#81]: https://github.com/joaotavora/eglot/issues/81
[github#82]: https://github.com/joaotavora/eglot/issues/82
[github#83]: https://github.com/joaotavora/eglot/issues/83
[github#84]: https://github.com/joaotavora/eglot/issues/84
[github#86]: https://github.com/joaotavora/eglot/issues/86
[github#87]: https://github.com/joaotavora/eglot/issues/87
[github#93]: https://github.com/joaotavora/eglot/issues/93
[github#94]: https://github.com/joaotavora/eglot/issues/94
[github#100]: https://github.com/joaotavora/eglot/issues/100
[github#115]: https://github.com/joaotavora/eglot/issues/115
[github#120]: https://github.com/joaotavora/eglot/issues/120
[github#121]: https://github.com/joaotavora/eglot/issues/121
[github#124]: https://github.com/joaotavora/eglot/issues/124
[github#126]: https://github.com/joaotavora/eglot/issues/126
[github#138]: https://github.com/joaotavora/eglot/issues/138
[github#144]: https://github.com/joaotavora/eglot/issues/144
[github#154]: https://github.com/joaotavora/eglot/issues/154
[github#156]: https://github.com/joaotavora/eglot/issues/156
[github#158]: https://github.com/joaotavora/eglot/issues/158
[github#160]: https://github.com/joaotavora/eglot/issues/160
[github#167]: https://github.com/joaotavora/eglot/issues/167
[github#178]: https://github.com/joaotavora/eglot/issues/178
[github#190]: https://github.com/joaotavora/eglot/issues/190
[github#196]: https://github.com/joaotavora/eglot/issues/196
[github#198]: https://github.com/joaotavora/eglot/issues/198
[github#204]: https://github.com/joaotavora/eglot/issues/204
[github#217]: https://github.com/joaotavora/eglot/issues/217
[github#235]: https://github.com/joaotavora/eglot/issues/235
[github#258]: https://github.com/joaotavora/eglot/issues/258
[github#264]: https://github.com/joaotavora/eglot/issues/264
[github#270]: https://github.com/joaotavora/eglot/issues/270
[github#279]: https://github.com/joaotavora/eglot/issues/279
[github#302]: https://github.com/joaotavora/eglot/issues/302
[github#303]: https://github.com/joaotavora/eglot/issues/303
[github#304]: https://github.com/joaotavora/eglot/issues/304
[github#311]: https://github.com/joaotavora/eglot/issues/311
[github#313]: https://github.com/joaotavora/eglot/issues/313
[github#316]: https://github.com/joaotavora/eglot/issues/316
[github#324]: https://github.com/joaotavora/eglot/issues/324
[github#326]: https://github.com/joaotavora/eglot/issues/326
[github#361]: https://github.com/joaotavora/eglot/issues/361
[github#411]: https://github.com/joaotavora/eglot/issues/411
[github#439]: https://github.com/joaotavora/eglot/issues/439
[github#454]: https://github.com/joaotavora/eglot/issues/454
[github#463]: https://github.com/joaotavora/eglot/issues/463
[github#481]: https://github.com/joaotavora/eglot/issues/481
[github#494]: https://github.com/joaotavora/eglot/issues/494
[github#603]: https://github.com/joaotavora/eglot/issues/603
[github#637]: https://github.com/joaotavora/eglot/issues/637
[github#643]: https://github.com/joaotavora/eglot/issues/643
[github#646]: https://github.com/joaotavora/eglot/issues/646
[github#686]: https://github.com/joaotavora/eglot/issues/686
[github#688]: https://github.com/joaotavora/eglot/issues/688
[github#694]: https://github.com/joaotavora/eglot/issues/694
[github#695]: https://github.com/joaotavora/eglot/issues/695
[github#721]: https://github.com/joaotavora/eglot/issues/721
[github#722]: https://github.com/joaotavora/eglot/issues/722
[github#724]: https://github.com/joaotavora/eglot/issues/724
[github#742]: https://github.com/joaotavora/eglot/issues/742
[github#750]: https://github.com/joaotavora/eglot/issues/750
[github#751]: https://github.com/joaotavora/eglot/issues/751
[github#769]: https://github.com/joaotavora/eglot/issues/769
[github#787]: https://github.com/joaotavora/eglot/issues/787
[github#792]: https://github.com/joaotavora/eglot/issues/792
[github#794]: https://github.com/joaotavora/eglot/issues/794
[github#797]: https://github.com/joaotavora/eglot/issues/797
[github#803]: https://github.com/joaotavora/eglot/issues/803
[github#810]: https://github.com/joaotavora/eglot/issues/810
[github#813]: https://github.com/joaotavora/eglot/issues/813
[github#845]: https://github.com/joaotavora/eglot/issues/845
[github#893]: https://github.com/joaotavora/eglot/issues/893
[github#899]: https://github.com/joaotavora/eglot/issues/899
[github#901]: https://github.com/joaotavora/eglot/issues/901
