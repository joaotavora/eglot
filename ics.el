;;; ics.el  a major mode for communicating with Internet Chess Servers
;;
;; Author: Mark Oakden <mark.oakden@camembert.freeserve.co.uk>
;;
;; (ICC,FICS,BICS,EICS:Sheridan)
;;
;;;;;;;;;;;;;;
;;
;;    Copyright (C) 1995-2000  Mark Oakden
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;----------------------------------------------------------------------
;;
;; how to use it!
;;
;; (see http://www.camembert.freeserve.co.uk/mark/icsel/ for more
;; detail)
;;
;; you'll need to load or e.g. autoload it in .emacs:---
;;
;; (autoload 'ics "ics-0.3.7alpha" "ics package" t)
;;
;; an example ics-startup-hook...
;;
;;(add-hook 'ics-startup-hook
;;	  (function (lambda ()
;;		      ;; handle and a funky regexp for highlighting
;;		      (setq ics-default-handle "Sheridan")
;;		      (setq ics-handle-regexp
;;			    '("[Ss]herr?[iy]?[\\- _]*\\([Dd]an\\)?" 0))
;;		      ;; I want it to send my handle
;;		      (setq ics-send-handle t)
;;                    ;; I like the buttons even if they are buggy
;;                    (setq ics-add-buttons t)
;;		      ;; I like abbrevs
;;		      (setq ics-use-abbrev-mode t)
;;		      ;; my emacs windows are light coloured backgrounds
;;		      (setq ics-background-mode 'light)
;;		      ;; reset the aliases list to use timestamp/timeseal
;;		      ;;   where possible
;;		      (setq ics-servers-alist
;;			    '(("i" "ICC" "192.231.221.16" "5000"
;;			       "timestamp") ; chess.lm.com
;;			      ("f" "A-FICS" "164.58.253.10" "5000"
;;			       "timeseal") ; ics.onenet.net
;;			      ("e" "E-FICS" "130.225.18.157" "5000"
;;			       "telnet") ; krypton.daimi.aau.dk
;;			      ("d" "D-FICS" "193.78.33.69" "5000"
;;			       "telnet") ; dds.hacktic.nl
;;			      ("b" "B-FICS" "137.205.192.12" "5000"
;;			       "telnet") ; holly.csv.warwick.ac.uk
;;			      ("g" "G-FICS" "131.246.89.3" "5000"
;;			       "telnet") ; chess.unix-ag.uni-kl.de
;;			      ("m" "M-FICS" "132.76.80.77" "5000"
;;			       "telnet") ; wisdom.weizmann.ac.il
;;			      )))))
;;
;; I have used IP addrs in my hook since timeseal does not seem
;; to look up hosts by name (at least under linux)
;;
;; Buttons are "buggy" in the sense that
;; 1) my regexps aren't currently exclusive enough... any number
;; followed by a word gets recognised as a "player".   this I know how
;; to handle (don't buttonify the output unless it is output from an
;; input command matching a regexp e.g. ics-who-regexp. then wait till
;; all the output is there (i.e. another prompt has been seen) before
;; highlighting and buttonising the region.)
;;;
;;
;; * ChangeLog *
;;
;; ics.el devel
;;
;; changes since 0.3.7:
;; * Multiple small tidyups+regexp tweaks including:-
;;   * tweaked regexp to make seekads/sought output work on ICC
;;   * tweaked regexp in example auto-command-alist to know about bracketed
;;     junk after handles e.g. (U) (*) etc.
;;   * added knowledge of "'" at end of handle into emote regexp
;;   * re-ordered some button regexps to help larger buttons take precedence
;;   * tidied up ics-button-alist regexps using concat
;;   * replaced literal ^M with \r,  literal ^G with \a
;;   * fixed gamelist regexp to know that there can be more than one space in
;;     front of the gamenumber
;;   * Fixed history list regexp to know about more "ending" flags
;;   * Added some button regexps for dealing with stored games
;;   * ics.el now sets the interface variable at ICS servers to show ics.el
;;     version and the sub-interface in use.  Currently only works at FICS
;;     since ICC seems to lock the interface variable once it is set.
;;
;; changes since 0.3.6:
;; * better shout default colour for dark backgrounds (turquise).
;; * better handle default colour for dark backgrounds (orange).
;; * better kibitz default colour for dark backgrounds (LightSeaGreen)
;;   (can anybody tell I now use dark backgrounds in my emacs???)
;; * fixed type in ics-shout-face (turquiose1 -> turquoise1)
;; * fixed handle matching regexp in ics-button-alist to know that
;;   "-" is now allowed in names.
;; * fixed sundry button regexps to know that titles in brackets exist
;; * rehashed the highlighting/buttonising code to remove
;;   the long-standing bug which caused virtually everything
;;   to turn 'bold in the buffer if it buttonised some unfinished output
;;   from the server.
;; * added knowledge of seek ads and sought output to the buttonisation
;;   code.  Clicking button issues appropriate "play" command
;; * added selected function tracing if ics-version matches *devel*
;;
;; ics.el v0.3.6 alpha
;;
;; changes since 0.3.5:
;; * added a line into ics-watch-for-login-and-send-handle to remove it
;;   from comint-output-filter-functions once it has run,  to avoid
;;   endless loop when an invalid handle is entered.
;; * removed all the ics-wakeup stuff.  It got broken between my
;;   prerelease version of 0.3.5 and the released version.  This has
;;   been replaced by a more general ics-auto-command feature, along
;;   with a boolean variable ics-idle-p to determine if user is idle.
;;   A sample ics-auto-command-alist shows how to use these to simulate
;;   the broken 0.3.5 ics-wakeup feature,  along with another
;;   auto-command (auto greet people on notify list)
;;
;; changes since 0.3.4:
;; * added timeout variable to ics-wakeup function.  Wakeup will now
;;   only produce alarm beeps if the elapsed time since the last beeps
;;   is greater than the user settable timeout period
;;   ics-wakeup-alarm-timeout
;; * added some messages to let user know what ics.el is doing
;;   e.g. Waiting for login prompt ... etc...
;; * ics-send-password and ics-password added.
;; * added wakeup function.  If ics-wakeup is t then any ics output
;;   matching ics-wakeup-regexp will cause emacs to (ding t)
;;   ics-wakeup-number-of-beeps times,  separated by
;;   ics-wakeup-beep-interval.
;; * Changed (ding) in ics-output-filter-function to (ding t) to
;;   prevent it from terminating keyboard macros.
;; * wrapped long regexp strings over multiple lines.  Must be careful not
;;   to reformat these sections of code!
;;
;; changes since 0.3.3:
;; * CHANGED variable ics-handle to ics-default-handle.  ics-handle is
;;   now used internally.
;; * added variable ics-wait-for-login-prompt. If this is t (default) and
;;   ics-send-handle is t, ics.el will wait for the login prompt to appear
;;   before sending handle.
;; * added button to observe gnotify notification games.
;; * added button on "Type next/more" messages.
;; * modified buttonisation code to re-search each button regexp
;;   separately to get around the problem with overflowing the regexp
;;   matcher's stack. (minor changes to
;;   ics-add-buttons-to-region). This modification may make the
;;   buttonising too slow.  If so I'll have to group the regexps in
;;   batches of, say, 5 regexps and do it the old way for each
;;   grouping. Thankfully, though, at present the soplution appears
;;   quick enough on both my linux box (i486) and DEC alphas.
;; * uncommented some button entries which were commented because of
;;   above problem.
;; * corrected a couple of doc strings.
;;
;; changes since 0.3.2:
;; * changed ics-mouse-push-button to ics-mouse-push-button-or-yank
;;   which does a yank if there is no button at the clicked spot.
;; * more buttons (finger buttons in shouts etc.)
;; * fixed bug which would always use the default handle even if you
;;   entered a different one.
;; * changed defaults for dark backgrounds.
;; * tweaked regexps
;; * moved the variables that most need user customisation nearer the
;;   top of the file so users might see them if browsing the head of the
;;   file.
;;
;; changes since 0.3.1:
;; * minor tweakings of regexps to work better across different ics servers
;; * new startup screen.
;; * examine button for history list entries
;; * new hooks ics-pre-connect-hook ics-post-connect-hook and ics-mode-hook
;;
;;
;;
;;
(require 'comint)

(defconst ics-version "0.4.0"
  "ics.el version devel")

(defvar ics-default-handle "RubberChicken"
  "*Default handle to use for ICS login.  This is a fine thing to set
up in your ics-startup-hook.")

(defvar ics-send-handle nil
  "*If t send ics handle at the start of login session.")

(defvar ics-wait-for-login-prompt t
  "*Should ics.el wait for the login prompt before sending the user's
handle if ics-send-handle is t?

non-nil means wait for login prompt.")

(defvar ics-send-password nil
  "*If t send ics-password just after sending handle.  Will only send
password if ics-send-handle is t")

(defvar ics-password "barf"
  "*Password to send to ICS for login.  If you require different passwords
for different servers,  you will have to modify this in ics-pre-connect-hook
conditionally on server-name or similar.")

(defvar ics-default-port "5000"
  "*Default port to use to connect to ics.")

(defvar ics-default-connect-method "telnet"
  "*Default connect method.  e.g. telnet,  or timeseal.")

(defvar ics-prompt-regexp "^[A-Za-z ]*[#$%>] *"
  "Regexp to match ics prompt.")

(defvar ics-login-prompt-regexp "^[Ll]ogin: "
  "regexp to match login prompt at ICC/ICS")

(defvar ics-password-prompt-regexp "^[Pp]assword: "
  "regexp to match ics password prompt.")

(defvar ics-inhibit-startup-screen nil)

(defvar ics-set-interface-variable t
  "If true,  set the interface variable to ics.el vX.X.X")

(defvar ics-interface-variable-set nil
  "If true, the interface variable at ics is set to the string
\"ics.el vX.X.X (using board-interface)\"
where board-interface is the interface spawned by ics.el e.g. xboard")

;;; Abbrev. table.
(defvar ics-mode-abbrev-table nil
  "ICS-mode abbreviation table.")

(define-abbrev-table 'ics-mode-abbrev-table ())

(defvar ics-use-abbrev-mode nil
  "If non-nil,  ics-mode will automatically enter abbrev-mode upon startup,
and read the user's abbreviations file,  if it exists.")

(defvar ics-abbrev-file (or (concat (getenv "HOME") "/.abbrev_defs")
				 (expand-file-name "~/.abbrev_defs"))
  "Name to use for saving and loading ics-mode abbreviations table.
This can be set in your .emacs file,  in ics-startup-hooks add e.g.
  \(setq ics-abbrev-file \"/foo/bar/.ics_abbreviations\"\)
To switch on abbrev-mode this session in ics buffer use \"M-x abbrev-mode\".
If you wish to have abbrev mode set and your abbreviations file read
automatically when you enter ics-mode, add the line
  \(setq ics-use-abbrev-mode t\)
to your ics-startup-hook in .emacs file.

See \"C-h f abbrev-mode\" for more about abbreviation mode.

*Note: since write-abbrev-file saves ALL mode abbrev tables along with the
global abbrev table,  it is probably better not to use a separate file for
ics-mode abbrev. saving.  Perhaps best to use a common abbrev file e.g.
.abbrev_defs for ALL your favourite abbrevs, and restrict ics abbrevs to the
ics-mode-table,  that way, other buffers in different modes in the same
emacs session as your ics buffer will still get your favourite abbrevs for
those modes set up.")

(defvar ics-mode-map '())

(defvar ics-startup-hook '()
  "*Hook for customising ICS mode.
You should set sensible values for ics-default-handle and ics-handle-regexp
in this hook in your .emacs file.
e.g. mine might read:-
\(add-hook 'ics-startup-hook
      \(function \(lambda \(\)
		  \(setq ics-default-handle \"Sheridan\"\)
		  \(setq ics-handle-regexp \"[Ss]heridan\"\)\)\)\)")

(defvar ics-mode-hook '()
  "*Hook run when ics-mode is switched on.")

(defvar ics-pre-connect-hook '()
  "*Hook run just before ics.el attempts to connect to server.")

(defvar ics-post-connect-hook '()
  "*Hook run just after ics.el connects to server.")
;;
;; alist of server names,  and other information.
;; numeric addresses have been used since many versions of
;; timestamp and timeseal require them.
;;
(defvar ics-servers-alist '(("i" "ICC" "192.231.221.16" "5000"
			     "telnet") ; chess.lm.com
			    ("f" "A-FICS" "164.58.253.10" "5000"
			     "telnet") ; ics.onenet.net
			    ("e" "E-FICS" "130.225.18.157" "5000"
			     "telnet") ; krypton.daimi.aau.dk
			    ("d" "D-FICS" "193.78.33.69" "5000"
			     "telnet") ; dds.hacktic.nl
			    ("b" "B-FICS" "137.205.192.12" "5000"
			     "telnet") ; holly.csv.warwick.ac.uk
			    ("g" "G-FICS" "131.246.89.3" "5000"
			     "telnet") ; chess.unix-ag.uni-kl.de
			    ("m" "M-FICS" "132.76.80.77" "5000"
			     "telnet") ; wisdom.weizmann.ac.il
			    )
  "* Alist of server information. Each entry in the list has the form

\(ABBREV SHORTNAME ADDRESS PORT CONNECTMETHOD\)

With PORT and CONNECTMETHOD optional.

For a list of current addresses see \"help addresses\" on A-FICS.

Where ABBREV is an abbreviation which you can enter at the ics server
prompt.  SHORTNAME is the name by which the server will be referred in
the buffername in the Emacs modeline.  ADDRESS is the address \(or the
IP number\) of the server.  \(If you are going to use timestamp \(help
\"timestamp\" on ICC\) or timeseal \(help \"timeseal\" on FICS\) then
for most versions,  it is necessary to use the IP number.\)  This will
be assigned to the (buffer-local) variable ics-address before calling
the interface program specified in ics-interface with arguments
specified in ics-interface-args.

PORT and CONNECTMETHOD are both optional.  If they are set,  they will
be assigned to the variables ics-port and ics-connect-method,
respectively.  These variables may then be used in the \(user
specified\) ics-interface-args variable to specify hairy connect
methods.  See the documentation on ics-interface-args for an example
of how to use this to use these variables to specify the use of
timestamp/timeseal with xboard when connecting to ICC/FICS.

Typical entries might look like

\(\"icc\" \"ICC\" \"chess.lm.com\" \"5000\" \"timestamp\"\)

or

\(\"bics\" \"BICS\" \"holly.csv.warwick.ac.uk\" \"5000\" \"telnet\"\)

Where the first specifies \"icc\" as an abbreviation for the ICC
server at chess.lm.com.  The second defines bics as an abbrev for BICS
at holly.csv.warwick.ac.uk.")

(defvar ics-interface "xboard"
  "* Interface to connect to ics servers with.  Typically this will
have the value \"xboard\" or \"xics\",  although if you just want to
use ics-mode's highlighting abilities with an ASCII connection,  you
may also use telnet for your \"interface\".  You should set up
ics-interface-args to an appropriate value for your interface.")

(defvar ics-interface-args '(list "-ics" "-icshost" ics-address "-icsport"
			     ics-port "-telnet" "-telnetProgram"
			     ics-connect-method "-size" "medium")
  "* List of arguments to be supplied to the interface program to
connect to an ics server.

The variables ics-address, ics-port, and ics-connect-method may be
used in this list.  These variables will be initialised from
ics-servers-alist.  If there is no match in ics-servers-alist for the
address input by the user,  it will be assumed to be an IP address or
number, and assigned to ics-address.  In this case, the user will be
prompted for the port number.

Typical values for ics-interface-args :--

e.g. If you are an xboard user, wishing to use timestamp/timeseal with
ICC/FICS, you should have

\(setq ics-interface \"xboard\"\)
\(setq ics-interface-args '\(list \"-ics\" \"-icshost\" ics-address
			     \"-icsport\" ics-port \"-telnet\"
			     \"-telnetProgram\" ics-connect-method
			     \"-size\" \"medium\"\)

in your ics-startup-hook, and make sure that the CONNECT-METHOD
entries in ics-servers-alist correspond to the names of your
timestamp/timeseal programs for each server (or telnet where
timestamp/timeseal are unavailable).  While an xics (or plain telnet)
user might use

\(setq ics-interface \"xics\"\)
\(setq ics-interface-args '\(list ics-address ics-port\)\) ")
;;
;; auto command stuff
;;
(defvar ics-auto-command-alist
  '(("^\\([a-zA-Z0-9-]+\\)\\(([^)]*)\\)* tells you: " 1 ics-idle-p
     ics-wakeup)
    ("^Notification: \\([^ ]+\\) has arrived" 1 t
     "i > (ics-auto-greet '%s)"))
  "alist of auto-command info

'\(\(REGEXP LEVEL CONDITION COMMAND\)
  \(REGEXP LEVEL CONDITION COMMAND\)
...\)

the REGEXPs are applied in turn to ics output until one matches. If a
match is found,  and CONDITION evaluates to t, COMMAND is exectued.
COMMAND may be either a lisp function (or variable whose value is a
function name) which should take one string
argument,  which is executed with match-level LEVEL of the regexp
match as its argument,  or a string,  which is passed to format with
the match level LEVEL'th substring of the regexp match,  and sent as
an ics command.")

(defvar ics-idle-time 300
  "number of seconds one must be idle before ics-idle-p gets set to
  t")

(defvar ics-do-auto-commands nil
  "*If this is t,  automatic commands in ics-auto-command-alist will
  be executed.")
;;
;; User variables for ics-wakeup auto-command feature
;;
(defvar ics-wakeup-alarm-timeout 120
  "*The time,  in seconds,  that must have elapsed since last alarm bell
before another bell will be sounded.")

(defvar ics-wakeup-command (concat "tell %s sorry,  I am idle "
				   "(or playing) at present.  "
				   "I have been notified of your "
				   "tell with an alarm signal.  "
				   "[This tell was generated "
				   "automatically]")
  "command to be sent to ICS when ics-wakeup is called , after
passing through
  \(format ics-wakeup-command trigger-name\)")

(defvar ics-wakeup-number-of-beeps 3
  "*Default number of times to beep to wake user up")

(defvar ics-wakeup-beep-interval 1.0
  "*Interval in seconds between beeps in wakeup alarm")
;;
;; internal variables for auto-command-related stuff...
;;
(defvar ics-idle-p nil
  "t if last ics command issued was more than ics-idle-time seconds
  ago, nil otherwise")
(defvar ics-last-command-time 0
  "time in seconds of last command issued")
(defvar ics-wakeup-last-alarm-time 0
  "Time when last alarm was sounded")
;;
;; end of auto-command variables...
;;;;;;;;;;;;;;;;;
;; highlighting etc.
;;
(defvar ics-highlight t
  "* If t then highlight tells, says etc.")

(defvar ics-background-mode 'light
  "* Type of background that you use with Emacs... either 'light or 'dark")

(defvar ics-highlight-items
  '("continuation-line" "game-request" "game-notification" "pinform"
    "notification" "kibitz" "s-shout" "channel" "shout" "emote" "tell"
    "stored-here" "stored-not-here" "adjudicate-notification"
    "sought-lightning" "sought-suicide" "sought-blitz" "sought-standard"
    "handle")
  "*List of types of item to highlight.  The ics-highlight-alist
variable is initialised according to the value of this variable,
which has the form of a list of strings.

e.g.

'\(\"continuation-line\" \"game-request\" \"game-notification\" \"pinform\"
   \"notification\" \"kibitz\" \"s-shout\" \"channel\" \"shout\"
   \"emote\" \"tell\" \"handle\"\)

and ics-highlight-alist is set up from the variables ics-X-regexp and
ics-X-face where X ranges through each item on the list.  Highlighting
is done in the order in which the items appear in ics-highlight-items.

NB No checking is done to ensure that all the variables ics-X-regexp
and ics-X-face exist and have the proper format.  If any of them do
not exist,  or are incorrectly formed,  ics.el will bomb.")

(defvar ics-last-highlight-end nil
  "internal,  buffer-local,  used to track where we last highlighted")

(defvar ics-last-add-buttons-end nil
  "internal,  buffer-local,  used to track where we added buttons")


;;;;;;;;
;;regexps and face info...
;;
;; ics-X-face is a list of 3 items each of which is either a valid
;; face name or a list of the form '(fg bg stipple bold italic
;; underline) ics-X-regexp is a list containing the regexp to match to
;; and an integer specifying the match level to highlight: 0 to
;; highlight the whole regexp, 1 to highlight the first parenthesis,
;; etc.

(defvar ics-handle-regexp '("RubberChicken" 0))
(defvar ics-handle-face '(("blue" nil nil t t nil)    ; light
			  ("orange" nil nil t t nil)    ; dark
			  bold))                     ; mono

(defvar ics-tell-regexp '("^[^ ]+ \\(tells you\\|says\\):.*$" 0)
  "Regexp to match ICS tells and says.")
(defvar ics-tell-face '(("red" nil nil nil nil nil)
			("LavenderBlush4" nil nil nil nil nil)
			bold))

(defvar ics-emote-regexp '("^--> [^ ]+[ '].*$" 0)
  "Regexp to match ICS emotes. \(\"i\" commands\).")
(defvar ics-emote-face '(("blue" nil nil nil t nil)
			 ("turquoise1" nil nil nil t nil)
			 italic))

(defvar ics-stored-here-regexp
  '(" *\\([0-9]+: [BW] \\([a-zA-Z]+[-a-zA-Z0-9]*\\) +Y \\[.+\\]\\) .*$" 0)
  "Regexp to match stored list entries for players who are here")
(defvar ics-stored-here-face '(("forest green" nil nil nil nil nil)
			      ("green" nil nil nil nil nil)
			      italic))

(defvar ics-stored-not-here-regexp
  '(" *\\([0-9]+: [BW] \\([a-zA-Z]+[-a-zA-Z0-9]*\\) +N \\[.+\\]\\) .*$" 0)
  "Regexp to match stored list entries for players who are here")
(defvar ics-stored-not-here-face '(("firebrick" nil nil nil nil nil)
				 ("turquoise1" nil nil nil nil nil)
				 italic))

(defvar ics-sought-suicide-regexp
  (list
   (concat
    "^ *\\(\\([0-9]+\\) +\\([0-9]+\\|----\\|++++\\) +"
    "\\([a-zA-Z]+[-a-zA-Z0-9]*\\(([^)]+)\\)*\\)\\) +"
    "\\([a-z0-9-()]*\\)? +[0-9]+ +[0-9]+ +\\(un\\)?rated"
    " +suicide.*$")
   0)
  "Regexp to match suicide games in sought output")
(defvar ics-sought-suicide-face '(("firebrick" nil nil nil nil nil)
				("LightSeaGreen" nil nil nil nil nil)
				underline))

(defvar ics-sought-lightning-regexp
  (list
   (concat
    "^ *\\(\\([0-9]+\\) +\\([0-9]+\\|----\\|++++\\) +"
    "\\([a-zA-Z]+[-a-zA-Z0-9]*\\(([^)]+)\\)*\\)\\) +"
    "\\([a-z0-9-()]*\\)? +[0-9]+ +[0-9]+ +\\(un\\)?rated"
    " +lightning.*$")
   0)
  "Regexp to match lightning games in sought output")
(defvar ics-sought-lightning-face '(("blue" nil nil nil nil nil)
				    ("orange" nil nil nil nil nil)
				    italic))

(defvar ics-sought-blitz-regexp
  (list
   (concat
    "^ *\\(\\([0-9]+\\) +\\([0-9]+\\|----\\|++++\\) +"
    "\\([a-zA-Z]+[-a-zA-Z0-9]*\\(([^)]+)\\)*\\)\\) +"
    "\\([a-z0-9-()]*\\)? +[0-9]+ +[0-9]+ +\\(un\\)?rated"
    " +blitz.*$")
   0)
  "Regexp to match blitz games in sought output")
(defvar ics-sought-blitz-face '(("forest green" nil nil nil nil nil)
				("yellow" nil nil nil nil nil)
				bold))

(defvar ics-sought-standard-regexp
  (list
   (concat
    "^ *\\(\\([0-9]+\\) +\\([0-9]+\\|----\\|++++\\) +"
    "\\([a-zA-Z]+[-a-zA-Z0-9]*\\(([^)]+)\\)*\\)\\) +"
    "\\([a-z0-9-()]*\\)? +[0-9]+ +[0-9]+ +\\(un\\)?rated"
    " +standard.*$")
   0)
  "Regexp to match blitz games in sought output")
(defvar ics-sought-standard-face '(("chocolate" nil nil nil nil nil)
				("green" nil nil nil nil nil)
				bold-italic))

(defvar ics-s-shout-regexp '("^[^ ]+ \\([cs]-?shouts\\|queries\\):.*$" 0)
  "Regexp to match ICS s-shouts.")
(defvar ics-s-shout-face '(("red" nil nil t t nil)
			   ("turquoise1" nil nil t t nil)
			   bold-italic))

(defvar ics-game-request-regexp
  '("^Challenge: .*$\\|^Ignoring\\(\\[\\|[ ](\\).*$\\|Issuing: .*$" 0)
  "Regexp to match game requests \(it is perhaps useful to have this
also match game requests that are ignored due to e.g. not being open,
or formula clashes etc.\)")
(defvar ics-game-request-face '(("black" "tan" nil t nil nil)
				("white" "navy" nil t nil nil)
				modeline))

(defvar ics-game-notification-regexp
  '("^Game notification:.*$\\|^{Game .*}.*$" 0)
  "Regexp to match ICS game notification messages.")
(defvar ics-game-notification-face '(("forest green" nil nil nil nil nil)
				     ("forest green" nil nil nil nil nil)
				     italic))

(defvar ics-kibitz-regexp '("^[^ ]+ \\(kibitzes\\|whispers\\):.*$" 0)
  "Regexp to match ICS kibitzes/whispers.")
(defvar ics-kibitz-face '(("firebrick" nil nil nil t nil)
			  ("LightSeaGreen" nil nil nil t nil)
			  italic))

(defvar ics-channel-regexp '("^[^ ]+([0-9]+):.*$" 0)
  "Regexp to match ics channel tells.")
(defvar ics-channel-face '(("firebrick" nil nil nil t nil)
			   ("LightSeaGreen" nil nil nil t nil)
			   italic))

(defvar ics-shout-regexp '("^[^ ]+ shouts:.*$" 0))
(defvar ics-shout-face '(("blue" nil nil nil t nil)
			 ("turquoise1" nil nil nil t nil)
			 italic))

(defvar ics-pinform-regexp
  '("^\\[[a-zA-Z0-9]+ +\\((.+)\\)? *has \\(dis\\)?connected.\\]$" 0))
(defvar ics-pinform-face '(("royal blue" nil nil nil nil nil)
			   ("orange" nil nil nil nil nil)
			   default))

(defvar ics-notification-regexp
  '("^Notification: [^ ]+ has \\(arrived\\|departed\\).$\\|\
^Present company includes.*$\\|^Your arrival was noted by: .*$" 0))
(defvar ics-notification-face '(("ForestGreen" nil nil t nil nil)
				("ForestGreen" nil nil t nil nil)
				bold))

(defvar ics-adjudicate-notification-regexp
  '("^You have [0-9]+ adjourned games." 0 )
  "Regexp to match initial storegames notifaction")
(defvar ics-adjudicate-notification-face '(("ForestGreen" nil nil nil nil nil)
					   ("ForestGreen" nil nil nil nil nil)
					   bold))

(defvar ics-continuation-line-regexp '("^\\\\ .*$" 0))
(defvar ics-continuation-line-face '(("grey50" nil nil t nil nil)
				     ("grey50" nil nil t nil nil)
				     default))
;;;;

(defvar ics-highlight-alist nil
  "Alist of regexps and highlighting info.  Don't set this variable up
yourself unless you know what you are doing.  It is initialised
automatically from ics-highlight-items, ics-X-regexp and ics-X-face
variables.  If you set this variable yourself,  this initialisation
will not take place.

To modify the highlighting,  change ics-highlight-items and the
ics-X-regexp and ics-X-face variables.")

;;;;
;; button stuff (many concepts and much code is lifted from gnus 5.0
;; by Lars Magne Ingebrigtson)
;;
;; button functions have been slightly rewritten,  since we have many
;; buttons.

(defvar ics-add-buttons nil
  "* If t then add buttons to ics buffer.")

(defvar ics-button-face 'bold)
(defvar ics-mouse-face 'highlight)

(defvar ics-players-command "players"
  "*Command to issue to get a player list")

(defvar ics-who-command "who"
  "*Command to issue to get a who listing")

(defvar ics-observe-command "observe %s"
  "*Command to observe a game")

(defvar ics-play-command "play %s"
  "*Command to issue to accept a seek request")

(defvar ics-finger-command "finger %s"
  "*Command to issue when a button in the who-list is pressed.
e.g. \"finger %s\" or \"match %s\"")

(defvar ics-match-command "match %s"
  "*Command to issue when a button in a player's finger stats is pressed.
e.g. \"match %s\"")

(defvar ics-button-url
  (cond ((boundp 'browse-url-browser-function) browse-url-browser-function)
	((fboundp 'w3-fetch) 'w3-fetch)
	((eq window-system 'x) 'ics-netscape-open-url))
  "function to fetch URL.")

(defvar ics-button-gamelist 'ics-observe
  "function to call on gamelist buttons")

(defvar ics-button-pinform 'ics-finger
  "function to call on pinform buttons")

(defvar ics-button-ginform 'ics-observe
  "function to call on ginform buttons")

(defvar ics-button-historylist 'ics-examine-from-historylist
  "funstion to call on historylist buttons")

(defvar ics-button-seeklist 'ics-play
  "function to call on seeklist buttons")

(defvar ics-button-storedgame 'ics-match
  "function to be called on stored game buttons")

(defvar ics-button-adjourned 'ics-stored
  "function to be called on notification of stored games")

(defvar ics-button-wholist 'ics-finger
  "Function to call on who list buttons.  e.g. 'ics-finger or 'ics-match.")

(defvar ics-button-fingerstat 'ics-match
  "Function to call on name button in finger stats.  e.g. 'ics-match.")

(defvar ics-button-match-offer 'ics-send-command
  "*Function to call on accept/decline buttons in match offers.")

(defvar ics-button-alist
  (list
   ;; who button... highlight just the player name...
   (list
    (concat
     "\\(^\\|[ \t]+\\)\\([0-9]+\\|----\\|++++\\)[ #:.^]"
     "\\([a-zA-Z]+[-a-zA-Z0-9]*\\)")
    3 t 'ics-button-wholist 3)
   ;; pinform button
   (list
    "^\\[\\([a-zA-Z0-9]+\\) +\\((.+)\\)? *has \\(dis\\)?connected.\\]$" 1 t
    ics-button-pinform 1)
   ;; ginform button
   (list
    (concat
     "^{Game \\([0-9]+\\) (\\([a-zA-Z0-9]+ vs. [a-zA-Z0-9]+\\))"
     " \\(Creating\\|Continuing\\).*$")
    2 t ics-button-ginform 1)
   ;; ICC gnotify button (FICS gnotify has same format as ginform so
   ;; is already handled by above regexp
   (list
    (concat
     "^Game notification: \\(\\([^ \t\n]+\\) (.+) vs\\. "
     "\\([^ \t\n]+\\) (.+)\\).*$")
    1 t 'ics-button-ginform 2)
   ;; shouts should have a finger button...
   (list
    "^\\([^ \t\n(]+\\)\\(([^)]+)\\)* \\(c-\\|s-\\)?shouts: .*$"
    1 t 'ics-finger 1)
   ;; emotes too...
   (list
    "^--> \\([^ \t\n(\'%:\"#.]+\\)\\(([^)]+)\\)*.*$" 1 t 'ics-finger 1)
   ;; accept and decline buttons in match offers (are these too loose??)
   ;; ICC version...
   (list
    "\"\\(\\(accept \\|decline \\)[a-zA-Z][a-zA-Z0-9]+\\)\"" 1 t
    'ics-button-match-offer 1)
   ;; FICS version (works with fics 1.2.23)
   (list
    "\\(^You can \"\\| or \"\\)\\(accept\\|decline\\)\"" 2 t
    'ics-button-match-offer 2)
   ;; gamelist button
   (list
    "^ *\\([0-9]+\\) \\(.* \\[.*\\]\\) .* \\(W\\|B\\): +[0-9]+$"
     2 t 'ics-button-gamelist 1)
   ;; history list entries.
   (list
    (concat
     "^ ?\\([0-9]+\\): [-+=a] \\(.* \\[.*\\]\\)"
     " \\(-+\\|[A-E][0-9][0-9]\\) +"
     "\\(Res\\|Fla\\|Mat\\|Rep\\|Agr\\|Sta\\|NM\\|TM\\|Adj"
     "\\|Dis\\|WQ\\|BQ\\|WNM\\|WLM\\|50\\) +.*$")
    2 t 'ics-button-historylist 1)
   ;; seeklist button
   (list
    (concat
     "^ *\\(\\([0-9]+\\) +\\([0-9]+\\|----\\|++++\\) +"
     "\\([a-zA-Z]+[-a-zA-Z0-9]*\\(([^)]+)\\)*\\)\\) +"
     "\\([a-z0-9-()]*\\)? +[0-9]+ +[0-9]+ +\\(un\\)?rated.*$" )
    1 t 'ics-button-seeklist 2)
   ;; seek ad button
   (list
    "\"play \\([0-9]+\\)\" to respond" 0 t 'ics-button-seeklist 1)
   ;; stored games list button
   (list
    (concat
     " *\\([0-9]+: [BW] \\([a-zA-Z]+[-a-zA-Z0-9]*\\) +[Y] \\[.+\\]\\)"
     " +[0-9]+-[0-9+]")
    1 t 'ics-button-storedgame 2)
   ;; stored game number notification
   (list
    "You have [0-9]+ adjourned games."
    0 t 'ics-button-adjourned 0)
   ;; stored opponent arrival notification
   (list
    (concat
     "^Notification: \\([a-zA-Z]+[-a-zA-Z0-9]*\\)"
     ", with whom you have an adjourned game,"
     " has \\(arrived\\|departed\\).")
    1 t 'ics-button-storedgame 1)
   ;; name in finger stats
   (list
    "Statistics for \\([^ \t(]+\\)" 1 t 'ics-button-fingerstat 1)
   ;; more/next page of output button
   (list
    "^Type \\[\\(next\\)\\] to see next page\\."
    1 t 'ics-send-command 1)
   (list
    "^\\[Type \"\\(more\\)\" to see more\\.\\]"
    1 t 'ics-send-command 1)
   ;; This is how URLs _should_ be embedded in text...
   (list
    "<URL:\\([^\n\r>]*\\)>" 0 t 'ics-button-url 1)
   ;; farm mail addresses off to emacs mail command...
   (list
    "[^ \t\n@<>(\":]+@[^ \t\n@><)\"]+" 0 t 'ics-button-mail 0)
   ;; Next regexp stolen from highlight-headers.el.
   ;; Modified by Vladimir Alexiev.
   ;; modified by MNO (removed mailto button ... "\\|mailto" after wais)
   ;; since it seemed more aesthetically pleasing to use Emacs to
   ;; do the mailing, by matching the email address with above regexp.
   (list
    (concat
     "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\)"
     ":\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*"
     "[-a-zA-Z0-9_=#$@~`%&*+|\\/]")
    0 t 'ics-button-url 0))
  "Alist of regexps matching buttons in the ICS buffer.

Each entry has the form (REGEXP BUTTON FORM CALLBACK PAR...), where
REGEXP: is the string matching text around the button,
BUTTON: is the number of the regexp grouping actually matching the button,
FORM: is a lisp expression which must eval to true for the button to
be added,
CALLBACK: is the function to call when the user push this button, and each
PAR: is a number of a regexp grouping whose text will be passed to CALLBACK.

CALLBACK can also be a variable, in that case the value of that
variable it the real callback function.")
;;
;; end of button variables
;;
;;
;; variable to keep track of ics handle in use. NOT A USER VARIABLE!!
(defvar ics-handle "BarfieIsCool"
  "this variable is NOT the place to set your default handle. this is used
internally in the watch for login prompt.")
;;
;;

;;; Function definitions

(defun ics ()
  "Connect to internet chess servers."
  (interactive)
  (cond ((not ics-mode-map)
	 (setq ics-mode-map (copy-keymap comint-mode-map))
	 ;;(define-key ics-mode-map "\C-c\C-z" 'ics-insert-zippyism)
	 (define-key ics-mode-map "\C-c\C-w" 'ics-who)
	 (define-key ics-mode-map "\C-c\C-p" 'ics-players)
	 (define-key ics-mode-map "\C-c\C-f" 'ics-finger)
	 (define-key ics-mode-map "\C-c\C-m" 'ics-match)
	 (define-key ics-mode-map "\C-c\C-c" 'ics-confirm-quit)))
  (run-hooks 'ics-startup-hook)
  (if ics-add-buttons
      (define-key ics-mode-map [mouse-2] 'ics-mouse-push-button-or-yank))
  (if (string-match ".*devel.*" ics-version)
      ;; development version so trace the "hot" functions
      (let ((tracebuff (get-buffer-create "*ICS functrace*")))
	(trace-function-background 'ics-add-buttons-to-last-output tracebuff)
	(trace-function-background 'ics-add-buttons-to-region tracebuff)))
  (ics-connect))

(defun ics-connect ()
  "Function to query user for ics server to use and either connect to
that server in a new buffer, or in an existing buffer for that server,
or switch to any existing buffer running the ics conenction to that
server."
  (or ics-inhibit-startup-screen
      (progn
	(switch-to-buffer (get-buffer-create "*ICS Connect*"))
	(erase-buffer)
	(setq scroll-step 1)
	;; many thanks to Alefith for permission to use his ASCII
	;; knight and rook pictures here!
	(insert (format "
      ^^__                                                          _  _  _
     /  - \\_                       Emacs-ICS                       | || || |
   <|    __<                                                       |_______|
   <|    \\           A text window manager for ICS sessions.       \\__ ___ /
   <|     \\                                                         |___|_|
   <|______\\               (C) 1995-1999 Mark Oakden                |_|___|
    _|____|_                                                        |___|_|
   (________)  Issued with NO WARRANTY under the terms of the GNU  (_______)
   /________\\  public licence [C-h C-c to see a copy of the GPL]   /_______\\

Email any problems, bugs, enhancement requests etc. to the author at
<mark.oakden@camembert.freeserve.co.uk>

ics.el homepage at http://www.camembert.freeserve.co.uk/mark/icsel/

Emacs-ICS Version: %s
Interface Program: %s
   Default Handle: %s

Aliases defined:

 Alias | Shortname | Address
-------+-----------+---------------
" ics-version ics-interface ics-default-handle))
	(let* ((alist ics-servers-alist))
	  (while (car alist)
	    (insert (format "%6s |%10s | %s\n" (car (car alist))
			    (car (cdr (car alist)))
			    (car (cdr (cdr (car alist))))))
	    (setq alist (cdr alist))))
	))
  ;;
  ;; now prompt for address, handle and start the process
  ;;
  (let* ((address-or-alias (read-from-minibuffer
			    "ICS Server address or alias: "))
	 (server-info-list (cdr (assoc address-or-alias
				       ics-servers-alist)))
	 (ics-address (or (car (cdr server-info-list))
		      address-or-alias))
	 (ics-connect-method (or (car (nthcdr 3 server-info-list))
		      ics-default-connect-method))
	 (server-name (or (car server-info-list)
			  address-or-alias))
	 (ics-port (or (car (nthcdr 2 server-info-list))
		   (read-from-minibuffer "ICS port: " ics-default-port)))
	 (interface ics-interface)
	 (handle (read-from-minibuffer "ICS Handle: "
					   ics-default-handle))
	 (proc (concat server-name ":" handle))
	 (buffer (concat "*" proc "*")))
    (setq ics-handle handle)            ; save value of
					; handle in a global variable
    (or ics-inhibit-startup-screen
	(kill-buffer "*ICS Connect*"))
    (if (not (comint-check-proc buffer))
	(progn
	  (run-hooks 'ics-pre-connect-hook)
	  (set-buffer
	   (apply 'make-comint proc interface nil
		  (eval ics-interface-args)))
	  (run-hooks 'ics-post-connect-hook)
	  (ics-mode)))
    (switch-to-buffer buffer)
    (set (make-variable-buffer-local 'ics-last-command-time)
	 (ics-current-time))
    (set (make-variable-buffer-local 'ics-idle-p) nil)
    (set (make-variable-buffer-local 'ics-interface-variable-set) nil)
    (set (make-variable-buffer-local 'ics-wakeup-last-alarm-time)
	 (ics-current-time))
    (set (make-variable-buffer-local 'ics-last-highlight-end) nil)
    (set (make-variable-buffer-local 'ics-last-add-buttons-end) nil)
    (if (and ics-send-handle (not ics-wait-for-login-prompt))
	(progn
	  (message "Sending ICS handle...")
	  (comint-simple-send (get-buffer-process (current-buffer))
			      ics-handle)
	  (message "Sending ICS handle... done.")
	  (if ics-send-password
	      (message "Sending password...")
	      (comint-simple-send (get-buffer-process (current-buffer))
				  ics-password)
	      (message "Sending password...done"))))))

(defun ics-mode ()
  "Major mode for communication with ICS.

Uses comint-mode and so shares some functionality and key bindings with
that.  Useful commands include M-p and M-n to recall previous and next
commands in the history ring.
Customisation: Entry to this mode runs hooks on 'ics-startup-hook' and
'comint-mode-hook' (in that order)."
  (interactive)
  (comint-mode)
  (auto-save-mode -1) ; turn off auto save
  (setq major-mode 'ics-mode)
  (setq mode-name "ICS")
  (setq local-abbrev-table ics-mode-abbrev-table)
  (use-local-map ics-mode-map)
  ;;
  ;; some ics specific comint setup
  (setq comint-prompt-regexp ics-prompt-regexp)
  (setq comint-password-prompt-regexp ics-password-prompt-regexp)
  ;;
  ;; some nice settings (should these be default here or left for the
  ;; user to set for himself in a hook?)
  ;;
  ;(setq comint-scroll-to-bottom-on-input t)
  ;(setq comint-scroll-show-maximum-output t)
  ;;
  ;; if ics-highlight is non-nil, add highlight function into
  ;; comint-output-filter-functions
  ;;
  (or (null ics-highlight)
      (progn
	(add-hook 'comint-output-filter-functions
		  'ics-highlight-last-output nil t)
	(ics-highlight-buffer)))
  (or (null ics-add-buttons)
      (progn
	(add-hook 'comint-output-filter-functions
		  'ics-add-buttons-to-last-output nil t)
	(ics-add-buttons-to-buffer)))
  ;;
  ;; idle time resetter
  ;;
  (add-hook 'comint-input-filter-functions
	    'ics-reset-last-command-time nil t)
  ;;
  ;; abbrev mode on?
  ;;
  (setq abbrev-file-name ics-abbrev-file)
  (or (null ics-use-abbrev-mode)
      (and (not (abbrev-mode 1))
	   (if (file-readable-p ics-abbrev-file)
	       (read-abbrev-file)
	     (message "No abbrev file found."))))
  ;; should we wait for login prompt before sending handle?
  (if (and ics-send-handle ics-wait-for-login-prompt)
      (progn
	(message "Waiting for login prompt from ICS...")
	(add-hook 'comint-output-filter-functions
		  'ics-watch-for-login-and-send-handle nil t)))
  ;;
  ;; add in the general purpose ics-output-filter
  ;;
  (add-hook 'comint-output-filter-functions 'ics-output-filter nil t)
  (if (not ics-send-password)
      (add-hook 'comint-output-filter-functions
		'comint-watch-for-password-prompt nil t))
  (run-hooks 'ics-mode-hook))

(defun ics-output-filter (&optional string)
  "Eventually,  this function might parse ICS output.

At present, this merely strips literal ^G chars from the buffer and calls
\(ding t\) for each one stripped.  Included in comint-output-filter-functions
to enable bell rings when ics outputs ^G.  Also now strips ^M chars,  useful
if you use telnet to connect.

Updates variable ics-idle-p if player is idle.

Also performs ics-auto-command duties,  if ics-do-auto-commands is t"
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char
       (if (interactive-p) comint-last-input-end comint-last-output-start))
      (while (re-search-forward "\a" pmark t)
	(replace-match "" t t)
	(ding t)))
    (save-excursion
      (goto-char
       (if (interactive-p) comint-last-input-end comint-last-output-start))
      (while (re-search-forward "\r" pmark t)
	(replace-match "" t t)))
    ;; update the interface variable at ICS if necessary
    (if (and ics-set-interface-variable
	     (not ics-interface-variable-set)
	     (save-excursion
	       (beginning-of-line)
	       (looking-at ics-prompt-regexp)))
	(ics-update-interface-variable))
    ;; reset idle flag
    (setq ics-idle-p (< ics-idle-time (ics-time-since
				       ics-last-command-time)))
    ;; and process the user defined automatic commands
    (if ics-do-auto-commands
	(let ((alist ics-auto-command-alist))
	  (while alist
	    (let* ((entry (car alist))
		   (rest (cdr alist))
		   (regexp (car entry))
		   (level (nth 1 entry))
		   (condition (nth 2 entry))
		   (command (nth 3 entry)))
	      (setq alist rest)
	      ;;
	      ;; is condition true and did last output match regexp?
	      ;;
	      (if (and (eval condition)
		       (string-match regexp string))
		  (progn
		    (setq alist nil)
		    (let ((data (match-string level string)))
		      (cond ((stringp command)
			     (ics-send-command (format command data)))
			    ((fboundp command)
			     (funcall command data))
			    ((and (boundp command)
				  (fboundp (symbol-value command)))
			     (funcall (symbol-value command) data))
			    (t
			     (message
			      "ICS Autocommand must be a string or\
 function"))))))))))))

;;
;; utility function for ics-idle-p determination
;;
(defun ics-reset-last-command-time (&optional string)
  "reset last command time..."
  (setq ics-last-command-time (ics-current-time)))

(defun ics-update-interface-variable ()
    ;; set interface string if the feature is switched on and
    ;; we have not already set it
  (setq ics-interface-variable-set t)
  (ics-send-command
   (format "set interface ics.el %s (using %s)"
	   ics-version ics-interface)))
;;
;; wakeup functions
;;
(defun ics-wakeup (name)
  "Function to wakeup user and send a command (e.g. a tell to whoever
triggered the wakeup alarm) to ics.  Useful for ics-auto-command-alist
entries along with the condition ics-idle-p to simulate the (broken)
0.3.5 ics-wakeup \"feature\""
  ;; send the command
  (ics-send-command (format ics-wakeup-command name))
  ;; if we haven't had alarm beeps recently,  send them again
  (if (> (ics-time-since ics-wakeup-last-alarm-time)
	 ics-wakeup-alarm-timeout)
      (ics-wakeup-sound-alarm)))

(defun ics-wakeup-sound-alarm ()
  (let ((repeats ics-wakeup-number-of-beeps)
	(interval ics-wakeup-beep-interval)
	(iloop 0))
    (while (< iloop repeats)
      (ding t)
      (sleep-for interval)
      (setq iloop (1+ iloop)))
    (setq ics-wakeup-last-alarm-time (ics-current-time))))
;;;;;;;;
;;
;; buttons & highlighting
;;
;; Highlighting functions
;;
(defun ics-setup-highlight-alist ()
  "make the alist from its components"
  (setq itemlist ics-highlight-items
	ics-highlight-alist nil)
  (while itemlist
    (setq item (car itemlist)
	  itemlist (cdr itemlist)
	  re (symbol-value (intern-soft (concat "ics-" item "-regexp")))
	  facelist (symbol-value (intern-soft (concat "ics-" item "-face")))
	  face (cond ((not (x-display-color-p))
		      (nth 2 facelist))
		     ((eq ics-background-mode 'dark)
		      (nth 1 facelist))
		     (t                 ;if all else fails assume 'light
		      (car facelist)))
	  ics-highlight-alist (cons (append
				     re (list (if (facep face)
						  face
						(apply 'ics-face-lookup
						       face))))
				    ics-highlight-alist))))

(defun ics-highlight-buffer ()
  "Highlight the whole ICS buffer. Useful for highlighting ics session
log files for easier reading.."
  (interactive)
  (save-excursion
    (ics-highlight-region (point-min) (point-max))))
;;
(defun ics-highlight-last-output (&optional string)
  "Highlight the last piece of ics output"
  ;; as a first approximation, use the end of the last highlighting
  ;; for the start of this batch and the process mark in the current
  ;; buffer for the end.  If ics-last-highlight-end is nil then
  ;; we haven't highlighted anything yet so use point-min
  (let ((start (if (null ics-last-highlight-end)
		   (point-min)
		 ics-last-highlight-end))
	(end (process-mark (get-buffer-process
			    (current-buffer)))))
    ;; but end could be in the middle of some output from the server
    ;; so we go to the start of the line...
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (setq end (point)))
    ;; only call ics-highlight-region if we have some output to do...
    ;; ics-highlight-region will update ics-last-highlight-end when
    ;; it is finished
    (if (> end start)
	(ics-highlight-region start end))))

(defun ics-highlight-region (start end)
  "Highlights between start and end"
  ;;
  ;; first check if setup has been run yet and run it if not...
  ;;
  (if (null ics-highlight-alist)
      (ics-setup-highlight-alist))
  (let ((alist ics-highlight-alist))
    (while alist
      (let ((re (car (car alist)))
	    (level (car (cdr (car alist))))
	    (theface (car (nthcdr 2 (car alist)))))
	(save-excursion
	  (goto-char start)
	  (while (re-search-forward re end t)
	    (put-text-property (match-beginning level)
			       (match-end level) 'face theface))))
      (setq alist (cdr alist))))
    (setq ics-last-highlight-end end))

;;
;; Button functions,  mostly stolen/modified from gnus-vis.el in the gnus 5.0
;; distribution. (originally written by Per Abrahamsen)
;;
(defvar ics-button-regexp-list nil)
(defvar ics-button-regexp nil)
(defvar ics-button-last nil)
;;
(defun ics-add-buttons-to-last-output (&optional string)
  "Add buttons to the last piece of ics output"
  ;; as a first approximation, use the end of the last highlighting
  ;; for the start of this batch and the process mark in the current
  ;; buffer for the end.  If ics-last-add-buttons-end is nil then use
  ;; point-min since we haven't highlighted yet...
  (let ((start (if (null ics-last-add-buttons-end)
		   (point-min)
		 ics-last-add-buttons-end))
	(end (process-mark (get-buffer-process
			    (current-buffer)))))
    ;; but end could be in the middle of some output from the server
    ;; so we go to the start of the line...
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (setq end (point)))
    ;; only call ics-add-buttons-to-region if we have some output to do...
    ;; ics-add-buttons-to-region will update ics-last-add-buttons-end when
    ;; it is finished
    (if (> end start)
	(ics-add-buttons-to-region start end))))

(defun ics-add-buttons-to-buffer ()
  "Adds buttons to the whole ICS buffer. "
  (interactive)
  (save-excursion
    (ics-add-buttons-to-region (point-min) (point-max))))

(defun ics-add-buttons-to-region (regstart regend)
  "Find external references in region regstart to regend and make them into
buttons.

External references are things like who list entries and URLs, as
specified by `ics-button-alist'."
  (if (eq ics-button-last ics-button-alist)
      ()
    (setq ics-button-regexp (mapconcat 'car ics-button-alist  "\\|")
	  ics-button-last ics-button-alist))
  (save-excursion
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist ics-button-alist))
      (while alist
	(setq entry (car alist))
	(setq ics-button-regexp (car entry))
	(setq alist (cdr alist))
	(goto-char regstart)
	(while (re-search-forward ics-button-regexp regend t)
	  (goto-char (match-beginning 0))
	  (let* ((from (point))
		 (start (and entry (match-beginning (nth 1 entry))))
		 (end (and entry (match-end (nth 1 entry))))
		 (form (nth 2 entry)))
	    (if (not entry)
		()
	      (goto-char (match-end 0))
	      (if (eval form)
		  (ics-add-button start end 'ics-button-push
				  (set-marker (make-marker)
					      from))))))))))


(defun ics-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (and ics-button-face
       (overlay-put (make-overlay from to)
		    'face ics-button-face))
  (add-text-properties from to
		       (append (and ics-mouse-face
				    (list 'mouse-face ics-mouse-face))
			       (list 'ics-callback fun)
			       (and data (list 'ics-data data))))
  (setq ics-last-add-buttons-end to))

(defun ics-button-entry ()
  ;; Return the first entry in `ics-button-alist' matching this place.
  (let ((alist ics-button-alist)
	(entry nil))
    (while alist
      (setq entry (car alist)
	    alist (cdr alist))
      (if (looking-at (car entry))
	  (setq alist nil)
	(setq entry nil)))
    entry))

(defun ics-mouse-push-button-or-yank (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `ics-callback' property,
call it with the value of the `ics-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (let* ((pos (posn-point (event-start event)))
	 (data (get-text-property pos 'ics-data))
	 (fun (get-text-property pos 'ics-callback)))
    (if fun (funcall fun data)
      (mouse-yank-at-click event nil))))


(defun ics-button-push (marker)
  ;; Push button starting at MARKER.
  (save-excursion
    (goto-char marker)
    (let* ((entry (ics-button-entry))
	   (inhibit-point-motion-hooks t)
	   (fun (nth 3 entry))
	   (args (mapcar (lambda (group)
			   (let ((string (buffer-substring
					  (match-beginning group)
					  (match-end group))))
			     (set-text-properties 0 (length string) nil string)
			     string))
			 (nthcdr 4 entry))))
      (cond ((fboundp fun)
	     (apply fun args))
	    ((and (boundp fun)
		  (fboundp (symbol-value fun)))
	     (apply (symbol-value fun) args))
	    (t
	     (message "You must define `%S' to use this button"
		      (cons fun args)))))))

;;
;; functions for ics buffer buttons
;;
;;
(defun ics-send-command (command)
  "send COMMAND to ics process"
  (comint-simple-send (get-buffer-process (current-buffer)) command))

(defun ics-button-mail (toaddr)
  "Function to start emacs mailer on email addresses."
  (mail-other-window nil toaddr))

(defun ics-play (index)
  "issue a play command to play a game in the seek list"
  (interactive "sWhich index? ")
  (comint-simple-send (get-buffer-process (current-buffer))
		      (format ics-play-command index)))

(defun ics-finger (player)
  "Function to finger a player."
  (interactive "sFinger who? ")
  (comint-simple-send (get-buffer-process (current-buffer))
		      (format ics-finger-command player)))

(defun ics-observe (game)
  "Function to observe a game."
  (interactive "sWhich game? ")
  (comint-simple-send (get-buffer-process (current-buffer))
		      (format ics-observe-command game)))

(defun ics-examine-from-historylist (game)
  "Function to examine a game from history list"
  (save-excursion
    (or (re-search-backward
	 "\\(Recent games of\\|History for\\) \\([a-zA-Z0-9]+\\):"
			(point-min) t)
	(error "Couldn't identify player"))
    (let ((player (buffer-substring (match-beginning 2) (match-end 2))))
      (comint-simple-send (get-buffer-process (current-buffer))
			  (format "examine %s %s" player game)))))

(defun ics-match (player)
  "Function to challenge a player."
  (interactive "sMatch arguments: ")
  (comint-simple-send (get-buffer-process (current-buffer))
		      (format ics-match-command player)))

(defun ics-stored (dummy)
  "Function to send the command stored to the server"
  (interactive)
  (comint-simple-send (get-buffer-process (current-buffer))
		      "stored"))

(defun ics-who ()
  "Function to get who listing."
  (interactive)
  (comint-simple-send (get-buffer-process (current-buffer)) ics-who-command))

(defun ics-players ()
  "Function to get players listing."
  (interactive)
  (comint-simple-send (get-buffer-process (current-buffer))
		      ics-players-command))

;;; URL netscape functions.. from gnus 5.0 source

(defun ics-netscape-open-url (url)
  "Open URL in netscape, or start new scape with URL."
  (let ((process (start-process (concat "netscape " url)
				nil
				"netscape"
				"-remote"
				(concat "openUrl(" url ")'"))))
    (set-process-sentinel process
			  (` (lambda (process change)
			       (or (eq (process-exit-status process) 0)
				   (ics-netscape-start-url (, url))))))))

(defun ics-netscape-start-url (url)
  "Start netscape with URL."
  (start-process (concat "netscape" url) nil "netscape" url))


;;;;;;;;
;;
;; some "utility" functions...
;;
(defun ics-watch-for-login-and-send-handle (string)
  "function to send handle only after login prompt appears."
  (if (string-match ics-login-prompt-regexp string)
      (progn
	;; remove the hook ... this function has to be "onetrip" since
	;; otherwise,  entering an invalid handle results in a loop,
	;; with ics.el sending the handle repeatedly.
	(remove-hook 'comint-output-filter-functions
		     'ics-watch-for-login-and-send-handle t)
	(message "Sending ICS handle...")
	(comint-simple-send (get-buffer-process (current-buffer))
			    ics-handle)
	(message "Sending ICS handle... done.")
	(if ics-send-password
	    (progn
	      (message "Sending ICS password...")
	      (comint-simple-send (get-buffer-process (current-buffer))
				  ics-password)
	      (message "Sending ICS password... done.")))
)))
;;
;; some time related functions...
;;
(defun ics-current-time ()
  "Returns the second integer in current-time"
  (car (cdr (current-time))))

(defun ics-time-since (prev-time)
  "Return time in seconds since PTIME"
  (let* ((c-time (ics-current-time))
	 (diff-time (- c-time prev-time)))
    diff-time))
;;
;; quit function...
;;
(defun ics-confirm-quit ()
  "Checks with the user before quitting.  Bound to \"C-c C-c\" by default"
  (interactive)
  (if (y-or-n-p "Really send quit signal? ")
      (comint-quit-subjob)))
;;
;; Zippyism function
;;
(defvar zippyism-start-string "i Zippy-ises: "
  "string to pre-pend to Zippy-isms")

(defun ics-insert-zippyism ()
  "inserts a zippy-ism sans any embedded newlines into the current
buffer, with zippyism-start-string prepended"
  (interactive)
  (save-excursion
    (let ((start (point)))
      (insert-string zippyism-start-string)
      (yow 1)
      (while (> (point) start)
	(if (search-backward "\n" start "foo")
	    (replace-match "")))))
  (end-of-line))

;;;
;; compatibility function for 19.28...
;;
(or (fboundp 'match-string)
    (defun match-string (level &optional string)
      "compatibility funstion for 19.28... returns string matched
at level NUM by last regexp match.

(match-string NUM &optional STRING)

Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
      (if string
	  (substring string (match-beginning level) (match-end level))
	(buffer-substring (match-beginning level) (match-end level)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; next stuff snaffled from Per Abrahamsen's custom.el v0.5
;;
(or (fboundp 'facep)
    ;; Introduced in Emacs 19.29.
    (defun facep (x)
      "Return t if X is a face name or an internal face vector."
      (and (or (and (fboundp 'internal-facep) (internal-facep x))
	       (and
		(symbolp x)
		(assq x (and (boundp 'global-face-data) global-face-data))))
	   t)))

(or (and (fboundp 'modify-face) (not (featurep 'face-lock)))
    ;; Introduced in Emacs 19.29.  Incompatible definition also introduced
    ;; by face-lock.el version 3.00 and above for Emacs 19.28 and below.
    ;; face-lock does not call modify-face, so we can safely redefine it.
    (defun modify-face (face foreground background stipple
			     bold-p italic-p underline-p)
  "Change the display attributes for face FACE.
FOREGROUND and BACKGROUND should be color strings or nil.
STIPPLE should be a stipple pattern name or nil.
BOLD-P, ITALIC-P, and UNDERLINE-P specify whether the face should be set bold,
in italic, and underlined, respectively.  (Yes if non-nil.)
If called interactively, prompts for a face and face attributes."
  (interactive
   (let* ((completion-ignore-case t)
	  (face	       (symbol-name (read-face-name "Modify face: ")))
	  (colors      (mapcar 'list x-colors))
	  (stipples    (mapcar 'list
			       (apply 'nconc
				      (mapcar 'directory-files
					      x-bitmap-file-path))))
	  (foreground  (modify-face-read-string
			face (face-foreground (intern face))
			"foreground" colors))
	  (background  (modify-face-read-string
			face (face-background (intern face))
			"background" colors))
	  (stipple     (modify-face-read-string
			face (face-stipple (intern face))
			"stipple" stipples))
	  (bold-p      (y-or-n-p (concat "Set face " face " bold ")))
	  (italic-p    (y-or-n-p (concat "Set face " face " italic ")))
	  (underline-p (y-or-n-p (concat "Set face " face " underline "))))
     (message "Face %s: %s" face
      (mapconcat 'identity
       (delq nil
	(list (and foreground (concat (downcase foreground) " foreground"))
	      (and background (concat (downcase background) " background"))
	      (and stipple (concat (downcase stipple) " stipple"))
	      (and bold-p "bold") (and italic-p "italic")
	      (and underline-p "underline"))) ", "))
     (list (intern face) foreground background stipple
	   bold-p italic-p underline-p)))
  (condition-case nil (set-face-foreground face foreground) (error nil))
  (condition-case nil (set-face-background face background) (error nil))
  (condition-case nil (set-face-stipple face stipple) (error nil))
  (if (string-match "XEmacs" emacs-version)
      (progn
	(funcall (if bold-p 'make-face-bold 'make-face-unbold) face)
	(funcall (if italic-p 'make-face-italic 'make-face-unitalic) face))
    (funcall (if bold-p 'make-face-bold 'make-face-unbold) face nil t)
    (funcall (if italic-p 'make-face-italic 'make-face-unitalic) face nil t))
  (set-face-underline-p face underline-p)
  (and (interactive-p) (redraw-display))))


(defun ics-face-lookup (fg bg stipple bold italic underline)
  "Lookup or create a face with specified attributes.
FG BG STIPPLE BOLD ITALIC UNDERLINE"
  (let ((name (intern (format "ics-face-%s-%s-%s-%S-%S-%S"
			      (or fg "default")
			      (or bg "default")
			      (or stipple "default")
			      bold italic underline))))
    (if (and (facep name)
	     (fboundp 'make-face))
	()
      (make-face name)
      (modify-face name
		   (if (string-equal fg "default") nil fg)
		   (if (string-equal bg "default") nil bg)
		   (if (string-equal stipple "default") nil stipple)
		   bold italic underline))
    name))
;;
;; end of snaffled stuff
;;;;;;;;;;;;;;;;;;

(provide 'ics)

;;; end of ics.el
