---
name: 🐞 Bug Report
about: Something didn't function the way you expected it to
title: ''
labels: ''
assignees: ''
---

<!-- Hello there, prospective issue reporter! Your bug reports are
     very valuable 💛.  They really are, and Eglot couldn't be made
     without them.  But there are lots of bugs and so little time.  So
     please - do not - remove or skip parts of this template.
  
     👉🏽 Need help or tech support?  Have an idea for a feature?
     Please don't open an issue!
  
     * Head to https://github.com/joaotavora/eglot/discussions to
       discuss.  Maybe that idea is already being discussed there.
  
     * Better yet, make an Emacs bug report, which can also be used
       for general discussion.  You'll potentially reach more people
       this way.  You can do it via `M-x report-emacs-bug` or just
       send email to `bug-gnu-emacs@gnu.org`.  Be sure to `CC:` (or
       better, `X-Debbugs-CC:` ) Eglot's maintainer, currently
       `joaotavora@gmail.com`.
  
     To make an issue, you need to provide some elements, which aren't
     hard to find.  Unfortunately, if you don't provide these
     elements, ** we may close the issue just like that 😐 **. -->
     
[ ] Server used:               <!-- (clangd, gopls, etc..) -->
[ ] Emacs version:             <!-- Type M-x emacs-version -->
[ ] Eglot version:             <!-- Look in M-x list-packages or tell Git SHA -->
[ ] Eglot installation method:       <!-- Git/package.el/straight/use-package/don't know -->
[ ] Using Doom:                <!-- Yes/No -->

#### LSP transcript (mandatory, unless Emacs inoperable)
<!-- Include the invaluable **events transcript**.  Inside Emacs, you
     can display that buffer with `M-x eglot-events-buffer`.  It
     contains the JSONRPC messages exchanged between client and
     server, as well as the messages the server prints to stderr.
     Copy that text and paste it below as a formatted code block
     (https://help.github.com/articles/creating-and-highlighting-code-blocks/)). -->
     
```lisp
... Paste the events transcript here ...  Try to start from the line that says
[client-request] (id:1) Sat Apr 10 21:40:09 2021:
(:jsonrpc "2.0" :id 1 :method "initialize" :params ...
```
    
#### Backtrace (mandatory, unless no error message seen or heard):
<!-- If Emacs errored (you saw -- and possibly heard -- an error
     message), make sure you repeat the process after enabling
     backtraces with `M-x toggle-debug-on-error`.  The backtrace
     buffer contains text that you should also include here, again as
     a formatted code block. -->
     
```lisp
... Paste the backtrace if you have it here ...
Debugger entered--Lisp error: (error "oh no")
  signal(error ("oh no"))
  error("oh no")
  eval((error "oh no") nil)
  pp-eval-expression((error "oh no"))
  funcall-interactively(pp-eval-expression (error "oh no"))
  call-interactively(pp-eval-expression nil nil)
  command-execute(pp-eval-expression)
```
   
#### Minimal configuration (mandatory)
<!-- Are you using Doom Emacs or Spacemacs Emacs or some very special
     pimped-out Emacs?  That's fine, but for this report we need you
     to replicate the problem with **as clean an Emacs run as
     possible**.
     
     Some people submit whole Git repositories with a sandboxed
     configuration.  That's fine, but sometimes simply Git-cloning the
     Eglot repo somewhere to your hard drive and a running this from
     the shell is more than enough: -->
     
 ```sh
 /path/to/a/certain/version/of/emacs -Q -f package-initialize -L /path/to/git-cloned/eglot -l eglot.el
 ```
 
 <!-- You can then add some lines of setup like: -->
 
 ```lisp
 (add-to-list 'eglot-server-programs '(foo-mode "foo-server"))
 (setq eglot-special-option-2000 '(foo bar with the airplane)) 
 (some-clearly-identified-third-party-package)
 ;; Add some clear descriptions of M-x commands executed  executed
 ;; If your bug relies of the contents of files such as 
 ;; .dir-locals.el or some project file with some specific content
 ;; also do include their contents in separate code blocks.
 ```
 
 <!-- For some bugs it this may seem like overkill but believe us,
      very often what seems like a "clear issue" is actually specific
      to some details of your setup. Having a runnable reproduction
      not only "proves" your bug to us but also allows us to spend all
      our effort fixing the bug instead of struggling to understand
      your issue.  (this well-worded paragraph taken from Rollup's bug
      tracker, BTW)-->
