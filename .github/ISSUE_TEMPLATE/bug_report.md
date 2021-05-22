---
name: 🐞 Bug Report
about: Something didn't function the way you expected it to
title: ''
labels: ''
assignees: ''
---
<!-- Hello there, prospective issue reporter! Your bug reports are
     very valuable 💛 💛 💛.  They really are, and Eglot couldn't be 
     made without them.  But there are lots of bugs and so little 
     time.  So:

        PLEASE - DO NOT - REMOVE OR SKIP PARTS OF THIS TEMPLATE.
  
     👉 Need help configuring or understanding Emacs, Eglot, or LSP?
       Have an idea for a feature?  Please DON'T OPEN A NEW ISSUE!
  
       Head to https://github.com/joaotavora/eglot/discussions to
       discuss.  Start a new discussion, there are no templates there,
       you can just speak your mind.

     👉 Maybe your issue is already solved or worked around.  Have glance at
       https://github.com/joaotavora/eglot/issues?q=is%3Aissue+label%3Aworkaround
  
     👉 You can also make an Emacs bug report, which can also be used
       for general discussion.  You'll potentially reach more people
       this way.  You can do it via `M-x report-emacs-bug` or just
       send email to `bug-gnu-emacs@gnu.org`.  Be sure to `CC:` (or
       better, `X-Debbugs-CC:` ) Eglot's maintainer, currently
       `joaotavora@gmail.com`.
  
     To make an issue, you need to provide some elements, which aren't
     hard to find.  Can't find all the elements for this template?
     No problem, just make a discussion 👆.

     Here's an example of a 👌 fine issue report following this template:
     https://github.com/joaotavora/eglot/issues/696
     
     If you don't provide the needed elements, WE MAY CLOSE THE ISSUE
     JUST LIKE THAT 😐. 
-->
     
* Server used:               <!-- (clangd, gopls, etc..) -->
* Emacs version:             <!-- Type M-x emacs-version -->
* Operating system:          <!-- (windows/mac osx/linux/don't know -->
* Eglot version:             <!-- Look in M-x list-packages or tell Git SHA -->
* Eglot installation method:       <!-- Git/package.el/straight/use-package/don't know -->
* Using Doom:                <!-- Yes/No -->

#### LSP transcript - M-x eglot-events-buffer (mandatory unless Emacs inoperable)
<!-- DO NOT SKIP: Include the invaluable LSP transcript.

     Inside Emacs, you can display that buffer with the M-x
     eglot-events-buffer command. It contains the JSONRPC messages
     exchanged between client and server, as well as the messages the
     server prints to stderr.  Copy that text and paste it below as a
     formatted code block
     (https://help.github.com/articles/creating-and-highlighting-code-blocks/)). -->
     
```lisp
... Paste the events transcript here ...  Try to start from the line that says
[client-request] (id:1) Sat Apr 10 21:40:09 2021:
(:jsonrpc "2.0" :id 1 :method "initialize" :params ...
```
    
#### Backtrace (mandatory, unless no error message seen or heard):
<!-- DO NOT SKIP: 

     If Emacs errored (you saw -- and possibly heard -- an error message), 
     make sure you repeat the process after enabling backtraces with 
     `M-x toggle-debug-on-error`.  The backtrace buffer contains text that 
     you should include here, again as a formatted code block. 
-->
     
```lisp
... Paste the backtrace here ...
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
<!-- DO NOT SKIP: 

     Are you using Doom Emacs or Spacemacs Emacs or some very special 
     pimped-out Emacs?  That's fine, but for this report we need to be 
     able to replicate the problem JUST AS IT HAPPENED TO YOU.  

     We can't and don't have time to replicate your complex configuration 
     and environment, so you need to provide a MINIMAL, REPRODUCIBLE and 
     COMPLETE recipe.
     
     How to do this?  The easiest recipes just start Emacs from the shell:
-->
     
 ```sh
 # Type this in a shell to start an Emacs with Eglot configured
 $ /path/to/a/certain/version/of/emacs -Q -f package-initialize -L /path/to/git-cloned/eglot -l eglot.el 
 ```
 
 <!-- Then you add a bit of Elisp code that can be typed into *scratch* -->
 
 ```lisp
 ;; Example of a minimal configuration
 ;;
 (add-to-list 'eglot-server-programs '(foo-mode "foo-server"))
 (setq eglot-special-option-2000 '(foo bar with the airplane)) 
 (some-clearly-identified-third-party-package)
  ```
 
 <!-- WHEW!! THANK YOU!
 
      For some bugs, all this may seem like overkill but believe us,
      very often what seems like a "clear issue" is actually specific
      to some details of your setup. Having a runnable reproduction
      not only "proves" your bug to us but also allows us to spend all
      our effort fixing the bug instead of struggling to understand
      your issue.
 
      Anyway, after you've launched Emacs + Eglot that please explain
      what options you clicked on or what commands you entered, in a
      way that is clear to the Eglot maintainers so they can replicate
      the problem JUST AS IT HAPPENED TO YOU.
      
      If this requires some bits of .emacs or init.el configuration,
      you need to add them (just as in the example above)
      
      If reproducing your error requires others files and or programs,
      you NEED TO ADD snippets for them or provide links to them.

      Some users also provide whole Git repositories perfectly
      "sandboxing" a configuration and setup.  If you're confident on
      how do to this, that also works.
      
      Thank you very much.
      
      (Adapted this template from RollupJS's bug tracker). -->
