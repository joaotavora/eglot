---
name: Formal bug report
about: Something didn't work right, and you've written a perfect MRE recipe
title: ''
labels: ''
assignees: ''
---
<!-- To continue,  you will need to provide some elements
including a description of a [MINIMUM REPRODUCIBLE EXAMPLE][1] (MRE).  If
you don't have that, DON'T CREATE AN BUG REPORT ISSUE.  

Need help configuring or understanding Emacs, Eglot, or LSP?  Have an
idea for a feature?  Strongly suspect there is an Eglot bug, but can't
supply a MINIMUM REPRODUCIBLE EXAMPLE?  Please DON'T OPEN A NEW BUG
REPORT ISSUE!

Head to https://github.com/joaotavora/eglot/discussions to
discuss.  Start a new discussion, there are no templates there,
you can just speak your mind.

Maybe your issue is already solved or worked around.  Have glance at
https://github.com/joaotavora/eglot/issues?q=is%3Aissue+label%3Aworkaround

You can also make an Emacs bug report, which can also be used
for general discussion.  You'll potentially reach more people
this way.  You can do it via `M-x report-emacs-bug` or just
send email to `bug-gnu-emacs@gnu.org`.  Be sure to `CC:` (or
better, `X-Debbugs-CC:` ) Eglot's maintainer, currently
`joaotavora@gmail.com`.

If you don't provide the needed elements, WE MAY CLOSE THE ISSUE
JUST LIKE THAT. 
-->
     
* Server used:               <!-- (clangd, gopls, etc..) -->
* Emacs version:             <!-- Type M-x emacs-version -->
* Operating system:          <!-- (windows/mac osx/linux/don't know -->
* Eglot version:             <!-- Look in M-x list-packages or tell Git SHA -->
* Eglot installation method:       <!-- Git/package.el/straight/use-package/don't know -->
* Using Doom:                <!-- Yes/No -->

#### LSP transcript - M-x eglot-events-buffer (mandatory unless Emacs inoperable)
<!-- DO NOT SKIP: Include the invaluable LSP transcript.

Inside Emacs, you can display that buffer with 'M-x
eglot-events-buffer'.  It contains the JSONRPC messages exchanged
between client and server, as well as the messages the server prints
to stderr.  Copy that text and paste it below as a formatted code
block
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
   
#### Minimum Reproducible Example (mandatory)
<!-- DO NOT SKIP: 

You need to provide a MINIMAL, REPRODUCIBLE and COMPLETE recipe.
This is the bit that most troubles some people.  So please, I
kindly ask that you read [this first][1] for a general idea of
what is requested.  

You HAVE to create a short guide to help us replicate the problem
JUST AS IT HAPPENED TO YOU.  Make sure to double check that
following your own guide leads to the problem reliably.
-->
     
Here's how I suggest you structure your recipe:

1. How is Emacs started?

3. Where does the language server executable live in your machine and
   how should it be installed?
   
4. What project files are needed to demonstrate the problem?  Please
   don't say "_just open any Zglorb source file_".

   Don't assume the Eglot maintainers have any time to learn about a
   specific programming language and its associated toolchain.  Attach
   the actual file or files, or link to a repository containing them.
   
5. How is Emacs operated/configured _before_ you invoke Eglot?

6. How is Emacs operated _after_ you invoke Eglot

7. What actions must be taken for the problem to manifest itself?

8. What is the expected behaviour?

9. What is the observed behaviour?

10. VERY IMPORTANT: follow your own recipe (steps 1-9) by yourself
   and verify that it leads to the error.
   
You don't HAVE to follow this guide if you are confident you can
describe your own [COMPLETE, MINIMAL and REPRODUCIBLE][1] recipe.  See
https://github.com/joaotavora/eglot/issues/696 for a good example of
an issue following this template.
 
<!-- THANK YOU!
 
Having a runnable reproduction not only "proves" your bug to us
but also allows us to spend all our effort fixing the bug
instead of struggling to understand your issue.  

If you find that building the MRE takes a lot of work, well, that may
be so :-) But keep in mind that that work will have to be expended at
least once by someone to solve your problem, and it's often the most
time-consuming task.  So it pays if you can do that work upfront.

Thank you very much. -->

[1]: https://stackoverflow.com/help/minimal-reproducible-example
