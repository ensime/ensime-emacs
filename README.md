[![Melpa Status](http://melpa.milkbox.net/packages/ensime-badge.svg)](http://melpa.milkbox.net/#/ensime)
[![Build Status](http://fommil.com/api/badges/ensime/ensime-emacs/status.svg)](http://fommil.com/ensime/ensime-emacs)

Documentation is available at [ensime.org](http://ensime.org/editors/emacs/)

Contribution guide (How to build&test) is at http://ensime.github.io/editors/emacs/contributing/ 

# for the impatient
If you want to run the mode directly, then
  1) clone the git repository to your computer
  2) remove the sbt-mode package from emacs

  3) add the following to .emacs
  
  (add-to-list 'load-path "/some/path/emacs-sbt-mode")
  
  (load-file "/some/path/emacs-sbt-mode/sbt-mode.el")
