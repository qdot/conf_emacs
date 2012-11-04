conf_emacs 
==========

By Kyle Machulis

Nonpolynomial Labs - [http://www.nonpolynomial.com](http://www.nonpolynomial.com)

WARNING
=======

What you see in this repo may not be exactly what I'm using right now.
I sync my emacs files via dropbox, and don't update the repo probably
as often as I should. So, if you want to try to use my full config
repo, and have problems, please feel free to contact me through
github, or IRC (I'm qDot_ on freenode #emacs).

Description
===========

This repository represents the deep, dark recesses of my soul, better
known as my emacs configuration files. I started by mercilessly
ripping the format from Alex Ott's wonderful emacs conf files and
tutorials, available at

[https://github.com/alexott/emacs-configs](https://github.com/alexott/emacs-configs)

But they've now morphed into something that is vaguely my own
doing/fault.

Any other attributions are usually made in the elisp code itself. 

Package management happens by way of el-get, available at

[http://github.com/dimitri/el-get/](http://github.com/dimitri/el-get/)

My el-get config is in elget-status.el, that will sync all of the
packages I use. Warning: there are a lot.

Things That Don't Come With The Repo
====================================

I have a few things that I use that don't automatically come with the
repo.

For python happy-making, I use rope and pyflakes. 

* For rope, you'll need to easy_install rope, then pull the latest
  code for ropemode and ropemacs from http://bitbucket.org/agr/
* For pyflakes, just easy_install pyflakes. Note that if you don't do
  this, you may get a weird error about index bounds being wrong,
  because flyflakes isn't very smart

