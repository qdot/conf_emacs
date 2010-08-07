conf_emacs 
==========

Emacs Configuration for Kyle Machulis <kyle@nonpolynomial.com>

Nonpolynomial Labs - [http://www.nonpolynomial.com](http://www.nonpolynomial.com)

WARNING
=======

What you see in this repo may not be exactly what I'm using right now. I sync my emacs files via dropbox, and don't update the repo probably as often as I should. So, if you want to try to use my full config repo, and have problems, please feel free to contact me through github, email, or IRC (I'm qDot_ on freenode #emacs).

Description
===========

This repository represents the deep, dark recesses of my soul, better known as my emacs configuration files. I've mercilessly ripped the format from Alex Ott's wonderful emacs conf files and tutorials, available at

[http://xtalk.msk.su/~ott/common/emacs/_emacs.html](http://xtalk.msk.su/~ott/common/emacs/_emacs.html)

Any other attributions are usually made in the elisp code itself. 

Everything else is maintained by way of git submodules, so whenever you pull this repository, do at

git submodule --init update

To get the latest submodules. Thanks goes to whoever runs

[http://github.com/emacsmirror](http://github.com/emacsmirror)

For keeping git versions of the cedet and w3m repos

And thanks to Dave Abrahams for maintaining the repos for wanderlust and requirements at 

[http://github.com/wanderlust/](http://github.com/wanderlust/)

All of my elisp code is covered by the license in LICENSE.txt. All sub-repos may have their own licenses, so check if you care.

Things That Require Weird Building
==================================

* Wanderlust is kinda crazy to build. Assuming you want to keep everything in source, you'll need to reset your config paths in the CFG files for apel, semi, flim, and wl. 

Things That Don't Come With The Repo
====================================

I have a few things that I use that don't automatically come with the repo.

Todochiku, which is a cross platform notify system, needs:

* OS X - growlnotify, available in the Extras portion of the Growl Installer, and expected to be at /usr/local/bin/growlnotify
* Linux - notify-send, which is in the libnotify-bin package on Ubuntu

For python happy-making, I use rope and pyflakes. 

* For rope, you'll need to easy_install rope, then pull the latest code for ropemode and ropemacs from http://bitbucket.org/agr/
* For pyflakes, just easy_install pyflakes. Note that if you don't do this, you may get a weird error about index bounds being wrong, because flyflakes isn't very smart

