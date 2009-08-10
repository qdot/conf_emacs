==========
conf_emacs
==========

Maintained by Kyle Machulis <kyle@nonpolynomial.com>
Nonpolynomial Labs - http://www.nonpolynomial.com

===========
Description
===========

This repository represents the deep, dark recesses of my soul, better known as my emacs configuration files. I've mercilessly ripped the format from Alex Ott's wonderful emacs conf files and tutorials, available at

http://xtalk.msk.su/~ott/common/emacs/_emacs.html

Any other attributions are usually made in the elisp code itself. I've included the full source for APEL, FLIM, and SEMI, as they are required for Wanderlust to work, and generally not the easiest thing in the world to find. If you decide to use my conf file and want to make sure you're at the latest, they're available at:

APEL: http://cvs.m17n.org/elisp/APEL/
FLIM: http://cvs.m17n.org/elisp/FLIM/index.html.ja.iso-2022-jp
SEMI: http://www.kanji.zinbun.kyoto-u.ac.jp/~tomo/elisp/SEMI/index.html.ja.iso-2022-jp

Everything else is maintained by way of git submodules, so whenever you pull this repository, do at 

git submodule --init update

To get the latest submodules. Thanks goes to whoever runs

http://github.com/emacs-pkg-mirrors

For keeping git versions of the cedet and w3m repos

All of my elisp code is covered by the license in LICENSE.txt. All sub-repos may have their own licenses, so check if you care.
