# Emacs-init #

Here are my Emacs customization files, which only compatible with
Emacs 24. I've used Emacs on Windows, GNU/Linux, and Mac OS X.

## Where to get the latest Emacs 24 binaries? ##

* [GNU Official Windows Binaries](http://ftp.gnu.org/gnu/emacs/windows/)
* [Mac Universal Binaries](http://emacsformacosx.com/)
* [By Homebrew on Mac](https://github.com/mxcl/homebrew/blob/master/Library/Formula/emacs.rb):
  * Stable version: `brew install emacs --cocoa`
  * Development version on git: `brew install emacs --cocoa --HEAD`

## How to build Emacs manually?
Clone Emacs source code from official repository with ```git clone git://git.sv.gnu.org/emacs.git```

* Build Emacs on Mac
  * See ```nextstep/INSTALL``` for instructions. Basically, it's:
  * ```./autogen.sh```
  * ```./configure --with-ns```
  * ```make install  # Assemble the app in nextstep/Emacs.app```
  * Link or copy ```nextstep/Emacs.app``` to ```/Applications```
* Build Emacs on Debian-based GNU/Linux
  * ```apt-get install build-essential```
  * ```apt-get build-dep emacs24  # Install packages for build-dependencies.```
  * See ```INSTALL.REPO``` in emacs repository for instructions. Basically, it's:
  * ```./autogen.sh```
  * ```./configure```
  * ```make bootstrap```
  * ```make```
  * ```emacs``` and ```emacsclient``` will be available under ```src``` and ```lib-src```, respectively.

## Where to find Emacs packages? ##

[MELPA](http://melpa.milkbox.net/) hosts many useful packages. Add
following lines to your emacs init file to use them.  See
[Usage](https://github.com/milkypostman/melpa) section on MELPA Github
to learn how to use it.

Then you can use `M-x list-packages` to browse and install packages.

## Where to find informations? ##

* [EmacsWiki](http://www.emacswiki.org/)
* [WikEmacs](http://wikemacs.org/)


## How to install Emacs-init? ##

Simply clone this repository to your home directory.

```bash
git clone --recursive git://github.com/aethanyc/emacs-init.git ~/.emacs.d
```
