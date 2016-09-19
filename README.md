# Emacs-init #

This repository contains my own Emacs customization, tested against lastest
Emacs release on Mac OS X and GNU/Linux.

## Where to get the latest Emacs binaries? ##

* [GNU Official Windows Binaries](http://ftp.gnu.org/gnu/emacs/windows/)
* [Mac Universal Binaries](http://emacsformacosx.com/)
* [Via Homebrew on Mac](https://github.com/Homebrew/homebrew-core/blob/master/Formula/emacs.rb) (Type `brew info emacs` to see install options)
* [Emacs Mac Port](https://github.com/railwaycat/homebrew-emacsmacport/releases)

## How to build Emacs manually?
Clone Emacs source code from official repository by `git clone git://git.sv.gnu.org/emacs.git`

* Build Emacs on Mac
  * Install Xcode command line tools by `xcode-select --install`
  * See `nextstep/INSTALL` for instructions. Basically, it's:
  * `make install  # Assemble the app in nextstep/Emacs.app`
  * Link or copy `nextstep/Emacs.app` to `/Applications`
* Build Emacs on Debian-based GNU/Linux
  * `apt-get install build-essential`
  * `apt-get build-dep emacs24  # Install packages for build-dependencies.`
  * See `INSTALL.REPO` in emacs repository for instructions. Basically, it's simply:
  * `make`
  * `emacs` and `emacsclient` will be available under `src` and `lib-src`, respectively.

## Where to find Emacs packages? ##

* [MELPA](http://melpa.org/) hosts up-to-date packages.
* [MELPA Stable](http://stable.melpa.org/) hosts stable packages.

Use <kbd>M-x list-packages</kbd> to browse and install packages.

## Where to find information? ##

* [EmacsWiki](http://www.emacswiki.org/)
* [WikEmacs](http://wikemacs.org/)


## How to install Emacs-init? ##

Simply clone this repository to your home directory via:

```bash
git clone --recursive git://github.com/aethanyc/emacs-init.git ~/.emacs.d
```
