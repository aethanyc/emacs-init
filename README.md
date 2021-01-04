# Emacs-init #

This repository contains my own Emacs customization, tested against latest Emacs
27 release branch on macOS and GNU/Linux.

## Where to get the latest Emacs binaries? ##

* [GNU Official Windows Binaries](http://ftp.gnu.org/gnu/emacs/windows/)
* [Mac Universal Binaries](http://emacsformacosx.com/) (`brew cask install emacs`)
* [Via Homebrew on Mac](https://github.com/Homebrew/homebrew-core/blob/master/Formula/emacs.rb) (Type `brew info emacs` to see install options)
* [Emacs Mac Port](https://github.com/railwaycat/homebrew-emacsmacport/releases)

## How to build Emacs manually?
Clone Emacs source code from [official repository](https://savannah.gnu.org/projects/emacs) by `git clone git://git.sv.gnu.org/emacs.git`

* Build Emacs on macOS
  * Install Xcode command line tools by `xcode-select --install`
  * Install dependencies by `brew install pkg-config gnutls` (Hint: `brew info emacs`)
  * `make install` (This step will assemble the app in `nextstep/Emacs.app`. See `nextstep/INSTALL` for more information.)
  * If the `libxml/tree.h` cannot be found, try [this workaround](https://lists.gnu.org/archive/html/emacs-devel/2019-10/msg00400.html)
  * Link or copy `nextstep/Emacs.app` to `/Applications`
* Build Emacs on Debian-based GNU/Linux
  * `apt install build-essential` and `apt build-dep emacs`. These commands install the necessary dependent packages.
  * `make`. See `INSTALL.REPO` in the emacs repository for more information.
  * After the build finished, `emacs` and `emacsclient` will be available under `src` and `lib-src`, respectively. Add them to the loading path of your preferred shell.

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
git clone --recurse-submodules https://github.com/aethanyc/emacs-init.git ~/.emacs.d
```
