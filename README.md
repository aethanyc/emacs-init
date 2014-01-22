# Emacs-init #

Here are my Emacs customization files, which only compatible with
Emacs 24. I've used Emacs on Windows, GNU/Linux, and Mac OS X.

## Where to get the latest Emacs 24? ##

* [GNU Official Windows Binaries](http://ftp.gnu.org/gnu/emacs/windows/)
* [Debian Snapshot Packages](http://emacs.naquadah.org/)
* [Ubuntu Snapshot Packages](https://launchpad.net/~cassou/+archive/emacs)
* [Mac Universal Binaries](http://emacsformacosx.com/)
* [By Homebrew on Mac](https://github.com/mxcl/homebrew/blob/master/Library/Formula/emacs.rb):
  * Stable version: ```brew install emacs --cocoa --srgb```
  * Development version on git: ```brew install emacs --cocoa --srgb --use-git-head --HEAD```

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
