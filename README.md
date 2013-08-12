# Emacs-init #
Here are my Emacs customization files, which only compatible with Emacs 24. I've used Emacs on Windows, GNU/Linux, and Mac OS X.

## Where to get the latest Emacs 24? ##

* [Windows Executable][]
* [Debian Packages][]
* [Ubuntu Packages][]
* [Mac Universal Binary][]
* [Via Homebrew on Mac][]: ```brew install emacs --cocoa --srgb```

## Where to find Emacs packages? ##

[MELPA][] and [Marmalade][] host many useful packages. Add following lines to your emacs init file to use them.

```lisp
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
```

Then you can use `M-x list-packages` to browse and install packages.

## How to install Emacs-init? ##
Simply clone this repository to your home directory.

```bash
git clone git://github.com/aethanyc/emacs-init.git ~/.emacs.d
```

[Windows Executable]: http://ftp.gnu.org/gnu/emacs/windows/
[Debian Packages]: http://emacs.naquadah.org/
[Ubuntu Packages]: https://launchpad.net/~cassou/+archive/emacs
[Mac Universal Binary]: http://emacsformacosx.com/
[Via Homebrew on Mac]: https://github.com/mxcl/homebrew/blob/master/Library/Formula/emacs.rb
[MELPA]: http://melpa.milkbox.net/
[Marmalade]: http://marmalade-repo.org/
