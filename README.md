My emacs config files.
Not likely to be useful for anyone else.

## windows
* install git
* create an environment variable called `HOME` pointing to `%userprofile%`
* `choco install ripgrep fd`
* run latest installer from https://ftp.gnu.org/gnu/emacs/windows/

### msys2
the msys2 version of emacs 28.1 seems to be broken. native compilation just produces errors. so probably good to not install emacs from here.
if you really want to you can run `pacman -S mingw-w64-x86_64-emacs`  
good to install msys2 for the unix utils though

## mac
* `brew install --cask emacs`
* `brew install ripgrep fd`

## setup
* `git submodule update --init`
* create a file called `~/.emacs.d/init.el` and put this in it:
```
;; (setq download-packages t) ; or not if you don't want to download external things
;; ;(setq deepness-company "propellerheads") ;; for company specific settings
(setq home-el-path "~/.emacs.d/el")
(load (concat home-el-path "/config.el"))
```
