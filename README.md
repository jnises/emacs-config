My emacs config files.
Not likely to be useful for anyone else.

## windows
* install git
* install msys2
* run `pacman -S mingw-w64-x86_64-emacs` in msys
* create an environment variable called `HOME` pointing to `%userprofile%`
* `choco install ripgrep fd`

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
