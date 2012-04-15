;; tag update functions
;;
;; author: Joel Nises
;;
;; Set the environment variable TAGDIR or use (setq patchwork-tags-dir
;; "[tagdir]") to specify where to put the generated tag file.
;;
;; A tags file will automatically be created when starting emacs if there
;; doesn't already exist a tags file in the current view.
;; Use M-x update-stn-tags to update/create tags.
;;
(require 'cl)

;; set the TAGDIR environment variable to a directory where you want your tag
;; file to be placed
(setq patchwork-tags-dir
      (catch 'break
        (dolist (dir (list (getenv "TAGDIR") "/vobs/stn/dev" "~/.emacs.d/tags" "~" "/tmp" ""))
          (when (and dir (file-directory-p dir) (file-writable-p dir))
            (throw 'break dir)))))

(setq patchwork-tags-prefix
      (let ((envval (getenv "USER")))
        (if envval (concat envval "__") "user__")))

;; additional arguments to send to etags
(setq patchwork-tags-etags-args "--members")

(setq patchwork-tags-tagfile (concat patchwork-tags-dir "/" "." patchwork-tags-prefix "stn_tags"))
(setq patchwork-tags-tmpfile (concat patchwork-tags-tagfile "_tmp"))


(defun patchwork-tags-visit-tagfile ()
  ;; temporarily increase the large file warning limit to 200 MB
  (let ((large-file-warning-threshold 200000000))
    (visit-tags-table patchwork-tags-tagfile)))
  

(defun update-stn-tags (&optional directories)
  "updates the tag file for the entire stn
warning, this might take a while"
  (interactive)

  ;; should check for running updates here

  (lexical-let ((dirs (mapconcat (lambda (x)
                                   (expand-file-name x))
                                 (if (null directories)
                                     '("/vobs/stn")
                                   ;;else
                                   directories)
                                 ;; separator
                                 " "
                                 )))
    (message "updating tags in the background...")
    ;; make sure the tagfile is in the revert-without-query list
    (add-to-list 'revert-without-query patchwork-tags-tagfile)
    ;; remove the temporary file if it exists
    (when (file-exists-p patchwork-tags-tmpfile)
      (delete-file patchwork-tags-tmpfile))
    (set-process-sentinel
     (start-process-shell-command "stn-update" "*stn-update*" 
                                  (concat "find " dirs " \\( -not -path '*/bin/*' \\) -a \\( -name '*.cpp' -o -name '*.h' -o -name '*.c' -o -name '*.cc' -o -name '*.hh' -o -name '*.sig' \\) -print | xargs etags " patchwork-tags-etags-args " -o- > " patchwork-tags-tmpfile "; mv " patchwork-tags-tmpfile " " patchwork-tags-tagfile)) 
     ;; finished callback
     (lambda (process event)
       (when (equal (process-status process) 'exit)
         (if (not (= (process-exit-status process) 0))
             (message "tag update failed")
           (patchwork-tags-visit-tagfile)
           (message "done updating tags for stn")))))))

(defun patchwork-tags-set-keybindings ()
  "Set some useful keybindings"
  (interactive)
  ;; loop through tags
  ;; forward
  (global-set-key (kbd "C-:") (lambda () (interactive) (find-tag "" t)))
  ;; backwards
  (global-set-key (kbd "C-;") (lambda () (interactive) (find-tag "" '-))))

(defun patchwork-tags-protected-stn-update ()
  "Update the stn tags unless there already exists a tag or
temporary tag file. If the tagfile already exists visit it.
But first make sure that the vob is mounted."
  (when (file-exists-p "/vobs/stn/dev")
    (if (file-exists-p patchwork-tags-tagfile)
        (patchwork-tags-visit-tagfile)
      ;; else, no tagfile, it needs to be created somehow
      (if (file-exists-p patchwork-tags-tmpfile)
          ;; if the tmpfile is too old
          (if (> (- (float-time (current-time)) (float-time (nth 5 (file-attributes patchwork-tags-tmpfile)))) 3600)
              (update-stn-tags)
            ;; if it is new enough, retry
            (run-at-time 60 nil 'patchwork-tags-protected-stn-update))
        ;; if the tempfile doesn't exist, update the tags
        (update-stn-tags)))))

(provide 'patchwork-tags)
