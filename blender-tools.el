(when (require 'xml-rpc nil t)
  (defun blender-eval-file ()
    (interactive)
    (let ((filename (buffer-file-name)))
      (xml-rpc-method-call "http://localhost:3718" 'exec (format "
with open(\"%s\") as f:
    code = compile(f.read(), \"%s\", \"exec\")
    exec(code)
" filename filename))))

  (defun blender-eval-region ()
    (interactive)
    (xml-rpc-method-call "http://localhost:3718" 'exec (buffer-substring (region-beginning) (region-end))))
  
  (global-set-key (kbd "C-c b f") 'blender-eval-file)
  (global-set-key (kbd "C-c b r") 'blender-eval-region)
)
