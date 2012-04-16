(defun uniview-send (message)
  (interactive "s")
  (let ((c (open-network-stream "uniview_connection" nil "localhost" 22000)))
    (unwind-protect
        (process-send-string c message)
      (delete-process c))))

(defun uniview-reload-gas-shader ()
  (interactive)
  (uniview-send "galaxy.reloadGasShader"))

(defun uniview-reload-fbo-shader ()
  (interactive)
  (uniview-send "galaxy.reloadFBOShader"))

(defun uniview-reload-star-shader ()
  (interactive)
  (uniview-send "galaxy.reloadStarShader"))

(defun uniview-reload-uses-shaders ()
  (interactive)
  (uniview-send "system.reloadallshaders"))

(defun uniview-reload-shaders ()
  (interactive)
  (uniview-reload-gas-shader)
  (uniview-reload-fbo-shader)
  (uniview-reload-star-shader)
  (uniview-reload-uses-shaders))

(global-set-key (kbd "C-c u r") 'uniview-reload-shaders)
