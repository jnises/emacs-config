(defun uniview-send (message &optional host port)
  (interactive "Mmessage: ")
  (let ((host (if host host "localhost"))
        (port (if port port 22000)))
    (let ((c (open-network-stream "uniview_connection" nil host port)))
      (unwind-protect
          (process-send-string c message)
        (delete-process c)))))

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
  ;; (uniview-reload-fbo-shader)
  ;; (uniview-reload-star-shader)
  ;; (uniview-send "ISS.reload")
  ;; (uniview-send "Magneto.reload")
  ;; (uniview-send "BowShock.reload")
  ;; (uniview-send "Cassini.reload")
  ;; (uniview-send "iso50k.reload")
  ;; (uniview-send "iso20k.reload")
  ;; (uniview-send "iso10k.reload")
  ;;(uniview-send "SunSurface.reload")
  ;;(uniview-send "rainbow.reload")
  ;; (uniview-send "hubble.reload")
  ;; (uniview-send "earthquakes.reload")
  ;;(uniview-send "Aurora.reload")
  ;;(uniview-send "testearth.reload")
  ;;(uniview-send "asteroids_hypothetical")
  ;;(uniview-send "curiosity.reload")
  (uniview-reload-uses-shaders)
  )

(global-set-key (kbd "C-c u s") 'uniview-send)
(global-set-key (kbd "C-c u r") 'uniview-reload-shaders)

(provide 'uniview-connection)
