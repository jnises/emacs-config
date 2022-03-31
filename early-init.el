;;; -*- lexical-binding: t -*-

;; we use straight
(setq package-enable-at-startup nil)

(set-language-environment "UTF-8")
;; set-language-enviornment sets default-input-method, which is unwanted
(setq default-input-method nil)

(setq deepness-early-inited t)
