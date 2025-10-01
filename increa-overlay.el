;;; increa-overlay.el --- Overlay display for increa -*- lexical-binding: t; -*-

;;; Commentary:
;; Ghost text completion display using overlays

;;; Code:

(defvar-local increa--overlay nil
  "Overlay for completion display.")

(defvar-local increa--keymap-overlay nil
  "Overlay for keymap activation.")

(defface increa-overlay-face
  '((t :inherit shadow))
  "Face for completion text.")

(defconst increa-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'increa-accept-completion)
    (define-key map (kbd "<tab>") #'increa-accept-completion)
    map)
  "Keymap active when completion is visible.")

(defun increa--get-overlay ()
  "Get or create overlay for completion."
  (unless (overlayp increa--overlay)
    (setq increa--overlay (make-overlay 1 1 nil nil t))
    (setq increa--keymap-overlay (make-overlay 1 1 nil nil t))
    (overlay-put increa--keymap-overlay 'keymap increa-completion-map)
    (overlay-put increa--keymap-overlay 'priority 101)
    (overlay-put increa--overlay 'keymap-overlay increa--keymap-overlay))
  increa--overlay)

(defun increa--set-overlay-text (text)
  "Display completion TEXT in overlay."
  (let ((ov (increa--get-overlay))
        (kov (overlay-get (increa--get-overlay) 'keymap-overlay)))
    (move-overlay ov (point) (line-end-position))
    (move-overlay kov (point) (min (point-max) (1+ (point))))

    (let* ((tail (buffer-substring (point) (line-end-position)))
           (full-text (concat text tail))
           (propertized (propertize text 'face 'increa-overlay-face)))
      (if (eolp)
          (progn
            (put-text-property 0 (min 1 (length propertized)) 'cursor t propertized)
            (overlay-put ov 'after-string propertized))
        (overlay-put ov 'display (substring propertized 0 1))
        (overlay-put ov 'after-string (substring propertized 1))))

    (overlay-put ov 'completion text)
    (overlay-put ov 'start (point))))

(defun increa-clear-overlay ()
  "Clear completion overlay."
  (interactive)
  (when (overlayp increa--overlay)
    (delete-overlay increa--overlay)
    (setq increa--overlay nil))
  (when (overlayp increa--keymap-overlay)
    (delete-overlay increa--keymap-overlay)
    (setq increa--keymap-overlay nil)))

(defun increa--overlay-visible-p ()
  "Return non-nil if completion overlay is visible."
  (and (overlayp increa--overlay)
       (overlay-buffer increa--overlay)))

(defun increa-accept-completion ()
  "Accept the current completion."
  (interactive)
  (when (increa--overlay-visible-p)
    (let* ((completion (overlay-get increa--overlay 'completion))
           (start (overlay-get increa--overlay 'start)))
      (goto-char start)
      (insert completion)
      (increa-clear-overlay)
      t)))

(provide 'increa-overlay)
;;; increa-overlay.el ends here