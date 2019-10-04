;;; Windman.el --- Windows Manager      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

;;; Variables

(defvar windman-transient-map (let ((map (make-sparse-keymap)))
                                (define-key map (kbd "f") #'windman-select-next-sibling)
                                (define-key map (kbd "b") #'windman-select-previous-sibling)
                                (define-key map (kbd "n") #'windman-select-child)
                                (define-key map (kbd "p") #'windman-select-parent)
                                (define-key map (kbd "2") #'windman-split-below)
                                (define-key map (kbd "3") #'windman-split-right)
                                (define-key map (kbd "1") #'windman-unify)
                                (define-key map (kbd "d") #'windman-delete-window)
                                map)
  "Transient map for windman operations.")

;;; Backstage

(defvar windman--selected-window nil
  "Windman operations will act on this (live or internal) window.")

(defun windman--window-internal-p (window)
  "Return t if WINDOW is an internal window."
  (and (not (window-live-p window)) (window-valid-p window)))

(defun windman--window-children (window)
  "Return all child window of WINDOW."
  (let* ((win (window-child window))
         lst)
    (while win
      (push win lst)
      (setq win (window-next-sibling win)))
    lst))

(defun windman--window-width (window)
  "Return the width of WINDOW.
This function works for internal window, unlike the default one."
  (if (windman--window-internal-p window)
      (apply #'+ (mapcar #'windman--window-width
                         (windman--window-children window)))
    (window-width window)))

(defun windman--highlight-window (window)
  "Highlight WINDOW."
  (cond ((windman--window-internal-p window)
         (mapc #'windman--highlight-window (windman--window-children window)))
        ((window-live-p window)
         (set-window-parameter window 'mode-line-format "%-")))
  ;; otherwise mode-line doesn’t update in transient-map, IDKW
  (force-mode-line-update))

(defun windman--dehighlight-window (window)
  "Dehighlight WINDOW."
  (cond ((windman--window-internal-p window)
         (mapc #'windman--dehighlight-window (windman--window-children window)))
        ((window-live-p window)
         (set-window-parameter window 'mode-line-format nil)))
  (force-mode-line-update))

;;; Commands

;;;; Select

(defun windman-select-window (&optional window)
  "Select WINDOW or selected window in windman."
  (interactive)
  (let ((window (or window (selected-window))))
    (set-transient-map windman-transient-map t #'windman-quit)
    (windman--select-window window)))

(defun windman--select-window (window)
  "Select WINDOW or selected window in windman."
  (interactive)
  (let ((win (or window (selected-window))))
    (windman--dehighlight-window windman--selected-window)
    (setq windman--selected-window win)
    (windman--highlight-window win)))

(defun windman-select-parent ()
  "Select the parent window."
  (interactive)
  (if-let ((parent (window-parent windman--selected-window)))
      (windman--select-window parent)
    (message "No parent window")))

(defun windman-select-child ()
  "Select the child window."
  (interactive)
  (if-let ((child (window-child windman--selected-window)))
      (windman--select-window child)
    (message "No child window")))

(defun windman-select-previous-sibling ()
  "Select the previous sibling window."
  (interactive)
  (if-let ((sibling (window-prev-sibling windman--selected-window)))
      (windman--select-window sibling)
    (message "No previous sibling window")))

(defun windman-select-next-sibling ()
  "Select the next sibling window."
  (interactive)
  (if-let ((sibling (window-next-sibling windman--selected-window)))
      (windman--select-window sibling)
    (message "No next sibling window")))

(defun windman-quit ()
  "Cancel all window selection."
  (interactive)
  (windman--dehighlight-window windman--selected-window))

;;;; Operation

(defun windman-delete-window ()
  "Delete selected window."
  (interactive)
  ;; select a sibling and delete the old window
  (let ((oldwin windman--selected-window)
        (prev (window-prev-sibling oldwin))
        (next (window-next-sibling oldwin)))
    (cond (prev (windman--select-window prev))
          (next (windman--select-window next))))
  (delete-window oldwin))

(defun windman-split-below ()
  "Split selected window below like \C-x 2."
  (interactive)
  (split-window windman--selected-window
                (/ (window-pixel-height windman--selected-window) 2)
                'below t))

(defun windman-split-right ()
  "Split selected window right like \C-x 3."
  (interactive)
  (split-window windman--selected-window
                ;; we use pixel size because char size doesn’t work
                ;; for internal windows
                (/ (window-pixel-width windman--selected-window) 2)
                'right t))

(defun windman-unify ()
  "Combine siblings into one, or expand to full screen."
  (interactive)
  (let ((win windman--selected-window))
    (cond ((windman--window-internal-p win)
           (mapc #'delete-window
                 (remove (selected-window)
                         (windman--window-children win))))
          ((window-live-p win)
           (delete-other-windows)))))

(provide 'windman)

;;; windman.el ends here
