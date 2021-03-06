;; counsel-bm.el --- Counsel interface for bm.  -*- lexical-binding: t; -*-

;; Filename: counsel-bm.el
;; Description: Counsel interface for bm.
;; Author:  zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer:  zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2020, zbelial, all rights reserved.
;; Created: 2020-09-03 14:08:12
;; Version: 0.1
;; URL: https://github.com/zbelial/counsel-bm.el
;; Package-Requires: ((bm "201905"))
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

(require 'bm)
(require 'ivy)
(require 'compile) ;; compilation-info-face, compilation-line-face

(defgroup counsel-bm nil
  "Bookmarks of bm.el related Applications and libraries for Counsel."
  :prefix "counsel-bm-" :group 'counsel)


(defface counsel-bm-annotation-face nil
  "Face used for annotation."
  :group 'counsel-bm)

(defun counsel-bm-bookmarks-all()
  (let (all bms)
    (dolist (buf (buffer-list))
      (setq bms (counsel-bm-bookmarks-in-buffer buf))
      (when bms
        (setq all (append all bms))))
    all))

(defun counsel-bm-bookmarks-in-buffer (&optional buf)
  "Gets a list of bookmarks in BUF, which can be a string or a buffer."
  (let ((buf (or buf (buffer-name)))
        (mklist (lambda (x) (if (listp x) x (list x)))))
    (funcall mklist
             (with-current-buffer buf
               (apply 'append
                      (mapcar mklist (remove nil (bm-lists))))))))

(defun counsel-bm-candidate-transformer (bm)
  "Return a string displayed in counsel buffer."
  (let ((bufname (plist-get bm :bufname))
        (lineno (plist-get bm :lineno))
        (content (plist-get bm :content))
        (annotation (plist-get bm :annotation)))
    (format "%s:%s:%s%s"
            (propertize bufname 'face compilation-info-face)
            (propertize lineno 'face compilation-line-face)
            content
            (if (s-blank? annotation) ""
              (concat "\n  "
                      (propertize annotation 'face
                                  'counsel-bm-annotation-face))))))

(defun counsel-bm-transform-to-candicate (bm)
  "Convert a BM to a CANDICATE."
  (let ((current-buf (overlay-buffer bm)))
    (with-current-buffer current-buf
      (let* ((start (overlay-start bm))
             (end (overlay-end bm))
             (bufname (buffer-name current-buf))
             (annotation (overlay-get bm 'annotation))
             (lineno (line-number-at-pos start)))
        (unless (< (- end start) 1)
          (list 
           :bufname bufname
           :lineno (int-to-string lineno)
           :content (buffer-substring-no-properties start (1- end))
           :annotation annotation))))))


(defun counsel-bm-collector (&optional all)
  (let ((bms (mapcar #'counsel-bm-transform-to-candicate
                     (if all
                         (counsel-bm-bookmarks-all)
                       (counsel-bm-bookmarks-in-buffer)))))
    (delq nil (mapcar #'(lambda (bm)
                          (cons (counsel-bm-candidate-transformer bm) bm))
                      bms))))

(defun counsel-bm-goto-line (linum &optional buf)
  (let ((buf (or buf (current-buffer))))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line (1- linum)))))

(defun counsel-bm-jump (cand)
  (let* ((bm (cdr cand))
         (bufname (plist-get bm :bufname))
         (lineno (plist-get bm :lineno)))
    (switch-to-buffer bufname)
    (counsel-bm-goto-line (string-to-number lineno))
    (recenter)))

(defun counsel-bm-update-fn ()
    (with-ivy-window
      (let ((current (ivy-state-current ivy-last))
            item bm bufname line)
        ;; (message "current %S" current)
        (when (not (string-empty-p current))
          (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
          ;; (message "item %S" item)
          (setq bm (cdr item))
          ;; (message "bm %S" bm)
          (setq bufname (plist-get bm :bufname))
          (setq line (plist-get bm :lineno))

          (when (get-buffer bufname)
            (when (not (string-equal (buffer-name) bufname))
              (set-window-buffer (selected-window) bufname)
              )
            (with-current-buffer bufname
              (goto-line (string-to-number line))
              (recenter)
              )
            )))))

(defvar counsel-bm--obuf nil)
(defvar counsel-bm--opoint nil)
(defun counsel-bm ()
  (interactive)
  (setq counsel-bm--obuf (buffer-name))
  (setq counsel-bm--opoint (point))

  (let* ((all (if (equal current-prefix-arg nil)
                  nil
                t))
         (bms (counsel-bm-collector all))
         (linum (line-number-at-pos))
         (preselect 0)
         res)
    (dolist (bm bms)
      (when (< (string-to-number (plist-get (cdr bm) :lineno)) linum)
        (setq preselect (1+ preselect)))
      )
    (unwind-protect
        (setq res (ivy-read "Visible bookmarks: " bms
                  :preselect preselect
                  :action '(1
                            ("o" counsel-bm-jump "jump to bookmark")
                            )
                  :update-fn #'counsel-bm-update-fn
                  :caller 'counsel-bm
                  ))
      (unless res
        (with-ivy-window
          (when (not (string-equal (buffer-name) counsel-bm--obuf))
            (set-window-buffer (selected-window) counsel-bm--obuf)
            )
          (with-current-buffer counsel-bm--obuf
            (goto-char counsel-bm--opoint))
          )
        )
      )
    )
  )


(defun counsel-bm-sorter (&optional l r)
  (let* ((lr (cdr l))
         (rr (cdr r))
         (lb (plist-get lr :bufname))
         (rb (plist-get rr :bufname))
         (lp (string-to-number (plist-get lr :lineno)))
         (rp (string-to-number (plist-get rr :lineno))))
    (or (string< lb rb)
        (or (and (string= lb rb)
                 (< lp rp))))))

(ivy-configure 'counsel-bm
  :sort-fn #'counsel-bm-sorter)

(provide 'counsel-bm)
