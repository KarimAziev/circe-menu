;;; circe-menu.el --- Transient menu for circe commands -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/circe-menu
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.5.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient menu for circe commands

;;; Code:

(require 'transient)

(declare-function circe "circe.el" (network-or-server &rest server-options))


(defcustom circe-menu-initial-network-or-server nil
  "Initial network or server when starting circe."
  :group 'circe
  :type '(radio (const :tag "Not set" nil)
                (string :tag "A network or server name as a string")))

(defcustom circe-menu-auth-source-bitlbee-entry-name nil
  "Entry name for retrieving bitlbee configuration with `pass'.

The entry should should have password the following fields:
\\='((secret . \"BITLBEE_PASSWORD\")
 (\"login\" . \"my-login\")
 (\"channels\" . \"(:immediate \\\"&bitlbee\\\" :after-auth \\\"#SERVER/CHANNEL\\\")\"))"
  :group 'circe
  :type '(radio (const :tag "Not set" nil)
                (string :tag "Pass entry")))

(defvar circe-menu-chat-buffers nil
  "List of chat buffers.")

(defun circe-menu-s-shared-start (s1 s2)
  "Return the longest prefix S1 and S2 have in common."
  (declare (pure t)
           (side-effect-free t))
  (let ((search-length (min (length s1)
                            (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i)
                   (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun circe-menu-key-builder-capitalize-variants (word)
  "Return list of words of WORD, but it with upcased letter."
  (let ((cands)
        (parts (split-string word "" t)))
    (dotimes (i (length parts))
      (let ((val (string-join (remove nil
                                      (list
                                       (when (> i 0)
                                         (string-join (seq-take parts i) ""))
                                       (upcase (nth i parts))
                                       (string-join (seq-drop parts (1+ i))
                                                    "")))
                              "")))
        (push val
              cands)))
    (reverse cands)))

(defun circe-menu-key-builder-safe-substring (len word)
  "Substring WORD from zero to LEN."
  (if (> (length word) len)
      (substring-no-properties word 0 len)
    word))

(defun circe-menu-key-builder-get-all-key-strategies (word len)
  "Generate preffered shortcut from WORD with length LEN."
  (let* ((parts (append (split-string word "[^a-z]" t)
                        (list (replace-regexp-in-string "[^a-z]" "" word))))
         (parts-len (length parts))
         (finalize (lambda (short)
                     (while (> len (length short))
                       (setq short (concat short (number-to-string
                                                  (random 10)))))
                     (circe-menu-key-builder-safe-substring len short)))
         (vars
          (mapcar finalize (circe-menu-key-builder-capitalize-variants
                            (circe-menu-key-builder-safe-substring
                             len
                             (replace-regexp-in-string
                              "[^a-z]"
                              ""
                              word))))))
    (seq-sort-by
     (lambda (it)
       (cond ((string-match-p "[0-9]" it)
              -2)
             ((member it vars)
              -1)
             (t (length (circe-menu-s-shared-start word it)))))
     #'>
     (seq-uniq (append
                vars
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'circe-menu-key-builder-safe-substring
                                       n)
                                      parts)))
                 (number-sequence 1 (min len parts-len)))
                (mapcar
                 (lambda (n)
                   (funcall finalize
                            (mapconcat
                             (apply-partially
                              #'circe-menu-key-builder-safe-substring n)
                             (reverse parts))))
                 (number-sequence 1 (min len parts-len))))))))

(defun circe-menu-key-builder-generate-shortcuts (items &optional key-fn
                                                        value-fn used-keys)
  "Generate shortcuts from list of ITEMS.
If KEY-FN is nil, ITEMS should be list of strings or symbols.
If KEY-FN is a function, it will be called with every item of list, and should
return string that will be as basis for shortcut.
If VALUE-FN is nil, result is an alist of generated keys and corresponding
items.
If VALUE-FN is non nil, return a list of results of calling VALUE-FN with two
arguments - generated shortcut and item.
USED-KEYS is a list of keys that shouldn't be used."
  (let* ((value-fn (or value-fn (lambda (key value)
                                  (if (proper-list-p value)
                                      (append (list key) value)
                                    (cons key value)))))
         (total (length items))
         (random-variants (append
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "a")
                                                    (string-to-char
                                                     "z")))
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "A")
                                                    (string-to-char
                                                     "Z")))))
         (variants-len (length random-variants))
         (min-len
          (if used-keys
              (length (car (seq-sort-by #'length #'> used-keys)))
            (cond ((>= variants-len total)
                   1)
                  ((>= variants-len (/ total 2))
                   2)
                  (t 3)))))
    (let ((shortcuts used-keys)
          (used-words '())
          (all-keys (mapcar (lambda (def)
                              (if key-fn
                                  (funcall key-fn def)
                                (if (symbolp def)
                                    (symbol-name def)
                                  def)))
                            items))
          (result))
      (dotimes (i (length items))
        (when-let* ((def (nth i items))
                    (word (if key-fn
                              (funcall key-fn def)
                            (if (symbolp def)
                                (symbol-name def)
                              def))))
          (when (not (member word used-words))
            (push word used-words)
            (let ((short
                   (downcase
                    (substring-no-properties word 0
                                             (min min-len
                                                  (length word))))))
              (setq short (replace-regexp-in-string "[^a-z]" "" short))
              (setq short (seq-find
                           (lambda (it)
                             (not
                              (seq-find (apply-partially
                                         #'string-prefix-p it)
                                        shortcuts)))
                           (append
                            (circe-menu-key-builder-get-all-key-strategies
                             word
                             min-len)
                            (when (= min-len 1)
                              (or (seq-remove (lambda (key)
                                                (seq-find (apply-partially
                                                           #'string-prefix-p
                                                           (downcase key))
                                                          all-keys))
                                              random-variants)
                                  random-variants)))))
              (while (and
                      (< (length short) min-len))
                (setq short (concat short (number-to-string (random 10)))))
              (push short shortcuts)
              (push
               (cond ((functionp value-fn)
                      (funcall value-fn short def))
                     (t (cons short def)))
               result)))))
      (reverse result))))


(defun circe-menu-with-circe-chat-mode-buffers (fn &rest args)
  "Execute FN with ARGS in `circe-chat-mode' buffers."
  (let ((results))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'circe-chat-mode)
          (push (apply fn args) results))))
    (reverse results)))

(defun circe-menu-group-with (fn items &optional transform-fn)
  "Group ITEMS by calling FN with every item.
FN should return key.
TRANSFORM-FN is called with two arguments - value and key."
  (seq-reduce (lambda (acc it)
                (let* ((key (funcall fn it))
                       (val (if transform-fn (funcall transform-fn
                                                      key it)
                              it))
                       (cell (assoc key acc))
                       (group (if cell
                                  (append (cdr cell)
                                          (list val))
                                (list val))))
                  (if cell
                      (setcdr cell group)
                    (push (cons key group) acc))
                  acc))
              (seq-copy items) '()))

(defun circe-menu-get-buffer-names ()
  "Return sorted list of chat buffers."
  (circe-menu-with-circe-chat-mode-buffers
   'buffer-name))

(defun circe-menu-index-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))))

(defun circe-menu-get-next-or-prev-element (element step switch-list)
  "Get ELEMENT depending on STEP value and SWITCH-LIST."
  (if-let ((pos (seq-position switch-list element #'string=)))
      (nth (circe-menu-index-switcher step pos switch-list) switch-list)
    (pcase step
      ((pred (> 0))
       (car (last switch-list)))
      (_ (car switch-list)))))


(defun circe-menu-switch-to-next-or-prev-buffer (arg)
  "Switch to the next (if ARG is greater than zero) or previous circe buffer."
  (let ((buff
         (if (derived-mode-p 'circe-chat-mode)
             (current-buffer)
           (or
            (when-let ((wnd (seq-find
                             (lambda (it)
                               (and (window-live-p it)
                                    (not (window-dedicated-p it))
                                    (when-let ((b (window-buffer it)))
                                      (and (not (minibufferp b))
                                           (with-current-buffer b
                                             (derived-mode-p
                                              'circe-chat-mode))))))
                             (window-list))))
              (select-window wnd)
              (current-buffer))
            (current-buffer))))
        (buffs (mapcan #'cdr (circe-menu-group-by-prefixes
                             circe-menu-chat-buffers))))
    (with-current-buffer buff
      (let* ((name (buffer-name buff))
             (pos (seq-position buffs name #'string=))
             (new-pos
              (when pos (if (> arg 0)
                            (1+ pos)
                          (if (= pos 0)
                              (1- (length buffs))
                            (1- pos)))))
             (next (or
                    (when new-pos
                      (nth new-pos buffs))
                    (if (> arg 0)
                        (car buffs)
                      (car (last buffs))))))
        (when (not (derived-mode-p 'circe-chat-mode))
          (select-window (or (window-right (selected-window))
                             (window-left (selected-window))
                             (split-window-right))))
        (switch-to-buffer next nil t)
        (transient-setup 'circe-menu)))))

(defun circe-menu-next-buffer ()
  "Switch to the next circe chat buffer."
  (interactive)
  (circe-menu-switch-to-next-or-prev-buffer 1))

(defun circe-menu-prev-buffer ()
  "Switch to the previous circe chat buffer."
  (interactive)
  (circe-menu-switch-to-next-or-prev-buffer -1))

(defun circe-menu-group-by-prefixes (strings)
  "Group STRINGS by longest common prefixes."
  (circe-menu-group-with
   (lambda (str)
     (car
      (seq-sort-by
       #'length
       '>
       (seq-uniq (delq nil
                       (mapcar
                        (apply-partially
                         #'circe-menu-s-shared-start
                         str)
                        (remove str strings)))))))
   strings))

(defun circe-menu-get-buffer-suffixes ()
  "Return buffer suffixes."
  (circe-menu-key-builder-generate-shortcuts
   (seq-reduce
    (lambda (acc gr)
      (let* ((prefix (car gr))
             (children (mapcar
                        (lambda (it)
                          (let ((sym (make-symbol
                                      (replace-regexp-in-string
                                       "[\s\t#@&_*]"
                                       ""
                                       (format
                                        "circe-switch-to-%s"
                                        it)))))
                            (fset sym
                                  `(lambda ()
                                     (interactive)
                                     (let ((all-buffs
                                            (circe-menu-get-buffer-names)))
                                       (when-let ((buff
                                                   (get-buffer
                                                    ,it)))
                                         (when (buffer-live-p
                                                buff)
                                           (if-let ((wnd
                                                     (seq-find
                                                      (lambda
                                                        (n)
                                                        (and
                                                         (buffer-live-p
                                                          (get-buffer
                                                           n))
                                                         (get-buffer-window
                                                          (get-buffer
                                                           n))))
                                                      all-buffs)))
                                               (progn
                                                 (select-window
                                                  (get-buffer-window
                                                   (get-buffer
                                                    wnd)))
                                                 (pop-to-buffer-same-window
                                                  buff))
                                             (switch-to-buffer
                                              buff))
                                           (transient-setup
                                            'circe-menu))))))
                            (list
                             (car (split-string
                                   (substring-no-properties
                                    it
                                    (length prefix))
                                   "@" t))
                             sym
                             :description
                             `(lambda
                                ()
                                (let* ((curr
                                        (buffer-name
                                         (current-buffer)))
                                       (face
                                        (when (string=
                                               ,it
                                               curr)
                                          'transient-red))
                                       (str
                                        (substring-no-properties
                                         ,it)))
                                  (if
                                      face
                                      (propertize
                                       str
                                       'face
                                       face)
                                    str))))))
                        (cdr gr))))
        (setq acc (nconc acc children))))
    (circe-menu-group-by-prefixes
     circe-menu-chat-buffers)
    '())
   #'car (lambda (k v)
           (setcar v k)
           v)
   '("n" "p")))

(defun circe-menu-plist-remove (keys plist)
  "Remove KEYS and values from PLIST."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and (not (memq key keys))
                       (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

(defun circe-menu-setup-bitlbee ()
  "Add and setup Bitlbee to `circe-network-options' if it is not present.
This function uses `pass' to retrieve \"bitlbee\".
The entry should should have password the following fields:
\\='((secret . \"BITLBEE_PASSWORD\")
 (\"login\" . \"my-login\")
 (\"channels\" . \"(:immediate \\\"&bitlbee\\\" :after-auth \\\"#SERVER/CHANNEL\\\")\"))"

  (when circe-menu-auth-source-bitlbee-entry-name
    (require 'auth-source-pass)
    (unless (and (boundp 'circe-network-options)
               (assoc-string "Bitlbee" circe-network-options))
    (when-let ((entry
                (when (fboundp 'auth-source-pass-parse-entry)
                  (auth-source-pass-parse-entry circe-menu-auth-source-bitlbee-entry-name))))
      (let ((nickserv-password (alist-get 'secret entry))
            (nick
             (if-let ((field (seq-find (lambda (field)
                                         (assoc-string field entry))
                                       '("login" "user" "username" "email"))))
                 (cdr (assoc-string field entry))
               (user-login-name)))
            (channels
             (if-let ((chans (cdr
                              (assoc-string
                               "channels"
                               entry))))
                 (car
                  (read-from-string
                   (cdr
                    (assoc-string
                     "channels"
                     entry))))
               '(:immediate "&bitlbee")))
            (defaults (cdr
                       (when-let* ((def (assoc-string "Bitlbee"
                                                      (when
                                                          (boundp
                                                           'circe-network-defaults)
                                                        circe-network-defaults)))
                                   (pl (circe-menu-plist-remove '(:lagmon-disabled)
                                                        (cdr def))))
                         (setcdr def pl)
                         def))))
        ;; Remove lagmon-disabled due to annoying warning: Unknown option :lagmon-disabled, ignored
        (when (boundp 'circe-network-options)
          (add-to-list
           'circe-network-options
           (append (list "Bitlbee")
                   (circe-menu-plist-remove '(:lagmon-disabled) defaults)
                   (list
                    :nick nick
                    :nickserv-password nickserv-password
                    :channels channels)))))))))



;;;###autoload (autoload 'circe-menu "circe-menu.el" nil t)
(transient-define-prefix circe-menu ()
  "Command dispatcher with circe commands."
  ["Chats"
   :setup-children
   (lambda (&rest _)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (circe-menu-get-buffer-suffixes)))
   :class transient-column]
  ["Next"
   ("p" circe-menu-prev-buffer
    :description
    (lambda ()
      (format "Prev chat: %s"
              (circe-menu-get-next-or-prev-element
               (buffer-name)
               -1
               (mapcan #'cdr (circe-menu-group-by-prefixes
                              circe-menu-chat-buffers))))))
   ("n" circe-menu-next-buffer
    :description (lambda ()
                   (format "Next chat: %s"
                           (circe-menu-get-next-or-prev-element
                            (buffer-name)
                            1
                            (mapcan #'cdr (circe-menu-group-by-prefixes
                                           circe-menu-chat-buffers))))))]
  (interactive)
  (setq circe-menu-chat-buffers (circe-menu-get-buffer-names))
  (if circe-menu-chat-buffers
      (transient-setup 'circe-menu)
    (require 'circe)
    (circe-menu-setup-bitlbee)
    (if circe-menu-initial-network-or-server
        (funcall-interactively #'circe circe-menu-initial-network-or-server)
      (call-interactively #'circe))))

(provide 'circe-menu)
;;; circe-menu.el ends here