;;; multi-term-tmux.el --- Managing remote and persistent terminal
;;; buffers in Emacs with multi-term and tmux.

;; Author: Todd Goodall <tgoodall@utexas.edu>
;; Copyright (C) 2015 Todd Goodall, all rights reserved.
;; Created: <2015-11-29>
;; Version: 0.0.1
;; Last-Updated: <2015-11-29>
;; URL: http://github.com/beyondmetis/multi-term-tmux
;; Keywords: term, terminal, multiple buffer, tmux
;; Compatibility: GNU Emacs 24.5.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;; `multi-term'
;;

;;; Commentary:
;;
;; This package is an extension of the well known `multi-term.el', its
;; purpose is to add easy ways to create remote terms and persistent
;; terms using the terminal multiplexer TMUX
;;

;;; Installation:
;;
;; Install via el-get
;;
;; (require 'multi-term)
;; (require 'multi-term-tmux)
;;
;; Below are the commands you can use:
;;
;; `multi-term-tmux-new' Creates a new local term buffer.
;; `multi-term-tmux-remote-new' Creates a new remote buffer.
;;
;; Variables that you can set:
;;

;;; Change log:

;; 2015/12/01
;; * ssh tmux connections now work (barely)
;; * renamed `multi-term-tmux-new' -> multi-term-tmux-open
;; * renamed `multi-term-tmux-remote-new' -> multi-term-tmux-remote-open
;;
;; 2015/11/29
;; * First released.
;; * `multi-term-tmux-new' function
;; * `multi-term-tmux-remote-new' function
;;

;;; Acknowledgments:
;;
;; Mark Triggs <mst@dishevelled.net>
;; For create multi-shell.el
;; Andy Stewart <lazycat.manatee@gmail.com>
;; For mantaining it.
;; Roman Gonzalez <romanandreg@gmail.com>
;; For his screen-based extension to multi-term.
;;

;;; Bug
;;
;;

;;; TODO
;; add ability to disconnect all other views by default (for tmux sizing)
;; add tmux parameters to make it look seemless (bar at the top)
;; "disconnect" option
;;    allows launching a terminal with tmux to get separate window going
;;    provide escape for overloading output

(defcustom multi-term-tmux-name "slave"
  "The default name for the slave buffer."
  :type 'string
  :group 'multi-term)

;;; Require:
(require 'multi-term)

(cl-defun multi-term-tmux-sessions (&optional (user+host nil))
    (interactive)
    (if user+host
	(let* ((sessionlist (shell-command-to-string (concat "ssh " user+host " -q -t -t tmux list-sessions"))) (sessionlist (split-string sessionlist "\n" t)) (sesslist nil)) 
	(dolist (elt sessionlist)
	    (setq sessname (split-string elt ":" t))
	    (setq sessname (nth 0 sessname))
	    (setq sesslist (append sesslist (list sessname))))
	sesslist)

	(let* ((sessionlist (shell-command-to-string "tmux list-sessions")) (sessionlist (split-string sessionlist "\n" t)) (sesslist nil)) 
	(dolist (elt sessionlist)
	    (setq sessname (split-string elt ":" t))
	    (setq sessname (nth 0 sessname))
	    (setq sesslist (append sesslist (list sessname))))
	sesslist)))

(defun multi-term-tmux-get (term-name session-name)
  "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input."
  (with-temp-buffer
    (setq tmuxls (multi-term-tmux-sessions))

    (if (member session-name tmuxls)
	(make-term term-name "tmux" nil "attach" "-t" session-name)
    (make-term term-name "tmux" nil "new" "-s" session-name))
    (setq term-name (concat "*" term-name "*"))

    (with-current-buffer term-name
        (multi-term-internal))
    (switch-to-buffer term-name)
    term-name))

(defun multi-term-tmux-remote-get (term-name session-name user+host)
  "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input."
  (with-temp-buffer
    (setq tmuxls (multi-term-tmux-sessions user+host))

    (if (member multi-term-tmux-name tmuxls)
	(make-term term-name "ssh"  nil user+host "-t" "tmux" "attach" "-t" multi-term-tmux-name)
    (make-term term-name "ssh" nil user+host "-t" "tmux" "new" "-s" multi-term-tmux-name))
    (setq term-name (concat "*" term-name "*"))

    (with-current-buffer term-name
        (multi-term-internal))
    (switch-to-buffer term-name)
    term-name))

;;; Code:
(defun multi-term-tmux-open (&optional session-name buffer-name)
  "Input: provided SESSION-NAME, BUFFER-NAME, SCREEN-SHELL."
  (interactive)
  ;; get a special buffer
  (setq term-name (or buffer-name (format "localhost-tmux-%s" multi-term-tmux-name)))
  (setq session-name (or session-name multi-term-tmux-name))

  (multi-term-tmux-get term-name session-name))


(defun multi-term-tmux-remote-open (user+host &optional session-name buffer-name)
  "Input: provide USER+HOST, SESSION-NAME."
  (interactive)

  (setq term-name (or buffer-name (format "%s-tmux-%s" user+host multi-term-tmux-name)))
  (setq session-name (or session-name multi-term-tmux-name))

  (multi-term-tmux-remote-get term-name session-name user+host))


;; End:
(provide 'multi-term-tmux)
;;; multi-term-tmux.el ends here
