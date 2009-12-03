;;; df.el --- Hack to display in the mode line space left on devices

;; Copyright (C) 1999 by Association April

;; Author: Benjamin Drieu <bdrieu@april.org>
;; Keywords: unix, tools

;; This file is NOT part of GNU Emacs.

;; GNU Emacs as this program are free software; you can redistribute
;; them and/or modify them under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.

;; They are both distributed in the hope that they will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;;  This is a slight hack to display disk usage in the mode line.
;;  Disk space remaining is updated every df-refresh second.

;;  If you work with a lot of users sharing the same partition, it
;;  sometimes happens that you have no place left to save your work,
;;  which drives you to serious brain damage when you lost important
;;  work.  This package allows you to have place left and buffer size
;;  displayed in the mode line, so you know when you can save your
;;  file or when it's time to do some cleanup.

;;  This package may (must) not be very optimized or efficient, but
;;  this is a quick hack.  Comments and suggestions are welcome.

;;  df is simple to use :
;;  - Put this file in your load-path
;;  - Put the following in your .emacs : (autoload 'df "df" nil t)
;;  - Add something like (df "/home") in your .emacs if you want to
;;    scan /home


;; $Id: df.el,v 1.1.1.1 2002/07/01 17:04:37 benj Exp $

;; $Log: df.el,v $
;; Revision 1.1.1.1  2002/07/01 17:04:37  benj
;; - version initiale
;;
;; Revision 1.8  2001/12/07 13:08:16  benj
;; - fixed a misplaced (interactive)
;;
;; Revision 1.7  2000/06/05 11:19:22  benj
;;  - put some variables local so buffer size is buffer-local
;;  - add a hook to find-file-hook to display correct size
;;
;; Revision 1.6  1999/11/05 22:04:03  benj
;; - Now use a minor mode instead of that ugly dance with mode-line-format
;; - Really use variables instead of constants in the code
;; - Better structuration (df-enable and df-disable)
;; - Some more documentation
;; - Licence typos fixed
;;
;; Revision 1.5  1999/01/24 17:25:54  drieu
;; - Add Paal Steihaug remarks :
;;   + use magic df argument, which only scan a partition
;;   + add (require 'cl)
;;   + df-update is now much clean
;;   + df now use either 'df -m' or 'df -k' when it is needed
;;
;; Revision 1.4  1999/01/04 14:51:01  drieu
;; - Correct a bug so Megabytes are *REALLY* Megabytes
;;
;; Revision 1.3  1999/01/02 15:46:44  drieu
;; - Fix few bugs one more time
;; - Add variables instead of hard-coded strings
;; - Add argument for df
;; - Document the file a bit more
;;
;; Revision 1.2  1998/12/15 17:37:42  drieu
;; - Fix few bugs
;; - Add Buffer size in the mode line
;; - Mesure either in K or Mega bytes
;; - And so on...


;;; Things to do :

;; - add 'customize' support
;; - sleep a little bit


;;; Code:

;; Variables that you may want to change
(defvar df-partition "/home" "*Partition to scan")
(defvar df-command "df" "*df command in order to find how to get disk usage")
(defvar df-in-kilobytes "-k" "*Argument to use when df works in kilobytes")
(defvar df-in-megabytes "-m" "*Argument to use when df works in megabytes")
(defvar df-command-arguments df-in-kilobytes "*df-command arguments")
(defvar df-refresh 60 "*Seconds between every refresh")
(defvar df-megabytes-unit "M" "String used for displaying megabytes")
(defvar df-kilobytes-unit "K" "String used for displaying kilobytes")


;; Seemless variables for you
(defvar df-mb-threshold 10 "*When free disk space reaches this amount (in Mb), show in Mb")
(defvar df-space-left "" "Space left on device")
(defvar df-unit nil "Unit (either M or K) used for space left")
(defvar df-mode nil)
(defvar df-string "")

;; You need it because of the 'when' construct
(require 'cl)


(defun df-update ()
  "Function to update disk usage.  It is used every df-refresh seconds"
  (interactive)
  (set-variable 'df-buffer-weight (int-to-string (/ (length (buffer-string)) 1000)))
   (cond
    ((> (string-to-int df-space-left) (* df-mb-threshold 1000))
     (set-variable 'df-unit df-megabytes-unit)
     (setq df-command-arguments df-in-megabytes))
    ((and (< (string-to-int df-space-left) df-mb-threshold)(string-equal df-command-arguments df-in-megabytes))
     (set-variable 'df-unit df-kilobytes-unit)
     (setq df-command-arguments df-in-kilobytes))
    ((not df-unit)
     (set-variable 'df-unit df-kilobytes-unit)))
   (set-process-filter (start-process df-command nil df-command df-command-arguments df-partition) 'df-filter))



(defun df-filter (proc string)
  "Filter for df output.  This function is responsible from updating
the mode-line from the df process."
  (when (string-match (format "\\(-?[0-9]+\\) *[0-9%%]+ *%s" df-partition) string)
    (setq df-space-left (match-string 1 string))
    (if (> (string-to-int df-space-left) 1000)
	(set-variable 'df-unit df-megabytes-unit)
      (set-variable 'df-unit df-kilobytes-unit))
    (when (equal df-unit df-megabytes-unit) 
      (setq df-space-left (substring df-space-left 0 (- (length df-space-left) 3)))))
  (setq df-string (format " %s%s/%s%s" df-buffer-weight df-kilobytes-unit df-space-left df-unit)))



(defun df-disable ()
  "Stop all df-mode actions"
  (interactive)
  (setq df-mode nil)
  (cancel-function-timers 'df-update))



(defun df-enable ()
  "Function to display disk statistics in the mode line"
  (interactive)
  (setq df-mode t)
  (make-variable-buffer-local 'df-buffer-weight)
  (make-variable-buffer-local 'df-string)
;  (set-default 'df-string " plop")
  (run-with-timer 0 df-refresh 'df-update)
  (if (not (assq 'df-mode minor-mode-alist))
      (setq minor-mode-alist
	    (cons minor-mode-alist '((df-mode df-string)))))
  (add-hook 'find-file-hooks 'df-update)
;  (add-hook 'write-file-hooks 'df-check)
  (df-update))



;(defun df-check () 
					; ca servira plus tard a
					; demander si on est sur de
					; sauvegarder le fichier quand
					; meme
; )


(defun df-mode (&optional arg)
  "Toggle display of space left on any filesystem in mode lines.
This display updates automatically every df-refresh seconds.

With a numeric argument, enable this display if arg is positive."
  (interactive)
  (if
      (if (null arg) (not df-mode)
	(> (prefix-numeric-value arg) 0))
      (df-enable)
    (df-disable)))



;;;###autoload
(defun df (&optional partition)
  "Enables display of space left on any filesystem in mode lines.
This display updates automatically every df-refresh seconds."
  (interactive)
  (when partition
    (set-variable 'df-partition partition))
  (df-mode 1))

;;; df.el ends here
