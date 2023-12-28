;;; ham-qso-logger.el --- Log QSOs in ADIF format  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Daniel Gonzalez Plaza

;; Author: Daniel Gonzalez Plaza <kk6gqj@danielgplaza.com>
;; Version: 1.0.0
;; Keywords: radio, adif, files, qso, logger, log
;; URL: https://github.com/Fackelmann/ham-qso-logger

;;; Commentary:

;; This package provides an interactive logger for ham radio contacts, following the ADIF format.

;;TODO:
;; - Add field validation

;;Wanted features:
;; - Edit existing QSO
;; - Show and search for existing QSOs
;; - Personalize fields (maybe)

;;; Customization

(defcustom qso-logfile-path "~/qso-log.adif"
  "Path to the QSO logfile."
  :type 'string
  :group 'qso-logger)

(defcustom qso-logfile-operator "YOURCALL"
  "Operator callsign to use in the QSOs."
  :type 'string
  :group 'qso-logger)

;;; Code:

(require 'widget)
(require 'wid-edit)

(eval-when-compile
  (require 'wid-edit))

(cl-defstruct (adif-item (:constructor adif-item--create))
  widget field)


(defun ham-qso-logger--adif-utc-button-create (item)
  "Create a button that, when clicked, populates the adif-item ITEM with a current utc timestamp"
  (widget-create 'push-button
		 :notify (lambda (&rest _ignore)
			   (widget-value-set (adif-item-widget item)
					     (format-time-string "%H%M%S" (current-time) t))
			   (widget-setup))
		 "Now"))

(defun ham-qso-logger--adif-widget-create (text size)
  "Create a single editable field widget with the given TEXT and SIZE."
  (widget-create 'editable-field
		 :size size
		 :format (concat text ": %v ")))

(defun ham-qso-logger--adif-item-create (text size field)
  "Create a widget with given parameters.
Then encapsulate it in an adif-item struct.
   
TEXT is the label for the field.
SIZE is the width of the field in characters.
FIELD is the associated attribute in the logfile for the field."
  (adif-item--create :widget
		     (widget-create 'editable-field
				    :size size
				    :format (concat text ": %v "))
		     :field field))

(defun ham-qso-logger--write-adif-header (logfile-path)
  "Write ADIF header to the log file specified by LOGFILE-PATH."
  (let ((adif-version "3.1.4") ; Specify the ADIF version being used
        (program-id "HAM-QSO-LOGGER") ; Your program's ID
        (program-version "1.0") ; Your program's version
        (current-time (format-time-string "%Y%m%d %H%M%S" (current-time) t))) ; Current UTC time
    (with-temp-buffer
      (insert (format "<ADIF_VER:%d>%s" (length adif-version) adif-version))
      (insert (format "<PROGRAMID:%d>%s" (length program-id) program-id))
      (insert (format "<PROGRAMVERSION:%d>%s" (length program-version) program-version))
      (insert (format "<CREATED_TIMESTAMP:%d>%s" (length current-time) current-time))
      (insert "<EOH>\n") ; End Of Header marker
      (write-region (point-min) (point-max) logfile-path))
    (message "ADIF header written to %s" logfile-path)))

(defun ham-qso-logger-write-qso (qso-data logfile-path)
  "Write QSO information to file.
QSO-DATA is an alist of (field . value) pairs.
LOGFILE-PATH is the log file path where the QSO should be appended."
  (when (or (not (file-exists-p logfile-path))
	    (zerop (nth 7 (file-attributes logfile-path))))
    (ham-qso-logger--write-adif-header logfile-path))
  (with-temp-buffer
    ;; Insert the OPERATOR field if it's non-zero length.
    (when (> (length qso-logfile-operator) 0)
      (insert (format "<OPERATOR:%d>%s" (length qso-logfile-operator) qso-logfile-operator)))
    ;; Insert other QSO data fields with non-zero length.
    (dolist (item qso-data)
      (let ((field (car item))
            (field-value (cdr item)))
	;; Only record the field if it has a non-zero length.
	(when (> (length field-value) 0)
          (insert (format "<%s:%d>%s" field (length field-value) field-value)))))
    (insert "<EOR>\n")
    (write-region (point-min) (point-max) logfile-path 'append))
  (message "QSO recorded.")
  (sit-for 2) ;; Waits to show message
  ;; Reload QSO widget for next entry
  (ham-qso-logger--setup-qso-widget logfile-path))

(defun main-widget ()
  "Create the main user interface widget.  UNUSED."
  (interactive)
  (switch-to-buffer "*QSO logger*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "What do you want to do? \n\n")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                           (ham-qso-logger--setup-qso-widget qso-logfile-path))
                 "Add QSO")
  (use-local-map widget-keymap)
  (widget-setup)
  (widget-forward 1))

(defun ham-qso-logger--setup-qso-widget (logfile-path)
  "Set up the QSO entry form, clearing all fields.
This prepares the interface for a new QSO entry which is written to LOGFILE-PATH"
  (interactive)
  (switch-to-buffer "*Add QSO*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (let ((adif-item-list '()))
    (push (ham-qso-logger--adif-item-create "Callsign" 10 "CALL") adif-item-list)
    (push (ham-qso-logger--adif-item-create "Freq" 8 "FREQ") adif-item-list)
    (push (ham-qso-logger--adif-item-create "Mode" 10 "MODE") adif-item-list)
    (widget-insert "\n")
    (push (ham-qso-logger--adif-item-create "Start time" 6 "TIME_ON") adif-item-list)
    (ham-qso-logger--adif-utc-button-create (first adif-item-list))
    (widget-insert "\n")
    (push (ham-qso-logger--adif-item-create "Stop Time" 6 "TIME_OFF") adif-item-list)
    (ham-qso-logger--adif-utc-button-create (first adif-item-list))
    (widget-insert "\n")
    (push (ham-qso-logger--adif-item-create "QTH" 20 "QTH") adif-item-list)
    (push (ham-qso-logger--adif-item-create "State" 2 "STATE") adif-item-list)
    (push (ham-qso-logger--adif-item-create "Country" 20 "COUNTRY") adif-item-list)
    (widget-insert "\n-----\n")
    (push (ham-qso-logger--adif-item-create "Notes" 100 "NOTES") adif-item-list)
    (widget-insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest _ignore)
			     (let (qso-data)
                               (dolist (item adif-item-list)
				 (let ((field (adif-item-field item))
                                       (value (widget-value (adif-item-widget item))))
				   (push (cons field value) qso-data)))
                               (ham-qso-logger-write-qso (nreverse qso-data) logfile-path)))
		   "Record QSO"))
  (use-local-map widget-keymap)
  (widget-setup)
  (widget-forward 1))

(ham-qso-logger--setup-qso-widget qso-logfile-path)

(provide 'ham-qso-logger)
;;; ham-qso-logger.el ends here
