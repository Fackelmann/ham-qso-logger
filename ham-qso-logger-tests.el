(require 'ert)
(require 'ham-qso-logger)

(ert-deftest ham-qso-logger-test-write-qso ()
  "Test that `ham-qso-logger--write-qso` writes correctly formatted ADIF data to a file."
  ;; Create a temporary file for testing
  (let ((temp-logfile (make-temp-file "qso-log")))
    (unwind-protect
        (let ((test-data '(("CALL" . "TEST1")
                           ("FREQ" . "14.070")
                           ("MODE" . "FT8")
                           ("COUNTRY" . "Neverland"))))
          ;; Call the function with the test data
          (ham-qso-logger-write-qso test-data temp-logfile)

          ;; Verify the content of the logfile
          (with-temp-buffer
            (insert-file-contents temp-logfile)
            (goto-char (point-min))
            ;; Assuming header is already written (if necessary, write it as part of test setup)
            (search-forward "<EOH>\n")
            (let ((qso-string (buffer-substring-no-properties (point) (point-max))))
              ;; Check if each field from `test-data` is correctly written in ADIF format
              (dolist (pair test-data)
                (let ((field (car pair))
                      (value (cdr pair)))
                  (should (string-match-p (format "<%s:%d>%s" field (length value) value) qso-string)))))))

      ;; Cleanup: Delete the temporary logfile
      (delete-file temp-logfile))))
