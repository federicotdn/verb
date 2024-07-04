;; checkdoc-batch.el --- Batch mode for checkdoc  -*- lexical-binding: t; -*-

(defun checkdoc-batch-and-exit ()
  ;; Skip in Emacs 26 and older
  (when (< 26 emacs-major-version)
    (checkdoc-batch-and-exit-1)))

(defun checkdoc-batch-and-exit-1 ()
  "Run checkdoc in batch mode for a set of files.
Exit with 1 if one or more warnings were emitted, otherwise with 0."
  (unless noninteractive
    (error "`checkdoc-batch-and-exit' is to be used only with -batch"))
  (let ((status 0)
        path)
    (while command-line-args-left
      (setq path (car command-line-args-left))
      (with-current-buffer (find-file-noselect path)
        (checkdoc-current-buffer t))
      (with-current-buffer "*Style Warnings*"
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((rule "[ \t\n\r\f]+")
                 (line (string-trim (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))
                                    rule rule)))
            (when (and (not (string= line ""))
                       (not (string-prefix-p "*" line)))
              (princ (concat line "\n"))
              (setq status 1)))
          (ignore-errors (next-line))))
      (setq command-line-args-left (cdr command-line-args-left)))
    (kill-emacs status)))
