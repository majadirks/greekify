;; greekify.el -- run "greekify" on the contents of the current file
;; and update the file with the results
;; Version 0.3 (March 2013)
;; Send bugs, errors, or other comments to
;; Matthew Dirks (majadirks@gmail.com)

;;greekify: run greekify on the whole file
(defun greekify()
  "run germanify on current file and revert the buffer"
  (interactive) ;;I don't know what this does.
  (setq myname (buffer-file-name))
  (save-buffer) ;;Save, lest greekify delete unsaved changes
  (write-file (concat myname ".bkp")) ;;Make backup, just in case
  (shell-command
   (format "greekify \"`cat %s`\" \> %s"
	   (shell-quote-argument myname)
	   (shell-quote-argument myname)))
  (switch-to-buffer (find-file-noselect myname)) ;;switch to non-backup version
  (revert-buffer t t)) ;;update buffer without asking for confirmation


;;greekify-region: run greekify on selected region
(defun greekify-region()
  "transliterate current region"
  (interactive)
  ;;Save selected region as string
  (setq to-trans (buffer-substring (region-beginning) (region-end)))
  ;;Kill selected region
  (delete-region (region-beginning) (region-end))
  ;;Replace with transliteration
  (insert
   (shell-command-to-string
    (format "greekify -o '%s'"
	    to-trans))))
