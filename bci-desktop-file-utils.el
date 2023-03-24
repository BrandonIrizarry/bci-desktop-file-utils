; -*- lexical-binding: t -*-

;;;###autoload
(defun --bci-create-desktop-object (filename &rest fieldnames)
  "Return a \"desktop object\", a plist based on the given
  key-value pairs implied by FIELDNAMES, e.g.:

(:exec \"firefox %u\"
 :categories \"Network;WebBrowser;\")
"
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((key-value-pairs (list :name filename)))
      (dolist (name fieldnames key-value-pairs)
        (beginning-of-buffer)
        (when (re-search-forward (format "^\\(%s\\)=\\(.+\\)" name) nil t)
          ;; Push a desktop object to DESKTOP-OBJECTS
          (let* ((key (intern (format ":%s" (downcase (match-string 1))))))
            ;; Modify KEY-VALUE-PAIRS in place
            ;; See documentation for NCONC in Elisp manual
            (nconc key-value-pairs (list key (match-string 2)))))))))

;;;###autoload
(defun bci-collect-all-desktop-objects ()
  "Proof of concept function that reads the filesystem for
desktop files, analyzing each corresponding program's set of categories.

The idea is to eventually make a tool that generates a Fluxbox menu."
  (let (main-list)
    (dolist (desktop-file (directory-files "/usr/share/applications" t "^.+\.desktop$") main-list)
      (push (--bci-create-desktop-object desktop-file "Exec" "Categories") main-list))))


;; https://specifications.freedesktop.org/menu-spec/menu-spec-1.0.html
;; https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html

(defvar *xdg-main-categories* '("AudioVideo"
                                "Audio"
                                "Video"
                                "Development"
                                "Education"
                                "Game"
                                "Graphics"
                                "Network"
                                "Office"
                                "Settings"
                                "System"
                                "Utility")
  "A list of main menu categories for the Freedesktop menu spec.")

;; Should be updated every time a menu is generated
(setq *all-desktop-files* (bci-parse-all-desktop-files))

(defun bci-find-all-applications-of-type (query)
  (seq-filter (pcase-lambda (`(,_ ,categories))
                (seq-find (lambda (c) (string-match query c)) categories))
              *all-desktop-files*))

(mapcar (lambda (type)
          (let* ((apps (bci-find-all-applications-of-type type))
                 (names (mapcar #'car apps)))
            (cons type (list names))))
        *xdg-main-categories*)

(provide 'bci-desktop-file-utils)
