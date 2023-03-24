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
    (let (desktop-object)
      (dolist (name fieldnames (apply #'append desktop-object))
        (beginning-of-buffer)
        (when (re-search-forward (format "^\\(%s\\)=\\(.+\\)" name) nil t)
          (push (list
                 (intern (format ":%s" (downcase (match-string 1))))
                 (match-string 2))
                desktop-object))))))

;;;###autoload
(defun bci-parse-all-desktop-files ()
  "Proof of concept function that reads the filesystem for
desktop files, analyzing each corresponding program's set of categories.

The idea is to eventually make a tool that generates a Fluxbox menu."
  (let (main-list)
    (dolist (desktop-file (directory-files "/usr/share/applications" t "^.+\.desktop$") main-list)
      (with-temp-buffer
        (insert-file-contents desktop-file)
        (let* ((lines (string-lines (string-chop-newline (buffer-string))))
               (categories-line
                (seq-find
                 (lambda (line) (string-match "^Categories" line))
                 lines)))
          (when categories-line
            (let* ((raw-categories (progn (string-match "^Categories=\\(.+\\)$" categories-line) (match-string 1 categories-line)))
                   (categories-tokens (butlast (split-string raw-categories ";"))))
              (push (cons desktop-file (list categories-tokens)) main-list))))
        ))))

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
