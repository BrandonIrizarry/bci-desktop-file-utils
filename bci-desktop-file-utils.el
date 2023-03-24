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
    (let ((key-value-pairs (list :filename filename)))
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

;;;###autoload
(defun bci-desktop-object-category-p (desktop-object category)
  "Return CATEGORY iff DESKTOP-OBJECT contains CATEGORY among its category values.

Else return NIL."
  ;; Some applications don't have a "Categories" field, so use WHEN-LET
  (when-let ((categories (plist-get desktop-object :categories)))
    (with-temp-buffer
      (save-excursion
        (insert categories)
        (let ((regexp (format "\\(^\\|;\\)\\(%s\\);" category)))
          (when (string-match regexp categories)
            (match-string 2 categories)))))))

(bci-desktop-object-category-p (--bci-create-desktop-object "/usr/share/applications/firefox.desktop" "Categories" "Exec" "Name") "Network")

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
(setq *all-desktop-files* (bci-collect-all-desktop-objects))

(defun bci-find-all-applications-of-type (query)
  (seq-filter (lambda (desktop-object)
                (bci-desktop-object-category-p desktop-object query))
              *all-desktop-files*))

(defun bci-build-desktop-database ()
  (mapcar (lambda (category)
            (cons category
                  (list (bci-find-all-applications-of-type category))))
          *xdg-main-categories*))

(setq *desktop-database* (bci-build-desktop-database))

;; See how many of each category I have
(mapcar (lambda (thing)
          (length (nth 1 thing)))
        *desktop-database*)

;; Fetch the Development-category members treating the database as an alist
(alist-get "Development" *desktop-database* nil nil #'string=)

(provide 'bci-desktop-file-utils)
