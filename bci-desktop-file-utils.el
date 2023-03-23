; -*- lexical-binding: t -*-

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

(provide 'bci-desktop-file-utils)
