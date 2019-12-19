(defun mh/components()
    (let ((lines '()))
    (while (search-forward "#[derive(Component)]" nil t)
      (skip-chars-forward "\n")
      (let (
           (actual-line (thing-at-point 'line))
           )
        (save-match-data
          (and (string-match "struct \\([[:alnum:]]+\\)" actual-line)
          (add-to-list 'lines (match-string 1 actual-line) t))))
      )
    lines))

(defun mh/get-components()
  "Searches for lines #[derive(Component)] and extracts struct identifier.

   #[derive(Component)]
   struct Identifier {
   ...
   }
   and adds Identifier to returned list 
  "
  (interactive)
   (save-excursion
     (mapcar 'message (mh/components))
     ))
