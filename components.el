(defun mh/components()
    (let ((lines '()))
    (while (re-search-forward "#\\[derive(.*Component.*)]" nil t)
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


(ert-deftest should-match-string ()
  (should (not (equal (string-match "#\\[foo]" "#[foo]") nil)
               ))
  (should (not (equal (string-match "#\\[(foo)]" "#[(foo)]") nil)
               ))
  (should (not (equal (string-match "#\\[derive(Component)]" "#[derive(Component)]") nil)
               ))
  (should (not (equal (string-match "#\\[derive(.*Component.*)]" "#[derive(Component, Debug, Serialize, Deserialize, Clone)]") nil)))
  (should (not (equal (string-match "#\\[derive(.*Component.*)]" "#[derive(Debug, Serialize, Component, Deserialize, Clone)]") nil)))
  )

(ert-deftest extract-between-curly-brackets ()
  (with-temp-buffer
    (insert-file-contents "foo.txt")
    (goto-char 1)
    (should (equal (mh/between-curly-brackets) ("foo")))
    )
  )

(defun mh/ert()
  "Runs eval-buffer and ert"
  (interactive)
  (save-excursion
    (eval-buffer)
    (ert t)
    )
  )

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1))))
  )
