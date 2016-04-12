;;; razzi/interactive --- Summary
; my interactive functions

(defun razzi/insert-newline-after ()
  (interactive)
  (save-excursion
    (forward-line)
    (move-beginning-of-line 1)
    (newline)
  )
)

(defun razzi/insert-newline-before ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (newline)
  )
)

(provide 'razzi/interactive)
