(provide 'my-theme-toggle)
(setq dark 'doom-peacock)
(setq light 'doom-flatwhite)


(defun haize/theme-toggle ()
  "Toggle between light and dark themes"
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (let((new-theme (if (eq  current-theme dark)
			light
		        dark)))
	(disable-theme current-theme )
	(load-theme new-theme t)
	(message "%S  %S" current-theme new-theme))
    
    nil))





