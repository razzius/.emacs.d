setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env) > /dev/null
setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")) > /dev/null
setenv GITHUB_API_TOKEN (eshell/cat "~/.config/github")

; abbr
;    ("g" "git" nil 0)
