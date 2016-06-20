# setenv "exec-path" (concat "/usr/local/bin" exec-path)
# setq exec-path (append exec-path '("/usr/local/bin"))

setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")) > /dev/null
setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env) > /dev/null
setenv GITHUB_API_TOKEN (eshell/cat "~/.config/github") > /dev/null

. ~/.config/vars.sh
