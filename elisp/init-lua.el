;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :hook
  (lua-mode . (lambda ()
                (aggressive-indent-mode -1)
                (make-variable-buffer-local 'completion-at-point-functions)
                (add-to-list 'completion-at-point-functions 'pnh-lua-complete)
                ))
  :interpreter ("lua" . lua-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  (defun pnh-lua-complete ()
    (let* ((boe (save-excursion (search-backward-regexp "[^\.a-zA-Z0-9_]")
                                (point)))
           (bot (save-excursion (when (symbol-at-point)
                                  (backward-word)) (point)))
           (expr (buffer-substring-no-properties (1+ boe) (point)))
           (file (make-temp-file "lua-completions-")))
      (lua-send-string (pnh-lua-completion-string-for expr file))
      (sit-for 0.1)
      (list bot (point) (when (file-exists-p file)
                          (with-temp-buffer
                            (insert-file-contents file)
                            (delete-file file)
                            (butlast (split-string (buffer-string) "\n")))))))
  ;; copy from https://immerrr.github.io/lua-mode/
  ;;(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (defun pnh-lua-completion-string-for (expr file)
    (mapconcat 'identity
               `("do"
                 "local clone = function(t)"
                 "  local n = {} for k,v in pairs(t) do n[k] = v end return n"
                 "end"
                 "local function cpl_for(input_parts, ctx, prefixes)"
                 "  if #input_parts == 0 and ctx ~= _G then"
                 "    return ctx"
                 "  elseif #input_parts == 1 then"
                 "    local matches = {}"
                 "    for k in pairs(ctx) do"
                 "      if k:find('^' .. input_parts[1]) then"
                 "        local parts = clone(prefixes)"
                 "        table.insert(parts, k)"
                 "        table.insert(matches, table.concat(parts, '.'))"
                 "      end"
                 "    end"
                 "    return matches"
                 "  else"
                 "    local token1 = table.remove(input_parts, 1)"
                 "    table.insert(prefixes, first_part)"
                 "    return cpl_for(input_parts, ctx[token1], prefixes)"
                 "  end"
                 "end"
                 "local i = {" ,@(mapcar (apply-partially 'format "'%s',")
                                         (split-string expr "\\.")) "}"
                 ,(format "local f = io.open('%s', 'w')" file)
                 ;; TODO: using _G here is pretty lame! try to get local context
                 "for _,l in ipairs(cpl_for(i, _G, {})) do"
                 "  f:write(l .. string.char(10))"
                 "end"
                 "f:close()"
                 "end") "\n"))
  )


(provide 'init-lua)
;;; init-lua ends here
