;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

(require 'init-define)

(defconst org-gtd-home (expand-file-name "gtd" user-emacs-directory))
(defconst org-gtd-inbox-file (expand-file-name "inbox.org" org-gtd-home))
(defconst org-gtd-calender-file (expand-file-name "calender.org" org-gtd-home))
(defconst org-gtd-draft-file (expand-file-name "draft.org" org-gtd-home))
(defconst org-gtd-trash-file (expand-file-name "trash.org" org-gtd-home))
(defconst org-gtd-favorite-file (expand-file-name "favorite.org" org-gtd-home))
(defconst org-gtd-history-file (expand-file-name "history.org" org-gtd-home))

(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :preface
  (defun hot-expand (str &optional mod)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org")
    :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :hook ((org-mode . (lambda ()
                       "Beautify org symbols."
                       (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)
                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil)))
         ('org-agenda-mode-hook 'hl-line-mode))

  :config
  ;; To speed up startup, don't put to init section
  (setq org-agenda-files (setq  org-agenda-files (list org-gtd-home))
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
          (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
          (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))

        org-todo-repeat-to-state "NEXT"

        org-todo-keyword-faces '(("NEXT" . warning)
                                 ("PROJECT" . "purple"))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?) "  " nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t)

  (setq org-capture-templates
        `(("t" "todo" entry (file org-gtd-inbox-file) ; "" => `org-default-notes-file'
           "* TODO %?" :clock-resume t)
          ("i" "idea" entry (file+headline org-gtd-favorite-file "Ideas")
           "* %? :IDEA\n%T\n" :clock-resume t)
          ("l" "link" entry (file+headline org-gtd-favorite-file "Links")
           "* %? :LINK\n%T\n" :clock-resume t)
          ("n" "note" entry (file+headline org-gtd-favorite-file "Notes")
           "* %? :NOTE\n%T\n" :clock-resume t)
          ))

  (setq org-refile-targets '(("~/.emacs.d/gtd/calender.org" :level . 1)
                             ("~/.emacs.d/gtd/draft.org" :maxlevel . 1)
                             ("~/.emacs.d/gtd/history.org" :level . 1)
                             ("~/.emacs.d/gtd/trash.org" :maxlevel . 3)))

  ;;; Refiling
  (defun semgilo/org-refile (file headline &optional arg)
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (org-refile nil nil (list headline file nil pos))))

  (defun semgilo/refile-to-calender-actions ()
    "Move headline to calender actions"
    (interactive)
    (semgilo/org-refile org-gtd-calender-file "Actions")
    (kill-buffer "*Org Agenda(g)*")
    (org-agenda () "g")
    )

  (defun semgilo/refile-to-calender-projects ()
    "Move headline to calender projects"
    (interactive)
    (semgilo/org-refile org-gtd-calender-file "Projects")
    (kill-buffer "*Org Agenda(g)*")
    (org-agenda () "g")
    )

  (defun semgilo/refile-to-history-actions ()
    "Move headline to history actions"
    (interactive)
    (semgilo/org-refile org-gtd-history-file "Actions")
    (kill-buffer "*Org Agenda(g)*")
    (org-agenda () "g")
    )

  (defun semgilo/refile-to-history-projects ()
    "Move headline to history projects"
    (interactive)
    (semgilo/org-refile org-gtd-history-file "Projects")
    (kill-buffer "*Org Agenda(g)*")
    (org-agenda () "g")
    )


  (defun semgilo/handle-outline-state-to-next ()
    "When todo keyword from todo to PROJECT/NEXT, refile outline to calender."
    (when (string= org-state "NEXT")
      (semgilo/refile-to-calender-actions))
    (when (string= org-state "PROJECT")
      (semgilo/refile-to-calender-projects))
    (when (or (string= org-state "DONE") (string= org-state "CANCEL"))
      (semgilo/refile-to-history-actions))
    )

  (setq org-refile-use-cache nil)

  (add-hook 'org-after-todo-state-change-hook 'semgilo/handle-outline-state-to-next)

  ;; (after-load 'org-agenda
  ;;             (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  ;; Exclude DONE state tasks from refile targets
  (defun sanityinc/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

  (defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
    "A version of `org-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-refile goto default-buffer rfloc msg)))

  (defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
    "A version of `org-agenda-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-agenda-refile goto rfloc no-update)))

  ;; Targets start with the file name - allows creating level 1 tasks
  ;;(setq org-refile-use-outline-path (quote file))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (let ((active-project-match "-INBOX/PROJECT"))

    (setq org-stuck-projects
          `(,active-project-match ("NEXT")))


    (setq org-agenda-compact-blocks t
          org-agenda-sticky t
          org-agenda-start-on-weekday nil
          org-agenda-span 'day
          org-agenda-include-diary nil
          org-agenda-filter-preset '("-FAVORITE")
          org-agenda-sorting-strategy
          '((agenda habit-down time-up user-defined-up effort-up category-keep)
            (todo category-up effort-up)
            (tags category-up effort-up)
            (search category-up))
          org-agenda-window-setup 'current-window
          org-agenda-custom-commands
          `(("N" "Notes" tags "NOTE"
             ((org-agenda-overriding-header "Notes")
              (org-tags-match-list-sublevels t)))
            ("g" "GTD"
             ((agenda "" nil)
              (tags "INBOX"
                    ((org-agenda-overriding-header "Inbox")
                     (org-tags-match-list-sublevels nil)))
              (tags-todo "CALENDER"
                         ((org-agenda-overriding-header "Calender")
                          (org-tags-match-list-sublevels nil)))
              (tags-todo "DRAFT"
                         ((org-agenda-overriding-header "Drafts")
                          (org-tags-match-list-sublevels nil)))
              (tags-todo "TRASH"
                         ((org-agenda-overriding-header "Trash")
                          (org-tags-match-list-sublevels nil)))
              (tags "HISTORY"
                    ((org-agenda-overriding-header "History")
                     (org-tags-match-list-sublevels t)
                     (org-agenda-skip-function
                      '(lambda ()
                         (or (org-agenda-skip-entry-if 'regexp' "Projects")
                             (org-agenda-skip-entry-if 'regexp' "Actions"))))

                     ))

              )))))

  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (xwidget-webkit-browse-url (concat "file://" file))
              (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
                (when (buffer-live-p buf)
                  (and (eq buf (current-buffer)) (quit-window))
                  (pop-to-buffer buf)))))
          org-file-apps))

  ;; Add gfm/md backends
  (use-package ox-gfm)
  (add-to-list 'org-export-backends 'md)

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; Prettify UI
  (use-package org-bullets
    :if (char-displayable-p ?⚫)
    :hook (org-mode . org-bullets-mode)
    :init (setq org-bullets-bullet-list '("⚫" "⚫" "⚫" "⚫")))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (char-displayable-p ?⯀)
                    '("⯀" "⯀" "⯀" "⯀")
                  '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (use-package ob-mermaid
    :init (cl-pushnew '(mermaid . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
           ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  (use-package org-mime
    :bind (:map message-mode-map
           ("C-c M-o" . org-mime-htmlize)
           :map org-mode-map
           ("C-c M-o" . org-mime-org-buffer-htmlize)))

  ;; Preview
  (use-package org-preview-html
    :diminish)

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
           ("C-<f7>" . org-tree-slide-mode)
           :map org-tree-slide-mode-map
           ("<left>" . org-tree-slide-move-previous-tree)
           ("<right>" . org-tree-slide-move-next-tree)
           ("S-SPC" . org-tree-slide-move-previous-tree)
           ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))

  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-agenda-mode-map
           ("P" . org-pomodoro))))

;; org-roam
(when (and emacs/>=26p (executable-find "cc"))
  (use-package org-roam
    :diminish
    :custom (org-roam-directory centaur-org-directory)
    :hook (after-init . org-roam-mode)
    :bind (:map org-roam-mode-map
           (("C-c n l" . org-roam)
            ("C-c n f" . org-roam-find-file)
            ("C-c n g" . org-roam-graph))
           :map org-mode-map
           (("C-c n i" . org-roam-insert)))))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
