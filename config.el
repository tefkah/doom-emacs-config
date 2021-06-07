(add-to-list 'load-path "~/.config/doom/elisp")

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Thomas F. K. Jorna"
      user-mail-address "jorna@jtrialerror.com")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/OneDrive/org-roam/"
      org-roam-directory "~/OneDrive/org-roam")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;  .

(setq default-frame-alist
      (append (list
	       ;; '(font . "Roboto Mono Emacs Regular:size=14")
	       '(min-height . 1)  '(height     . 45)
	       '(min-width  . 1) '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 40)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))



;  (setq centaur-tabs-style "wave")
;  (setq centaur-tabs-set-bar 'under)
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
;(setq x-underline-at-descent-line t)


;;
;;; Packages

(setq doom-theme 'doom-plain)

(add-hook! 'solaire-mode-hook
  ;(set-face-attribute 'solaire-fringe-face nil :background (face-background 'solaire-hl-line-face))
  (set-face-attribute 'fringe nil :background (face-background 'solaire-default-face))
  )

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-face 'variable-pitch))

(setq doom-font (font-spec :family "Fira Code" :size 15 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "Noto Serif" :style "Regular" :size 18 :weight 'regular))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You an either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info vcs word-count)
    '(buffer-position misc-info major-mode "                  "))
  (custom-set-faces! '(mode-line :family "Fira Code"))
  (doom-modeline--set-char-widths doom-modeline-rhs-icons-alist))

(use-package! dashboard
  :init
  (dashboard-setup-startup-hook)
  ;(setq initial-buffer-choice
  ;      (lambda () (get-buffer "*dashboard*")))
  :config
  (setq dashboard-center-content t
        dashboard-banner-logo-title "Emacs"
        dashboard-startup-banner 'logo
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-set-init-info t
        dashboard-week-agenda t
        ))

(setq blink-cursor-alist '((box . box)))
(setq blinking-cursor-mode 1)

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  )

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package! org-transclusion
  :after org-roam
  )

(map! :map org-mode-map
:nie "C-M-SPC" (cmd! (insert "\u200B")))

(alert "AAA" :style 'message)

        (defvar +org-roam-open-buffer-on-find-file nil
  "If non-nil, open the org-roam buffer when opening an org roam file.")
(use-package! org-roam
  ;;:hook (org-load . org-roam-mode)
  :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-dailies-find-date
             org-roam-dailies-find-today
             org-roam-dailies-find-tomorrow
             org-roam-dailies-find-yesterday)
  :preface
  ;; Set this to nil so we can later detect if the user has set custom values
  ;; for these variables. If not, default values will be set in the :config
  ;; section.
  (defvar org-roam-directory nil)
  (defvar org-roam-db-location nil)
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        :prefix ("m" . "org-roam")
        "b" #'org-roam-switch-to-buffer
        "f" #'org-roam-find-file
        "g" #'org-roam-graph
        "i" #'org-roam-insert
        "I" #'org-roam-insert-immediate
        "m" #'org-roam
        "t" #'org-roam-tag-add
        "T" #'org-roam-tag-delete
        (:prefix ("d" . "by date")
         :desc "Find previous note" "b" #'org-roam-dailies-find-previous-note
         :desc "Find date"          "d" #'org-roam-dailies-find-date
         :desc "Find next note"     "f" #'org-roam-dailies-find-next-note
         :desc "Find tomorrow"      "m" #'org-roam-dailies-find-tomorrow
         :desc "Capture today"      "n" #'org-roam-dailies-capture-today
         :desc "Find today"         "t" #'org-roam-dailies-find-today
         :desc "Capture Date"       "v" #'org-roam-dailies-capture-date
         :desc "Find yesterday"     "y" #'org-roam-dailies-find-yesterday
         :desc "Find directory"     "." #'org-roam-dailies-find-directory))
  :config
  (setq org-roam-directory
        (file-name-as-directory
         (file-truename
          (expand-file-name (or org-roam-directory "roam")
                            org-directory)))
        org-roam-db-location (or org-roam-db-location
                                 (concat doom-etc-dir "org-roam.db"))
        org-roam-verbose nil   ; https://youtu.be/fn4jIlFwuLU
        ;; Make org-roam buffer sticky; i.e. don't replace it when opening a
        ;; file with an *-other-window command.
        org-roam-buffer-window-parameters '((no-delete-other-windows . t))
        org-roam-completion-everywhere t
        org-roam-completion-system
        (cond ((featurep! :completion helm) 'helm)
              ((featurep! :completion ivy) 'ivy)
              ((featurep! :completion ido) 'ido)
              ('default)))

  ;; Normally, the org-roam buffer doesn't open until you explicitly call
  ;; `org-roam'. If `+org-roam-open-buffer-on-find-file' is non-nil, the
  ;; org-roam buffer will be opened for you when you use `org-roam-find-file'
  ;; (but not `find-file', to limit the scope of this behavior).
  (add-hook! 'find-file-hook
    (defun +org-roam-open-buffer-maybe-h ()
      (and +org-roam-open-buffer-on-find-file
           (memq 'org-roam-buffer--update-maybe post-command-hook)
           (not (window-parameter nil 'window-side)) ; don't proc for popups
           (not (eq 'visible (org-roam-buffer--visibility)))
           (with-current-buffer (window-buffer)
             (org-roam-buffer--get-create)))))

  ;; Hide the mode line in the org-roam buffer, since it serves no purpose. This
  ;; makes it easier to distinguish from other org buffers.
  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode))

(after! org-roam
    (setq org-roam-capture-templates
          `(("s" "standard" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%<%Y%m%d%H%M%S>-${slug}"
     :head "#+title: ${title}\n#+roam_tags: \n\n* ${title}\n\n"
     :unnarrowed t)
        ("d" "definition" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "${slug}"
         :head "#+title: ${title}\n#+roam_tags: definition \n\n* ${title}\n\n\n* Examples\n"
         :unnarrowed t)))
  )

;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)

(use-package! org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8081
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20
        org-roam-server-network-vis-options "{\"physics\": {\"stabilization\": {\"iterations\": 100}}}"
        ;i;"{\"physics\": {\"enabled\": true, \"barnesHut\":{\"gravitationalConstant\" : -6000, \"avoidOverlap\" : 0.5, \"springLength\" : 200}, \"stabilization\": {\"enabled\": true, \"iterations\": 30}},
        ;;\"edges\": {\"physics\": true, \"hidden\": false, \"smooth\": {\"enabled\": false, \"type\": \"continuous\"}}}"
        org-roam-server-cite-edge-dashes nil
        org-roam-server-extra-cite-edge-options (list (cons 'width 3))
        ))

(defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (smartparens-global-mode -1)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))
    (smartparens-global-mode 1))

;; automatically enable server-mode
(after! org-roam
  (smartparens-global-mode -1)
  (org-roam-server-mode)
  (smartparens-global-mode 1))

(use-package! org-ref
    ;:after org-roam
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list "/home/thomas/OneDrive/org-roam/bib/Library.bib")
         org-ref-bibliography-notes "/home/thomas/OneDrive/org-roam/bibnotes.org"
         org-ref-note-title-format "* %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "/home/thomas/OneDrive/org-roam/"
         org-ref-notes-function 'orb-edit-notes
    ))

(after! org-ref
(setq
 bibtex-completion-notes-path "/home/thomas/OneDrive/org-roam/"
 bibtex-completion-bibliography "/home/thomas/OneDrive/org-roam/bib/Library.bib"
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
  )
 )
)

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
   '("citekey" "title" "url" "file" "author-or-editor" "keywords" "pdf" "doi" "author" "tags"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}
\n#+ROAM_TAGS: reference ${keywords} \n
\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${citekey}\n  :DOI: ${doi}\n  :AUTHOR: ${author}\n  :END:\n\n
\n* Summary
\n\n\n* Rough note space\n"
           :unnarrowed t))))
;)

   (use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   ;;org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   ;;org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the rclone mega
   org-noter-notes-search-path "/home/thomas/OneDrive/org-roam"
   )
  )


(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link))
(use-package! org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

        (use-package! nroam
  :after org-roam
  :config
  (add-hook 'org-roam-mode-hook  #'nroam-setup-maybe)
)

(use-package! org-ol-tree
  :after org
  :commands org-ol-tree
  :hook (org-ol-tree-mode . visual-line-mode)
  :config
  (setq org-ol-tree-ui-window-auto-resize nil
        org-ol-tree-ui-window-max-width 0.3
        org-ol-tree-ui-window-position 'left))
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree)

(defun org-mode-remove-stars ()
  (font-lock-add-keywords
   nil
   '(("^\\*+ "
      (0
       (prog1 nil
         (put-text-property (match-beginning 0) (match-end 0)
                            'invisible t)))))))

(add-hook! 'org-mode-hook #'org-mode-remove-stars)

  ;; hide title / author ... keywords

;;; Ugly org hooks
(defun nicer-org ()
  (progn
  (+org-pretty-mode 1)
  (mixed-pitch-mode 1)
  (hl-line-mode -1)
  (display-line-numbers-mode -1)
  (olivetti-mode 1)
  (org-num-mode 1)
  (org-superstar-mode -1)
  ;(org-indent-mode -1)
  ))

(add-hook! 'org-mode-hook  #'nicer-org)

(after! org
  (setq org-startup-with-latex-preview 1
        org-startup-with-inline-images 1
        ;org-hide-leading-stars 1
        org-startup-indented nil
        ;org-superstar-headline-bullets-list`("\u200b")
  ;      org-startup-folded nil
        ;org-startup-numerated 1
        org-hidden-keywords '(title author date startup roam_tags)
        org-pretty-entities 1
        org-num-max-level 3
        header-line-format nil))

(after! org
(custom-set-faces!
  '((org-block) :background nil)
  )
  (defface redd
    '((((class color) (min-colors 88) (background light))
      :foreground "red"))
    "Red."
    :group 'basic-faces)
  (custom-set-faces!
    ;'(org-document-title :height 1.6 :weight bold)
    '(org-level-1 :height 1.3 :weight extrabold :slant normal)
    '(org-level-2 :height 1.2 :weight bold :slant normal)
    '(org-level-3 :height 1.1 :weight regular :slant normal)
    ;'(org-document-info  :inherit 'nano-face-faded)
    '(org-document-title   ;:foreground ,(doom-color 'black)
                           :family "Noto Serif"
                           :height 250
                           :weight medium)))

(after! org
(setq org-emphasis-alist
        '(("*" (bold))
          ("/" italic)
          ("_" underline)
          ("=" redd)
          ("~" code)
          ("+" (:strike-through t)))))

        (after! org
(setq org-ellipsis " ▾ ")
  (appendq! +ligatures-extra-symbols
          `(:checkbox      "☐"
            :pending       "◼"
            :checkedbox    "☑"
            :list_property "∷"
            :em_dash       "—"
            :ellipses      "…"
            :arrow_right   "→"
            :arrow_left    "←"
            :title         nil
            :subtitle      "𝙩"
            :author        "𝘼"
            :date          "𝘿"
            :property      "☸"
            :options       "⌥"
            :startup       "⏻"
            :macro         "𝓜"
            :html_head     "🅷"
            :html          "🅗"
            :latex_class   "🄻"
            :latex_header  "🅻"
            :beamer_header "🅑"
            :latex         "🅛"
            :attr_latex    "🄛"
            :attr_html     "🄗"
            :attr_org      "⒪"
            :begin_quote   "❝"
            :end_quote     "❞"
            :caption       "☰"
            :header        "›"
            :results       "🠶"
            :begin_export  "⏩"
            :end_export    "⏪"
            :properties    "⚙"
            :end           "∎"
            :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
            :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
            :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
            :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
            :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)
            :roam_tags ""))
(set-ligatures! 'org-mode
  :merge t
  :checkbox      "[ ]"
  :pending       "[-]"
  :checkedbox    "[X]"
  :list_property "::"
  :em_dash       "---"
  :ellipsis      "..."
  :arrow_right   "->"
  :arrow_left    "<-"
  :title         "#+title:"
  :subtitle      "#+subtitle:"
  :author        "#+author:"
  :date          "#+date:"
  :property      "#+property:"
  :options       "#+options:"
  :startup       "#+startup:"
  :macro         "#+macro:"
  :html_head     "#+html_head:"
  :html          "#+html:"
  :latex_class   "#+latex_class:"
  :latex_header  "#+latex_header:"
  :beamer_header "#+beamer_header:"
  :latex         "#+latex:"
  :attr_latex    "#+attr_latex:"
  :attr_html     "#+attr_html:"
  :attr_org      "#+attr_org:"
  :begin_quote   "#+begin_quote"
  :end_quote     "#+end_quote"
  :caption       "#+caption:"
  :header        "#+header:"
  :begin_export  "#+begin_export"
  :end_export    "#+end_export"
  :results       "#+RESULTS:"
  :property      ":PROPERTIES:"
  :end           ":END:"
  :priority_a    "[#A]"
  :priority_b    "[#B]"
  :priority_c    "[#C]"
  :priority_d    "[#D]"
  :priority_e    "[#E]"
  :roam_tags     "#+roam_tags:")
(plist-put +ligatures-extra-symbols :name "⁍")
)

(with-eval-after-load 'org
  (plist-put org-format-latex-options :background 'default))

(use-package! org-gtd
  :after org
  :config
  ;; where org-gtd will put its files. This value is also the default one.
  (setq org-gtd-directory "~/OneDrive/org-roam/")
  ;; package: https://github.com/Malabarba/org-agenda-property
  ;; this is so you can see who an item was delegated to in the agenda
  (setq org-agenda-property-list '("DELEGATED_TO"))
  ;; I think this makes the agenda easier to read
  (setq org-agenda-property-position 'next-line)
  ;; package: https://www.nongnu.org/org-edna-el/
  ;; org-edna is used to make sure that when a project task gets DONE,
  ;; the next TODO is automatically changed to NEXT.
  (setq org-edna-use-inheritance t)
  (org-edna-load)
  :bind
  (("C-c d c" . org-gtd-capture) ;; add item to inbox
  ("C-c d a" . org-agenda-list) ;; see what's on your plate today
  ("C-c d p" . org-gtd-process-inbox) ;; process entire inbox
  ("C-c d n" . org-gtd-show-all-next) ;; see all NEXT items
  ("C-c d s" . org-gtd-show-stuck-projects)) ;; see projects that don't have a NEXT item
  :init
  (bind-key "C-c c" 'org-gtd-clarify-finalize)) ;; the keybinding to hit when you're done editing an item in the processing phase

(setq org-agenda-files '("~/OneDrive/org-roam/inbox" "~/OneDrive/org-roam/actionable.org"
                         "~/OneDrive/org-roam/agenda.org" "~/OneDrive/org-roam/incubate.org"
                         "~/OneDrive/org-roam/openquestions.org"))

(after! org
(setq org-capture-templates `(("i" "Inbox"
                                 entry (file "~/OneDrive/org-roam/inbox.org")
                                 "* %?\n%U\n\n  %i"
                                 :kill-buffer t)
                                ("l" "Todo with link"
                                 entry (file "~/OneDrive/org-rom/inbox.org")
                                 "* %?\n%U\n\n  %i\n  %a"
                                 :kill-buffer t)
                                ("m" "Meeting"
                                 entry (file+headline "/home/thomas/OneDrive/org-roam/agenda.org" "Future")
                                ,(concat "* TODO %? :meeting:\n" "<%<%Y-%m-%d %a %H:00>>"))
                                ("o" "Open Question Thesis"
                                 entry (file+headline "~/OneDrive/org-roam/openquestions.org" "Questions")
                                 "* OPEN %? \n %U\n")))
(set-face-attribute 'org-headline-done nil :strike-through t)
)

(use-package! org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
)


  (setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t
      org-agenda-start-day nil)
(setq org-agenda-custom-commands
      '(("d" "Get Things DONE"
         ((agenda "" ((org-agenda-span 1)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date nil
                                :todo "TODAY"
                                :scheduled nil
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:discard (:todo "TODO"))
                          (:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 1)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "Thesis"
                                 :tag "thesis"
                                 :order 10)
                          (:name "ESN"
                                 :tag "esn"
                                 :order 12)
                          (:name "JOTE"
                                 :tag "jote"
                                 :order 13)
                          (:name "Emacs"
                                 :tag "emacs"
                                 :order 14)
                          (:name "Home"
                                 :tag "home"
                                 :order 30)
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 20)
                          (:name "Notes"
                                 :tag "notes"
                                 :order 20)
                          ;(:name "Open Questions"
                          ;       :todo "OPEN"
                          ;       :order 3)
                          (:name "trivial"
                                 :priority<= "C"
                                 :tag ("Trivial" "Unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

(setq default-frame-alist
      (append (list
	       ;; '(font . "Roboto Mono Emacs Regular:size=14")
	       '(min-height . 1)  '(height     . 45)
	       '(min-width  . 1) '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 40)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(add-hook! 'solaire-mode-hook (set-face-background 'internal-border (face-background 'fringe)))

(set-frame-parameter nil 'internal-border-width 60)

(defvar writing-header--default-format header-line-format
  "Storage for the default `mode-line-format'.
So it can be restored when 'writer-header-line-mode' is disabled.")

(defvar writing-modeline--default-format mode-line-format)

(define-minor-mode writing-header-line-mode
  "Adds a bar with the same color as the fringe as the header-line.
Imitates the look of wordprocessors a bit."
  :init-value nil
  :global nil
  (if writing-header-line-mode
      (progn
      (setq header-line-format
            (concat
             (propertize " " 'display (list 'space :width 'left-fringe) 'face 'fringe)
             (propertize " " 'display (list 'space :width 'left-margin) 'face (list (list :height 400) 'default))
             (propertize " " 'display (list 'space :width 'text) 'face (list (list :height 400) 'default))
             ;(propertize (format " %dW" (count-words (point-min) (point-max))) 'face 'default)
             (propertize " " 'display (list 'space :width 'left-margin) 'face (list (list :height 400) 'default))
    ;;(propertize (format " %dW" (count-words (point-min) (point-max))) 'face 'fringe)
   ;; '("" mode-line-misc-info)
             (propertize " " 'display (list 'space :width 'left-fringe) 'face 'fringe))) ;
        (setq mode-line-format header-line-format))
    (setq header-line-format writing-header--default-format
          mode-line-format writing-modeline--default-format)))

(use-package! olivetti
  :after org-roam
  :hook (olivetti-mode . writing-header-line-mode)
  :config
    (setq olivetti-min-body-width 50
          olivetti-body-width 80
          olivetti-style t ; fantastic new layout
          olivetti-margin-width 12)
    (add-hook! 'olivetti-mode-hook (window-divider-mode -1)))

(use-package! focus
  :after org-roam
  :config
        (add-to-list 'focus-mode-to-thing '(org-mode . paragraph))
  )
;(require 'nano-writer)

;;;;;


;;
;;    Custom Minor Modes
;;
;;;;;

(define-minor-mode prot/scroll-center-cursor-mode
  "Toggle centred cursor scrolling behavior"
  :init-value nil
  :lighter " S="
  :global nil
  (if prot/scroll-center-cursor-mode
      (setq-local scroll-margin (* (frame-height) 2)
                  scroll-conservatively 0
                  maximum-scroll-margin 0.5)
    (dolist (local '(scroll-preserve-screen-position
                     scroll-conservatively
                     maximum-scroll-margin
                     scroll-margin))
      (kill-local-variable `,local)))
  )

(define-minor-mode prot/variable-pitch-mode
  "Toggle 'mixed-pitch-modei, except for programming modes"
  :init-value nil
  :global nil
  (if prot/variable-pitch-mode
      (unless (derived-mode-p 'prog-mode)
        (variable-pitch-mode 1))
    (variable-pitch-mode -1)))

(define-minor-mode prot/display-line-number-mode
  "Disable line numbers, except for programming modes."
  :init-value nil
  :global nil
  (if prot/display-line-number-mode
      (unless (derived-mode-p 'prog-mode)
        (display-line-numbers-mode -1))
    (display-line-numbers-mode 1)))

                ;;;;;;;;
;;
;; org-latex-export
;;
;;;;;;;;

(after! org
   (add-to-list 'org-latex-classes
                '("tufte"
                  "\\documentclass{tufte-book}"
                  ("\\part{%s}" . "\\part*{%s}")
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
   (add-to-list 'org-latex-classes
                '("memoir"
                  "\\documentclass{memoir}"
                  ("\\part{%s}" . "\\part*{%s}")
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                )
    (setq org-latex-text-markup-alist '((bold . "\\textbf{%s}")
                                        (code . protectedtexttt)
                                        (italic . "\\emph{%s}")
                                        (strike-through . "\\sout{%s}")
                                        (underline . "\\uline{%s}")
                                        (verbatim . "{\\color{red}%s}")))
(setq org-latex-default-packages-alist
      '(
 ;("AUTO" "inputenc" t
 ; ("pdflatex"))
 ;("T1" "fontenc" t
 ; ("pdflatex"))
 ("utf8" "inputenc" nil)
 ("" "graphicx" t)
 ("" "grffile" t)
 ("" "longtable" nil)
 ("" "wrapfig" nil)
 ("" "rotating" nil)
 ("normalem" "ulem" t)
 ("" "amsmath" t)
 ("" "textcomp" t)
 ("" "amssymb" t)
 ("" "capt-of" nil)
 ("style=apa, backend=biber" "biblatex" nil)
 ("" "braket" nil)
 ("" "xcolor" nil)
 ("" "hyperref" nil))
)
(setq  org-latex-pdf-process
       '("latexmk -shell-escape -bibtex -pdf %f -f")
       org-latex-compiler "xelatex"
       org-latex-bib-compiler "biber")
    )
    ;(add-to-list 'org-latex-default-packages-alist
    ;             '("" "xcolor" nil))
    ;(add-to-list 'org-latex-default-packages-alist
    ;             '("" "braket" nil))
    ;(add-to-list 'org-latex-default-packages-alist '("style=apa, backend=biber" "biblatex" nil)))
    ;(setq org-format-latex-header (concat org-format-latex-header "\n\\")))

;(use-package! tree-sitter
;  :config
;  (require 'tree-sitter-langs)
;  (global-tree-sitter-mode)
;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;(use-package tree-sitter-langs
;  :ensure t
;  :after tree-sitter
;  :config
; (tree-sitter-require 'tsx)
;(add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(evil-workman-global-mode t)

(map!
 :map evil-window-map
      "y" #'evil-window-left
      "Y" #'+evil/window-move-left
      "n" #'evil-window-down
      "N" #'+evil/window-move-down
      "e" #'evil-window-up
      "E" #'+evil/window-move-up
      "o" #'evil-window-right
      "O" #'+evil/window-move-right)

(after! evil-org
  (setq evil-org-movement-bindings
        '((up . "e")
          (down . "n")
          (left . "y")
          (right . "o")))
  (evil-define-key 'normal evil-org-mode-map
    "o"         'evil-forward-char
    "l"          'evil-org-open-below
    "L"         'evil-org-open-above
    "gy"        'org-backward-element
    "gn"        'org-down-element
    "ge"        'org-up-element
    "go"        'org-forward-element)
  )

(after! doom-modeline
  (setq doom-modeline-enable-word-count t
        doom-modeline-header-line nil
        ;doom-modeline-hud nil
        doom-themes-padded-modeline t
        doom-flatwhite-brighter-modeline nil
        doom-plain-brighter-modeline nil))
(add-hook! 'doom-modeline-mode-hook
           (progn
  (set-face-attribute 'header-line nil
                      :background (face-background 'mode-line)
                      :foreground (face-foreground 'mode-line))
  ))

        ;;;;;;;;;;;;;
;;;
;;; Other
;;;
;;;;;;;;;;;;

(setq vterm-shell "/usr/bin/fish")

(setq evil-escape-key-sequence "qd")

(map! :leader
      (:prefix ("r" . "roam")
       :desc "find file"            "f"   #'org-roam-find-file
       :desc "highlight"            "r"   #'org-noter-insert-note
       :desc "center scroll"        "s"   #'prot/scroll-center-cursor-mode
       :desc "start taking notes"   "S"   #'org-noter
       :desc "toggle buffer"        "b"   #'org-roam-buffer-toggle-display
       :desc "insert note"          "i"   #'org-roam-insert
       :desc "server"               "g"   #'org-roam-server
       :desc "quit notes"           "q"   #'org-noter-kill-session
       :desc "tag (roam)"           "t"   #'org-roam-tag-add
       :desc "tag (org)"            "T"   #'org-set-tags-command
       :desc "pomodoro"             "p"   #'org-pomodoro
       :desc "change nano-theme"    "n"   #'nano-toggle-theme
       :desc "rebuid db"            "d"   #'org-roam-db-build-cache
       :desc "cite"                 "c"   #'helm-bibtex
       :desc "thesaurus this word"  "w"  #'powerthesaurus-lookup-word-at-point
       :desc "thesaurus lookup word" "W"   #'powerthesaurus-lookup-word
       :desc "outline"              "o"   #'org-ol-tree
       (:prefix ("m" . "transclusion")
                :desc "make link"            "m"   #'org-transclusion-make-from-link
                :desc "transclusion mode"    "t"   #'org-transclusion-mode
                :desc "add at point"         "a"   #'org-transclusion-add-at-point
                :desc "add all in buffer"    "A"   #'org-transclusion-add-all-in-buffer
                :desc "remove at point"      "r"   #'org-transclusion-remove-at-point
                :desc "remove all in buffer" "R"   #'org-transclusion-remove-all-in-buffer
                :desc "start live edit"      "s"   #'org-transclusion-live-sync-start-at-point
                :desc "stop live edit"       "S"   #'org-transclusion-live-sync-exit-at-point)
       )
      (:prefix ("d" . "GTD")
       :desc  "process inbox" "p"#'org-gtd-process-inbox
       :desc  "agenda list" "a"#'org-agenda-list
       :desc  "capture" "c"#'org-gtd-capture
       :desc  "show next" "n" #'org-gtd-show-all-next
       :desc  "show stuck project" "s" #'org-gtd-show-stuck-projects)
      )
