        ;;;;;;;;;;;;;
;;;
;;; Other
;;;
;;;;;;;;;;;;

(setq vterm-shell "/usr/bin/fish")

(setq evil-escape-key-sequence "qd")

(add-hook! 'fundamental-mode-hook (solaire-mode -1))
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
;  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

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
       :desc "change nano-theme"    "n"   #'nano-change-theme
       :desc "rebuid db"            "d"   #'org-roam-db-build-cache
       :desc "cite"                 "c"   #'helm-bibtex
       :desc "writing-mode"         "w"   #'thomas/writing-mode
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

(evil-workman-global-mode t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/OneDrive/org-roam/"
      org-roam-directory "~/OneDrive/org-roam")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode org-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement"
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;Startup function
;;;;;
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/
(global-subword-mode 1)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-theme 'doom-plain)

        (setq doom-font (font-spec :family "Fira Code" :size 15 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "Noto Serif" :style "Regular" :size 18 :weight 'regular))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-heigth t)
  (set-face-attribute 'variable-pitch nil :height 1.3 :family "Noto Serif" :weight 'regular))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You an either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(defun doom-modeline--set-char-widths (alist)
  "Set correct widths of icons characters in ALIST."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (dolist (pair alist)
    (let ((width 1)
          (chars (cdr pair))
          (table (make-char-table nil)))
      (dolist (char chars)
        (set-char-table-range table char width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(after! doom-modeline
  (setq doom-modeline-enable-word-count t
        doom-modeline-header-line t
        ;doom-modeline-hud nil
        doom-themes-padded-modeline t
        ;doom-flatwhite-brighter-modeline t)
        )
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info vcs word-count)
    '(buffer-position misc-info major-mode "                  "))
  (custom-set-faces! '(mode-line :family "Fira Code"))
  (doom-modeline--set-char-widths doom-modeline-rhs-icons-alist))

(use-package! mlscroll
  :after doom-modeline
  :hook (doom-modeline-mode . mlscroll-mode)
  :config
  (setq mlscroll-right-align nil)
  (add-to-list 'mode-line-misc-info '(:eval (mlscroll-mode-line)))
  (setq mlscroll-border 10)
  (setq mlscroll-width-chars 20)
  )

;(setq +doom-dashboard-functions
;      '(
;        doom-dashboard-banner
;        doom-dashboard-loaded
;        thomas-dashboad-text
;        ))


(defvar nano-theme-light-var t)
;(require 'disp-table)
;(require 'nano-theme-dark)
;(require 'nano-theme-light)
;(nano-theme-set-light)
;(require 'nano-layout)
;(require 'nano-base-colors)
(require 'nano-faces)
(nano-faces)
;(require 'nano-theme)
;(nano-theme)
;(require 'nano-modeline)
;(require 'nano-writer)
;;(nano-theme)
;;

(setq default-frame-alist
      (append (list
	       ;; '(font . "Roboto Mono Emacs Regular:size=14")
	       '(min-height . 1)  '(height     . 45)
	       '(min-width  . 1) '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))



(defun nano-change-theme-dark ()
  (interactive)
  (nano-theme-set-dark)
  (nano-faces)
  (nano-theme))

(defun nano-change-theme-light ()
  (interactive)
  (nano-theme-set-light)
  (nano-faces)
  (nano-theme))

(defun nano-change-theme ()
  (interactive)
  (if nano-theme-light-var (nano-change-theme-dark) (nano-change-theme-light))
  (setq nano-theme-light-var (not nano-theme-light-var)))

(defvar +org-roam-open-buffer-on-find-file nil
  "If non-nil, open the org-roam buffer when opening an org roam file.")


;  (setq centaur-tabs-style "wave")
;  (setq centaur-tabs-set-bar 'under)
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
;(setq x-underline-at-descent-line t)


;;
;;; Packages

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  )

(use-package! org-transclusion
  :after org-roam
  )

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
  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode)

  (setq org-roam-capture-templates `(("s" "standard" plain (function org-roam--capture-get-point)
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

(use-package! company-org-roam
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

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

;;; Ugly org hooks
(defun nicer-org ()
  (+org-pretty-mode 1)
  (mixed-pitch-mode 1)
  (hl-line-mode -1)
  (display-line-numbers-mode -1)
  (olivetti-mode 1)
  (org-num-mode 1)
  ;(org-indent-mode -1)
  )

(add-hook! 'org-mode-hook  #'nicer-org)
(after! org
  (setq org-startup-with-latex-preview 1
        org-startup-with-inline-images 1
        org-hide-leading-stars 1
        org-startup-indented nil
        org-superstar-headline-bullets-list`("\u200b")
  ;      org-startup-folded nil
        ;org-startup-numerated 1
        org-pretty-entities 1)
  (custom-set-faces!
  '((org-block) :background nil)
  )
  (defface redd
    '((((class color) (min-colors 88) (background light))
      :foreground "red"))
    "Red."
    :group 'basic-faces)
  (custom-set-faces! '(org-level-1 :height 1.6 :weight bold :slant normal) '(org-level-2 :height 1.4 :weight regular :slant normal) '(org-level-3 :height 1.3 :weight regular :slant italic))
  (setq org-emphasis-alist
        '(("*" (bold))
          ("/" italic)
          ("_" underline)
          ("=" redd)
          ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
          ("+" (:strike-through t))))
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
            :title         "𝙏"
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
            :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))
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
  :priority_e    "[#E]")
(plist-put +ligatures-extra-symbols :name "⁍")
)

(with-eval-after-load 'org
  (plist-put org-format-latex-options :background 'default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      Getting Things Done
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
