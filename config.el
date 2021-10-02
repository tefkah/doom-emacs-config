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

(setq doom-theme 'doom-flatwhite)

(add-hook! 'solaire-mode-hook
  ;(set-face-attribute 'solaire-fringe-face nil :background (face-background 'solaire-hl-line-face))
  (set-face-attribute 'fringe nil :background (face-background 'solaire-default-face))
  )

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-face 'variable-pitch))

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 15 :weight 'light)
       doom-variable-pitch-font (font-spec :family "Roboto" :style "Regular" :size 12 :weight 'regular))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You an either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

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

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info vcs word-count)
    '(buffer-position misc-info major-mode)))

(after! centaur-tabs
  (setq centaur-tabs-style "wave"))

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  )

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config (setq
           org-appear-autolinks t
           org-appear-autoentities t
           org-appear-autosubmarkers t ))

(use-package! org-transclusion
  :after org-roam
  )

(map! :map org-mode-map
:nie "C-M-SPC" (cmd! (insert "\u200B")))

(setq org-roam-v2-ack t)

(use-package! org-roam
  :after org
  :config
  (setq org-roam-v2-ack t)
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section
              #'org-roam-unlinked-references-insert-section))
  (org-roam-setup))

(defun org-roam-buffer-setup ()
  "Function to make org-roam-buffer more pretty."
  (progn
    (setq-local olivetti-body-width 44)
    (variable-pitch-mode 1)
    (olivetti-mode 1)
    (centaur-tabs-local-mode -1)

  (set-face-background 'magit-section-highlight (face-background 'default))))

(after! org-roam
(add-hook! 'org-roam-mode-hook #'org-roam-buffer-setup))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-open-on-start nil)
  (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url))

(after! org-roam
    (setq org-roam-capture-templates
          `(("s" "standard" plain "%?"
     :if-new
     (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
      "#+title: ${title}\n#+filetags: \n\n ")
     :unnarrowed t)
        ("d" "definition" plain
         "%?"
         :if-new
         (file+head "${slug}.org" "#+title: ${title}\n#+filetags: definition \n\n* Definition\n\n\n* Examples\n")
         :unnarrowed t)
        ("r" "ref" plain "%?"
           :if-new
           (file+head "${citekey}.org"
           "#+title: ${slug}: ${title}\n
\n#+filetags: reference ${keywords} \n
\n* ${title}\n\n
\n* Summary
\n\n\n* Rough note space\n")
           :unnarrowed t)
          ("p" "person" plain "%?"
           :if-new
           (file+head "${slug}.org" "%^{relation|some guy|family|friend|colleague}p %^{birthday}p %^{address}p
#+title:${slug}\n#+filetags: :person: \n"
                      :unnarrowed t)))))

(use-package! org-ref
    ;:after org-roam
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list "/Users/thomas/OneDrive/org-roam/bib/Library.bib")
         org-ref-bibliography-notes "/Users/thomas/OneDrive/org-roam/bibnotes.org"
         org-ref-note-title-format "* %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "/Users/thomas/OneDrive/org-roam/"
         org-ref-notes-function 'orb-edit-notes
    ))

(after! org-ref
(setq
 bibtex-completion-notes-path "/Users/thomas/OneDrive/org-roam/"
 bibtex-completion-bibliography "/Users/thomas/OneDrive/org-roam/bib/Library.bib"
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
  :hook (org-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
   '("citekey" "title" "url" "file" "author-or-editor" "keywords" "pdf" "doi" "author" "tags" "year" "author-bbrev")))
;)

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
  ;(org-num-mode 1)
  (org-superstar-mode -1)
  (org-indent-mode -1)
  ))

(add-hook! 'org-mode-hook  #'nicer-org)

(setq org-preview-latex-process-alist
  '((dvipng
     :programs ("/Library/TeX/texbin/latex" "/Library/TeX/texbin/dvipng")
     :description "dvi > png"
     :message "you need to install the programs: latex and dvipng."
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("/Library/TeX/texbin/latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("/Library/TeX/texbin/dvipng -D %D -T tight -bg Transparent -o %O %f"))
    (dvisvgm
     :programs ("/Library/TeX/texbin/latex" "/Library/TeX/texbin/dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: latex and dvisvgm."
     :image-input-type "dvi"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("/Library/TeX/texbin/latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("/Library/TeX/texbin/dvisvgm %f -n -b min -c %S -o %O"))
    (imagemagick
     :programs ("latex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %O"))))

(after! org
  (setq org-startup-with-latex-preview 1 ;always preview latex
        org-latex-create-formula-image-program 'dvipng
        LaTeX-command "/Library/TeX/texbin/latex"
        latex-run-command "/Library/TeX/texbin/latex"
        org-startup-with-inline-images 1 ;always preview images
        ;org-hide-leading-stars 1
        org-startup-indented nil         ; don't indent
  ;      org-startup-folded nil
        org-hidden-keywords '(filetags title author date startup roam_tags)
        org-pretty-entities 1            ; show unicode characters
        org-num-max-level 3              ; no 1.1.1.2
        org-indirect-buffer-display 'other-window
        line-spacing 3 ; let me B R E A T H E
        )
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin")))

(defun +org-tree-to-indirect-buffer-options (option)
    (let* ((old-value org-indirect-buffer-display))
          (progn
            (setq org-indirect-buffer-display option)
          (org-tree-to-indirect-buffer)
          (setq org-indirect-buffer-display old-value))))

(defun +org-tree-to-indirect-other-window ()
  (interactive)
  (+org-tree-to-indirect-buffer-options 'other-window))

(defun +org-tree-to-indirect-current-window ()
  (interactive)
  (+org-tree-to-indirect-buffer-options 'current-window))

(defun +org-tree-to-indirect-dedicated-frame ()
  (interactive)
  (+org-tree-to-indirect-buffer-options 'dedicated-frame))

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
                           :family "Roboto"
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
            :property      ""
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
            :properties    ""
            :end           "∎"
            :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
            :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
            :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
            :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
            :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)
            :roam_tags nil
            :filetags nil))
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
  :roam_tags     "#+roam_tags:"
  :filetags      "#+filetags:")
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
                                 entry (file+headline "/Users/thomas/OneDrive/org-roam/agenda.org" "Future")
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
               '(internal-border-width . 30)
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

(defcustom double-modeline-margin-inner-height 60
  "inner"
  :type 'integer)
(defcustom double-modeline-margin-outer-height 10
  "outer"
  :type 'integer)

(after! org
        (require 'svg))
(defun make-svg-rectangle (width height-1 bg-1 height-2 bg-2)
  (let* ((svg (svg-create width (+ height-1 height-2))))
    (svg-rectangle svg 0 0 width height-1 :fill-color bg-1)
    (svg-rectangle svg 0 height-1 width height-2 :fill-color bg-2)
    svg))

(defun make-svg-rectangles (width height-1 bg-1 &rest other)
  (let* ((temptt 0)
         (height-temp height-1)
         (svg (svg-create width
                           (+ height-1
                             (dotimes
                                (i (/ (length other) 2) temptt)
                                         (setq temptt
                                               (+
                                          (nth (* i 2) other)
                                          temptt)))))))
    (svg-rectangle svg 0 0 width height-1 :fill-color bg-1)
    (when other
      (dotimes (i (/ (length other) 2))
    (svg-rectangle svg 0
                   (if (eq i 0) height-1
                     (setq-local height-temp
                                 (+ height-temp
                                    (nth (* (- i 2) 2) other))))
                   width
                   (nth (* i 2) other)
                   :fill-color (nth (+ (* i 2) 1) other))))
    svg))

(defun mode-line-compose (height-1 bg-1 height-2 bg-2
                                   header)
  (let* ((fringe-width (car (window-fringes nil)))
         (body-width (window-body-width nil t))
         (margin-width (* (frame-char-width)
                        (+ (car (window-margins))
                          (cdr (window-margins))))))
    (concat
  (format-mode-line
   (propertize " " 'display (svg-image
    (make-svg-rectangle fringe-width height-1
      bg-1 height-2 bg-1))))
  (format-mode-line
   (propertize " " 'display (svg-image
                            (if header
                             (make-svg-rectangle
                              (+ margin-width body-width)
                        height-1 bg-1 height-2 bg-2)
                             (make-svg-rectangle
                              (+ margin-width body-width)
                        height-2 bg-2 height-1 bg-1)))))
  (format-mode-line
   (propertize " " 'display (svg-image
    (make-svg-rectangle fringe-width height-1
      bg-1 height-2 bg-1)))))))

(defvar double-modeline--default-header-format header-line-format
  "Storage for the default `mode-line-format'.
So it can be restored when 'writer-header-line-mode' is disabled.")

(defvar double-modeline--default-modeline-format mode-line-format)

(define-minor-mode double-header-line-mode
  "Adds a bar with the same color as the fringe as the header-line.
Imitates the look of wordprocessors a bit."
  :init-value nil
  :global nil
  (if double-header-line-mode
      (progn
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'header-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
        (setq header-line-format '((:eval (mode-line-compose
                                   double-modeline-margin-outer-height
                                   (face-background 'fringe)
                                   double-modeline-margin-inner-height
                                   (face-background 'default)
                                   t
                                   ))))
        (setq mode-line-format '((:eval (mode-line-compose
                                   double-modeline-margin-outer-height
                                   (face-background 'fringe)
                                   double-modeline-margin-inner-height
                                   (face-background 'default)
                                   nil
                                   )))))
    (setq header-line-format 'double-modeline--default-header-format
          mode-line-format 'double-modeline--default-modeline-format)))

(after! olivetti-mode (setq double-modeline-margin-inner-height  (round (* 0.6 (* (frame-char-width) (car (window-margins)))))))

(use-package! page-break-mode)

(use-package! olivetti
  :after org
  ;:hook (olivetti-mode . double-header-line-mode)
  :config
    (setq olivetti-min-body-width 50
          olivetti-body-width 80
          olivetti-style 'fancy ; fantastic new layout
          olivetti-margin-width 12)
    (add-hook! 'olivetti-mode-hook (window-divider-mode -1))
    (add-hook! 'olivetti-mode-hook (set-face-attribute 'window-divider nil :foreground (face-background 'fringe) :background (face-background 'fringe)))
    (add-hook! 'olivetti-mode-hook (set-face-attribute 'vertical-border nil :foreground (face-background 'fringe) :background (face-background 'fringe)))
    )

(require 'org-inlinetask)

;(use-package! org-sidebar
;  :after org
;  :config
  ;(setq org-sidebar-default-fns '(org-sidebar--todo-items))
  ;(add-hook! 'org-sidebar-window-after-display-hook (solaire-mode 1))
;   )

(after! org
  (remove-hook 'org-agenda-finalize-hook '+org-exclude-agenda-buffers-from-workspace-h)
  (remove-hook 'org-agenda-finalize-hook
               '+org-defer-mode-in-agenda-buffers-h))

(defun thomas/org-get-overview ()
  "Open outline and sidebar."
  (progn
    (org-ol-tree)
    (org-sidebar)))

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

(add-hook! 'latex-mode-hook (setq TeX-engine 'xetex) 99)

    ;  (call-process TeX-shell nil (TeX-process-buffer-name file) nil
     ;               TeX-shell-command-option (concat command file))))

(after! latex
(defun latex-dwim ()
  "Compile the current file if it's a .tex file using LaTeXMK.
Otherwise compile the TeX file with the same name as the current TeXey file,
such as a .cls or .bib.
Otherwise compile all the .tex files you find using LaTexMK."
  (interactive)
  (save-buffer)
  (if-let ((files (thomas/find-tex-file))
           (command
            "latexmk -pdf -pdflatex=lualatex --synctex=1 -interaction=nonstopmode  -file-line-error ")
        (hook (nth 2 (assoc "LatexMk" TeX-command-list))))
      (if (stringp files)
      (TeX-run-format "LatexMk" (concat command files) files)
        (dolist (file files)
      (TeX-run-format "LatexMk" (concat command file) file)))
    (message "No file found, whoops.")))

(defun thomas/find-tex-file ()
  "Find the correct TeX file(s)."
  (let* ((fname (buffer-file-name))
         (ext (file-name-extension fname))
         (potential-main  (f-join (f-slash (f-parent fname)) (concat (f-base fname) ".tex")))
         (alltex (f-entries (f-parent fname) (lambda (f) (f-ext-p f "tex"))))
    (correct-file
     (cond ((string= ext "tex")
           fname)
           ((seq-contains-p '("bib" "cls" "sty") ext)
            (if (f-exists-p potential-main)
                potential
              alltex))
           (t nil))))
    correct-file)))

(map! :map 'doom-leader-regular-map
      :desc "LatexMk dwim" "l" #'latex-dwim)

(add-hook! 'after-init-hook #'treemacs)

(after! treemacs
(add-hook! 'treemacs-mode-hook (setq window-divider-mode -1
                                     variable-pitch-mode 1
                                     treemacs-follow-mode 1))
)

(use-package! visual-regexp
  :config
        (map! :map 'doom-leader-regular-map
              (:prefix ("v" . "visual regex")
               :desc "Replace regexp" "r"#'vr/replace)))

(use-package! visual-regexp-steroids
  :after 'visual-regexp)

(use-package! devdocs
  :after lsp
  :config
  (add-hook! 'devdocs-mode-hook
    (face-remap-add-relative 'variable-pitch '(:family "Noto Sans"))))

(add-hook! 'after-init-hook
           (progn
  (setq-hook! 'typescript-mode-hook +format-with :nil)
  (add-hook! 'typescript-mode-hook 'prettier-mode)
  (setq-hook! 'rjsx-mode-hook +format-with :nil)
  (add-hook! 'rjsx-mode-hook 'prettier-mode)
  (setq-hook! 'js2-mode-hook +format-with :nil)
  (add-hook! 'js2-mode-hook 'prettier-mode)
  (setq-hook! 'typescript-tsx-mode-hook +format-with :nil)
  (add-hook! 'typescript-tsx-mode-hook 'prettier-mode)
  ))

(use-package! eva
:init
(setq ess-history-file "~/OneDrive/self/data/.Rhistory")
(setq ess-ask-for-ess-directory nil)
  (setq eva-ai-name "Ea"
        eva-user-name "Thomas"
        eva-user-birthday "2021-07-16"
        eva-user-short-title "Bruh"
        eva-fallback-to-emacs-idle t)
      (setq eva--idle-secs-fn #'eva--idle-secs-gnome)
  (setq eva-idle-log-path         "~/OneDrive/self/data/idle.tsv")
  (setq eva-buffer-focus-log-path "~/OneDrive/self/data/buffer-focus.tsv")
  (setq eva-buffer-info-path      "~/OneDrive/self/data/buffer-info.tsv")
  (setq eva-main-ledger-path      "~/OneDrive/self/journal/finances/l.ledger")
  (setq eva-main-datetree-path    "~/OneDrive/org-roam/diary.org")
  :config
  (setq org-journal-dir "~/OneDrive/org-roam/journal")
    (setq org-journal-file-format "%F.org")
    (require 'eva-builtin)
  (require 'eva-activity)
    (add-hook 'eva-after-load-vars-hook #'eva-check-dangling-clock)
  (add-hook 'eva-after-load-vars-hook #'eva-check-org-variables)
   (setq eva-items
        (list
         (eva-item-create :fn #'eva-greet
                          :min-hours-wait 1)

         (eva-item-create :fn #'eva-query-mood
                          :dataset "~/OneDrive/self/data/mood.tsv"
                          :min-hours-wait 1)

         (eva-item-create :fn #'eva-query-activity
                          :dataset "~/OneDrive/self/data/activities.tsv"
                          :min-hours-wait 1)

         (eva-item-create :fn #'eva-present-diary
                          :max-successes-per-day 1)

         (eva-item-create :fn #'eva-query-weight
                          :dataset "~/OneDrive/self/data/weight.tsv"
                          :max-entries-per-day 1)

         (eva-item-create :fn #'eva-plot-weight
                          :max-entries-per-day 1)

         (eva-item-create :fn #'eva-query-sleep
                          :dataset "~/OneDrive/self/data/sleep.tsv"
                          :min-hours-wait 5
                          :lookup-posted-time t)

         (eva-item-create :fn #'eva-present-ledger-report)

         (eva-item-create :fn #'eva-present-org-agenda)

         (eva-item-create :fn #'eva-query-ingredients
                          :dataset "~/OneDrive/self/data/ingredients.tsv"
                          :min-hours-wait 5)

         (eva-item-create :fn #'eva-query-cold-shower
                          :dataset "~/OneDrive/self/data/cold.tsv"
                          :max-entries-per-day 1)

         ;; you can inline define the functions too
         (eva-item-create
          :fn (eva-defun my-bye ()
                (message (eva-emit "All done for now."))
                (bury-buffer (eva-buffer-chat)))
          :min-hours-wait 0)))
        (transient-replace-suffix 'eva-dispatch '(0)
    '["General actions"
      ("q" "Quit" bury-buffer)
      ("l" "View Ledger report" eva-present-ledger-report)
      ("f" "View Ledger file" eva-present-ledger-file)
      ("a" "View Org agenda" org-agenda-list)])

  (define-key eva-chat-mode-map (kbd "l") #'eva-present-ledger-report)
  (define-key eva-chat-mode-map (kbd "a") #'org-agenda-list)

  ;; Activities
  (setq eva-activity-list
        (list (eva-activity-create :name "sleep"
                                   :cost-false-pos 3
                                   :cost-false-neg 3)

              (eva-activity-create :name "studying"
                                   :cost-false-pos 8
                                   :cost-false-neg 8)

              (eva-activity-create :name "coding"
                                   :cost-false-pos 5
                                   :cost-false-neg 5)

              (eva-activity-create :name "working"
                                   :cost-false-pos 5
                                   :cost-false-neg 5)
              (eva-activity-create :name "unknown"
                                   :cost-false-pos 0
                                   :cost-false-neg 0)))
  (eva-mode))

        ;;;;;;;;;;;;;
;;;
;;; Other
;;;
;;;;;;;;;;;;

(setq vterm-shell "/usr/bin/fish")

(setq evil-escape-key-sequence "qd")

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

(map! :leader
      (:prefix-map ("r" . "regular")
       :desc "find file"            "f"   #'org-roam-node-find
       :desc "find ref"             "F"   #'org-roam-ref-find
       :desc "center scroll"        "s"   #'prot/scroll-center-cursor-mode
       :desc "start taking notes"   "S"   #'org-noter
       :desc "toggle buffer"        "b"   #'org-roam-buffer-toggle
       :desc "insert note"          "i"   #'org-roam-node-insert
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
       (:prefix  ("r" . "orui")
                :desc "orui-mode" "r" #'org-roam-ui-mode
                :desc "zoom" "z" #'orui-node-zoom
                :desc "open" "o" #'orui-open
                :desc "local" "l" #'orui-node-local
                :desc "sync theme" "t" #'orui-sync-theme
                :desc "follow" "f" #'orui-follow-mode)
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

(map! "C-w" nil)
(global-set-key  (kbd "C-<tab>") #'evil-window-next)
 (global-set-key             (kbd "C-<iso-lefttab>") #'evil-window-prev)
     (global-set-key   (kbd "C-w") #'ace-window)

(map!
    :nvig "C-<iso-lefttab>" #'evil-window-prev
      :nvig  "C-w" #'ace-window)
(map! :nvig "C-<tab>" #'evil-window-next)

(map!  :nvig "C-'" #'er/expand-region)

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

(defun set-evil-keybindings ()
  (progn
  ;(iscroll-mode 1)
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
    "go"        'org-forward-element
    "n"         'evil-next-visual-line
    "e"         'evil-previous-visual-line
;    "n"         'iscroll-forward-line
;    "e"         'iscroll-previous-line
    "N"         'evil-next-line
    "E"         'evil-previous-line
    (kbd "C-n") 'follow-scroll-up
    (kbd "C-e") 'follow-scroll-down
    "zn"        '+org-tree-to-indirect-other-window
    "zs"        '+org-tree-to-indirect-current-window
    "zv"        '+org-tree-to-indirect-other-frame)
  ))

(after! org (set-evil-keybindings))

;; JUST TO BE REALLY FUCKING SURE
(add-hook 'org-mode-hook #'set-evil-keybindings 99)
(defun iscroll-mode-keybinds ()
  (when (eq iscroll-mode t)
      (evil-define-key 'normal evil-org-mode-map
        "n" 'iscroll-forward-line
        "e" 'iscroll-previous-line)))

(use-package! all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode))

(defun margin-width-pixel (&optional right)
  "Return the width of the left or optionally right margin in pixels."
  (if (window-margins)
     (if right
           (* (frame-char-width) (cdr (window-margins))) ;;right margin
          (* (frame-char-width) (car (window-margins))))
          0))

(defun org-latex-refresh ()
  (interactive)
  (progn
  (org-clear-latex-preview)
  (org--latex-preview-region (buffer-end -1) (buffer-end 1))))

(defun org-latex-clear-preview ()
  (interactive)
  (org-clear-latex-preview))

(server-start)
