;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; NOTE(Felix): Todos left
;;  - [X] Highlight todos in code
;;  - [X] Fragtog delay super high
;;  - [X] Magit ?
;;  - [X] Fixed Dir locals such that nice headers in vault
;;  - [X] Org bullets for non vault org
;;  - [X] More packages for latex preview
;;  - [X] bind C-o to embark-export in vertico mode
;;  - [X] latex scale to small
;;  - [X] roam find not listing tags

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Felix Brendel"
      user-mail-address "felix@brendel.io")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "noto sans mono" :slant 'normal :weight 'normal :height 127 :width 'normal)
      doom-modeline-major-mode-icon t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-miramare)

(use-package! neotree
  :custom
  (doom-themes-neotree-enable-variable-pitch nil)
  (doom-themes-neotree-file-icons 'fancy)
  (neo-hidden-regexp-list
      '("^\\.\\(?:git\\|hg\\|svn\\)$"
        "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
        "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
        "^\\.\\(?:sync\\|export\\|attach\\)$"
        "~$"
        "^#.*#$"
        "^\\.archives$"
        "\\.\\(?:db\\|db-shm\\|db-wal\\|log\\|orgids\\|projectile\\|dir-locals.el\\)$"))
  (neo-show-hidden-files nil))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/../../org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


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
;; (use-package! ivy-posframe
;;   :config
;;   (setq ivy-posframe-display-functions-alist
;;       '((t . ivy-posframe-display-at-frame-center)))
;;  (ivy-posframe-mode 1))

(use-package! company-posframe
  :defer t
  :config
  (company-posframe-mode 1))

(use-package! valign)

(use-package org-fragtog
  :custom (org-fragtog-preview-delay 99999999999999999)
  :bind   (:map org-mode-map ("<f2>" . org-toggle-latex-fragment))
  :hook   (org-mode . org-fragtog-mode))



(use-package! org-roam
  :init
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.25)
                 (window-parameters . ((no-delete-other-windows . t)))))
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/../../org/vault/")
  (org-roam-db-location "~/../../org/vault/.roam.db")
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  ;; NOTE(Felix): hopefullly teporary fix 
  (progn
    (require 'emacsql-sqlite3)
    (defun org-roam-db ()
      "Entrypoint to the Org-roam sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
      (unless (and (org-roam-db--get-connection)
                   (emacsql-live-p (org-roam-db--get-connection)))
        (let ((init-db (not (file-exists-p org-roam-db-location))))
          (make-directory (file-name-directory org-roam-db-location) t)
          ;; (let ((conn (emacsql-sqlite org-roam-db-location)))
          (let ((conn (emacsql-sqlite3 org-roam-db-location)))
            (emacsql conn [:pragma (= foreign_keys ON)])
            (set-process-query-on-exit-flag (emacsql-process conn) nil)
            (puthash (expand-file-name org-roam-directory)
                     conn
                     org-roam-db--connection)
            (when init-db
              (org-roam-db--init conn))
            (let* ((version (caar (emacsql conn "PRAGMA user_version")))
                   (version (org-roam-db--upgrade-maybe conn version)))
              (cond
               ((> version org-roam-db-version)
                (emacsql-close conn)
                (user-error
                 "The Org-roam database was created with a newer Org-roam version.  "
                 "You need to update the Org-roam package"))
               ((< version org-roam-db-version)
                (emacsql-close conn)
                (error "BUG: The Org-roam database scheme changed %s"
                       "and there is no upgrade path")))))))
      (org-roam-db--get-connection))
    (defun org-roam-db--init (db)
      "Initialize database DB with the correct schema and user version."
      (emacsql-with-transaction db
        ;; (emacsql db "PRAGMA foreign_keys = ON")
        (emacsql db [:pragma (= foreign_keys ON)])
        (pcase-dolist (`(,table ,schema) org-roam-db--table-schemata)
          (emacsql db [:create-table $i1 $S2] table schema))
        (pcase-dolist (`(,index-name ,table ,columns) org-roam-db--table-indices)
          (emacsql db [:create-index $i1 :on $i2 $S3] index-name table columns))
        (emacsql db (format "PRAGMA user_version = %s" org-roam-db-version))))
    )

  
  (org-roam-setup))

(use-package! websocket   :after org-roam)
(use-package! org-roam-ui :after org-roam
    :config (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook 'org-mode-hook 'org-fragtog-mode)


(defun duplicate-line()
  (interactive)
  (let ((inhibit-message 1))
  (save-excursion
    (move-beginning-of-line 1)
    (set-mark (point-marker))
    (move-end-of-line 1)
    (copy-region-as-kill (region-beginning) (region-end))
    (deactivate-mark)
    (insert "\n")
    (yank)
    (pop kill-ring))
  (call-interactively 'next-line)))

(defun mark-word-or-next-word-like-this ()
  "if there is no active region the word under
   the point will be marked, otherwise the next word is selected."
  (interactive)
  (if (region-active-p)
      ;; then
      (progn
        (mc/mark-next-like-this 1)
        (mc/cycle-forward)
        (mc/maybe-multiple-cursors-mode))
    ;; else
    (mc--mark-symbol-at-point)))

(map! :map vertico-map
      "C-o" 'embark-export)

(map! "C-s"         'consult-line
      "C-j"         'join-line
      "C-d"         'mark-word-or-next-word-like-this
      "C-S-d"       'duplicate-line
      "C-S-c C-S-c" 'mc/edit-lines
      "<f1>"        '+doom-dashboard/open
      "C-r"         'org-roam-node-insert
      "C-o"         'org-roam-node-find
      "C-c f p" 'doom/find-file-in-private-config
      )

(map! :map global-map
      "C-c f p" 'doom/find-file-in-private-config)

(map! :map org-mode-map
      "C-r"  'org-roam-node-insert
      "C-j"  'join-line
      "<f2>" 'org-roam-buffer-toggle)

(setq +doom-dashboard-menu-sections
      '(("Find node in vault"
         :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
         :action org-roam-node-find)
        ("Open random node from vault"
         :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
         :action org-roam-node-random)
        ("Show knowledge graph"
         :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
         :action org-roam-ui-mode)
        ("Open Calendar"
         :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
         :action open-my-calendar)
        ("Open private configuration"
         :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
         :when (file-directory-p doom-private-dir)
         :action doom/open-private-config)))

(defun org-take-and-insert-screenshot ()
  "Take a screenshot into a time stamped unique-named file in ab `images'
   directory, next to the org-buffer and insert a link to this file."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Not in org mode buffer"))
  (setq filename
        (concat "images/"
                (file-name-nondirectory (buffer-file-name))
                " -- "
                (format-time-string "%d-%m-%Y_%H-%M-%S")
                ".png"))

  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))

  ; take screenshot
  (when (eq system-type 'windows-nt)
      (shell-command "snippingtool /clip")
      (shell-command (concat "powershell -command "
                             "\"Add-Type -AssemblyName System.Windows.Forms;"
                             "if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {"
                             "    $image = [System.Windows.Forms.Clipboard]::GetImage();"
                             "    [System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png);"
                             "    Write-Output 'clipboard content saved as file'"
                             "} else {"
                             "    Write-Output 'clipboard does not contain image data'"
                             "}\"")))
  (when (eq system-type 'gnu/linux)
    (call-process "import" nil nil nil filename))

  ; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]"))))


(add-hook 'org-mode-hook 'valign-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook (lambda () (setq fill-column 75)))

(setq org-startup-indented t
      org-startup-with-latex-preview t
      org-startup-with-inline-images t
      org-fontify-whole-heading-line t
      org-fontify-quote-and-verse-blocks t
      org-hide-emphasis-markers t
      org-highlight-latex-and-related '(latex))

(with-eval-after-load 'org
  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template "${title:*}${tags}")

  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
  (add-to-list 'org-latex-packages-alist '("" "pgfplots" t)))

;; NOTE(Felix): we gotta set org-format-latex-options here after requiring org,
;;   otherwise if we use with-eval-after-load, it would only get loaded after we
;;   open the first org file, which at load time would already try to latex
;;   compile the fragments, before executing the `setq org-format-latex-options'
;;   it seems ...
(require 'org)
(setq org-format-latex-options
        '(:foreground default :background default :scale 2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                      ("begin" "$1" "$" "$$" "\\(" "\\[")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup. Like bold
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

(defun open-my-calendar ()
  (interactive)
  (org-agenda nil "o"))

(map! "<f2>" 'open-my-calendar)

(setq! calendar-week-start-day 0
       calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                                "Donnerstag" "Freitag" "Samstag"]
       calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                  "Juni" "Juli" "August" "September"
                                  "Oktober" "November" "Dezember"])

(setq solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))

(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

;; Feiertage für Bayern, weitere auskommentiert
(setq holiday-christian-holidays
      '((holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc -48 "Rosenmontag")
        ;; (holiday-easter-etc -3 "Gründonnerstag")
        (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariae Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        ;; (holiday-float 11 3 1 "Buss- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20)))



(setq org-tags-column -90)
(setq org-log-done t)
(setq org-agenda-category-icon-alist
      `(("teaching" ,(list "\xf130")       nil nil :ascent center)
        ("pizza"    ,(list (all-the-icons-material "local_pizza"))           nil nil)
        ("coffee"   ,(list (all-the-icons-faicon "coffee"))           nil nil)
        ("events"   ,(list (all-the-icons-faicon "calendar-check-o")) nil nil)
        ("todo"     ,(list (all-the-icons-faicon "check"))            nil nil)
        ("pprog"    ,(list (all-the-icons-faicon "align-left"))       nil nil)
        ("dbsys"    ,(list (all-the-icons-faicon "table"))            nil nil)
        ("gemji"    ,(list (all-the-icons-faicon "gamepad"))          nil nil)
        ("mocap"    ,(list (all-the-icons-faicon "eye"))              nil nil)
        ("uni"      ,(list (all-the-icons-faicon "graduation-cap"))   nil nil)))

(setq org-agenda-block-separator (string-to-char " "))
(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)
(setq org-agenda-dim-blocked-tasks 'invisible)

(setq org-archive-location "~/org/.archives/%s_archive::")

(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date 1 nil))
         (day (cadr date))
         (month (car date))
         (monthname (elt calendar-month-name-array (- month 1))))
    (format "  %-2s. %2d %s"
            dayname day monthname)))

(defun format-lectures ()
  (concat "[ " (nth 0 (org-get-outline-path)) " ]"))

(defun format-deadlines ()
  (concat (format-lectures)
          " <"
          (org-format-time-string "%d.%m.%Y" (org-get-deadline-time (point)))
          ">"))


(defun my/org-skip-function (part)
  "Partitions things to decide if they should go into the agenda '(agenda future-scheduled done)"
  (let* ((skip (save-excursion (org-entry-end-position)))
         (dont-skip nil)
         (deadline (org-get-deadline-time (point)))
         (deadline-seconds (time-convert deadline 'integer))
         (time-now (time-convert (current-time) 'integer))
         (result
          (or (and deadline
                   (< time-now (+ deadline-seconds 86400))
                   ;; 86400 == seconds per day -- basically check if the
                   ;; deadline time (which maybe was midnight) is today or in
                   ;; the future
                   'agenda)
              'done)))                 ; Everything else should go in the agenda
    (if (eq result part) dont-skip skip)))

(setq org-agenda-custom-commands
      '(("o" "Plan"
         ((tags-todo
           "today"
           ((org-agenda-remove-tags t)
            (org-agenda-overriding-header "Other todos today:\n")))
          (todo ""
                ((org-agenda-overriding-header "Lectures still to watch:\n")
                 (org-agenda-files '("~/org/uni.org"))
                 (org-agenda-prefix-format   "  %-2i  %-10(format-lectures) ")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'timestamp))))
          (tags  "assignments"
                 ((org-agenda-skip-function '(my/org-skip-function 'agenda))
                  (org-agenda-prefix-format   "  %-2i  %-23(format-deadlines) ")
                  (org-agenda-sorting-strategy '(deadline-up))
                  (org-agenda-files '("~/org/uni.org"))
                  (org-agenda-overriding-header "Due assignments:\n")
                  (org-agenda-remove-tags t)))
          (agenda ""
                  ((org-agenda-start-day "-1d")
                   (org-agenda-span 15)
                   (org-agenda-overriding-header "Agenda:\n")
                   (org-agenda-repeating-timestamp-show-all nil)
                   (org-agenda-remove-tags t)
                   (org-agenda-prefix-format   "    %-2i  %-20b %-11t  %s")
                   (org-agenda-todo-keyword-format "%-1s")
                   (org-agenda-current-time-string "<┈┈┈┈┈┈┈┈┈┈ now ┈┈┈┈┈┈┈┈┈┈>")
                   (org-agenda-deadline-leaders '("Deadline:  " "In %3d t:  " "Vor %2d t: "))
                   (org-agenda-scheduled-leaders '("Scheduled: " "Overdue: "))
                   (org-agenda-time-grid '((daily today remove-match)
                                           (0800 0900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
                                           " . . ." "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈"))))))))

(setq org-export-babel-evaluate nil)

(setq lang :de)
(defun change-lang ()
  (interactive)
  ;; (keyboard-translate ?z ?y)
  (if (eq lang :de)
      (progn
        (keyboard-translate ?y ?z)  ; For german keyboards
        (keyboard-translate ?z ?y)
        (set-input-method 'korean-hangul)
        (setq lang :kr))
    (toggle-input-method nil)
    (keyboard-translate ?y ?y)  ; For german keyboards
    (keyboard-translate ?z ?z)
    (setq lang :de)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; startup
(neotree-toggle)
(neotree-dir "~/../../org/")
(neotree-refresh)
(switch-to-buffer "*doom*")
(delete-other-windows)


(setq org-preview-latex-process-alist
      '((dvipng :programs
                ("latex" "dvipng")
                :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
                (1.0 . 1.0)
                :latex-compiler
                ("latex -interaction nonstopmode -output-directory %o %f")
                :image-converter
                ("dvipng -D %D -T tight -o %O %f")
                :transparent-image-converter
                ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
        (dvisvgm :programs
                 ("latex" "dvisvgm")
                 :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
                 (1.7 . 1.5)
                 :latex-compiler
                 ("latex -interaction nonstopmode -output-directory %o %f")
                 :image-converter
                 ("dvisvgm %f -n -b min -c %S -o %O"))
        (imagemagick :programs
                     ("latex" "convert")
                     :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                     (1.0 . 1.0)
                     :latex-compiler
                     ("pdflatex -interaction nonstopmode -output-directory %o %f")
                     :image-converter
                     ("convert -density %D -trim -antialias %f -quality 100 %O"))))

(setq org-preview-latex-default-process 'imagemagick)
