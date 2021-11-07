;;; * Org Mode Org-Capture Customizations

;;; ** Temporary patch for an oops
;;; ref: https://github.com/Somelauw/evil-org-mode/issues/93
;;; You should remove this section once the problem has been properly fixed!!
(fset 'evil-redirect-digit-argument 'ignore) ; before evil-org loaded

(add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line)
(evil-define-key 'motion 'evil-org-mode
    (kbd "0") 'evil-org-beginning-of-line)

;;; ** Fundamental Paths

(setq org-directory (expand-file-name "~/Notes"))
(setq org-default-notes-file (expand-file-name "Notes" org-directory))

;;; ** Bindings

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun org-mode-jgd ()
	(setq variable-pitch-mode 1)
	(set-face-attribute 'org-table nil :inherit 'fixed-pitch) )

(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'org-mode-jgd)

;;;; ** Capture-Mode

;; We're capturing everything under our GTD directory
;; Maybe someday we'll separate these things out?

(global-set-key "\C-cc" 'org-capture)

(defconst gtd-dir (expand-file-name "GTD" org-directory))
(defconst gtd-inbox-org (expand-file-name "inbox.org" gtd-dir))
(defconst gtd-gtd-org (expand-file-name "gtd.org" gtd-dir))
(defconst gtd-tickler-org (expand-file-name "tickler.org" gtd-dir))
(defconst gtd-someday-org (expand-file-name "someday.org" gtd-dir))

(defconst gtd-bookmarks-org (expand-file-name "bookmarks.org" gtd-dir))
(defconst gtd-journal-org (expand-file-name "journal.org" gtd-dir))
(defconst gtd-ideas-org (expand-file-name "ideas.org" gtd-dir))
(defconst gtd-pass-org (expand-file-name "pass.org" gtd-dir))
(defconst gtd-hai-org (expand-file-name "hai.org" gtd-dir))
(defconst gtd-hai-elp-org (expand-file-name "hai-elp.org" gtd-dir))
(defconst gtd-selp-org (expand-file-name "selp.org" gtd-dir))
(defconst gtd-misc-org (expand-file-name "misc.org" gtd-dir))

(setq org-agenda-files (list gtd-inbox-org gtd-gtd-org gtd-tickler-org))

(setq org-capture-templates
	`( ( "t" "Todo [inbox]" entry (file+headline ,gtd-inbox-org "Tasks")
			 "* TODO %i%?" )
		 ( "T" "Tickler" entry (file+headline ,gtd-tickler-org "Tickler")
			 "* %i%? \n %U" )
		 ( "b" "Bookmarks" entry (file+headline ,gtd-bookmarks-org "Bookmarks")
			 "* %?\nEntered %U\n  %i\n  %a" )
		 ( "j" "Journal" entry (file+datetree ,gtd-journal-org)
			 "* %?\nEntered %U\n  %i\n  %a" )
		 ( "i" "Ideas" entry (file+headline ,gtd-ideas-org "Ideas")
			 "* %?\nEntered %U\n  %i\n  %a" )
		 (  "p" "Pass/Enter/Start" entry (file+headline ,gtd-pass-org "Pass")
			 "* %i%? \n %U" )
		 ( "h" "HAI" entry (file+headline ,gtd-hai-org "HAI")
			 "* %?\nEntered %U\n  %i\n  %a\n  %x" )
		 ( "e" "ELP" entry (file+headline ,gtd-hai-elp-org "HAI ELP")
			 "* %?\nEntered %U\n  %i\n  %a\n  %x" )
		 ( "s" "SELP" entry (file+headline ,gtd-selp-org "SELP")
			 "* %?\nEntered %U\n  %i\n  %a\n  %x" )
		 ( "m" "Misc" entry (file+headline ,gtd-misc-org "Miscellany")
			 "* %?\nEntered %U\n  %i\n  %a\n  %x" )
		 ) )

;; There's some confusion about org-refile-targets
;; Initially none of these worked.  Then the first
;; one started working.  I may have needed to put
;; in a couple of levels of headings.
;; I would definitely like more flexibility and/or
;; to be able to file things to a deeper level!

(setq org-refile-targets `((,gtd-gtd-org :maxlevel . 3)
                           (,gtd-someday-org :level . 1)
                           (,gtd-tickler-org :maxlevel . 2)
                           (,gtd-bookmarks-org :level . 1)
                           (,gtd-journal-org :maxlevel . 4)
                           (,gtd-ideas-org :maxlevel . 4)
                           (,gtd-misc-org :maxlevel . 2)
))

;; find ~/Notes/{Current,Folks,GTD} -name '*.org' | sed -e "s@$HOME@~@" -e 's@/[^/]*$@@'  -e 's@.*$@"&"@' | sort -u

(setq org-agenda-files (delete-dups (append org-agenda-files  (quote (
"~/Notes/"
"~/Notes/Current/"
"~/Notes/Current/HAI/"
"~/Notes/Current/SELP-2019/"
"~/Notes/Current/SELP-2019/Julie/"
"~/Notes/Current/SELP-2019/Erica/"
"~/Notes/Current/SELP-2019/MaryAnn/"
"~/Notes/Current/SELP-2019/Participants/"
"~/Notes/Folks/"
"~/Notes/GTD/"
																																			 )))))

;; (setq org-refile-targets `(
;; 			    (nil :maxlevel . 5)
;; 			    (,org-agenda-files :maxlevel . 5) ; do I want , here?
;; 			    (,gtd-someday-org :level . 1) ))
                           
;; (setq org-refile-targets `(
;; 			    (,org-agenda-files :maxlevel . 5) ; do I want , here?
;; 			    (,gtd-someday-org :level . 1) ))
