;;; ox-fw.el --- Foswiki Back-End for Org Export Engine

;; this file is based off of ox-md.el which is
;; Author: Nicolas Goaziou <n.goaziou@gmail.com>
;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.

;; Author: Josiah Schwab <jschwab@gmail.com>
;; Keywords: org, wp, foswiki

;;; Commentary:

;; This library implements a Foswiki back-end Org exporter, based on
;; `html' back-end.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-fw-export-as-foswiki' (temporary buffer) and
;; `org-fw-export-to-foswiki' ("fw" file).

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-html)



;;; User-Configurable Variables

(defgroup org-export-fw nil
  "Options specific to Foswiki export back-end."
  :tag "Org Foswiki"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))



;;; Define Back-End

(org-export-define-derived-backend 'fw 'html
  :export-block '("FW" "FOSWIKI")
  :menu-entry
  '(?f "Export to Foswiki"
       ((?F "To temporary buffer"
	    (lambda (a s v b) (org-fw-export-as-foswiki a s v)))
	(?f "To file" (lambda (a s v b) (org-fw-export-to-foswiki a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-fw-export-to-foswiki t s v)
		(org-open-file (org-fw-export-to-foswiki nil s v)))))))
  :translate-alist '((bold . org-fw-bold)
		     (headline . org-fw-headline)
		     (horizontal-rule . org-fw-horizontal-rule)
             (inner-template . org-fw-inner-template)
		     (italic . org-fw-italic)
		     (item . org-fw-item)
		     (line-break . org-fw-line-break)
		     (paragraph . org-fw-paragraph)
		     (plain-list . org-fw-plain-list)
		     (plain-text . org-fw-plain-text)
		     (section . org-fw-section)
		     (template . org-fw-template)
		     (verbatim . org-fw-verbatim)))



;;; Transcode Functions

;;;; Bold

(defun org-fw-bold (bold contents info)
  "Transcode BOLD object into Foswiki format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "*%s*" contents))


;;;; Headline

(defun org-fw-headline (headline contents info)
  "Transcode HEADLINE element into Foswiki format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (format "     :%s:"
				     (mapconcat 'identity tag-list ":"))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title)))
      (concat "---" (make-string level ?+) " " heading tags "\n\n" contents)
    )))


;;;; Horizontal Rule

(defun org-fw-horizontal-rule (horizontal-rule contents info)
  "Transcode HORIZONTAL-RULE element into Foswiki format.
CONTENTS is the horizontal rule contents.  INFO is a plist used
as a communication channel."
  "---")


;;;; Italic

(defun org-fw-italic (italic contents info)
  "Transcode ITALIC object into Foswiki format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "_%s_" contents))


;;;; Item

(defun org-fw-item (item contents info)
  "Transcode ITEM element into Foswiki format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
	 (struct (org-element-property :structure item))
	 (bullet (if (not (eq type 'ordered)) "*"
		   (concat (number-to-string
			    (car (last (org-list-get-item-number
					(org-element-property :begin item)
					struct
					(org-list-prevs-alist struct)
					(org-list-parents-alist struct)))))
			   "."))))
    (concat "   " bullet " "
	    (case (org-element-property :checkbox item)
	      (on "[X] ")
	      (trans "[-] ")
	      (off "[ ] "))
	    (let ((tag (org-element-property :tag item)))
	      (and tag (format "**%s:** "(org-export-data tag info))))
	    (org-trim (replace-regexp-in-string "^" "   " contents)))))


;;;; Line Break

(defun org-fw-line-break (line-break contents info)
  "Transcode LINE-BREAK object into Foswiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  "  \n")


;;;; Paragraph

(defun org-fw-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Foswiki format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-match "\\`#" first-object))
	(replace-regexp-in-string "\\`#" "\\#" contents nil t)
      contents)))


;;;; Plain List

(defun org-fw-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element into Foswiki format.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Plain Text

(defun org-fw-plain-text (text info)
  "Transcode a TEXT string into Foswiki format.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (when (plist-get info :with-smart-quotes)
    (setq text (org-export-activate-smart-quotes text :html info)))
  ;; Protect ambiguous #.  This will protect # at the beginning of
  ;; a line, but not at the beginning of a paragraph.  See
  ;; `org-fw-paragraph'.
  (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
  ;; Protect ambiguous !
  (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
  ;; Protect `, *, _ and \
  (setq text (replace-regexp-in-string "[`*_\\]" "\\\\\\&" text))
  ;; Handle special strings, if required.
  (when (plist-get info :with-special-strings)
    (setq text (org-html-convert-special-strings text)))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  text)


;;;; Section

(defun org-fw-section (section contents info)
  "Transcode SECTION element into Foswiki format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Template

(defun org-fw-inner-template (contents info)
  "Return body of document string after Foswiki conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Title
   (format "---+!!%s \n" (org-export-data (plist-get info :title) info))
   ;; Table of contents.
   (if (plist-get info :with-toc) "%TOC%\n\n")
   ;; Document contents.
   contents))


(defun org-fw-template (contents info)
  "Return complete document string after Foswiki conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)



;;; Interactive function

;;;###autoload
(defun org-fw-export-as-foswiki (&optional async subtreep visible-only)
  "Export current buffer to a Foswiki buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org FW Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'fw "*Org FW Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-fw-convert-region-to-fw ()
  "Assume the current region has org-mode syntax, and convert it to Foswiki.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in a Foswiki buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'fw))


;;;###autoload
(defun org-fw-export-to-foswiki (&optional async subtreep visible-only)
  "Export current buffer to a Foswiki file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".fw" subtreep)))
    (org-export-to-file 'fw outfile async subtreep visible-only)))


(provide 'ox-fw)

;;; ox-fw.el ends here
