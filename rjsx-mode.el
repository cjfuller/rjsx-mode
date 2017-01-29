;;; rjsx-mode.el --- major mode for editing react jsx files
;;; -*- coding: utf-8 -*-

;;; Code:
(require 'seq)

;;---- CONSTS ------------------------------------------------------------------

(defconst rjsx-mode-version "0.1.0"
  "Rjsx mode version.")

;;---- GROUPS ------------------------------------------------------------------

(defgroup rjsx-mode nil
  "Major mode for editing react jsx files."
  :group 'languages
  :prefix "web-"
  :link '(url-link :tag "Repository" "https://github.com/cjfuller/rjsx-mode"))

(defgroup rjsx-mode-faces nil
  "Faces for syntax highlighting."
  :group 'rjsx-mode
  :group 'faces)

;;---- CUSTOMS -----------------------------------------------------------------

(defcustom rjsx-mode-script-padding 1
  "Script element left padding."
  :type 'integer
  :group 'rjsx-mode)

(defcustom rjsx-mode-style-padding 1
  "Style element left padding."
  :type 'integer
  :group 'rjsx-mode)

(defcustom rjsx-mode-block-padding 0
  "Multi-line block left padding."
  :type 'integer
  :group 'rjsx-mode)

(defcustom rjsx-mode-attr-indent-offset nil
  "Html attribute indentation level."
  :type 'integer
  :safe #'integerp
  :group 'rjsx-mode)

(defcustom rjsx-mode-attr-value-indent-offset nil
  "Html attribute value indentation level."
  :type 'integer
  :safe #'integerp
  :group 'rjsx-mode)

(defcustom rjsx-mode-markup-indent-offset
  (if (and (boundp 'standard-indent) standard-indent) standard-indent 2)
  "Html indentation level."
  :type 'integer
  :safe #'integerp
  :group 'rjsx-mode)

(defcustom rjsx-mode-code-indent-offset
  (if (and (boundp 'standard-indent) standard-indent) standard-indent 2)
  "Code (javascript, etc.) indentation level."
  :type 'integer
  :safe #'integerp
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-auto-indentation (display-graphic-p)
  "Auto-indentation."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-auto-closing (display-graphic-p)
  "Auto-closing."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-auto-pairing (display-graphic-p)
  "Auto-pairing."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-auto-opening (display-graphic-p)
  "Html element auto-opening."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-auto-quoting (display-graphic-p)
  "Add double quotes after the character = in a tag."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-auto-expanding nil
  "e.g. s/ expands to <span>|</span>."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-control-block-indentation t
  "Control blocks increase indentation."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-current-element-highlight nil
  "Disable element highlight."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-current-column-highlight nil
  "Show column for current element."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-whitespace-fontification nil
  "Enable whitespaces."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-html-entities-fontification nil
  "Enable html entities fontification."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-block-face nil
  "Enable block face (useful for setting a background for example).
See rjsx-mode-block-face."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-part-face nil
  "Enable part face (useful for setting background of <style> or <script>
 elements for example). See rjsx-mode-part-face."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-inlays nil
  "Enable inlays (e.g. LaTeX) highlighting."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-sexp-functions t
  "Enable specific sexp functions."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-string-interpolation t
  "Enable string interpolation fontification."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-sql-detection nil
  "Enable fontification and indentation of sql queries in strings."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-heredoc-fontification t
  "Enable heredoc fontification. The identifier should contain JS, JAVASCRIPT, CSS or HTML."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-element-content-fontification nil
  "Enable element content fontification. The content of an element can have a face associated."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-enable-element-tag-fontification nil
  "Enable tag name fontification."
  :type 'boolean
  :group 'rjsx-mode)

(defcustom rjsx-mode-comment-style 1
  "Comment style : 1 = default, 2 = force server comments outside a block."
  :group 'rjsx-mode
  :type '(choice (const :tag "default" 1)
                 (const :tag "force engine comments" 2)))

(defcustom rjsx-mode-indent-style 2
  "Indentation style."
  :group 'rjsx-mode
  :type '(choice (const :tag "default (all lines are indented)" 2)
                 (const :tag "text at the beginning of line is not indented" 1)))

(defcustom rjsx-mode-auto-close-style 1
  "Auto-close style."
  :group 'rjsx-mode
  :type '(choice (const :tag "Auto-close on </" 1)
                 (const :tag "Auto-close on > and </" 2)))

(defcustom rjsx-mode-auto-quote-style 1
  "Auto-quoting style."
  :group 'rjsx-mode
  :type '(choice (const :tag "Auto-quotes with double quote" 1)
                 (const :tag "Auto-quotes with single quote" 2)))

(defcustom rjsx-mode-extra-expanders '()
  "A list of additional expanders."
  :type 'list
  :group 'rjsx-mode)

(defcustom rjsx-mode-extra-auto-pairs '()
  "A list of additional auto-pairs."
  :type 'list
  :group 'rjsx-mode)

(defcustom rjsx-mode-extra-builtins '()
  "A list of additional builtins."
  :type 'list
  :group 'rjsx-mode)

(defcustom rjsx-mode-extra-constants '()
  "A list of additional constants."
  :type 'list
  :group 'rjsx-mode)

(defcustom rjsx-mode-extra-keywords '()
  "A list of additional keywords."
  :type 'list
  :group 'rjsx-mode)

(defcustom rjsx-mode-extra-types '()
  "A list of additional types."
  :type 'list
  :group 'rjsx-mode)

(defcustom rjsx-mode-extra-control-blocks '()
  "A list of additional control blocks."
  :type 'list
  :group 'rjsx-mode)

(defcustom rjsx-mode-tests-directory (concat default-directory "tests/")
  "Directory containing all the unit tests."
  :type 'list
  :group 'rjsx-mode)

(defcustom rjsx-mode-jsx-depth-faces nil
;;  '(rjsx-mode-jsx-depth-1-face
;;    rjsx-mode-jsx-depth-2-face
;;    rjsx-mode-jsx-depth-3-face
;;    rjsx-mode-jsx-depth-4-face)
  "Each jsx depth has is own face."
  :type 'list
  :group 'rjsx-mode)

(defcustom rjsx-mode-commands-like-expand-region '(rjsx-mode-mark-and-expand
                                                  er/expand-region
                                                  mc/mark-next-like-this)
  "Add it to here if you have some wrapper function for er/expand-region"
  :type '(repeat function)
  :group 'rjsx-mode)

;;---- FACES -------------------------------------------------------------------

(defface rjsx-mode-error-face
  '((t :background "red"))
  "Face for warning."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-warning-face
  '((t :inherit font-lock-warning-face))
  "Face for warning."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-preprocessor-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for preprocessor."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-block-delimiter-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for block delimiters."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-block-control-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for preprocessor."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for builtins."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-symbol-face
  '((t :foreground "goldenrod2"))
  "Face for symbols."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-html-tag-face
  '((((class color) (min-colors 88) (background dark))  :foreground "Snow4")
    (((class color) (min-colors 88) (background light)) :foreground "Snow4")
    (((class color) (min-colors 16) (background dark))  :foreground "Snow4")
    (((class color) (min-colors 16) (background light)) :foreground "Grey15")
    (((class color) (min-colors 8))                     :foreground "Snow4")
    (((type tty) (class mono))                          :inverse-video t)
    (t                                                  :foreground "Snow4"))
  "Face for html tags."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-html-tag-custom-face
  '((t :inherit rjsx-mode-html-tag-face))
  "Face for html custom tags (e.g. <polymer-element>)."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-html-tag-namespaced-face
  '((t :inherit rjsx-mode-block-control-face))
  "Face for html namespaced tags (e.g. <c:forEach>)."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-html-tag-bracket-face
  '((((class color) (min-colors 88) (background dark))  :foreground "Snow3")
    (((class color) (min-colors 88) (background light)) :foreground "Grey14")
    (((class color) (min-colors 16) (background dark))  :foreground "Snow3")
    (((class color) (min-colors 16) (background light)) :foreground "Grey14")
    (((class color) (min-colors 8))                     :foreground "Snow3")
    (((type tty) (class mono))                          :inverse-video t)
    (t                                                  :foreground "Snow3"))
  "Face for html tags angle brackets (<, > and />)."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-html-attr-name-face
  '((((class color) (min-colors 88) (background dark))  :foreground "Snow3")
    (((class color) (min-colors 88) (background light)) :foreground "Snow4")
    (((class color) (min-colors 16) (background dark))  :foreground "Snow3")
    (((class color) (min-colors 16) (background light)) :foreground "Grey13")
    (((class color) (min-colors 8))                     :foreground "Snow3")
    (((type tty) (class mono))                          :inverse-video t)
    (t                                                  :foreground "Snow4"))
  "Face for html attribute names."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-html-attr-custom-face
  '((t :inherit rjsx-mode-html-attr-name-face))
  "Face for custom attribute names (e.g. data-*)."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-html-attr-engine-face
  '((t :inherit rjsx-mode-block-delimiter-face))
  "Face for custom engine attribute names (e.g. ng-*)."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-html-attr-equal-face
  '((t :inherit rjsx-mode-html-attr-name-face))
  "Face for the = character between name and value."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-html-attr-value-face
  '((t :inherit font-lock-string-face))
  "Face for html attribute values."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-block-attr-name-face
  '((t :foreground "#8fbc8f"))
  "Face for block attribute names."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-block-attr-value-face
  '((t :foreground "#5f9ea0"))
  "Face for block attribute values."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variable names."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-filter-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-function-call-face
  '((t :inherit font-lock-function-name-face))
  "Face for function calls."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-string-face
  '((t :inherit font-lock-string-face))
  "Face for strings."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-block-string-face
  '((t :inherit rjsx-mode-string-face))
  "Face for block strings."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-part-string-face
  '((t :inherit rjsx-mode-string-face))
  "Face for part strings."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-javascript-string-face
  '((t :inherit rjsx-mode-string-face))
  "Face for javascript strings."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-json-key-face
  '((t :foreground "plum"))
  "Face for json key strings."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-json-context-face
  '((t :foreground "orchid3"))
  "Face for json context strings."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-json-string-face
  '((t :inherit rjsx-mode-string-face))
  "Face for json strings."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-block-comment-face
  '((t :inherit rjsx-mode-comment-face))
  "Face for server comments."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-part-comment-face
  '((t :inherit rjsx-mode-comment-face))
  "Face for part comments."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-json-comment-face
  '((t :inherit rjsx-mode-comment-face))
  "Face for json comments."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-javascript-comment-face
  '((t :inherit rjsx-mode-comment-face))
  "Face for javascript comments."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for language constants."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-type-face
  '((t :inherit font-lock-type-face))
  "Face for language types."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for language keywords."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-param-name-face
  '((t :foreground "Snow3"))
  "Face for server attribute names."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-whitespace-face
  '((t :background "DarkOrchid4"))
  "Face for whitespaces."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-inlay-face
  '((((class color) (min-colors 88) (background dark))  :background "Black")
    (((class color) (min-colors 88) (background light)) :background "LightYellow1")
    (((class color) (min-colors 16) (background dark))  :background "Brey18")
    (((class color) (min-colors 16) (background light)) :background "LightYellow1")
    (((class color) (min-colors 8))                     :background "Black")
    (((type tty) (class mono))                          :inverse-video t)
    (t                                                  :background "Grey"))
  "Face for inlays. Must be used in conjunction with rjsx-mode-enable-inlays."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-block-face
  '((((class color) (min-colors 88) (background dark))  :background "Black")
    (((class color) (min-colors 88) (background light)) :background "LightYellow1")
    (((class color) (min-colors 16) (background dark))  :background "Grey18")
    (((class color) (min-colors 16) (background light)) :background "LightYellow1")
    (((class color) (min-colors 8))                     :background "Black")
    (((type tty) (class mono))                          :inverse-video t)
    (t                                                  :background "Grey"))
  "Face for blocks (useful for setting a background for example).
Must be used in conjunction with rjsx-mode-enable-block-face."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-part-face
  '((t :inherit rjsx-mode-block-face))
  "Face for parts."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-script-face
  '((t :inherit rjsx-mode-part-face))
  "Face for javascript inside a script element."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-folded-face
  '((t :underline t))
  "Overlay face for folded."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-bold-face
  '((t :weight bold))
  "bold face."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-italic-face
  '((t :slant italic))
  "bold face."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-underline-face
  '((t :underline t))
  "bold face."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-current-element-highlight-face
  '((t :background "#000000"))
  "Overlay face for element highlight."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-current-column-highlight-face
  '((t :background "#3e3c36"))
  "Overlay face for current column."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-comment-keyword-face
  '((t :weight bold :box t))
  "Comment keywords."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-html-entity-face
  '((t :slant italic))
  "Face html entities (e.g. &#8211;, &eacute;)."
  :group 'rjsx-mode-faces)

(defface rjsx-mode-jsx-depth-1-face
  '((t :background "#333333"))
  "jsx depth 1"
  :group 'rjsx-mode-faces)

(defface rjsx-mode-jsx-depth-2-face
  '((t :background "#222222"))
  "jsx"
  :group 'rjsx-mode-faces)

(defface rjsx-mode-jsx-depth-3-face
  '((t :background "#111111"))
  "jsx"
  :group 'rjsx-mode-faces)

(defface rjsx-mode-jsx-depth-4-face
  '((t :background "#000000"))
  "jsx"
  :group 'rjsx-mode-faces)

;;---- VARS --------------------------------------------------------------------

(defvar font-lock-beg)
(defvar font-lock-end)

(defvar rjsx-mode-debug nil)

(defvar rjsx-mode-auto-pairs nil)
(defvar rjsx-mode-block-regexp nil)
(defvar rjsx-mode-change-beg nil)
(defvar rjsx-mode-change-end nil)
(defvar rjsx-mode-chunk-length 64)
(defvar rjsx-mode-column-overlays nil)
(defvar rjsx-mode-comments-invisible nil)
(defvar rjsx-mode-content-type "")
(defvar rjsx-mode-engine nil)
(defvar rjsx-mode-engine-attr-regexp nil)
(defvar rjsx-mode-engine-font-lock-keywords nil)
(defvar rjsx-mode-engine-token-regexp nil)
(defvar rjsx-mode-expand-initial-pos nil)
(defvar rjsx-mode-expand-initial-scroll nil)
(defvar rjsx-mode-expand-previous-state "")
(defvar rjsx-mode-font-lock-keywords '(rjsx-mode-font-lock-highlight))
(defvar rjsx-mode-inhibit-fontification nil)
(defvar rjsx-mode-inlay-regexp nil)
(defvar rjsx-mode-is-scratch nil)
(defvar rjsx-mode-minor-engine nil)
(defvar rjsx-mode-obarray nil)
(defvar rjsx-mode-overlay-tag-start nil)
(defvar rjsx-mode-overlay-tag-end nil)
(defvar rjsx-mode-time (current-time))

(defvar rjsx-mode-indentless-elements
  '("code" "pre" "textarea"))

(defvar rjsx-mode-indentless-attributes
  '("onclick" "onmouseover" "onmouseout" "onsubmit"))

(defvar rjsx-mode-void-elements
  '("area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen"
    "link" "meta" "param" "source" "track" "wbr"))

(defvar rjsx-mode-part-content-types '("javascript" "json" "jsx"))

(defvar rjsx-mode-javascript-languages '("javascript" "jsx"))

;; NOTE: without 'syntax-table forward-word fails (#377)
(defvar rjsx-mode-scan-properties
  (list 'tag-beg 'tag-end 'tag-name 'tag-type
        'tag-attr 'tag-attr-beg 'tag-attr-end
        'part-side 'part-token
        'jsx-beg 'jsx-end 'jsx-depth
        'block-side 'block-token 'block-controls 'block-beg 'block-end
        'syntax-table)
  "Text properties used for code regions/tokens and html nodes.")

(defvar rjsx-mode-start-tag-regexp "<\\([[:alpha:]][[:alnum:]:-]*\\)"
  "Regular expression for HTML/XML start tag.")

(defvar rjsx-mode-whitespaces-regexp
  "^[ \t]\\{2,\\}$\\| \t\\|\t \\|[ \t]+$\\|^[ \n\t]+\\'\\|^[ \t]?[\n]\\{2,\\}"
  "Regular expression for whitespaces.")

(defvar rjsx-mode-imenu-regexp-list
  '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">")
    ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 "id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">"))
  "Regexps to match imenu items (see http://rjsx-mode.org/doc/imenu.txt)")

(defvar rjsx-mode-indentation-params
  '(("lineup-args"       . t)
    ("lineup-calls"      . t)
    ("lineup-concats"    . t)
    ("lineup-quotes"     . t)
    ("lineup-ternary"    . t)
    ("case-extra-offset" . t)
    ))

(defvar rjsx-mode-engines
  ;; TODO(colin): remove
  nil
  "Engine name aliases")

(defvar rjsx-mode-content-types
  '(("javascript" . "\\.\\([jt]s\\|[jt]s\\.erb\\)\\'")
    ("json"       . "\\.\\(api\\|json\\|jsonld\\)\\'")
    ("jsx"        . "\\.[jt]sx\\'")
    )
  "content types")

(defvar rjsx-mode-last-enabled-feature nil)

(defvar rjsx-mode-features
  '(("css-colorization"          . rjsx-mode-enable-css-colorization)
    ("element-highlight"         . rjsx-mode-enable-current-element-highlight)
    ("column-highlight"          . rjsx-mode-enable-current-column-highlight)
    ("whitespace-fontification"  . rjsx-mode-enable-whitespace-fontification)
    ("element-tag-fontification" . rjsx-mode-enable-element-tag-fontification)
    ("block-face"                . rjsx-mode-enable-block-face)
    ("part-face"                 . rjsx-mode-enable-part-face)))

(defvar rjsx-mode-comment-prefixing t)

(defvar rjsx-mode-comment-formats
  '(("java"       . "/*")
    ("javascript" . "/*")
    ))

(defvar rjsx-mode-smart-quotes
  '("«" . "»")
  "Preferred smart quotes")

(defvar rjsx-mode-xml-chars
  '((?\& . "&amp;")
    (?\< . "&lt;")
    (?\> . "&gt;"))
  "XML chars")

(defvar rjsx-mode-html-entities
  '(("AElig" . 198) ("Aacute" . 193) ("Acirc" . 194) ("Agrave" . 192)
    ("Alpha" . 913) ("Aring" . 197) ("Atilde" . 195) ("Auml" . 196)
    ("Beta" . 914)
    ("Ccedil" . 199) ("Chi" . 935)
    ("Dagger" . 8225) ("Delta" . 916)
    ("ETH" . 208) ("Eacute" . 201) ("Ecirc" . 202) ("Egrave" . 200)
    ("Epsilon" . 917) ("Eta" . 919) ("Euml" . 203)
    ("Gamma" . 915)
    ("Iacute" . 205) ("Icirc" . 206) ("Igrave" . 204) ("Iota" . 921)
    ("Iuml" . 207)
    ("Kappa" . 922)
    ("Lambda" . 923)
    ("Mu" . 924)
    ("Ntilde" . 209) ("Nu" . 925)
    ("OElig" . 338) ("Oacute" . 211) ("Ocirc" . 212) ("Ograve" . 210)
    ("Omega" . 937) ("Omicron" . 927) ("Oslash" . 216) ("Otilde" . 213)
    ("Ouml" . 214)
    ("Phi" . 934) ("Pi" . 928) ("Prime" . 8243) ("Psi" . 936)
    ("Rho" . 929)
    ("Scaron" . 352) ("Sigma" . 931)
    ("THORN" . 222) ("Tau" . 932) ("Theta" . 920)
    ("UArr" . 8657) ("Uacute" . 218) ("Uacute" . 250) ("Ucirc" . 219)
    ("Ugrave" . 217)  ("Upsih" . 978)
    ("Upsilon" . 933) ("Uuml" . 220) ("Uuml" . 252)
    ("Xi" . 926)
    ("Yacute" . 221) ("Yuml" . 376)
    ("Zeta" . 918)
    ("aacute" . 225) ("acirc" . 226) ("acute" . 180) ("aelig" . 230)
    ("agrave" . 224) ("alefsym" . 8501) ("alpha" . 945) ("amp" . 38)
    ("ang" . 8736) ("apos" . 39) ("aring" . 229) ("asymp" . 8776)
    ("atilde" . 227) ("auml" . 228)
    ("bdquo" . 8222) ("beta" . 946) ("brvbar" . 166) ("bull" . 8226)
    ("cap" . 8745) ("ccedil" . 231) ("cedil" . 184) ("cent" . 162)
    ("chi" . 967) ("circ" . 710) ("clubs" . 9827) ("cong" . 8773)
    ("copy" . 169) ("crarr"  . 8629) ("cup" . 8746) ("curren" . 164)
    ("dArr" . 8659) ("dagger" . 8224) ("darr" . 8595) ("deg" . 176)
    ("delta" . 948) ("diams" . 9830) ("divide" . 247)
    ("eacute" . 233) ("ecirc"  . 234) ("egrave" . 232) ("empty" . 8709)
    ("emsp" . 8195) ("ensp" . 8194) ("epsilon" . 949) ("equiv" . 8801)
    ("eta" . 951) ("eth" . 240) ("euml" . 235) ("euro" . 8364) ("exist" . 8707)
    ("fnof" . 402) ("forall" . 8704) ("frac12" . 189) ("frac14" . 188)
    ("frac34" . 190) ("frasl" . 8260)
    ("gamma" . 947) ("ge" . 8805) ("gt" . 62)
    ("hArr" . 8660) ("harr" . 8596) ("hearts" . 9829) ("hellip" . 8230)
    ("iacute" . 237) ("icirc" . 238) ("iexcl" . 161) ("igrave" . 236)
    ("image" . 8465) ("infin" . 8734) ("int" . 8747) ("iota" . 953)
    ("iquest" . 191) ("isin" . 8712) ("iuml" . 239)
    ("kappa" . 954)
    ("lArr" . 8656) ("lambda" . 955) ("lang" . 9001) ("laquo" . 171)
    ("larr" . 8592) ("lceil" . 8968) ("ldquo" . 8220) ("le" . 8804)
    ("lfloor" . 8970) ("lowast" . 8727) ("loz" . 9674) ("lrm" . 8206)
    ("lsaquo" . 8249) ("lsquo" . 8249) ("lt" . 60)
    ("macr" . 175) ("mdash" . 8212) ("micro" . 181) ("middot" . 183)
    ("minus" . 8722) ("mu" . 956)
    ("nabla" . 8711) ("nbsp" . 160) ("ndash" . 8211) ("ne" . 8800)
    ("ni" . 8715) ("not" . 172) ("notin" . 8713) ("nsub" . 8836)
    ("ntilde" . 241) ("nu" . 957) ("oacute" . 243) ("ocirc" . 244)
    ("oelig" . 339) ("ograve" . 242) ("oline" . 8254) ("omega" . 969)
    ("omicron" . 959) ("oplus" . 8853) ("or" . 8744) ("ordf" . 170)
    ("ordm" . 186) ("oslash" . 248) ("otilde" . 245) ("otimes" . 8855)
    ("ouml" . 246)
    ("para" . 182) ("part" . 8706) ("permil" . 8240) ("perp" . 8869)
    ("phi" . 966) ("pi" . 960) ("piv" . 982) ("plusmn" . 177) ("pound" . 163)
    ("prime" . 8242) ("prod" . 8719) ("prop" . 8733) ("psi" . 968)
    ("quot" . 34)
    ("rArr" . 8658) ("radic" . 8730) ("rang" . 9002) ("raquo" . 187)
    ("rarr" . 8594) ("rceil" . 8969) ("rdquo" . 8221) ("real" . 8476)
    ("reg" . 174) ("rfloor" . 8971) ("rho" . 961) ("rlm" . 8207)
    ("rsaquo" . 8250) ("rsquo" . 8250) ("sbquo" . 8218)
    ("scaron" . 353) ("sdot" . 8901) ("sect" . 167) ("shy" . 173)
    ("sigma" . 963) ("sigmaf" . 962) ("sim" . 8764) ("spades" . 9824)
    ("sub" . 8834) ("sube" . 8838) ("sum" . 8721) ("sup" . 8835)
    ("sup1" . 185) ("sup2" . 178) ("sup3" . 179) ("supe" . 8839)
    ("szlig" . 223)
    ("tau" . 964) ("there4" . 8756) ("theta" . 952) ("thetasym" . 977)
    ("thinsp" . 8201) ("thorn" . 254) ("tilde" . 732) ("times" . 215)
    ("trade" . 8482)
    ("uarr" . 8593) ("ucirc" . 251) ("ugrave" . 249) ("uml" . 168)
    ("upsilon" . 965)
    ("weierp" . 8472)
    ("xi" . 958)
    ("yacute" . 253) ("yen" . 165) ("yuml" . 255)
    ("zeta" . 950) ("zwj" . 8205) ("zwnj" . 8204)))

;; http://webdesign.about.com/od/localization/l/blhtmlcodes-ascii.htm
(defvar rjsx-mode-display-table
  (let ((table (make-display-table)))
    (aset table 9  (vector ?\xBB ?\t))
    (aset table 10 (vector ?\xB6 ?\n))
    (aset table 32 (vector ?\xB7))
    table)
  "Display table used when switching to the whitespace visualization.")

(defvar rjsx-mode-expanders
  '(("a/" . "<a href=\"|\"></a>")
    ("b/" . "<table><tbody><tr><td>|</td><td></td></tr></tbody></table>")
    ("c/" . "<div class=\"|\"></div>")
    ("d/" . "<div>|</div>")
    ("e/" . "<em>|</em>")
    ("f/" . "<form>|</form>")
    ("g/" . "<strong>|</strong>")
    ("h/" . "<h1>|</h1>")
    ("i/" . "<img src=\"|\" />")
    ("j/" . "<script>|</script>")
    ("l/" . "<li>|</li>")
    ("m/" . "<main>|</main>")
    ("n/" . "<input type=\"|\" />")
    ("p/" . "<p>|</p>")
    ("q/" . "<quote>|</quote>")
    ("s/" . "<span>|</span>")
    ("t/" . "<td>|</td>")
    ("u/" . "<ul><li>|</li><li></li></ul>")
    ("x/" . "<textarea>|</textarea>")
    ("2/" . "<h2>|</h2>")
    ("3/" . "<h3>|</h3>")
    ))

(defvar rjsx-mode-normalization-rules
  '(("tag-case"          . "lower-case")
    ("attr-case"         . "lower-case")
    ("special-chars"     . "unicode") ;"unicode" "entities"
    ("css-indentation"   . t)
    ("smart-apostrophes" . t)
    ("smart-quotes"      . t)
    ("whitespaces"       . t)
    ("indentation"       . t))
  "Normalization rules")

(defvar rjsx-mode-element-tag-faces
  (list
   '("h1"     . rjsx-mode-underline-face)
   '("h2"     . rjsx-mode-underline-face)
   '("h3"     . rjsx-mode-underline-face)
   '("h4"     . rjsx-mode-underline-face)
   '("title"  . rjsx-mode-underline-face)
   '("em"     . rjsx-mode-italic-face)
   '("strong" . rjsx-mode-bold-face)
   ))

(defvar rjsx-mode-element-content-faces
  (list
   '("h1"     . rjsx-mode-underline-face)
   '("h2"     . rjsx-mode-underline-face)
   '("h3"     . rjsx-mode-underline-face)
   '("h4"     . rjsx-mode-underline-face)
   '("title"  . rjsx-mode-underline-face)
   '("em"     . rjsx-mode-italic-face)
   '("strong" . rjsx-mode-bold-face)
   ))

(defvar rjsx-mode-comment-keywords
  (regexp-opt
   (append
    (cdr (assoc "comment" rjsx-mode-extra-keywords))
    '("FIXME" "TODO" "BUG" "KLUDGE" "WORKAROUND" "OPTIMIZE" "HACK" "REFACTOR" "REVIEW"))))

(defvar rjsx-mode-links
  '(("\\.\\(png\\|jpe?g\\|gif\\|webp\\)$" "<img src=\"%s\" alt=\"\" />" nil 4)
    ("\\.svg$" "<object data=\"%s\" type=\"image/svg+xml\"></object>" nil 0)
    ("\\.js$" "<script type=\"text/javascript\" src=\"%s\"></script>" t 0)
    ("\\.html?$" "<a href=\"%s\"></a>" nil 4))
  "List of elements and extensions for `rjsx-mode-file-link'. It
consists of a string that contains the regular expression that
matches the appropriate files, a format string with element that
contains the link (%s should be put where the path goes,) a bool
that tells if the element belongs in the <head> element, and
number of characters to move back if needed (or 0 if point
shouldn't be moved back.)")

(defvar rjsx-mode-directives
  (eval-when-compile
    (regexp-opt
     '("include" "page" "taglib"
       "Assembly" "Control" "Implements" "Import"
       "Master" "OutputCache" "Page" "Reference" "Register"))))

(defvar rjsx-mode-javascript-keywords
  (regexp-opt
   (append
    (cdr (assoc "javascript" rjsx-mode-extra-keywords))
    '("as" "async" "await" "break" "case" "catch" "class" "const" "continue"
      "debugger" "default" "delete" "do" "else" "enum" "eval"
      "export" "extends" "finally" "for" "from" "function" "get" "if"
      "implements" "import" "in" "instanceof" "interface" "let"
      "new" "of" "package" "private" "protected" "public"
      "return" "set" "static" "super" "switch"
      "throw" "try" "typeof" "var" "void" "while" "with" "yield"))))

(defvar rjsx-mode-javascript-constants
  (regexp-opt
   '("false" "null" "undefined" "Infinity" "NaN" "true" "arguments" "this")))

(defvar rjsx-mode-selector-font-lock-keywords
  nil) ;; TODO(colin): remove

(defvar rjsx-mode-declaration-font-lock-keywords
  (list
   '("--[[:alnum:]-]+" 0 'rjsx-mode-css-variable-face)
   '("$[[:alnum:]-]+" 0 'rjsx-mode-css-variable-face)
   '("\\([[:alpha:]-]+\\)[ ]?:" 0 'rjsx-mode-css-property-name-face)
   '("\\([[:alpha:]-]+\\)[ ]?(" 1 'rjsx-mode-css-function-face)
   '("#[[:alnum:]]\\{1,6\\}" 0 'rjsx-mode-css-color-face t t)
   '("![ ]?important" 0 'rjsx-mode-css-priority-face t t)
   '("\\([^,]+\\)[ ]+{" 1 'rjsx-mode-css-selector-face)
   '("'[^']*'\\|\"[^\"]*\"" 0 'rjsx-mode-string-face t t)
   ))

(defvar rjsx-mode-html-font-lock-keywords
  (list
   '("</?[[:alnum:]]+[ >]\\|>" 0 'rjsx-mode-html-tag-face t)
   '(" \\([[:alnum:]-]+=\\)\\(\"[^\"]+\"\\)"
     (1 'rjsx-mode-html-attr-name-face)
     (2 'rjsx-mode-html-attr-value-face))
   ))

;; voir https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
(defvar rjsx-mode-javascript-font-lock-keywords
  (list
   '("@\\([[:alnum:]_]+\\)\\_>" 0 'rjsx-mode-keyword-face)
   (cons (concat "\\_<\\(function\\*\\)\\_>") '(1 'rjsx-mode-keyword-face))
   (cons (concat "\\([ \t}{(]\\|^\\)\\(" rjsx-mode-javascript-keywords "\\)\\_>") '(2 'rjsx-mode-keyword-face))
   (cons (concat "\\_<\\(" rjsx-mode-javascript-constants "\\)\\_>") '(0 'rjsx-mode-constant-face))
   '("\\_<\\(new\\|instanceof\\|class\\|extends\\) \\([[:alnum:]_.]+\\)\\_>" 2 'rjsx-mode-type-face)
   '("\\_<\\([[:alnum:]_]+\\):[ ]*function[ ]*(" 1 'rjsx-mode-function-name-face)
   '("\\_<\\(function\\|get\\|set\\)[ ]+\\([[:alnum:]_]+\\)"
     (1 'rjsx-mode-keyword-face)
     (2 'rjsx-mode-function-name-face))
   '("([ ]*\\([[:alnum:]_]+\\)[ ]*=>" 1 'rjsx-mode-function-name-face)
   '("[ ]*\\([[:alnum:]_]+\\)[ ]*=[ ]*([^)]*)[ ]*=>[ ]*{" 1 'rjsx-mode-function-name-face)
   '("\\_<\\(var\\|let\\|const\\)[ ]+\\([[:alnum:]_]+\\)" 2 'rjsx-mode-variable-name-face)
   '("({" "\\([[:alnum:]_]+\\)[, }]+" nil nil (1 'rjsx-mode-variable-name-face)) ;#738
   '("\\([[:alnum:]_]+\\)[ ]*=> [{(]" 1 'rjsx-mode-variable-name-face)
   '("\\(function\\|[,=]\\|^\\)[ ]*("
     ("\\([[:alnum:]_]+\\)\\([ ]*=[^,)]*\\)?[,)]" nil nil (1 'rjsx-mode-variable-name-face)))
   '("\\([[:alnum:]_]+\\):" 1 'rjsx-mode-variable-name-face)
   '("\\_<\\([[:alnum:]_-]+\\)[ ]?(" 1 'rjsx-mode-function-call-face)
   ))

(defvar rjsx-mode-html-tag-font-lock-keywords
  (list
   '("\\(</?\\)\\([[:alnum:]]+\\)"
     (1 'rjsx-mode-html-tag-bracket-face)
     (2 'rjsx-mode-html-tag-face))
   '("\"[^\"]*\"" 0 'rjsx-mode-html-attr-value-face)
   '("\\([[:alnum:]]+\\)" 1 'rjsx-mode-html-attr-name-face)
   '("/?>" 0 'rjsx-mode-html-tag-bracket-face)
  ))

(defvar rjsx-mode-expression-font-lock-keywords
  (list
   '("[[:alpha:]_]" 0 'rjsx-mode-variable-name-face)
   ))

(defvar rjsx-mode-ejs-font-lock-keywords
  rjsx-mode-javascript-font-lock-keywords)

(defvar rjsx-mode-before-auto-complete-hooks nil
  "List of functions to run before triggering the auto-complete library.

Auto-complete sources will sometimes need some tweaking to work
nicely with rjsx-mode. This hook gives users the chance to adjust
the environment as needed for ac-sources, right before they're used.")

(defvar rjsx-mode-ignore-ac-start-advice nil
  "If not nil 'defadvice' for 'ac-start' will be ignored.

Can be set inside a hook in 'rjsx-mode-before-auto-complete-hooks' to
non nil to ignore the defadvice which sets ac-sources according to current
language. This is needed if the corresponding auto-completion triggers
another auto-completion with different ac-sources")

(defvar rjsx-mode-ac-sources-alist nil
  "alist mapping language names to ac-sources for that language.")

(defvar rjsx-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?_ "_" table) ;#563
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    table)
  "Syntax table used to reveal whitespaces.")

(defvar rjsx-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map [menu-bar wm]             (cons "Rjsx-Mode" (make-sparse-keymap)))
    (define-key map [menu-bar wm dom]         (cons "Dom" (make-sparse-keymap)))
    (define-key map [menu-bar wm blk]         (cons "Block" (make-sparse-keymap)))
    (define-key map [menu-bar wm attr]        (cons "Html Attr" (make-sparse-keymap)))
    (define-key map [menu-bar wm tag]         (cons "Html Tag" (make-sparse-keymap)))
    (define-key map [menu-bar wm elt]         (cons "Html Element" (make-sparse-keymap)))

    (define-key map [menu-bar wm sep-1]       '(menu-item "--"))

    (define-key map [menu-bar wm dom dom-xpa] '(menu-item "XPath" rjsx-mode-dom-xpath))
    (define-key map [menu-bar wm dom dom-tra] '(menu-item "Traverse" rjsx-mode-dom-traverse))
    (define-key map [menu-bar wm dom dom-err] '(menu-item "Show error(s)" rjsx-mode-dom-errors-show))
    (define-key map [menu-bar wm dom dom-ent] '(menu-item "Replace html entities" rjsx-mode-dom-entities-replace))
    (define-key map [menu-bar wm dom dom-quo] '(menu-item "Replace dumb quotes" rjsx-mode-dom-quotes-replace))
    (define-key map [menu-bar wm dom dom-apo] '(menu-item "Replace apostrophes" rjsx-mode-dom-apostrophes-replace))
    (define-key map [menu-bar wm dom dom-nor] '(menu-item "Normalise" rjsx-mode-dom-normalize))

    (define-key map [menu-bar wm blk blk-sel] '(menu-item "Select" rjsx-mode-block-select))
    (define-key map [menu-bar wm blk blk-pre] '(menu-item "Previous" rjsx-mode-block-previous))
    (define-key map [menu-bar wm blk blk-nex] '(menu-item "Next" rjsx-mode-block-next))
    (define-key map [menu-bar wm blk blk-kil] '(menu-item "Kill" rjsx-mode-block-kill))
    (define-key map [menu-bar wm blk blk-end] '(menu-item "End" rjsx-mode-block-end))
    (define-key map [menu-bar wm blk blk-clo] '(menu-item "Close" rjsx-mode-block-close))
    (define-key map [menu-bar wm blk blk-beg] '(menu-item "Beginning" rjsx-mode-block-beginning))

    (define-key map [menu-bar wm attr attr-ins] '(menu-item "Insert" rjsx-mode-attribute-insert))
    (define-key map [menu-bar wm attr attr-end] '(menu-item "End" rjsx-mode-attribute-end))
    (define-key map [menu-bar wm attr attr-beg] '(menu-item "Beginning" rjsx-mode-attribute-beginning))
    (define-key map [menu-bar wm attr attr-sel] '(menu-item "Select" rjsx-mode-attribute-select))
    (define-key map [menu-bar wm attr attr-kil] '(menu-item "Kill" rjsx-mode-attribute-kill))
    (define-key map [menu-bar wm attr attr-nex] '(menu-item "Next" rjsx-mode-attribute-next))
    (define-key map [menu-bar wm attr attr-pre] '(menu-item "Previous" rjsx-mode-attribute-previous))
    (define-key map [menu-bar wm attr attr-tra] '(menu-item "Transpose" rjsx-mode-attribute-transpose))

    (define-key map [menu-bar wm tag tag-beg] '(menu-item "Sort Attributes" rjsx-mode-tag-attributes-sort))
    (define-key map [menu-bar wm tag tag-sel] '(menu-item "Select" rjsx-mode-tag-select))
    (define-key map [menu-bar wm tag tag-pre] '(menu-item "Previous" rjsx-mode-tag-previous))
    (define-key map [menu-bar wm tag tag-nex] '(menu-item "Next" rjsx-mode-tag-next))
    (define-key map [menu-bar wm tag tag-end] '(menu-item "End" rjsx-mode-tag-end))
    (define-key map [menu-bar wm tag tag-beg] '(menu-item "Beginning" rjsx-mode-tag-beginning))

    (define-key map [menu-bar wm elt elt-wra] '(menu-item "Wrap" rjsx-mode-element-wrap))
    (define-key map [menu-bar wm elt elt-van] '(menu-item "Vanish" rjsx-mode-element-vanish))
    (define-key map [menu-bar wm elt elt-exc] '(menu-item "Transpose" rjsx-mode-element-transpose))
    (define-key map [menu-bar wm elt elt-sel] '(menu-item "Select" rjsx-mode-element-select))
    (define-key map [menu-bar wm elt elt-ren] '(menu-item "Rename" rjsx-mode-element-rename))
    (define-key map [menu-bar wm elt elt-pre] '(menu-item "Previous" rjsx-mode-element-previous))
    (define-key map [menu-bar wm elt elt-par] '(menu-item "Parent" rjsx-mode-element-parent))
    (define-key map [menu-bar wm elt elt-nex] '(menu-item "Next" rjsx-mode-element-next))
    (define-key map [menu-bar wm elt elt-mut] '(menu-item "Mute blanks" rjsx-mode-element-mute-blanks))
    (define-key map [menu-bar wm elt elt-del] '(menu-item "Kill" rjsx-mode-element-kill))
    (define-key map [menu-bar wm elt elt-end] '(menu-item "End" rjsx-mode-element-end))
    (define-key map [menu-bar wm elt elt-inn] '(menu-item "Content (select)" rjsx-mode-element-content-select))
    (define-key map [menu-bar wm elt elt-clo] '(menu-item "Close" rjsx-mode-element-close))
    (define-key map [menu-bar wm elt elt-ins] '(menu-item "Insert" rjsx-mode-element-insert))
    (define-key map [menu-bar wm elt elt-dup] '(menu-item "Clone" rjsx-mode-element-clone))
    (define-key map [menu-bar wm elt elt-cfo] '(menu-item "Children fold" rjsx-mode-element-children-fold-or-unfold))
    (define-key map [menu-bar wm elt elt-chi] '(menu-item "Child" rjsx-mode-element-child))
    (define-key map [menu-bar wm elt elt-beg] '(menu-item "Beginning" rjsx-mode-element-beginning))

    (define-key map [menu-bar wm fol]         '(menu-item "Fold/Unfold" rjsx-mode-fold-or-unfold))
    (define-key map [menu-bar wm hig]         '(menu-item "Highlight buffer" rjsx-mode-buffer-highlight))
    (define-key map [menu-bar wm ind]         '(menu-item "Indent buffer" rjsx-mode-buffer-indent))
    (define-key map [menu-bar wm nav]         '(menu-item "Tag/Block navigation" rjsx-mode-navigate))
    (define-key map [menu-bar wm exp]         '(menu-item "Mark and Expand" rjsx-mode-mark-and-expand))
    (define-key map [menu-bar wm spa]         '(menu-item "Toggle whitespaces" rjsx-mode-whitespaces-show))

    ;;--------------------------------------------------------------------------
    ;; "C-c <LETTER>" are reserved for users

    (define-key map (kbd "C-c C-a b") 'rjsx-mode-attribute-beginning)
    (define-key map (kbd "C-c C-a e") 'rjsx-mode-attribute-end)
    (define-key map (kbd "C-c C-a i") 'rjsx-mode-attribute-insert)
    (define-key map (kbd "C-c C-a n") 'rjsx-mode-attribute-next)
    (define-key map (kbd "C-c C-a s") 'rjsx-mode-attribute-select)
    (define-key map (kbd "C-c C-a k") 'rjsx-mode-attribute-kill)
    (define-key map (kbd "C-c C-a p") 'rjsx-mode-attribute-previous)
    (define-key map (kbd "C-c C-a t") 'rjsx-mode-attribute-transpose)

    (define-key map (kbd "C-c C-b b") 'rjsx-mode-block-beginning)
    (define-key map (kbd "C-c C-b c") 'rjsx-mode-block-close)
    (define-key map (kbd "C-c C-b e") 'rjsx-mode-block-end)
    (define-key map (kbd "C-c C-b k") 'rjsx-mode-block-kill)
    (define-key map (kbd "C-c C-b n") 'rjsx-mode-block-next)
    (define-key map (kbd "C-c C-b p") 'rjsx-mode-block-previous)
    (define-key map (kbd "C-c C-b s") 'rjsx-mode-block-select)

    (define-key map (kbd "C-c C-d a") 'rjsx-mode-dom-apostrophes-replace)
    (define-key map (kbd "C-c C-d d") 'rjsx-mode-dom-errors-show)
    (define-key map (kbd "C-c C-d e") 'rjsx-mode-dom-entities-replace)
    (define-key map (kbd "C-c C-d n") 'rjsx-mode-dom-normalize)
    (define-key map (kbd "C-c C-d q") 'rjsx-mode-dom-quotes-replace)
    (define-key map (kbd "C-c C-d t") 'rjsx-mode-dom-traverse)
    (define-key map (kbd "C-c C-d x") 'rjsx-mode-dom-xpath)

    (define-key map (kbd "C-c C-e /") 'rjsx-mode-element-close)
    (define-key map (kbd "C-c C-e a") 'rjsx-mode-element-content-select)
    (define-key map (kbd "C-c C-e b") 'rjsx-mode-element-beginning)
    (define-key map (kbd "C-c C-e c") 'rjsx-mode-element-clone)
    (define-key map (kbd "C-c C-e d") 'rjsx-mode-element-child)
    (define-key map (kbd "C-c C-e e") 'rjsx-mode-element-end)
    (define-key map (kbd "C-c C-e f") 'rjsx-mode-element-children-fold-or-unfold)
    (define-key map (kbd "C-c C-e i") 'rjsx-mode-element-insert)
    (define-key map (kbd "C-c C-e k") 'rjsx-mode-element-kill)
    (define-key map (kbd "C-c C-e m") 'rjsx-mode-element-mute-blanks)
    (define-key map (kbd "C-c C-e n") 'rjsx-mode-element-next)
    (define-key map (kbd "C-c C-e p") 'rjsx-mode-element-previous)
    (define-key map (kbd "C-c C-e r") 'rjsx-mode-element-rename)
    (define-key map (kbd "C-c C-e s") 'rjsx-mode-element-select)
    (define-key map (kbd "C-c C-e t") 'rjsx-mode-element-transpose)
    (define-key map (kbd "C-c C-e u") 'rjsx-mode-element-parent)
    (define-key map (kbd "C-c C-e v") 'rjsx-mode-element-vanish)
    (define-key map (kbd "C-c C-e w") 'rjsx-mode-element-wrap)

    (define-key map (kbd "C-c C-t a") 'rjsx-mode-tag-attributes-sort)
    (define-key map (kbd "C-c C-t b") 'rjsx-mode-tag-beginning)
    (define-key map (kbd "C-c C-t e") 'rjsx-mode-tag-end)
    (define-key map (kbd "C-c C-t m") 'rjsx-mode-tag-match)
    (define-key map (kbd "C-c C-t n") 'rjsx-mode-tag-next)
    (define-key map (kbd "C-c C-t p") 'rjsx-mode-tag-previous)
    (define-key map (kbd "C-c C-t s") 'rjsx-mode-tag-select)

    ;;--------------------------------------------------------------------------

    ;;(define-key map (kbd "M-q")       'fill-paragraph)
    (define-key map (kbd "M-;")       'rjsx-mode-comment-or-uncomment)

    ;;C-c C-a : attribute
    ;;C-c C-b : block
    ;;C-c C-d : dom
    ;;C-c C-e : element
    (define-key map (kbd "C-c C-f")   'rjsx-mode-fold-or-unfold)
    (define-key map (kbd "C-c C-h")   'rjsx-mode-buffer-highlight)
    (define-key map (kbd "C-c C-i")   'rjsx-mode-buffer-indent)
    (define-key map (kbd "C-c C-l")   'rjsx-mode-file-link)
    (define-key map (kbd "C-c C-m")   'rjsx-mode-mark-and-expand)
    (define-key map (kbd "C-c C-n")   'rjsx-mode-navigate)
    (define-key map (kbd "C-c C-r")   'rjsx-mode-reload)
    ;;C-c C-t : tag
    (define-key map (kbd "C-c C-w")   'rjsx-mode-whitespaces-show)

    map)
  "Keymap for `rjsx-mode'.")

;;---- COMPATIBILITY -----------------------------------------------------------

(eval-and-compile
  ;; compatibility with emacs < 24.3
  (defun rjsx-mode-buffer-narrowed-p ()
    (if (fboundp 'buffer-narrowed-p)
        (buffer-narrowed-p)
      (/= (- (point-max) (point-min)) (buffer-size))))

  ;; compatibility with emacs < 24.3
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

  ) ;eval-and-compile

;;---- MAJOR MODE --------------------------------------------------------------

;;;###autoload
(define-derived-mode rjsx-mode prog-mode "(React)JSX"
  "Major mode for editing web templates."

  (make-local-variable 'rjsx-mode-attr-indent-offset)
  (make-local-variable 'rjsx-mode-attr-value-indent-offset)
  (make-local-variable 'rjsx-mode-auto-pairs)
  (make-local-variable 'rjsx-mode-block-regexp)
  (make-local-variable 'rjsx-mode-change-beg)
  (make-local-variable 'rjsx-mode-change-end)
  (make-local-variable 'rjsx-mode-code-indent-offset)
  (make-local-variable 'rjsx-mode-column-overlays)
  (make-local-variable 'rjsx-mode-comment-formats)
  (make-local-variable 'rjsx-mode-comment-style)
  (make-local-variable 'rjsx-mode-content-type)
  (make-local-variable 'rjsx-mode-display-table)
  (make-local-variable 'rjsx-mode-enable-block-face)
  (make-local-variable 'rjsx-mode-enable-inlays)
  (make-local-variable 'rjsx-mode-enable-part-face)
  (make-local-variable 'rjsx-mode-enable-sexp-functions)
  (make-local-variable 'rjsx-mode-expand-initial-pos)
  (make-local-variable 'rjsx-mode-expand-initial-scroll)
  (make-local-variable 'rjsx-mode-expand-previous-state)
  (make-local-variable 'rjsx-mode-indent-style)
  (make-local-variable 'rjsx-mode-indentless-attributes)
  (make-local-variable 'rjsx-mode-indentless-elements)
  (make-local-variable 'rjsx-mode-inhibit-fontification)
  (make-local-variable 'rjsx-mode-is-scratch)
  (make-local-variable 'rjsx-mode-last-enabled-feature)
  (make-local-variable 'rjsx-mode-markup-indent-offset)
  (make-local-variable 'rjsx-mode-minor-engine)
  (make-local-variable 'rjsx-mode-overlay-tag-end)
  (make-local-variable 'rjsx-mode-overlay-tag-start)
  (make-local-variable 'rjsx-mode-time)

  (make-local-variable 'comment-end)
  (make-local-variable 'comment-region-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'fill-paragraph-function)
  (make-local-variable 'font-lock-beg)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-extend-region-functions)
  (make-local-variable 'font-lock-end)
  (make-local-variable 'font-lock-support-mode)
  (make-local-variable 'font-lock-unfontify-region-function)
  (make-local-variable 'imenu-case-fold-search)
  (make-local-variable 'imenu-create-index-function)
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'parse-sexp-lookup-properties)
  (make-local-variable 'uncomment-region-function)
  (make-local-variable 'yank-excluded-properties)

  (setq comment-end "-->"
        comment-region-function 'rjsx-mode-comment-or-uncomment-region
        comment-start "<!--"
        fill-paragraph-function 'rjsx-mode-fill-paragraph
        font-lock-defaults '(rjsx-mode-font-lock-keywords t)
        font-lock-extend-region-functions '(rjsx-mode-extend-region)
        font-lock-support-mode nil
        font-lock-unfontify-region-function 'rjsx-mode-unfontify-region
        imenu-case-fold-search t
        imenu-create-index-function 'rjsx-mode-imenu-index
        indent-line-function 'rjsx-mode-indent-line
        parse-sexp-lookup-properties t
        yank-excluded-properties t
        uncomment-region-function 'rjsx-mode-comment-or-uncomment-region)

  (substitute-key-definition 'indent-new-comment-line
                             'rjsx-mode-comment-indent-new-line
                             rjsx-mode-map global-map)

  (add-hook 'after-change-functions 'rjsx-mode-on-after-change nil t)
  (add-hook 'after-save-hook        'rjsx-mode-on-after-save t t)
  (add-hook 'change-major-mode-hook 'rjsx-mode-on-exit nil t)
  (add-hook 'post-command-hook      'rjsx-mode-on-post-command nil t)

  (when rjsx-mode-enable-whitespace-fontification
    (rjsx-mode-whitespaces-on))

  (when rjsx-mode-enable-sexp-functions
    (setq-local forward-sexp-function 'rjsx-mode-forward-sexp))

  (rjsx-mode-guess-engine-and-content-type)
  (setq rjsx-mode-change-beg (point-min)
        rjsx-mode-change-end (point-max))
  (when (> (point-max) 256000)
    (rjsx-mode-buffer-highlight))

  (when (and (boundp 'hs-special-modes-alist)
             (not (assoc major-mode hs-special-modes-alist)))
    (add-to-list 'hs-special-modes-alist '(rjsx-mode "{" "}" "/[*/]" rjsx-mode-forward-sexp nil))
    ) ;when

  )

;;---- DEFUNS ------------------------------------------------------------------
(defun rjsx-mode-hypothesize-unused ()
  "Mark code that I suspect is unused.
With debug off, it logs a message; with debug on, it errors to get a traceback."
  (if rjsx-mode-debug
      (error "Suspected dead code in use")
    (message "Warning: suspected dead code is in use; set rjsx-mode-debug to t to see a traceback.")))

(defun rjsx-mode-scan-region (beg end &optional content-type)
  "Identify nodes/parts/blocks and syntactic symbols (strings/comments)."
  ;;(message "scan-region: beg(%d) end(%d) content-type(%S)" beg end content-type)
  (with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((inhibit-point-motion-hooks t)
               (inhibit-quit t))
           (remove-list-of-text-properties beg end rjsx-mode-scan-properties)
           (cond
            ((and content-type (member content-type rjsx-mode-part-content-types))
             (put-text-property beg end 'part-side
                                (cond
                                 ((string= content-type "javascript") 'javascript)
                                 ((string= content-type "json") 'json)
                                 ((string= content-type "jsx") 'jsx)
                                 ))
             (rjsx-mode-scan-blocks beg end)
             (rjsx-mode-part-scan beg end content-type))
            ((member rjsx-mode-content-type rjsx-mode-part-content-types)
             (rjsx-mode-scan-blocks beg end)
             (rjsx-mode-part-scan beg end))
            (t
             (rjsx-mode-scan-blocks beg end)
             (rjsx-mode-scan-elements beg end)
             (rjsx-mode-process-parts beg end 'rjsx-mode-part-scan))
            ) ;cond
           (cons beg end)
           ))))))

(defun rjsx-mode-scan-blocks (reg-beg reg-end)
  "Identifies blocks (with block-side, block-beg, block-end text properties)."
  (save-excursion

    (let ((i 0) open close closing-string start sub1 sub2 pos tagopen tmp delim-open delim-close part-beg part-end tagclose)

      (goto-char reg-beg)

      ;;(message "%S: %Sx%S" (point) reg-beg reg-end)
      ;;(message "regexp=%S" rjsx-mode-block-regexp)
      (while (and (< i 2000)
                  (> reg-end (point))
                  rjsx-mode-block-regexp
                  (re-search-forward rjsx-mode-block-regexp reg-end t)
                  (not (eobp)))

        (setq i (1+ i)
              closing-string nil
              close nil
              tagopen (match-string 0)
              open (match-beginning 0)
              delim-open nil
              delim-close nil
              pos nil)

        (let ((l (length tagopen)))
          (when (member (string-to-char tagopen) '(?\s ?\t))
            (setq tagopen (replace-regexp-in-string "\\`[ \t]*" "" tagopen))
            (setq open (+ open (- l (length tagopen))))
            (setq l (length tagopen))
            )
          (setq sub1 (substring tagopen 0 1)
                sub2 (substring tagopen 0 (if (>= l 2) 2 1)))
          )
        ;;(message " found block #(%S) at pos=(%S), part-type=(%S)" i open (get-text-property open 'part-side))

        (when closing-string

          (cond

           ((listp closing-string)
            (cond
             ((rjsx-mode-rsf-balanced (car closing-string) (cdr closing-string) reg-end t)
              (setq close (match-end 0)
                    pos (point))
              )
             ) ;cond
            ) ;case listp

           ((string= closing-string "EOL")
            (end-of-line)
            (setq close (point)
                  pos (point)))

           ((string= closing-string "EOC")
            (setq close (point)
                  pos (point)))

           ((string= closing-string "EOR")
            (rjsx-mode-razor-skip open)
            (setq close (if (> (point) reg-end) reg-end (point))
                  pos (if (> (point) reg-end) reg-end (point)))
            (goto-char pos))

           ((string= closing-string "EOV")
            (rjsx-mode-velocity-skip open)
            (setq close (point)
                  pos (point)))

           ((search-forward closing-string reg-end t)
            (setq close (match-end 0)
                  pos (point)))
           ) ;cond

          (when (and close (>= reg-end pos))
            ;;(message "pos(%S) : open(%S) close(%S)" pos open close)
            (put-text-property open (1+ open) 'block-beg 0)
            (put-text-property open (1+ open) 'block-controls 0)
            (put-text-property open close 'block-side t)
            (put-text-property (1- close) close 'block-end t)
            (when delim-open
              (rjsx-mode-block-delimiters-set open close delim-open delim-close))
            (rjsx-mode-block-scan open close)
            ;;(message "%S %s" (point) tagopen)
            (when (and (member tagopen '("<r:script" "<r:style"
                                         "<c:js" "<c:css"
                                         "<%= javascript_tag do %>"
                                         "<%block filter=\"collect_js\">"
                                         "<%block filter=\"collect_css\">"))
                       (setq part-beg close)
                       (setq tagclose
                             (cond
                              ((string= tagopen "<r:script") "</r:script")
                              ((string= tagopen "<r:style") "</r:style")
                              ((string= tagopen "<c:js") "</c:js")
                              ((string= tagopen "<c:css") "</c:css")
                              ((string= tagopen "<%= javascript_tag do %>") "<% end %>")
                              ((member tagopen '("<%block filter=\"collect_js\">"
                                                 "<%block filter=\"collect_css\">")) "</%block")
                              ))
                       (rjsx-mode-sf tagclose)
                       (setq part-end (match-beginning 0))
                       (> part-end part-beg))
              (put-text-property part-beg part-end
                                 'part-side
                                 (cond
                                  ((member tagopen '("<r:style" "<c:css" "<%block filter=\"collect_css\">")) 'css)
                                  (t 'javascript)))
              (setq pos part-beg
                    part-beg nil
                    part-end nil)
              ) ;when
            ) ;when close

          (if pos (goto-char pos))

          ) ;when closing-string

        ) ;while
      )))

(defun rjsx-mode-block-delimiters-set (reg-beg reg-end delim-open delim-close)
  "Set text-property 'block-token to 'delimiter-(beg|end) on block delimiters"
  ;;(message "reg-beg(%S) reg-end(%S) delim-open(%S) delim-close(%S)" reg-beg reg-end delim-open delim-close)
  (when delim-open
    (put-text-property reg-beg (+ reg-beg (length delim-open))
                       'block-token 'delimiter-beg))
  (when delim-close
    (put-text-property (- reg-end (length delim-close)) reg-end
                       'block-token 'delimiter-end))
  )

(defun rjsx-mode-process-blocks (reg-beg reg-end func)
  (let ((i 0) (continue t) (block-beg reg-beg) (block-end nil))
    (while continue
      (setq block-end nil)
      (unless (get-text-property block-beg 'block-beg)
        (setq block-beg (rjsx-mode-block-next-position block-beg)))
      (when (and block-beg (< block-beg reg-end))
        (setq block-end (rjsx-mode-block-end-position block-beg)))
      (cond
       ((> (setq i (1+ i)) 2000)
        (message "process-blocks ** warning (%S) **" (point))
        (setq continue nil))
       ((or (null block-end) (> block-end reg-end))
        (setq continue nil))
       (t
        (setq block-end (1+ block-end))
        (funcall func block-beg block-end)
        (setq block-beg block-end)
        ) ;t
       ) ;cond
      ) ;while
    ))

(defun rjsx-mode-process-parts (reg-beg reg-end func)
  (let ((i 0) (continue t) (part-beg reg-beg) (part-end nil))
    (while continue
      (setq part-end nil)
      (unless (get-text-property part-beg 'part-side)
        (setq part-beg (rjsx-mode-part-next-position part-beg)))
      (when (and part-beg (< part-beg reg-end))
        (setq part-end (rjsx-mode-part-end-position part-beg)))
      (cond
       ((> (setq i (1+ i)) 100)
        (message "process-parts ** warning (%S) **" (point))
        (setq continue nil))
       ((or (null part-end) (> part-end reg-end))
        (setq continue nil))
       (t
        (setq part-end (1+ part-end))
        (funcall func part-beg part-end)
        (setq part-beg part-end)
        )
       ) ;cond
      ) ;while
    ))

(defun rjsx-mode-block-scan (block-beg block-end)
  (let (sub1 sub2 sub3 regexp token-type)

    ;;(message "block-beg=%S block-end=%S" block-beg block-end)
    ;;(remove-text-properties block-beg block-end rjsx-mode-scan-properties)

    (goto-char block-beg)

    (cond
     ((>= (point-max) (+ block-beg 3))
      (setq sub3 (buffer-substring-no-properties block-beg (+ block-beg 3))
            sub2 (buffer-substring-no-properties block-beg (+ block-beg 2))
            sub1 (buffer-substring-no-properties block-beg (+ block-beg 1)))
      )
     ((>= (point-max) (+ block-beg 2))
      (setq sub3 (buffer-substring-no-properties block-beg (+ block-beg 2))
            sub2 (buffer-substring-no-properties block-beg (+ block-beg 2))
            sub1 (buffer-substring-no-properties block-beg (+ block-beg 1)))
      )
     (t
      (setq sub1 (buffer-substring-no-properties block-beg (+ block-beg 1)))
      (setq sub2 sub1
            sub3 sub1)
      )
     )

    (cond
     (token-type
      (put-text-property block-beg block-end 'block-token token-type))
     ((and regexp
           (> (- block-end block-beg) 6))
      (rjsx-mode-block-tokenize
       (rjsx-mode-block-code-beginning-position block-beg)
       (rjsx-mode-block-code-end-position block-beg)
       regexp)
      )
     ) ;cond

    ))

(defun rjsx-mode-block-tokenize (reg-beg reg-end &optional regexp)
  ;;(message "tokenize: reg-beg(%S) reg-end(%S) regexp(%S)" reg-beg reg-end regexp)
  ;;(message "tokenize: reg-beg(%S) reg-end(%S) command(%S)" reg-beg reg-end this-command)
  ;;(message "%S>%S : %S" reg-beg reg-end (buffer-substring-no-properties reg-beg reg-end))
  (save-excursion
    (let ((pos reg-beg) beg end char match continue (flags 0) token-type token-end)

      (remove-list-of-text-properties reg-beg reg-end '(block-token))

      ;; TODO : vérifier la cohérence
      (put-text-property reg-beg reg-end 'block-side t)

      (goto-char reg-beg)

      (when (> reg-beg reg-end)
        (message "block-tokenize ** reg-beg(%S) reg-end(%S) **" reg-beg reg-end))

      (while (and (< reg-beg reg-end) (re-search-forward regexp reg-end t))
        (setq beg (match-beginning 0)
              match (match-string 0)
              continue t
              token-type 'comment
              token-end (if (< reg-end (line-end-position)) reg-end (line-end-position))
              char (aref match 0))
        (cond

         ((eq char ?\')
          (setq token-type 'string)
          (while (and continue (search-forward "'" reg-end t))
            ;;(if (looking-back "\\\\+'" reg-beg t)
            ;;    (setq continue (= (mod (- (point) (match-beginning 0)) 2) 0))
            ;;  (setq continue nil)
            ;;  ) ;if
            (setq continue (rjsx-mode-string-continue-p reg-beg))
            ) ;while
          )

         ((eq char ?\")
          (setq token-type 'string)
          (while (and continue (search-forward "\"" reg-end t))
            ;;(if (looking-back "\\\\+\"" reg-beg t)
            ;;    (setq continue (= (mod (- (point) (match-beginning 0)) 2) 0))
            ;;  (setq continue nil)
            ;;  ) ;if
            (setq continue (rjsx-mode-string-continue-p reg-beg))
            ) ;while
          )

         ((string= match "//")
          (goto-char token-end))

         ((eq char ?\;)
          (goto-char token-end))

         ((string= match "#|")
          (unless (search-forward "|#" reg-end t)
            (goto-char token-end)))

         ((eq char ?\#)
          (goto-char token-end))

         ((string= match "/*")
          (unless (search-forward "*/" reg-end t)
            (goto-char token-end))
          )

         ((string= match "@*")
          (unless (search-forward "*@" reg-end t)
            (goto-char token-end)))

         ((eq char ?\<)
          (setq token-type 'string)
          (re-search-forward (concat "^[ ]*" (match-string 1)) reg-end t))

         (t
          (message "block-tokenize ** token end (%S) **" beg)
          (setq token-type nil))

         ) ;cond

        ;;(when (eq token-type 'comment) (message "comment: %S %S" beg (point)))

        (put-text-property beg (point) 'block-token token-type)

        (when (eq token-type 'comment)
          (put-text-property beg (1+ beg) 'syntax-table (string-to-syntax "<"))
          (if (or (< (point) (line-end-position)) (= (point) (point-max)))
              (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax ">")) ;#445 #480
            (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax ">")) ;#377
            )
          )

        ) ;while

      (rjsx-mode-block-controls-unset pos)

      )))

(defun rjsx-mode-block-controls-reduce (controls)
  (when (and (eq (car (car controls)) 'open)
             (member (cons 'close (cdr (car controls))) controls))
    (setq controls nil))
  controls)

(defun rjsx-mode-block-controls-unset (pos)
  (cond
   ((null (get-text-property pos 'block-side))
    (message "block-controls-unset ** invalid value (%S) **" pos))
   ((or (get-text-property pos 'block-beg)
        (setq pos (rjsx-mode-block-beginning-position pos)))
    (put-text-property pos (1+ pos) 'block-controls 0))
   (t
    (message "block-controls-unset ** failure (%S) **" (point)))
   ))

(defun rjsx-mode-block-controls-get (pos)
  (with-silent-modifications
   (let ((controls nil))
     (cond
      ((null (get-text-property pos 'block-side))
       (message "block-controls-get ** invalid value (%S) **" pos))
      ((or (get-text-property pos 'block-beg)
           (setq pos (rjsx-mode-block-beginning-position pos)))
       (setq controls (get-text-property pos 'block-controls))
       (when (integerp controls)
         (rjsx-mode-block-controls-set pos (rjsx-mode-block-end-position pos))
         (setq controls (get-text-property pos 'block-controls))
         )
       )
      (t
       (message "block-controls-get ** failure (%S) **" (point)))
      ) ;cond
     controls)))

(defun rjsx-mode-block-controls-set (reg-beg reg-end)
  (save-excursion
    (goto-char reg-beg)
    (let (controls pos type control)

      (cond

       ((null rjsx-mode-engine)
        (message "block-controls-set ** unknown engine (%S) **" rjsx-mode-engine)
        )
       ) ;cond engine

      (put-text-property reg-beg (1+ reg-beg) 'block-controls controls)
      ;;      (message "(%S) controls=%S" reg-beg controls)

      )))

(defun rjsx-mode-block-is-opened-sexp (reg-beg reg-end)
  (let ((n 0))
    (save-excursion
      (goto-char reg-beg)
      (while (rjsx-mode-block-rsf "[()]" reg-end)
        (if (eq (char-before) ?\() (setq n (1+ n)) (setq n (1- n)))))
    (> n 0)))

(defvar rjsx-mode-regexp1 "<\\(/?[[:alpha:]][[:alnum:]:-]*\\|!--\\|!\\[CDATA\\[\\|!doctype\\|!DOCTYPE\\|\?xml\\)")

(defvar rjsx-mode-regexp2 "<\\(/?[[:alpha:]][[:alnum:]:-]*\\|!--\\|!\\[CDATA\\[\\)")

(defun rjsx-mode-scan-elements (reg-beg reg-end)
  (save-excursion
    (let (part-beg part-end flags limit close-expr props tname tbeg tend element-content-type (regexp rjsx-mode-regexp1) part-close-tag char)
      ;;(message "%S" rjsx-mode-engine)
      (goto-char reg-beg)

      (while (rjsx-mode-dom-rsf regexp reg-end)

        (setq flags 0
              tname (downcase (match-string-no-properties 1))
              char (aref tname 0)
              tbeg (match-beginning 0)
              tend nil
              element-content-type nil
              limit reg-end
              part-beg nil
              part-end nil
              props nil
              close-expr nil
              part-close-tag nil)

        (cond
         ((not (member char '(?\! ?\?)))
          (cond
           ((string-match-p "-" tname)
            (setq flags (logior flags 2)))
           ((string-match-p ":" tname)
            (setq flags (logior flags 32)))
           )
          (cond
           ((eq char ?\/)
            (setq props (list 'tag-name (substring tname 1) 'tag-type 'end)
                  flags (logior flags 4)
                  limit (if (> reg-end (line-end-position)) (line-end-position) reg-end))
            )
           ((rjsx-mode-element-is-void tname)
            (setq props (list 'tag-name tname 'tag-type 'void)))
           (t
            (setq props (list 'tag-name tname 'tag-type 'start)))
           ) ;cond
          )
         ) ;cond

        (cond
         ((and (null close-expr) (eq (char-after) ?\>))
          (setq flags (logior flags 16)
                tend (1+ (point))))
         ((null close-expr)
          (setq flags (logior flags (rjsx-mode-attr-skip reg-end)))
          (when (> (logand flags 8) 0)
            (setq props (plist-put props 'tag-type 'void)))
          (setq tend (point)))
         ((rjsx-mode-dom-sf close-expr limit t)
          (setq tend (point)))
         (t
          (setq tend (line-end-position)))
         )

        (cond
         ((string= tname "script")
          (let (script)
            (setq script (buffer-substring-no-properties tbeg tend)
                  part-close-tag "</script>")
            (cond
             ((string-match-p " type[ ]*=[ ]*[\"']text/\\(jsx\\|babel\\)" script)
              (setq element-content-type "jsx"))
             ((string-match-p " type[ ]*=[ ]*[\"']application/\\(ld\\+json\\|json\\)" script)
              (setq element-content-type "json"))
             (t
              (setq element-content-type "javascript"))
             ) ;cond
            ) ;let
          ) ;script
         )

        (add-text-properties tbeg tend props)
        (put-text-property tbeg (1+ tbeg) 'tag-beg flags)
        (put-text-property (1- tend) tend 'tag-end t)

        (when (and part-close-tag
                   (rjsx-mode-dom-sf part-close-tag reg-end t)
                   (setq part-beg tend)
                   (setq part-end (match-beginning 0))
                   (> part-end part-beg))
          (put-text-property part-beg part-end 'part-side
                             (intern element-content-type rjsx-mode-obarray))
          (setq tend part-end)
          ) ;when

        (goto-char tend)

        ) ;while

      )))

;; FLAGS: tag
;; (1)attrs (2)custom (4)slash-beg (8)slash-end (16)bracket-end
;; (32)namespaced

;; FLAGS: attr
;; (1)custom-attr (2)engine-attr (4)spread-attr[jsx] (8)code-value

;; attr states
;; (0)nil (1)space (2)name (3)space-before (4)equal (5)space-after
;; (6)value-uq (7)value-sq (8)value-dq (9)value-bq : jsx attr={}

(defun rjsx-mode-attr-skip (limit)

  (let ((tag-flags 0) (attr-flags 0) (continue t) (attrs 0) (counter 0) (brace-depth 0)
        (pos-ori (point)) (state 0) (equal-offset 0) (go-back nil)
        (is-jsx (or (string= rjsx-mode-content-type "jsx") (eq (get-text-property (point) 'part-type) 'jsx)))
        attr name-beg name-end val-beg char pos escaped spaced quoted)

    (while continue

      (setq pos (point)
            char (char-after)
            spaced (eq char ?\s))

      (when quoted (setq quoted (1+ quoted)))

      (cond

       ((>= pos limit)
        (setq continue nil)
        (setq go-back t)
        (setq attrs (+ attrs (rjsx-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        )

       ((or (and (= state 8) (not (member char '(?\" ?\\))))
            (and (= state 7) (not (member char '(?\' ?\\))))
            (and (= state 9) (not (member char '(?} ?\\))))
            )
        (when (and (= state 9) (eq char ?\{))
          (setq brace-depth (1+ brace-depth)))
        )

       ((and (= state 9) (eq char ?\}) (> brace-depth 1))
        (setq brace-depth (1- brace-depth)))

       ((get-text-property pos 'block-side)
        (when (= state 2)
          (setq name-end pos))
        )

       ((and (= state 2) is-jsx (eq char ?\}) (eq attr-flags 4))
        (setq name-end pos)
        (setq attrs (+ attrs (rjsx-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 0
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((or (and (= state 8) (eq ?\" char) (not escaped))
            (and (= state 7) (eq ?\' char) (not escaped))
            (and (= state 9) (eq ?\} char) (= brace-depth 1))
            )

        ;;(message "%S %S" (point) attr-flags)
        (setq attrs (+ attrs (rjsx-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 0
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((and (member state '(4 5)) (member char '(?\' ?\" ?\{)))
        (setq val-beg pos)
        (setq quoted 1)
        (setq state (cond ((eq ?\' char) 7)
                          ((eq ?\" char) 8)
                          (t             9)))
        (when (= state 9)
          (setq brace-depth 1))
        )

       ((and (eq ?\= char) (member state '(2 3)))
        (setq equal-offset (- pos name-beg))
        (setq state 4)
        (setq attr (buffer-substring-no-properties name-beg (1+ name-end)))
        (when (and rjsx-mode-indentless-attributes (member (downcase attr) rjsx-mode-indentless-attributes))
          ;;(message "onclick")
          (setq attr-flags (logior attr-flags 8)))
        )

       ((and spaced (= state 0))
        (setq state 1)
        )

       ((and (eq char ?\<) (not (member state '(7 8 9))))
        (setq continue nil)
        (setq go-back t)
        (setq attrs (+ attrs (rjsx-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        )

       ((and (eq char ?\>) (not (member state '(7 8 9))))
        (setq tag-flags (logior tag-flags 16))
        (when (eq (char-before) ?\/)
          (setq tag-flags (logior tag-flags 8))
          )
        (setq continue nil)
        (when name-beg
          (setq attrs (+ attrs (rjsx-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset))))
        )

       ((and spaced (member state '(1 3 5)))
        )

       ((and spaced (= state 2))
        (setq state 3)
        )

       ((and (eq char ?\/) (member state '(4 5)))
        (setq attrs (+ attrs (rjsx-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 1
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((and (eq char ?\/) (member state '(0 1)))
        )

       ((and spaced (= state 4))
        (setq state 5)
        )

       ((and (= state 3)
             (or (and (>= char 97) (<= char 122)) ;a - z
                 (and (>= char 65) (<= char 90)) ;A - Z
                 (eq char ?\-)))
        (setq attrs (+ attrs (rjsx-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 2
              attr-flags 0
              equal-offset 0
              name-beg pos
              name-end pos
              val-beg nil)
        )

       ((and (eq char ?\n) (not (member state '(7 8 9))))
        (setq attrs (+ attrs (rjsx-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 1
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((and (= state 6) (member char '(?\s ?\n ?\/)))
        (setq attrs (+ attrs (rjsx-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 1
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((and quoted (= quoted 2) (member char '(?\s ?\n ?\>)))
        (when (eq char ?\>)
          (setq tag-flags (logior tag-flags 16))
          (setq continue nil))
        (setq state 6)
        (setq attrs (+ attrs (rjsx-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 1
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((and (not spaced) (= state 1))
        (when (and is-jsx (eq char ?\{))
          (setq attr-flags 4))
        (setq state 2)
        (setq name-beg pos
              name-end pos)
        )

       ((member state '(4 5))
        (setq val-beg pos)
        (setq state 6)
        )

       ((= state 1)
        (setq state 2)
        )

       ((= state 2)
        (setq name-end pos)
        (when (and (= attr-flags 0) (member char '(?\- ?\:)))
          (let (attr)
            (setq attr (buffer-substring-no-properties name-beg (1+ name-end)))
            (cond
             ((member attr '("http-equiv"))
              (setq attr-flags (1- attr-flags))
              )
             ((and (eq char ?\-) (not (string= attr "http-")))
              (setq attr-flags (logior attr-flags 1)))
             ) ;cond
            ) ;let
          ) ;when attr-flags = 1
        ) ;state=2

       ) ;cond

      ;;(message "point(%S) end(%S) state(%S) c(%S) name-beg(%S) name-end(%S) val-beg(%S) attr-flags(%S) equal-offset(%S)" pos end state char name-beg name-end val-beg attr-flags equal-offset)

      (when (and quoted (>= quoted 2))
        (setq quoted nil))

      (setq escaped (eq ?\\ char))
      (when (null go-back)
        (forward-char))

      ) ;while

    (when (> attrs 0) (setq tag-flags (logior tag-flags 1)))

    tag-flags))

(defun rjsx-mode-attr-scan (state char name-beg name-end val-beg flags equal-offset)
;;  (message "point(%S) state(%S) c(%c) name-beg(%S) name-end(%S) val-beg(%S) flags(%S) equal-offset(%S)"
;;           (point) state char name-beg name-end val-beg flags equal-offset)
  (if (null flags) (setq flags 0))
  (cond
   ((null name-beg)
;;    (message "name-beg is null (%S)" (point))
    0)
   ((or (and (= state 8) (not (eq ?\" char)))
        (and (= state 7) (not (eq ?\' char))))
    (put-text-property name-beg (1+ name-beg) 'tag-attr-beg flags)
    (put-text-property name-beg val-beg 'tag-attr t)
    (put-text-property (1- val-beg) val-beg 'tag-attr-end equal-offset)
    1)
   ((and (member state '(4 5)) (null val-beg))
    (put-text-property name-beg (1+ name-beg) 'tag-attr-beg flags)
    (put-text-property name-beg (+ name-beg equal-offset 1) 'tag-attr t)
    (put-text-property (+ name-beg equal-offset) (+ name-beg equal-offset 1) 'tag-attr-end equal-offset)
    1)
   (t
    (let (val-end)
      (if (null val-beg)
          (setq val-end name-end)
        (setq val-end (point))
        (when (or (null char) (member char '(?\s ?\n ?\> ?\/)))
          (setq val-end (1- val-end))
          )
        ) ;if
      (put-text-property name-beg (1+ name-beg) 'tag-attr-beg flags)
      (put-text-property name-beg (1+ val-end) 'tag-attr t)
      (put-text-property val-end (1+ val-end) 'tag-attr-end equal-offset)
      ) ;let
    1) ;t
   ) ;cond
  )

(defun rjsx-mode-part-scan (reg-beg reg-end &optional content-type depth)
  (save-excursion
    (let (token-re ch-before ch-at ch-next token-type beg continue)
      ;;(message "%S %S" reg-beg reg-end)
      (cond
       (content-type
        )
       ((member rjsx-mode-content-type rjsx-mode-part-content-types)
        (setq content-type rjsx-mode-content-type))
       (t
        (setq content-type (symbol-name (get-text-property reg-beg 'part-side))))
       ) ;cond

      (goto-char reg-beg)

      (cond
       ((member content-type '("javascript" "json"))
        (setq token-re "/\\|\"\\|'\\|`"))
       ((member content-type '("jsx"))
        (setq token-re "/\\|\"\\|'\\|`\\|</?[[:alpha:]]"))
       (t
        (setq token-re "/\\*\\|\"\\|'"))
       )

      (while (and token-re (< (point) reg-end) (rjsx-mode-dom-rsf token-re reg-end t))

        (setq beg (match-beginning 0)
              token-type nil
              continue t
              ch-at (char-after beg)
              ch-next (or (char-after (1+ beg)) ?\d)
              ch-before (or (char-before beg) ?\d))

        ;;(message "[%S>%S|%S] %S %c %c %c" reg-beg reg-end depth beg ch-before ch-at ch-next)

        (cond

         ((eq ?\' ch-at)
          (while (and continue (search-forward "'" reg-end t))
            (cond
             ((get-text-property (1- (point)) 'block-side)
              (setq continue t))
             (t
              (setq continue (rjsx-mode-string-continue-p reg-beg)))
             )
            ) ;while
          (setq token-type 'string))

         ((eq ?\` ch-at)
          (while (and continue (search-forward "`" reg-end t))
            (cond
             ((get-text-property (1- (point)) 'block-side)
              (setq continue t))
             (t
              (setq continue (rjsx-mode-string-continue-p reg-beg)))
             )
            ) ;while
          (setq token-type 'string))

         ((eq ?\" ch-at)
          (while (and continue (search-forward "\"" reg-end t))
            (cond
             ((get-text-property (1- (point)) 'block-side)
              (setq continue t))
             (t
              (setq continue (rjsx-mode-string-continue-p reg-beg)))
             ) ;cond
            ) ;while
          (cond
           ((string= content-type "json")
            (if (looking-at-p "[ ]*:")
                (cond
                 ((eq ?\@ (char-after (1+ beg)))
                  (setq token-type 'context))
                 (t
                  (setq token-type 'key))
                 )
              (setq token-type 'string))
            ) ;json
           (t
            (setq token-type 'string))
           ) ;cond
          )

         ((eq ?\< ch-at)
          ;;(message "before [%S>%S|%S] pt=%S" reg-beg reg-end depth (point))
          (search-backward "<")
          (if (rjsx-mode-jsx-skip reg-end)
              (rjsx-mode-jsx-scan-element beg (point) depth)
            (forward-char))
          ;;(message "after [%S>%S|%S] pt=%S" reg-beg reg-end depth (point))
          )

         ((and (eq ?\/ ch-at) (member content-type '("javascript" "jsx")))
          (cond
           ((eq ?\\ ch-before)
            )
           ((eq ?\* ch-next)
            ;;(message "--> %S %S" (point) reg-end)
            (when (search-forward "*/" reg-end t)
              (setq token-type 'comment))
            )
           ((eq ?\/ ch-next)
            (setq token-type 'comment)
            (goto-char (if (< reg-end (line-end-position)) reg-end (line-end-position)))
            )
           ((and (looking-at-p ".*/")
                 (looking-back "[[(,=:!&|?{};][ ]*/" (point-min)))
                 ;;(re-search-forward "/[gimyu]*" reg-end t))
            (let ((eol (line-end-position)))
              (while (and continue (search-forward "/" eol t))
                (cond
                 ((get-text-property (1- (point)) 'block-side)
                  (setq continue t))
                 ((looking-back "\\\\+/" reg-beg t)
                  (setq continue (= (mod (- (point) (match-beginning 0)) 2) 0)))
                 (t
                  (re-search-forward "[gimyu]*" eol t)
                  (setq token-type 'string)
                  (setq continue nil))
                 )
                ) ;while
              ) ;let
            )
           ) ;cond
          )

         ((eq ?\/ ch-next)
          (unless (eq ?\\ ch-before)
            (setq token-type 'comment)
            (goto-char (if (< reg-end (line-end-position)) reg-end (line-end-position)))
            )
          )

         ((eq ?\* ch-next)
          (cond
           ((search-forward "*/" reg-end t)
            (setq token-type 'comment))
           ((not (eobp))
            (forward-char))
           ) ;cond
          )

         ) ;cond

        (when (and beg (>= reg-end (point)) token-type)
          (put-text-property beg (point) 'part-token token-type)
          (cond
           ((eq token-type 'comment)
            (put-text-property beg (1+ beg) 'syntax-table (string-to-syntax "<"))
            (when (< (point) (point-max))
              (if (< (point) (line-end-position))
                  (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax ">")) ;#445
                (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax ">")) ;#377
                )
              ) ;when
            ) ;comment
           ((eq token-type 'string)
            (put-text-property beg (1+ beg) 'syntax-table (string-to-syntax "|"))
            (when (< (point) (point-max))
              (if (< (point) (line-end-position))
                  (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax "|"))
                (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "|"))
                )
              ) ;when
            ) ;string
           ) ;cond
          ) ;when

        (when (> (point) reg-end)
          (message "reg-beg(%S) reg-end(%S) token-type(%S) point(%S)" reg-beg reg-end token-type (point)))

        ;;(message "#[%S>%S|%S] %S %c %c %c | (%S)" reg-beg reg-end depth beg ch-before ch-at ch-next (point))

        ) ;while

      )))

(defun rjsx-mode-string-continue-p (reg-beg)
  "Is `point' preceeded by an odd number of backslashes?"
  (let* ((p (1- (point))))
    (while (and (< reg-beg p) (eq ?\\ (char-before p)))
      (setq p (1- p)))
    (= (mod (- (point) p) 2) 0)))

(defun rjsx-mode-jsx-skip (reg-end)
  (let ((continue t) (pos nil) (i 0) tag)
    (looking-at "<\\([[:alpha:]][[:alnum:]:-]*\\)")
    (setq tag (match-string-no-properties 1))
    ;;(message "point=%S tag=%S" (point) tag)
    (save-excursion
      (while continue
        (cond
         ((> (setq i (1+ i)) 1000)
          (message "jsx-skip ** warning **")
          (setq continue nil))
         ((looking-at "<[[:alpha:]][[:alnum:]:-]*[ ]*/>")
          (goto-char (match-end 0))
          (setq pos (point))
          (setq continue nil))
         ((not (rjsx-mode-dom-rsf ">\\([ \t\n]*[\];,)':}|&]\\)\\|{" reg-end))
          (setq continue nil)
          )
         ((eq (char-before) ?\{)
          (backward-char)
          (rjsx-mode-closing-paren reg-end)
          (forward-char)
          )
         (t
          (setq continue nil)
          (setq pos (match-beginning 1))
          ) ;t
         ) ;cond
        ) ;while
      ) ;save-excursion
    (when pos (goto-char pos))
    ;;(message "jsx-skip: %S" pos)
    pos))

(defun rjsx-mode-jsx-skip2 (reg-end)
  (let ((continue t) (pos nil) (i 0) (tag nil) (regexp nil) (counter 1))
    (looking-at "<\\([[:alpha:]][[:alnum:]:-]*\\)")
    (setq tag (match-string-no-properties 1))
    (setq regexp (concat "</?" tag))
    ;;(message "point=%S tag=%S" (point) tag)
    (save-excursion
      (while continue
        (cond
         ((> (setq i (1+ i)) 100)
          (message "jsx-skip ** warning **")
          (setq continue nil))
         ((looking-at "<[[:alpha:]][[:alnum:]:-]*[ ]*/>")
          (goto-char (match-end 0))
          (setq pos (point))
          (setq continue nil))
         ((not (rjsx-mode-dom-rsf ">\\([ \t\n]*[\];,)':}]\\)\\|{" reg-end))
          (setq continue nil)
          )
         ((eq (char-before) ?\{)
          (backward-char)
          (rjsx-mode-closing-paren reg-end)
          (forward-char)
          )
         (t
          (setq continue nil)
          (setq pos (match-beginning 1))
          ) ;t
         ) ;cond
        ) ;while
      ) ;save-excursion
    (when pos (goto-char pos))
    ;;(message "jsx-skip: %S" pos)
    pos))

;; http://facebook.github.io/jsx/
;; https://github.com/facebook/jsx/blob/master/AST.md
(defun rjsx-mode-jsx-scan-element (reg-beg reg-end depth)
  (unless depth (setq depth 1))
  (save-excursion
    (let (token-beg token-end regexp)
      (goto-char reg-beg)
      (put-text-property reg-beg (1+ reg-beg) 'jsx-beg depth)
      (put-text-property (1- reg-end) reg-end 'jsx-end depth)
      (put-text-property reg-beg reg-end 'jsx-depth depth)
      (goto-char reg-beg)
      ;;(while (rjsx-mode-part-sf "/*" reg-end t)
      ;;  (goto-char (match-beginning 0))
      ;;  (if (looking-back "{")
      ;;      (progn
      ;;        (backward-char)
      ;;        (setq regexp "*/}"))
      ;;    (setq regexp "*/"))
      ;;  (setq token-beg (point))
      ;;  (if (not (rjsx-mode-part-sf regexp reg-end t))
      ;;      (goto-char reg-end)
      ;;    (setq token-end (point))
      ;;    (put-text-property token-beg token-end 'part-token 'comment)
      ;;    ) ;if
      ;;  ) ;while
      (rjsx-mode-scan-elements reg-beg reg-end)
      (rjsx-mode-jsx-scan-expression reg-beg reg-end (1+ depth))
      )))

(defun rjsx-mode-jsx-scan-expression (reg-beg reg-end depth)
  (let ((continue t) beg end)
    (save-excursion
      (goto-char reg-beg)
      ;;(message "reg-beg=%S reg-end=%S" reg-beg reg-end)
      (while (and continue (search-forward "{" reg-end t))
        (backward-char)
        (setq beg (point)
              end (rjsx-mode-closing-paren reg-end))
        (cond
         ((eq (get-text-property beg 'part-token) 'comment)
          (forward-char))
         ((not end)
          (setq continue nil))
         (t
          (setq end (1+ end))
          (put-text-property beg end 'jsx-depth depth)
          (put-text-property beg (1+ beg) 'jsx-beg depth)
          (put-text-property (1- end) end 'jsx-end depth)
          (rjsx-mode-part-scan beg end "jsx" (1+ depth))
          ) ;t
         ) ;cond
        )
      )
    ))

(defun rjsx-mode-jsx-is-html (&optional pos)
  (interactive)
  (unless pos (setq pos (point)))
  (let ((depth (get-text-property pos 'jsx-depth)))
    (cond
     ((or (null depth) (<= pos 2))
      (setq pos nil))
     ((and (= depth 1) (get-text-property pos 'jsx-beg))
      (setq pos nil))
     ((get-text-property pos 'jsx-beg)
      (setq pos (null (get-text-property pos 'tag-beg))))
     ((setq pos (rjsx-mode-jsx-depth-beginning-position pos))
      (setq pos (not (null (get-text-property pos 'tag-beg)))))
     (t
      (setq pos nil))
     ) ;cond
    ;;(message "is-html: %S (depth=%S)" pos depth)
    pos))

(defun rjsx-mode-jsx-depth-beginning-position (&optional pos target-depth)
  (interactive)
  (unless pos (setq pos (point)))
  (unless target-depth (setq target-depth (get-text-property pos 'jsx-depth)))
  (cond
   ((or (null target-depth) (bobp))
    (setq pos nil))
   ((and (get-text-property pos 'jsx-beg) (= target-depth (get-text-property pos 'jsx-depth)))
    )
   (t
    (let ((continue t) depth)
      (while continue
        (setq pos (previous-single-property-change pos 'jsx-depth))
        (cond
         ((or (null pos)
              (null (setq depth (get-text-property pos 'jsx-depth))))
          (setq continue nil
                pos nil))
         ((and (get-text-property pos 'jsx-beg) (= target-depth depth))
          (setq continue nil))
         ) ;cond
        ) ;while
      ) ;let
    ) ;t
   ) ;cond
  pos)

(defun rjsx-mode-jsx-element-next (reg-end)
  (let (continue beg end)
    (setq beg (point))
    (unless (get-text-property beg 'jsx-depth)
      (setq beg (next-single-property-change beg 'jsx-beg)))
    (setq continue (and beg (< beg reg-end))
          end beg)
    (while continue
      (setq end (next-single-property-change end 'jsx-end))
      (cond
       ((or (null end) (> end reg-end))
        (setq continue nil
              end nil))
       ((eq (get-text-property end 'jsx-depth) 1)
        (setq continue nil))
       (t
        (setq end (1+ end)))
       ) ;cond
      ) ;while
    ;;(message "beg=%S end=%S" beg end)
    (if (and beg end (< beg end)) (cons beg end) nil)))

(defun rjsx-mode-jsx-expression-next (reg-end)
  (let (beg end depth continue pos)
    (setq beg (point))
    ;;(message "pt=%S" beg)
    (unless (and (get-text-property beg 'jsx-beg) (null (get-text-property beg 'tag-beg)))
      ;;(setq beg (next-single-property-change beg 'jsx-beg))
      (setq continue t
            pos (1+ beg))
      (while continue
        (setq pos (next-single-property-change pos 'jsx-beg))
        (cond
         ((null pos)
          (setq continue nil
                beg nil))
         ((> pos reg-end)
          (setq continue nil
                beg nil))
         ((null (get-text-property pos 'jsx-beg))
          )
         ((null (get-text-property pos 'tag-beg))
          (setq continue nil
                beg pos))
         ;;(t
         ;; (setq pos (1+ pos)))
         ) ;cond
        ) ;while
      ) ;unless
    ;;(message "beg=%S" beg)
    (when (and beg (< beg reg-end))
      (setq depth (get-text-property beg 'jsx-beg)
            continue (not (null depth))
            pos beg)
      ;;(message "beg=%S" beg)
      (while continue
        (setq pos (next-single-property-change pos 'jsx-end))
        ;;(message "pos=%S" pos)
        (cond
         ((null pos)
          (setq continue nil))
         ((> pos reg-end)
          (setq continue nil))
         ((eq depth (get-text-property pos 'jsx-end))
          (setq continue nil
                end pos))
         (t
          ;;(setq pos (1+ pos))
          )
         ) ;cond
        ) ;while
      ) ;when
    ;;(message "%S > %S" beg end)
    (if (and beg end) (cons beg end) nil)))

(defun rjsx-mode-jsx-depth-next (reg-end)
  (let (beg end depth continue pos)
    (setq beg (point))
    ;;(message "pt=%S" beg)
    (unless (get-text-property beg 'jsx-beg)
      ;;(setq beg (next-single-property-change beg 'jsx-beg))
      ;;(setq pos (1+ beg))
      (setq pos (next-single-property-change (1+ beg) 'jsx-beg))
      (cond
       ((null pos)
        (setq beg nil))
       ((>= pos reg-end)
        (setq beg nil))
       (t
        (setq beg pos))
       ) ;cond
      ) ;unless
    ;;(message "beg=%S" beg)
    (when beg
      (setq depth (get-text-property beg 'jsx-beg)
            continue (not (null depth))
            pos beg)
      ;;(message "beg=%S" beg)
      (while continue
        (setq pos (next-single-property-change pos 'jsx-end))
        ;;(message "pos=%S" pos)
        (cond
         ((null pos)
          (setq continue nil))
         ((> pos reg-end)
          (setq continue nil))
         ((eq depth (get-text-property pos 'jsx-end))
          (setq continue nil
                end pos))
         (t
          ;;(setq pos (1+ pos))
          )
         ) ;cond
        ) ;while
      ) ;when
    ;;(message "%S > %S" beg end)
    (if (and beg end) (cons beg end) nil)))

(defun rjsx-mode-jsx-beginning ()
  (interactive)
  (let (depth (continue t) (reg-beg (point-min)) (pos (point)))
    (setq depth (get-text-property pos 'jsx-depth))
    (cond
     ((not depth)
      )
     ((get-text-property (1- pos) 'jsx-beg)
      (goto-char (1- pos)))
     (t
      (while continue
        (setq pos (previous-single-property-change pos 'jsx-beg))
        ;;(message "pos=%S" pos)
        (cond
         ((null pos)
          (setq continue nil))
         ((<= pos reg-beg)
          (setq continue nil))
         ((eq depth (get-text-property pos 'jsx-beg))
          (setq continue nil))
         ) ;cond
        ) ;while
      (rjsx-mode-go pos)
      ) ;t
     ) ;cond
    ))

(defun rjsx-mode-jsx-end ()
  (interactive)
  (let (depth (continue t) (reg-end (point-max)) (pos (point)))
    (setq depth (get-text-property pos 'jsx-depth))
    (cond
     ((not depth)
      )
     ((get-text-property pos 'jsx-end)
      (goto-char (+ pos 1)))
     (t
      (while continue
        (setq pos (next-single-property-change pos 'jsx-end))
        ;;(message "pos=%S" pos)
        (cond
         ((null pos)
          (setq continue nil))
         ((> pos reg-end)
          (setq continue nil))
         ((eq depth (get-text-property pos 'jsx-end))
          (setq continue nil))
         ) ;cond
        ) ;while
      (rjsx-mode-go pos 1)
      ) ;t
     ) ;cond
    ))

(defun rjsx-mode-velocity-skip (pos)
  (goto-char pos)
  (let ((continue t) (i 0))
    (when (eq ?\# (char-after))
      (forward-char))
    (when (member (char-after) '(?\$ ?\@))
      (forward-char))
    (when (member (char-after) '(?\!))
      (forward-char))
    (if (member (char-after) '(?\{))
        (search-forward "}")
      (setq continue t)
      (while continue
        (skip-chars-forward "a-zA-Z0-9_-")
        (when (> (setq i (1+ i)) 500)
          (message "velocity-skip ** warning (%S) **" pos)
          (setq continue nil))
        (when (member (char-after) '(?\())
          (search-forward ")" nil t))
        (if (member (char-after) '(?\.))
            (forward-char)
          (setq continue nil))
        ) ;while
      ) ;if
    ))

(defun rjsx-mode-razor-skip (pos)
  (goto-char pos)
  (let ((continue t) (i 0))
    (while continue
      (skip-chars-forward " =@a-zA-Z0-9_-")
      (cond
       ((> (setq i (1+ i)) 500)
        (message "razor-skip ** warning **")
        (setq continue nil))
       ((and (eq (char-after) ?\*)
             (eq (char-before) ?@))
        (when (not (search-forward "*@" nil t))
          (setq continue nil))
        )
       ((looking-at-p "@[({]")
        (forward-char)
        (when (setq pos (rjsx-mode-closing-paren-position (point)))
          (goto-char pos))
        (forward-char)
        )
       ((and (not (eobp)) (eq ?\( (char-after)))
        (if (looking-at-p "[ \n]*[<@]")
            (setq continue nil)
          (when (setq pos (rjsx-mode-closing-paren-position))
            (goto-char pos))
          (forward-char)
          ) ;if
        )
       ((and (not (eobp)) (eq ?\. (char-after)))
        (forward-char))
       ((and (not (eobp)) (looking-at-p "[ \n]*else"))
        (re-search-forward "[ \t]*else")
        )
       ((looking-at-p "[ \n]*{")
        (search-forward "{")
        (if (looking-at-p "[ \n]*[<@]")
            (setq continue nil)
          (backward-char)
          (when (setq pos (rjsx-mode-closing-paren-position))
            (goto-char pos))
          (forward-char)
          ) ;if
        )
       ((looking-at-p "}")
        (forward-char))
       (t
        (setq continue nil))
       ) ;cond
      ) ;while
    ))

(defun rjsx-mode-propertize (&optional beg end)

  (unless beg (setq beg rjsx-mode-change-beg))
  (unless end (setq end rjsx-mode-change-end))

  ;;(message "%S %S" rjsx-mode-content-type (get-text-property beg 'part-side))
  ;;(message "propertize: beg(%S) end(%S)" rjsx-mode-change-beg rjsx-mode-change-end)
  ;;(message "%S %S" (get-text-property beg 'part-side) (get-text-property end 'part-side))

  (when (and end (> end (point-max)))
    (setq end (point-max)))

  (setq rjsx-mode-change-beg nil
        rjsx-mode-change-end nil)
  (cond

   ((or (null beg) (null end))
    nil)

   ((and (or (member rjsx-mode-content-type '("jsx" "javascript"))
             (and (get-text-property beg 'part-side)
                  (get-text-property end 'part-side)
                  (> beg (point-min))
                  (get-text-property (1- beg) 'part-side))
             ))
    ;;(message "invalidate part (%S > %S)" beg end)
    (rjsx-mode-invalidate-part-region beg end))

   (t
    ;;(message "invalidate default (%S > %S)" beg end)
    (rjsx-mode-invalidate-region beg end))

   ) ;cond

  )

;; NOTE: il est important d'identifier des caractères en fin de ligne
;; rjsx-mode-block-tokenize travaille en effet sur les fins de lignes pour
;; les commentaires de type //
(defun rjsx-mode-invalidate-block-region (pos-beg pos-end)
  ;;  (message "pos-beg(%S) pos-end(%S)" pos-beg pos-end)
  (save-excursion
    (let (beg end code-beg code-end)
      ;;(message "invalidate-block-region: pos-beg(%S)=%S" pos-beg (get-text-property pos 'block-side))
      ;;(message "code-beg(%S) code-end(%S) pos-beg(%S) pos-end(%S)" code-beg code-end pos-beg pos-end)
      (cond
       ((not (and (setq code-beg (rjsx-mode-block-code-beginning-position pos-beg))
                  (setq code-end (rjsx-mode-block-code-end-position pos-beg))
                  (>= pos-beg code-beg)
                  (<= pos-end code-end)
                  (> code-end code-beg)))
        (rjsx-mode-invalidate-region pos-beg pos-end))
       (t
        (goto-char pos-beg)
        (if (rjsx-mode-block-rsb "[;{}(][ ]*\n" code-beg)
            (setq beg (match-end 0))
          (setq beg code-beg))
        (goto-char pos-end)
        (if (rjsx-mode-block-rsf "[;{})][ ]*\n" code-end)
            (setq end (1- (match-end 0)))
          (setq end code-end))
        (rjsx-mode-block-tokenize beg end)
        ;;(message "beg(%S) end(%S)" beg end)
        (cons beg end)
        )
       ) ;cond
      )))

(defun rjsx-mode-invalidate-part-region (pos-beg pos-end)
  (save-excursion
    (let (beg end part-beg part-end language)
      (if (member rjsx-mode-content-type rjsx-mode-part-content-types)
          (setq language rjsx-mode-content-type)
        (setq language (symbol-name (get-text-property pos-beg 'part-side))))
      (setq part-beg (rjsx-mode-part-beginning-position pos-beg)
            part-end (rjsx-mode-part-end-position pos-beg))
      ;;(message "language(%S) pos-beg(%S) pos-end(%S) part-beg(%S) part-end(%S)"
      ;;         language pos-beg pos-end part-beg part-end)
      (goto-char pos-beg)
      (cond
       ((not (and part-beg part-end
                  (>= pos-beg part-beg)
                  (<= pos-end part-end)
                  (> part-end part-beg)))
        (rjsx-mode-invalidate-region pos-beg pos-end))
       ((member language '("javascript" "json" "jsx"))
        (if (rjsx-mode-javascript-rsb "[;{}(][ ]*\n" part-beg)
            (setq beg (match-end 0))
          (setq beg part-beg))
        (goto-char pos-end)
        (if (rjsx-mode-javascript-rsf "[;{})][ ]*\n" part-end)
            (setq end (match-end 0))
          (setq end part-end))
        (rjsx-mode-scan-region beg end language))
       (t
        (setq beg part-beg
              end part-end)
        (rjsx-mode-scan-region beg end language))
       ) ;cond
      )))

(defun rjsx-mode-invalidate-region (reg-beg reg-end)
  ;;(message "%S | reg-beg(%S) reg-end(%S)" (point) reg-beg reg-end)
  (setq reg-beg (rjsx-mode-invalidate-region-beginning-position reg-beg)
        reg-end (rjsx-mode-invalidate-region-end-position reg-end))
  ;;(message "invalidate-region: reg-beg(%S) reg-end(%S)" reg-beg reg-end)
  (rjsx-mode-scan-region reg-beg reg-end))

(defun rjsx-mode-invalidate-region-beginning-position (pos)
  (save-excursion
    (goto-char pos)
    (when (and (bolp) (not (bobp)))
      (backward-char))
    (beginning-of-line)
    ;;(message "pos=%S %S" (point) (text-properties-at (point)))
    (setq pos (point-min))
    (let ((continue (not (bobp))))
      (while continue
        (cond
         ((bobp)
          (setq continue nil))
         ;; NOTE: Going back to the previous start tag is necessary
         ;; when inserting a part endtag (e.g. </script>).
         ;; Indeed, parts must be identified asap.
         ((and (progn (back-to-indentation) t)
               (get-text-property (point) 'tag-beg)
               (eq (get-text-property (point) 'tag-type) 'start))
          (setq pos (point)
                continue nil))
         (t
          (forward-line -1))
         ) ;cond
        ) ;while
      ;;(message "pos=%S" pos)
      pos)))

(defun rjsx-mode-invalidate-region-end-position (pos)
  (save-excursion
    (goto-char pos)
    ;;(message "pos=%S %S" pos (get-text-property pos 'block-token))

    (setq pos (point-max))
    (let ((continue (not (eobp))))
      (while continue
        (end-of-line)
        ;;(message "%S %S" (point) (get-text-property (point) 'block-token))
        (cond
         ((eobp)
          (setq continue nil))
         ;;()
         ((and (not (get-text-property (point) 'tag-type))
               (not (get-text-property (point) 'part-side))
               (not (get-text-property (point) 'block-side)))
          (setq pos (point)
                continue nil))
         (t
          (forward-line))
         ) ;cond
        ) ;while
      pos)))

(defun rjsx-mode-buffer-scan ()
  "Scan entine buffer."
  (interactive)
  (rjsx-mode-scan-region (point-min) (point-max)))

;;---- FONTIFICATION -----------------------------------------------------------

(defun rjsx-mode-font-lock-highlight (limit)
  ;;(message "font-lock-highlight: point(%S) limit(%S) change-beg(%S) change-end(%S)" (point) limit rjsx-mode-change-beg rjsx-mode-change-end)
  (cond
   (rjsx-mode-inhibit-fontification
    nil)
   (t
    (rjsx-mode-highlight-region (point) limit)
    nil)
   ))

(defun rjsx-mode-buffer-highlight ()
  (interactive)
  ;;(if (fboundp 'font-lock-flush)
  ;;    (font-lock-flush)
  ;;  (font-lock-fontify-buffer))
  (if (fboundp 'font-lock-flush)
      (progn
        (font-lock-flush)
        (font-lock-ensure))
    ;;(font-lock-fontify-buffer)
    (font-lock-fontify-region (point-min) (point-max)) ;emacs 24
    ) ;if
  )

(defun rjsx-mode-extend-region ()
  ;;(message "extend-region: flb(%S) fle(%S) wmcb(%S) wmce(%S)" font-lock-beg font-lock-end rjsx-mode-change-beg rjsx-mode-change-end)
  ;;  (setq font-lock-beg rjsx-mode-change-beg
  ;;        font-lock-end rjsx-mode-change-end)
  (cond
   (rjsx-mode-inhibit-fontification
    nil)
   (t ;;(and rjsx-mode-change-beg rjsx-mode-change-end)
    (when (or (null rjsx-mode-change-beg) (< font-lock-beg rjsx-mode-change-beg))
      ;;(message "font-lock-beg(%S) < rjsx-mode-change-beg(%S)" font-lock-beg rjsx-mode-change-beg)
      (setq rjsx-mode-change-beg font-lock-beg))
    (when (or (null rjsx-mode-change-end) (> font-lock-end rjsx-mode-change-end))
      ;;(message "font-lock-end(%S) > rjsx-mode-change-end(%S)" font-lock-end rjsx-mode-change-end)
      (setq rjsx-mode-change-end font-lock-end))
    (let ((region (rjsx-mode-propertize rjsx-mode-change-beg rjsx-mode-change-end)))
      (when region
        ;;(message "region: %S" region)
        (setq font-lock-beg (car region)
              font-lock-end (cdr region)
              ;;rjsx-mode-change-beg (car region)
              ;;rjsx-mode-change-end (cdr region)
              )
        ) ;when
      ) ;let
    nil) ;t
   ))

(defun rjsx-mode-unfontify-region (beg end)
  ;;(message "unfontify: %S %S" beg end)
  )

(defun rjsx-mode-highlight-region (&optional beg end) ;; content-type)
  ;;(message "highlight-region: beg(%S) end(%S)" beg end)
  (with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((buffer-undo-list t)
               (inhibit-point-motion-hooks t)
               (inhibit-quit t))
           (remove-list-of-text-properties beg end '(font-lock-face face))
           (cond
            ((and (get-text-property beg 'block-side)
                  (not (get-text-property beg 'block-beg)))
             (rjsx-mode-block-highlight beg end))
            ((or (member rjsx-mode-content-type rjsx-mode-part-content-types)
                 (get-text-property beg 'part-side))
             (rjsx-mode-part-highlight beg end)
             (rjsx-mode-process-blocks beg end 'rjsx-mode-block-highlight))
            ((string= rjsx-mode-engine "none")
             (rjsx-mode-highlight-tags beg end)
             (rjsx-mode-process-parts beg end 'rjsx-mode-part-highlight))
            (t
             (rjsx-mode-highlight-tags beg end)
             (rjsx-mode-process-parts beg end 'rjsx-mode-part-highlight)
             (rjsx-mode-process-blocks beg end 'rjsx-mode-block-highlight))
            ) ;cond
           (when rjsx-mode-enable-element-content-fontification
             (rjsx-mode-highlight-elements beg end))
           (when rjsx-mode-enable-whitespace-fontification
             (rjsx-mode-highlight-whitespaces beg end))
           ;;(message "%S %S" font-lock-keywords font-lock-keywords-alist)
           ))))))

(defun rjsx-mode-highlight-tags (reg-beg reg-end &optional depth)
  (let ((continue t))
    (goto-char reg-beg)
    (when (and (not (get-text-property (point) 'tag-beg))
               (not (rjsx-mode-tag-next)))
      (setq continue nil))
    (when (and continue (>= (point) reg-end))
      (setq continue nil))
    (while continue
      (cond
       (depth
        (when (eq depth (get-text-property (point) 'jsx-depth))
          (rjsx-mode-tag-highlight))
        )
       (t
        (rjsx-mode-tag-highlight))
       ) ;cond
      (when (or (not (rjsx-mode-tag-next))
                (>= (point) reg-end))
        (setq continue nil))
      ) ;while
    (when rjsx-mode-enable-inlays
      (when (null rjsx-mode-inlay-regexp)
        (setq rjsx-mode-inlay-regexp (regexp-opt '("\\[" "\\(" "\\begin{align}"))))
      (let (beg end expr)
        (goto-char reg-beg)
        (while (rjsx-mode-dom-rsf rjsx-mode-inlay-regexp reg-end)
          (setq beg (match-beginning 0)
                end nil
                expr (substring (match-string-no-properties 0) 0 2))
          (setq expr (cond
                      ((string= expr "\\[") "\\]")
                      ((string= expr "\\(") "\\)")
                      (t "\\end{align}")))
          (when (and (rjsx-mode-dom-sf expr reg-end)
                     (setq end (match-end 0))
                     (not (text-property-any beg end 'tag-end t)))
            (font-lock-append-text-property beg end 'font-lock-face 'rjsx-mode-inlay-face)
            ) ;when
          ) ;while
        ) ;let
      ) ;when
    (when rjsx-mode-enable-html-entities-fontification
      (let (beg end)
        (goto-char reg-beg)
        (while (rjsx-mode-dom-rsf "&\\([#]?[[:alnum:]]\\{2,8\\}\\);" reg-end)
          (setq beg (match-beginning 0)
                end (match-end 0))
          (when (not (text-property-any beg end 'tag-end t))
            (font-lock-append-text-property beg end 'font-lock-face 'rjsx-mode-html-entity-face)
            ) ;when
          ) ;while
        ) ;let
      ) ;when
    ))

(defun rjsx-mode-tag-highlight (&optional beg end)
  (unless beg (setq beg (point)))
  (unless end (setq end (1+ (rjsx-mode-tag-end-position beg))))
  (let (name type face flags slash-beg slash-end bracket-end)
    (setq flags (get-text-property beg 'tag-beg)
          type (get-text-property beg 'tag-type)
          name (get-text-property beg 'tag-name))
    (cond
     ((eq type 'comment)
      (put-text-property beg end 'font-lock-face 'rjsx-mode-comment-face))
     (name
      (setq face (cond
                  ((and rjsx-mode-enable-element-tag-fontification
                        (setq face (cdr (assoc name rjsx-mode-element-tag-faces))))
                   face)
                  ((> (logand flags 32) 0) 'rjsx-mode-html-tag-namespaced-face)
                  ((> (logand flags 2) 0)  'rjsx-mode-html-tag-custom-face)
                  (t                       'rjsx-mode-html-tag-face))
            slash-beg (> (logand flags 4) 0)
            slash-end (> (logand flags 8) 0)
            bracket-end (> (logand flags 16) 0))
      (put-text-property beg (+ beg (if slash-beg 2 1))
                         'font-lock-face 'rjsx-mode-html-tag-bracket-face)
      (put-text-property (+ beg (if slash-beg 2 1)) (+ beg (if slash-beg 2 1) (length name))
                         'font-lock-face face)
      (when (or slash-end bracket-end)
        (put-text-property (- end (if slash-end 2 1)) end 'font-lock-face 'rjsx-mode-html-tag-bracket-face)
        ) ;when
      (when (> (logand flags 1) 0)
        ;;(message "%S>%S" beg end)
        (rjsx-mode-highlight-attrs beg end))
      ) ;case name
     ) ;cond
    ))

(defun rjsx-mode-highlight-attrs (reg-beg reg-end)
  (let ((continue t) (pos reg-beg) beg end flags offset face)
    ;;(message "highlight-attrs %S>%S" reg-beg reg-end)
    (while continue
      (setq beg (rjsx-mode-attribute-next-position pos reg-end))
      (cond
       ((or (null beg) (>= beg reg-end))
        (setq continue nil))
       (t
        (setq flags (or (get-text-property beg 'tag-attr-beg) 0))
        (setq face (cond
                    ((= (logand flags 1) 1) 'rjsx-mode-html-attr-custom-face)
                    ((= (logand flags 2) 2) 'rjsx-mode-html-attr-engine-face)
                    ((= (logand flags 4) 4) nil)
                    (t                      'rjsx-mode-html-attr-name-face)))
        ;;(setq end (if (get-text-property beg 'tag-attr-end) beg (rjsx-mode-attribute-end-position beg)))
        (setq end (rjsx-mode-attribute-end-position beg))
        ;;(message "beg=%S end=%S" beg end)
        (cond
         ((or (null end) (>= end reg-end))
          (setq continue nil))
         (t
          (setq offset (get-text-property end 'tag-attr-end))
          (if (= offset 0)
              (put-text-property beg (1+ end) 'font-lock-face face)
            (put-text-property beg (+ beg offset) 'font-lock-face face)
            (put-text-property (+ beg offset) (+ beg offset 1)
                               'font-lock-face
                               'rjsx-mode-html-attr-equal-face)
            (when (not (get-text-property (+ beg offset 1) 'jsx-beg))
              (put-text-property (+ beg offset 1) (1+ end)
                                 'font-lock-face
                                 'rjsx-mode-html-attr-value-face)
              )
            ) ;if offset
          (setq pos (1+ end))
          ) ;t
         ) ;cond
        ) ;t
       );cond
      ) ;while
    ))

(defun rjsx-mode-block-highlight (reg-beg reg-end)
  (let (sub1 sub2 sub3 continue char keywords token-type face beg end (buffer (current-buffer)))
    ;;(message "reg-beg=%S reg-end=%S" reg-beg reg-end)

    ;; NOTE: required for block inside tag attr
    (remove-list-of-text-properties reg-beg reg-end '(font-lock-face))

    (goto-char reg-beg)

    (when (null rjsx-mode-engine-font-lock-keywords)
      (setq sub1 (buffer-substring-no-properties
                  reg-beg (+ reg-beg 1))
            sub2 (buffer-substring-no-properties
                  reg-beg (+ reg-beg 2))
            sub3 (buffer-substring-no-properties
                  reg-beg (+ reg-beg (if (>= (point-max) (+ reg-beg 3)) 3 2))))
      )

    (cond

     ((and (get-text-property reg-beg 'block-beg)
           (eq (get-text-property reg-beg 'block-token) 'comment))
      (put-text-property reg-beg reg-end 'font-lock-face 'rjsx-mode-comment-face)
      ) ;comment block

     (rjsx-mode-engine-font-lock-keywords
      (setq keywords rjsx-mode-engine-font-lock-keywords)
      )
     ) ;cond

    (when keywords
      (rjsx-mode-fontify-region reg-beg reg-end keywords)
      (setq continue t)
      (setq end reg-beg)
      (while continue
        (if (get-text-property end 'block-token)
            (setq beg end)
          (setq beg (next-single-property-change end 'block-token buffer reg-end)))
        (setq end nil)
        (when beg (setq char (char-after beg)))
        (if (and beg (< beg reg-end))
            (progn
              (setq token-type (get-text-property beg 'block-token))
              (setq face (cond
                          ((eq token-type 'string)  'rjsx-mode-block-string-face)
                          ((eq token-type 'comment) 'rjsx-mode-block-comment-face)
                          (t                        'rjsx-mode-block-delimiter-face)))
              (setq end (next-single-property-change beg 'block-token buffer reg-end))
;;              (message "end=%S" end)
              (if (and end (<= end reg-end))
                  (progn
                    ;;(message "%S > %S face(%S)" beg end face)
                    (remove-list-of-text-properties beg end '(face))
                    (put-text-property beg end 'font-lock-face face)
                    )
                (setq continue nil
                      end nil)
                ) ;if end
              ) ;progn beg
          (setq continue nil
                end nil)
          ) ;if beg
        (when (and beg end)
          (save-match-data
            (when (and rjsx-mode-enable-heredoc-fontification
                       (eq char ?\<)
                       (> (- end beg) 8)
                       ;;(progn (message "%S" (buffer-substring-no-properties beg end)) t)
                       (string-match-p "JS\\|JAVASCRIPT\\|HTM\\|CSS" (buffer-substring-no-properties beg end)))
              (setq keywords
                    (cond
                     ((string-match-p "H" (buffer-substring-no-properties beg (+ beg 8)))
                      rjsx-mode-html-font-lock-keywords)
                     (t
                      rjsx-mode-javascript-font-lock-keywords)
                     ))
              (rjsx-mode-fontify-region beg end keywords)
            ))
;;          (message "%S %c %S beg=%S end=%S" rjsx-mode-enable-string-interpolation char rjsx-mode-engine beg end)
          ) ;when beg end
        ) ;while continue
      ) ;when keywords

    (when rjsx-mode-enable-block-face
;;      (message "block-face %S %S" reg-beg reg-end)
      (font-lock-append-text-property reg-beg reg-end 'face 'rjsx-mode-block-face))

    ))

(defun rjsx-mode-part-highlight (reg-beg reg-end &optional depth)
  (save-excursion
    (let (start continue token-type face pos beg end string-face comment-face content-type)
      ;;(message "part-highlight: reg-beg(%S) reg-end(%S)" reg-beg reg-end)
      (if (member rjsx-mode-content-type rjsx-mode-part-content-types)
          (setq content-type rjsx-mode-content-type)
        (setq content-type (symbol-name (get-text-property reg-beg 'part-side))))
      ;;(message "content-type=%S" content-type)
      (unless depth
        (when (string= content-type "jsx") (setq depth 0))
        )
      (setq string-face 'rjsx-mode-part-string-face
            comment-face 'rjsx-mode-part-comment-face)
      (cond
       ((member content-type '("javascript" "jsx"))
        (setq string-face 'rjsx-mode-javascript-string-face
              comment-face 'rjsx-mode-javascript-comment-face)
        (rjsx-mode-fontify-region reg-beg reg-end rjsx-mode-javascript-font-lock-keywords))
       ((string= content-type "json")
        (setq string-face 'rjsx-mode-json-string-face
              comment-face 'rjsx-mode-json-comment-face)
        (rjsx-mode-fontify-region reg-beg reg-end rjsx-mode-javascript-font-lock-keywords))
       ) ;cond

      (goto-char reg-beg)

      ;;(when (string= content-type "jsx") (rjsx-mode-highlight-tags reg-beg reg-end))
      ;;(setq continue (and pos (< pos reg-end)))
      (setq continue t
            pos reg-beg)
      (while continue
        (if (get-text-property pos 'part-token)
            (setq beg pos)
          (setq beg (next-single-property-change pos 'part-token)))
        (cond
         ((or (null beg) (>= beg reg-end))
          (setq continue nil
                end nil))
         ((and (eq depth 0) (get-text-property beg 'jsx-depth))
          (setq pos (or (next-single-property-change beg 'jsx-depth) (point-max))))
         (t
          (setq token-type (get-text-property beg 'part-token))
          (setq face (cond
                      ((eq token-type 'string)  string-face)
                      ((eq token-type 'comment) comment-face)
                      ((eq token-type 'context) 'rjsx-mode-json-context-face)
                      ((eq token-type 'key)     'rjsx-mode-json-key-face)
                      (t                        nil)))
          (setq end (or (next-single-property-change beg 'part-token) (point-max))
                pos end)
          (cond
           ((or (null end) (> end reg-end))
            (setq continue nil
                  end nil))
           (t
            (when face
              (remove-list-of-text-properties beg end '(face))
              (put-text-property beg end 'font-lock-face face))
            (cond
             ((< (- end beg) 6)
              )
             ((eq token-type 'string)
              (when (and rjsx-mode-enable-string-interpolation
                         (member content-type '("javascript" "jsx")))
                (rjsx-mode-interpolate-javascript-string beg end)))
             ) ;cond
            ) ;t
           ) ;cond
          ) ;t
         ) ;cond
        ) ;while

      (when (and (string= rjsx-mode-content-type "html") rjsx-mode-enable-part-face)
        (font-lock-append-text-property reg-beg reg-end 'face
                                        (cond
                                         ((string= content-type "javascript")
                                          'rjsx-mode-script-face)
                                         (t
                                          'rjsx-mode-part-face)))
        )

      (when (and (eq depth 0) (string= content-type "jsx"))
        (let (pair elt-beg elt-end exp-beg exp-end exp-depth)
          (goto-char reg-beg)
          (while (setq pair (rjsx-mode-jsx-element-next reg-end))
            ;;(message "elt-pair=%S" pair)
            (setq elt-beg (car pair)
                  elt-end (cdr pair))
            (remove-list-of-text-properties elt-beg (1+ elt-end) '(face))
            (rjsx-mode-highlight-tags elt-beg elt-end 1)
            (goto-char elt-beg)
            (while (setq pair (rjsx-mode-jsx-expression-next elt-end))
              ;;(message "exp-pair=%S elt-end=%S" pair elt-end)
              (setq exp-beg (car pair)
                    exp-end (cdr pair))
              (when (eq (char-after exp-beg) ?\{)
                ;;(message "%S : %c %c" exp-beg (char-after (+ exp-beg 1)) (char-after (+ exp-beg 2)))
                (cond
                 ;;((and (eq (char-after (+ exp-beg 1)) ?\/) (eq (char-after (+ exp-beg 2)) ?\*))
                 ;; (put-text-property exp-beg (1+ exp-end) 'font-lock-face 'rjsx-mode-part-comment-face)
                 ;; )
                 (t
                  (setq exp-depth (get-text-property exp-beg 'jsx-depth))
                  (remove-list-of-text-properties exp-beg exp-end '(font-lock-face))
                  (put-text-property exp-beg (1+ exp-beg) 'font-lock-face 'rjsx-mode-block-delimiter-face)
                  (when (and (eq (get-text-property exp-beg 'tag-attr-beg) 4) (rjsx-mode-looking-at-p "\.\.\." (1+ exp-beg)))
                  (put-text-property exp-beg (+ exp-beg 4) 'font-lock-face 'rjsx-mode-block-delimiter-face))
                  (put-text-property exp-end (1+ exp-end) 'font-lock-face 'rjsx-mode-block-delimiter-face)
                  (rjsx-mode-highlight-tags (1+ exp-beg) exp-end (1+ exp-depth))
                  (rjsx-mode-part-highlight (1+ exp-beg) exp-end exp-depth)
                  (rjsx-mode-fontify-region (1+ exp-beg) exp-end rjsx-mode-javascript-font-lock-keywords)
                  ) ;t
                 ) ;cond
                ) ;when
              (goto-char (1+ exp-beg))
              ) ;while exp

            (when (and elt-beg rjsx-mode-jsx-depth-faces)
              (let (depth-beg depth-end jsx-face)
                (goto-char elt-beg)
                (while (setq pair (rjsx-mode-jsx-depth-next reg-end))
                  ;;(message "depth-pair=%S" pair)
                  (setq depth-beg (car pair)
                        depth-end (cdr pair)
                        depth (get-text-property depth-beg 'jsx-depth)
                        jsx-face (elt rjsx-mode-jsx-depth-faces (1- depth)))
                  ;;(message "%S" jsx-face)
                  (font-lock-prepend-text-property depth-beg (1+ depth-end) 'face jsx-face)
                  (goto-char (+ depth-beg 2))
                  )
                ) ;let
              )

            (goto-char (1+ elt-end))
            ) ;while elt
          ) ;let
        ) ;when

      ) ;let
    ) ;save-excursion
  )

(defun rjsx-mode-fontify-region (beg end keywords)
;;  (message "beg=%S end=%S" beg end);; (symbol-name keywords))
  (save-excursion
    (let ((font-lock-keywords keywords)
          (font-lock-multiline nil)
          (font-lock-keywords-case-fold-search
           nil)
          (font-lock-keywords-only t)
          (font-lock-extend-region-functions nil))
      ;;      (message "%S" keywords)
      (when (listp font-lock-keywords)
        (font-lock-fontify-region beg end)
        )
      )
    ))

(defun rjsx-mode-colorize-foreground (color)
  (let* ((values (x-color-values color))
	 (r (car values))
	 (g (cadr values))
	 (b (car (cdr (cdr values)))))
    (if (> 128.0 (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256))
	"white" "black")))

(defun rjsx-mode-colorize (beg end)
  (let (str plist len)
    (setq str (buffer-substring-no-properties beg end))
    (setq len (length str))
    (cond
     ((string= (substring str 0 1) "#")
      (setq plist (list :background str
                        :foreground (rjsx-mode-colorize-foreground str)))
      (put-text-property beg end 'face plist))
     ((or (string= (substring str 0 4) "rgb(") (string= (substring str 0 5) "rgba("))
      (setq str (format "#%02X%02X%02X"
                        (string-to-number (match-string-no-properties 1))
                        (string-to-number (match-string-no-properties 2))
                        (string-to-number (match-string-no-properties 3))))
      (setq plist (list :background str
                        :foreground (rjsx-mode-colorize-foreground str)))
      (put-text-property beg end 'face plist))
     ) ;cond
    ))

(defun rjsx-mode-interpolate-block-tag (beg end)
  (save-excursion
    (goto-char (+ 4 beg))
    (setq end (1- end))
    (while (re-search-forward "${.*?}" end t)
      (remove-list-of-text-properties (match-beginning 0) (match-end 0) '(face))
      (rjsx-mode-fontify-region (match-beginning 0) (match-end 0)
                               rjsx-mode-uel-font-lock-keywords))
    ))

(defun rjsx-mode-interpolate-javascript-string (beg end)
  (save-excursion
    (goto-char (1+ beg))
    (setq end (1- end))
    (while (re-search-forward "${.*?}" end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'font-lock-face
                         'rjsx-mode-variable-name-face)
      )
    ))

;; todo : parsing plus compliqué: {$obj->values[3]->name}
(defun rjsx-mode-interpolate-block-string (beg end)
  (save-excursion
    (goto-char (1+ beg))
    (setq end (1- end))
    ))

(defun rjsx-mode-fill-paragraph (&optional justify)
  (save-excursion
    (let ((pos (point)) fill-coll
          prop pair beg end delim-beg delim-end chunk fill-col)
      (cond
       ((or (eq (get-text-property pos 'part-token) 'comment)
            (eq (get-text-property pos 'block-token) 'comment))
        (setq prop
              (if (get-text-property pos 'part-token) 'part-token 'block-token))
        (setq pair (rjsx-mode-property-boundaries prop pos))
        (when (and pair (> (- (cdr pair) (car pair)) 6))
          (setq fill-coll (if (< fill-column 10) 70 fill-column))
          (setq beg (car pair)
                end (cdr pair))
          (goto-char beg)
          (setq chunk (buffer-substring-no-properties beg (+ beg 2)))
          (cond
           ((string= chunk "//")
            (setq delim-beg "//"
                  delim-end "EOL"))
           ((string= chunk "/*")
            (setq delim-beg "/*"
                  delim-end "*/"))
           ((string= chunk "{#")
            (setq delim-beg "{#"
                  delim-end "#}"))
           ((string= chunk "<!")
            (setq delim-beg "<!--"
                  delim-end "-->"))
           )
          )
        ) ;comment - case

       ((rjsx-mode-is-content)
        (setq pair (rjsx-mode-content-boundaries pos))
        (setq beg (car pair)
              end (cdr pair))
        )

       ) ;cond
      ;;(message "beg(%S) end(%S)" beg end)
      (when (and beg end)
        (fill-region beg end))
      t)))

(defun rjsx-mode-property-boundaries (prop &optional pos)
  "property boundaries (cdr is 1+)"
  (unless pos (setq pos (point)))
  (let (beg end val)
    (setq val (get-text-property pos prop))
    (if (null val)
        val
      (if (or (bobp)
              (not (eq (get-text-property (1- pos) prop) val)))
          (setq beg pos)
        (setq beg (previous-single-property-change pos prop))
        (when (null beg) (setq beg (point-min))))
      (if (or (eobp)
              (not (eq (get-text-property (1+ pos) prop) val)))
          (setq end pos)
        (setq end (next-single-property-change pos prop))
        (when (null end) (setq end (point-min))))
      (cons beg end))))

(defun rjsx-mode-content-boundaries (&optional pos)
  (unless pos (setq pos (point)))
  (let (beg end)
    (setq beg (or (previous-property-change pos (current-buffer))
                  (point-max)))
    (setq end (or (next-property-change pos (current-buffer))
                  (point-min)))
    (while (and (< beg end) (member (char-after beg) '(?\s ?\n)))
      (setq beg (1+ beg)))
    (while (and (> end beg) (member (char-after (1- end)) '(?\s ?\n)))
      (setq end (1- end)))
;;    (message "beg(%S) end(%S)" beg end)
    (cons beg end)
    ))

(defun rjsx-mode-dom-errors-show ()
  "Show unclosed tags."
  (interactive)
  (let (beg end tag pos l n tags i cont cell overlay overlays first
            (ori (point))
            (errors 0)
            (continue t)
        )
    (setq overlays (overlays-in (point-min) (point-max)))
    (when overlays
      (dolist (overlay overlays)
        (when (eq (overlay-get overlay 'face) 'rjsx-mode-warning-face)
          (delete-overlay overlay)
          )
        )
      )
    (goto-char (point-min))
    (when (not (or (get-text-property (point) 'tag-beg)
                   (rjsx-mode-tag-next)))
      (setq continue nil))
    (while continue
      (setq pos (point))
      (setq tag (get-text-property pos 'tag-name))
      (cond
       ((eq (get-text-property (point) 'tag-type) 'start)
        (setq tags (add-to-list 'tags (list tag pos)))
;;        (message "(%S) opening %S" pos tag)
        )
       ((eq (get-text-property (point) 'tag-type) 'end)
        (setq i 0
              l (length tags)
              cont t)
        (while (and (< i l) cont)
          (setq cell (nth i tags))
;;          (message "cell=%S" cell)
          (setq i (1+ i))
          (cond
           ((string= tag (nth 0 cell))
            (setq cont nil)
            )
           (t
            (setq errors (1+ errors))
            (setq beg (nth 1 cell))
            (setq end (rjsx-mode-tag-end-position beg))
            (unless first
              (setq first beg))
            (setq overlay (make-overlay beg (1+ end)))
            (overlay-put overlay 'font-lock-face 'rjsx-mode-warning-face)
;;            (message "invalid <%S> at %S" (nth 0 cell) (nth 1 cell))
            )
           ) ;cond
          ) ;while

        (dotimes (i i)
          (setq tags (cdr tags)))

        )
       ) ;cond
      (when (not (rjsx-mode-tag-next))
        (setq continue nil))
      ) ;while
    (message "%S error(s) detected" errors)
    (if (< errors 1)
        (goto-char ori)
      (goto-char first)
      (recenter))
    ;;    (message "%S" tags)
    ))

(defun rjsx-mode-highlight-elements (beg end)
  (save-excursion
    (goto-char beg)
    (let ((continue (or (get-text-property (point) 'tag-beg) (rjsx-mode-tag-next)))
          (i 0) (ctx nil) (face nil))
      (while continue
        (cond
         ((> (setq i (1+ i)) 1000)
          (message "highlight-elements ** too much tags **")
          (setq continue nil))
         ((> (point) end)
          (setq continue nil))
         ((not (get-text-property (point) 'tag-beg))
          (setq continue nil))
         ((eq (get-text-property (point) 'tag-type) 'start)
          (when (and (setq ctx (rjsx-mode-element-boundaries (point)))
                     (<= (car (cdr ctx)) end)
                     (setq face (cdr (assoc (get-text-property (point) 'tag-name) rjsx-mode-element-content-faces))))
            (font-lock-prepend-text-property (1+ (cdr (car ctx))) (car (cdr ctx))
                                             'font-lock-face face))
          )
         ) ;cond
        (when (not (rjsx-mode-tag-next))
          (setq continue nil))
        ) ;while
      )))

(defun rjsx-mode-enable (feature)
  "Enable one feature."
  (interactive
   (list (completing-read
          "Feature: "
          (let (features)
            (dolist (elt rjsx-mode-features)
              (setq features (append features (list (car elt)))))
            features))))
  (when (and (or (not feature) (< (length feature) 1)) rjsx-mode-last-enabled-feature)
    (setq feature rjsx-mode-last-enabled-feature))
  (when feature
    (setq rjsx-mode-last-enabled-feature feature)
    (setq feature (cdr (assoc feature rjsx-mode-features)))
    (cond
     ((eq feature 'rjsx-mode-enable-current-column-highlight)
      (rjsx-mode-column-show))
     ((eq feature 'rjsx-mode-enable-current-element-highlight)
      (when (not rjsx-mode-enable-current-element-highlight)
        (rjsx-mode-toggle-current-element-highlight))
      )
     ((eq feature 'rjsx-mode-enable-whitespace-fontification)
      (rjsx-mode-whitespaces-on))
     (t
      (set feature t)
      (rjsx-mode-buffer-highlight))
     )
    ) ;when
  )

(defun rjsx-mode-disable (feature)
  "Disable one feature."
  (interactive
   (list (completing-read
          "Feature: "
          (let (features)
            (dolist (elt rjsx-mode-features)
              (setq features (append features (list (car elt)))))
            features))))
  (when (and (or (not feature) (< (length feature) 1)) rjsx-mode-last-enabled-feature)
    (setq feature rjsx-mode-last-enabled-feature))
  (when feature
    (setq feature (cdr (assoc feature rjsx-mode-features)))
    (cond
     ((eq feature 'rjsx-mode-enable-current-column-highlight)
      (rjsx-mode-column-hide))
     ((eq feature 'rjsx-mode-enable-current-element-highlight)
      (when rjsx-mode-enable-current-element-highlight
        (rjsx-mode-toggle-current-element-highlight))
      )
     ((eq feature 'rjsx-mode-enable-whitespace-fontification)
      (rjsx-mode-whitespaces-off))
     (t
      (set feature nil)
      (rjsx-mode-buffer-highlight))
     )
    ) ;when
  )

(defun rjsx-mode-make-tag-overlays ()
  (unless rjsx-mode-overlay-tag-start
    (setq rjsx-mode-overlay-tag-start (make-overlay 1 1)
          rjsx-mode-overlay-tag-end (make-overlay 1 1))
    (overlay-put rjsx-mode-overlay-tag-start
                 'font-lock-face
                 'rjsx-mode-current-element-highlight-face)
    (overlay-put rjsx-mode-overlay-tag-end
                 'font-lock-face
                 'rjsx-mode-current-element-highlight-face)))

(defun rjsx-mode-delete-tag-overlays ()
  (when rjsx-mode-overlay-tag-start
    (delete-overlay rjsx-mode-overlay-tag-start)
    (delete-overlay rjsx-mode-overlay-tag-end)))

(defun rjsx-mode-column-overlay-factory (index)
  (let (overlay)
    (when (null rjsx-mode-column-overlays)
      (dotimes (i 100)
        (setq overlay (make-overlay 1 1))
        (overlay-put overlay 'font-lock-face 'rjsx-mode-current-column-highlight-face)
        (setq rjsx-mode-column-overlays (append rjsx-mode-column-overlays (list overlay)))
        )
      ) ;when
    (setq overlay (nth index rjsx-mode-column-overlays))
    (when (null overlay)
      (setq overlay (make-overlay 1 1))
      (overlay-put overlay 'font-lock-face 'rjsx-mode-current-column-highlight-face)
      (setq rjsx-mode-column-overlays (append rjsx-mode-column-overlays (list overlay)))
      ) ;when
    overlay))

(defun rjsx-mode-column-hide ()
  (setq rjsx-mode-enable-current-column-highlight nil)
  (remove-overlays (point-min) (point-max)
                   'font-lock-face
                   'rjsx-mode-current-column-highlight-face))

(defun rjsx-mode-column-show ()
  (let ((index 0) overlay diff column line-to line-from)
    (rjsx-mode-column-hide)
    (setq rjsx-mode-enable-current-column-highlight t)
    (save-excursion
      (back-to-indentation)
      (setq column (current-column)
            line-to (rjsx-mode-line-number))
      (when (and (get-text-property (point) 'tag-beg)
                 (member (get-text-property (point) 'tag-type) '(start end))
                 (rjsx-mode-tag-match)
                 (setq line-from (rjsx-mode-line-number))
                 (not (= line-from line-to)))
        (when (> line-from line-to)
          (let (tmp)
            (setq tmp line-from)
            (setq line-from line-to)
            (setq line-to tmp))
          ) ;when
        ;;(message "column(%S) line-from(%S) line-to(%S)" column line-from line-to)
        (goto-char (point-min))
        (when (> line-from 1)
          (forward-line (1- line-from)))
        (while (<= line-from line-to)
          (setq overlay (rjsx-mode-column-overlay-factory index))
          (setq diff (- (line-end-position) (point)))
          (cond
           ((or (and (= column 0) (= diff 0))
                (> column diff))
            (end-of-line)
            (move-overlay overlay (point) (point))
            (overlay-put overlay
                         'after-string
                         (concat
                          (if (> column diff) (make-string (- column diff) ?\s) "")
                          (propertize " "
                                      'font-lock-face
                                      'rjsx-mode-current-column-highlight-face)
                          ) ;concat
                         )
            )
           (t
            (move-to-column column)
            (overlay-put overlay 'after-string nil)
            (move-overlay overlay (point) (1+ (point)))
            )
           ) ;cond
          (setq line-from (1+ line-from))
          (forward-line)
          (setq index (1+ index))
          ) ;while
        ) ;when
      ) ;save-excursion
    ) ;let
  )

(defun rjsx-mode-highlight-current-element ()
  (let ((ctx (rjsx-mode-element-boundaries)) len)
    (cond
     ((null ctx)
      (rjsx-mode-delete-tag-overlays))
     (t
      (rjsx-mode-make-tag-overlays)
      (setq len (length (get-text-property (caar ctx) 'tag-name)))
      (move-overlay rjsx-mode-overlay-tag-start (+ (caar ctx) 1) (+ (caar ctx) 1 len))
      (move-overlay rjsx-mode-overlay-tag-end (+ (cadr ctx) 2) (+ (cadr ctx) 2 len))
      ) ;t
     ) ;cond
    ))

(defun rjsx-mode-highlight-whitespaces (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward rjsx-mode-whitespaces-regexp end t)
      (add-text-properties (match-beginning 0) (match-end 0)
                           '(face rjsx-mode-whitespace-face))
      ) ;while
    ))

(defun rjsx-mode-whitespaces-show ()
  "Toggle whitespaces."
  (interactive)
  (if rjsx-mode-enable-whitespace-fontification
      (rjsx-mode-whitespaces-off)
    (rjsx-mode-whitespaces-on)))

(defun rjsx-mode-whitespaces-on ()
  "Show whitespaces."
  (interactive)
  (when rjsx-mode-display-table
    (setq buffer-display-table rjsx-mode-display-table))
  (setq rjsx-mode-enable-whitespace-fontification t))

(defun rjsx-mode-whitespaces-off ()
  (setq buffer-display-table nil)
  (setq rjsx-mode-enable-whitespace-fontification nil))

(defun rjsx-mode-use-tabs ()
  "Tweaks vars to be compatible with TAB indentation."
  (let (offset)
    (setq rjsx-mode-block-padding 0)
    (setq rjsx-mode-script-padding 0)
    (setq rjsx-mode-style-padding 0)
    (setq offset
          (cond
           ((and (boundp 'tab-width) tab-width) tab-width)
           ((and (boundp 'standard-indent) standard-indent) standard-indent)
           (t 4)))
    ;;    (message "offset(%S)" offset)
    (setq rjsx-mode-attr-indent-offset offset)
    (setq rjsx-mode-code-indent-offset offset)
    (setq rjsx-mode-markup-indent-offset offset)
    (add-to-list 'rjsx-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'rjsx-mode-indentation-params '("lineup-calls" . nil))
    (add-to-list 'rjsx-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'rjsx-mode-indentation-params '("lineup-ternary" . nil))
    ))

(defun rjsx-mode-buffer-indent ()
  "Indent all buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(defun rjsx-mode-buffer-change-tag-case (&optional type)
  "Change html tag case."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((continue t) f)
      (setq f (if (member type '("upper" "uppercase" "upper-case")) 'uppercase 'downcase))
      (when (and (not (get-text-property (point) 'tag-beg))
                 (not (rjsx-mode-tag-next)))
        (setq continue nil))
      (while continue
        (skip-chars-forward "<!/")
        (if (looking-at "\\([[:alnum:]:-]+\\)")
            (replace-match (funcall f (match-string 0)) t))
;;        (message "tag: %S (%S)"
;;                 (get-text-property (point) 'tag-name)
;;                 (point))
        (unless (rjsx-mode-tag-next)
          (setq continue nil))
        ) ;while
      )))

(defun rjsx-mode-buffer-change-attr-case (&optional type)
  "Change case of html attribute names."
  (interactive)
  (unless type (setq type "downcase"))
  (save-excursion
    (goto-char (point-min))
    (let ((continue t)
          (fun (if (eq (aref (downcase type) 0) ?u) 'uppercase 'downcase)))
      (while continue
        (cond
         ((not (rjsx-mode-attribute-next))
          (setq continue nil))
         ((looking-at "\\([[:alnum:]-]+\\)")
          (replace-match (funcall fun (match-string 0)) t)
          )
         ) ;cond
        ) ;while
      )))

;; tag-case=lower|upper-case , attr-case=lower|upper-case
;; special-chars=unicode|html-entities
;; smart-apostrophes=bool , smart-quotes=bool , indentation=bool
(defun rjsx-mode-dom-normalize ()
  "Normalize buffer"
  (interactive)
  (save-excursion
    (let ((rules rjsx-mode-normalization-rules) elt)
      (when (setq elt (cdr (assoc "tag-case" rules)))
        (rjsx-mode-buffer-change-tag-case elt))
      (when (setq elt (cdr (assoc "attr-case" rules)))
        (rjsx-mode-buffer-change-attr-case elt))
      (when (setq elt (cdr (assoc "smart-apostrophes" rules)))
        (rjsx-mode-dom-apostrophes-replace))
      (when (setq elt (cdr (assoc "smart-quotes" rules)))
        (rjsx-mode-dom-quotes-replace))
      (when (setq elt (cdr (assoc "special-chars" rules)))
        (if (string= elt "entities")
            (rjsx-mode-dom-entities-encode)
          (rjsx-mode-dom-entities-replace)))
      (when (setq elt (cdr (assoc "whitespaces" rules)))
        (goto-char (point-min))
        (while (not (eobp))
          (forward-line)
          (delete-blank-lines))
        (delete-trailing-whitespace)
        (untabify (point-min) (point-max)))
      (when (setq elt (cdr (assoc "indentation" rules)))
        (rjsx-mode-buffer-indent))
      )))

;;---- INDENTATION -------------------------------------------------------------

(defvar rjsx-mode-indentation-rules nil
  "A list of indentation rules to apply head-to-tail.
An indenation rule consists of a pair with car a predicate that takes a context
and returns whether the rule applies, and a cdr a function that takes a
context and returns the desired indentation.")

(defvar rjsx-mode-indent-debug rjsx-mode-debug
  "If t, print debugging messages when applying indentation.")

(defmacro def-indentation-rule (name cond-form &rest body)
  "Define an indenation rule.
NAME: the name of the rule (for documentation and debugging).
COND-FORM: a condition that determines whether the rule applies.  It can
reference the variable `ctx`, which contains a context for the point being
indented.
BODY: the rule itself, which should return the amount of indentation required.
Can also reference `ctx`.  Will be wrapped in a (save-excursion) to ensure that
it doesn't change point."
  `(let ((rule (cons (lambda (ctx) (save-excursion ,cond-form))
                      (lambda (ctx)
                        (when rjsx-mode-indent-debug (message "IR: %S" ,name))
                        (save-excursion ,@body)))))
     (setq rjsx-mode-indentation-rules (cons rule rjsx-mode-indentation-rules))))

(defun rjsx-mode-point-context (pos)
  "POS should be at the beginning of the indentation."
  (save-excursion
    (let (curr-char curr-indentation curr-line curr-line-number
          language
          options
          reg-beg reg-col
          prev-char prev-indentation prev-line prev-pos
          token
          part-language
          depth
          char)

      (setq reg-beg (point-min)
            reg-col 0
            token "live"
            options ""
            language ""
            prev-line ""
            prev-char 0
            prev-pos nil)

      (when (get-text-property pos 'part-side)
        (setq part-language (symbol-name (get-text-property pos 'part-side))))

      (cond

       ((and (bobp) (member rjsx-mode-content-type '("html" "xml")))
        (setq language rjsx-mode-content-type)
        )

       ((member rjsx-mode-content-type '("javascript" "json"))
        (setq language "javascript"
              curr-indentation rjsx-mode-code-indent-offset))

       ((or (string= rjsx-mode-content-type "jsx")
            (and part-language (string= part-language "jsx")))
        (setq language "jsx"
              curr-indentation rjsx-mode-code-indent-offset)
        (cond
         ((rjsx-mode-jsx-is-html pos)
          (setq curr-indentation rjsx-mode-markup-indent-offset
                options "is-html"))
         ((and (setq depth (get-text-property pos 'jsx-depth)) (> depth 1))
          (when (get-text-property pos 'jsx-beg)
            (setq depth (1- depth)))
          (setq reg-beg (rjsx-mode-jsx-depth-beginning-position pos depth))
          (setq reg-beg (1+ reg-beg))
          ;;(message "%S" (point))
          (save-excursion
            (goto-char reg-beg)
            (cond
             ((and (not (looking-at-p "[ ]*$"))
                   (looking-back "^[[:space:]]*{" (point-min)))
              (setq reg-col (+ (current-indentation) 1
                               (cond
                                ((looking-at "[ ]+") (length (match-string-no-properties 0)))
                                (t 0))
                               ))
              )
             ((looking-at-p "[ ]*\\[[ ]*$") ;; #0659
              (setq reg-col (current-indentation))
              )
             ((and (looking-back "=[ ]*{" (point-min)) ;; #0739
                   (looking-at-p "{[ ]*"))
              (setq reg-col (current-indentation))
              )
             (t
              ;;(message "%S : %S %S" (point) (current-indentation) rjsx-mode-code-indent-offset)
              ;;(setq reg-col (+ (current-indentation) rjsx-mode-code-indent-offset rjsx-mode-jsx-expression-padding)))
              (setq reg-col (+ (current-indentation) rjsx-mode-code-indent-offset)))
             )

            ;;(message "%S %S %S" (point) (current-indentation) reg-col)
            ) ;save-excursion
          )
         ((string= rjsx-mode-content-type "jsx")
          (setq reg-beg (point-min)))
         (t
          (setq reg-beg (or (rjsx-mode-part-beginning-position pos) (point-min)))
          (save-excursion
            (goto-char reg-beg)
            (search-backward "<" nil t)
            (setq reg-col (current-column))
            ) ;save-excursion
          )
         ) ;cond
        ;;(message "jsx reg-beg=%S" reg-beg)
        ) ;jsx

       ((or (string= rjsx-mode-content-type "xml"))
        (setq language "xml"
              curr-indentation rjsx-mode-markup-indent-offset))

       ;; TODO: est ce util ?
       ((and (get-text-property pos 'tag-beg)
             (get-text-property pos 'tag-name)
             ;;(not (get-text-property pos 'part-side))
             )
        (setq language "html"
              curr-indentation rjsx-mode-markup-indent-offset))

       ((and (get-text-property pos 'block-side)
             (not (get-text-property pos 'block-beg)))

        (setq reg-beg (or (rjsx-mode-block-beginning-position pos) (point-min)))
        (goto-char reg-beg)
        (setq reg-col (current-column))
        (setq language rjsx-mode-engine)
        (setq curr-indentation rjsx-mode-code-indent-offset)
        ) ;block-side

       ((and part-language (member part-language '("javascript")))
        (setq reg-beg (or (rjsx-mode-part-beginning-position pos) (point-min)))
        (goto-char reg-beg)
        (search-backward "<" nil t)
        (setq reg-col (current-column))
        (setq language part-language)
        (cond
         (t
          (setq language "javascript"
                curr-indentation rjsx-mode-code-indent-offset))
         )
        ) ;part-side

       (t
        (setq language "html"
              curr-indentation rjsx-mode-markup-indent-offset)
        )

       ) ;cond

      (cond
       ((or (and (> pos (point-min))
                 (eq (get-text-property pos 'part-token) 'comment)
                 (eq (get-text-property (1- pos) 'part-token) 'comment)
                 (progn
                   (setq reg-beg (previous-single-property-change pos 'part-token))
                   t))
            (and (> pos (point-min))
                 (eq (get-text-property pos 'block-token) 'comment)
                 (eq (get-text-property (1- pos) 'block-token) 'comment)
                 (progn
                   (setq reg-beg (previous-single-property-change pos 'block-token))
                   t))
            (and (> pos (point-min))
                 (eq (get-text-property pos 'tag-type) 'comment)
                 (not (get-text-property pos 'tag-beg))
                 (progn
                   (setq reg-beg (rjsx-mode-tag-beginning-position pos))
                   t))
            )
        (setq token "comment"))
       ((or (and (> pos (point-min))
                 (member (get-text-property pos 'part-token)
                         '(string context key))
                 (member (get-text-property (1- pos) 'part-token)
                         '(string context key)))
            (and (eq (get-text-property pos 'block-token) 'string)
                 (eq (get-text-property (1- pos) 'block-token) 'string)))
        (setq token "string"))
       )

      (goto-char pos)
      (setq curr-line-number (line-number-at-pos))
      (setq curr-line (rjsx-mode-trim
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))
      (setq curr-char (if (string= curr-line "") 0 (aref curr-line 0)))

      (when (or (member language '("javascript" "jsx"))
                (and (member language '("html" "xml"))
                     (not (eq ?\< curr-char))))
        (let (prev)
          (cond
           ((member language '("html" "xml" "javascript" "jsx"))
            (when (setq prev (rjsx-mode-part-previous-live-line reg-beg))
              (setq prev-line (nth 0 prev)
                    prev-indentation (nth 1 prev)
                    prev-pos (nth 2 prev))
              )
            )
           ((setq prev (rjsx-mode-block-previous-live-line))
            (setq prev-line (car prev)
                  prev-indentation (cdr prev))
            (setq prev-line (rjsx-mode-clean-block-line prev-line)))
           ) ;cond
          ) ;let
        (when (>= (length prev-line) 1)
          (setq prev-char (aref prev-line (1- (length prev-line))))
          (setq prev-line (substring-no-properties prev-line))
          )
        )

      (cond
       ((not (member rjsx-mode-content-type '("html" "xml")))
        )
       ((member language '("javascript" "jsx"))
        (setq reg-col (if rjsx-mode-script-padding (+ reg-col rjsx-mode-script-padding) 0)))
       ((not (member language '("html" "xml")))
        (setq reg-col (if rjsx-mode-block-padding (+ reg-col rjsx-mode-block-padding) 0)))
       )
      (back-to-indentation)
      (setq char (char-after))
      (setq pos (point))

      (list
            :char char
            :curr-char curr-char
            :curr-indentation curr-indentation
            :curr-line curr-line
            :curr-line-number curr-line-number
            :language language
            :options options
            :pos pos
            :prev-char prev-char
            :prev-indentation prev-indentation
            :prev-line prev-line
            :prev-pos prev-pos
            :reg-beg reg-beg
            :reg-col reg-col
            :token token)
      )))

(defun rjsx-mode-call-matching-rule (ctx)
  "Call the first matching indentation rule.
CTX: the current context at the point being indented."
  (let ((rule (seq-find (lambda (rule) (funcall (car rule) ctx)) rjsx-mode-indentation-rules)))
    (if rule
        (funcall (cdr rule) ctx)
      ;; TODO(colin): perhaps assert that a rule was found instead of returing 0 in debug mode?
      0)))

;; Indentation helpers
(defun rjsx-mode-comment-indentation (ctx)
  "Find the indentation for a comment.
CTX: the current indentatation context at point."
  (save-excursion
    (let ((offset 0)
          (pos (plist-get ctx :pos)))
      (if (eq (get-text-property pos 'tag-type) 'comment)
          (rjsx-mode-tag-beginning)
        (goto-char (car
                    (rjsx-mode-property-boundaries
                     (if (eq (get-text-property (plist-get ctx :pos) 'part-token) 'comment)
                         'part-token
                       'block-token)
                     pos))))
      (setq offset (current-column))
      (cond
       ((member (buffer-substring-no-properties (point) (+ (point) 2)) '("/*" "{*" "@*"))
        (cond
         ((eq ?\* (plist-get ctx :curr-char))
          (setq offset (+ offset 1)))
         (t
          (setq offset (+ offset 3)))
         ) ;cond
        )
       ;; TODO(colin): remove non-jsx comment syntax.
       ((string= (buffer-substring-no-properties (point) (+ (point) 4)) "<!--")
        (rjsx-mode-hypothesize-unused)
        (cond
         ((string-match-p "^<!\\[endif" (plist-get ctx :curr-line))
          )
         ((looking-at-p "<!--\\[if")
          (setq offset (+ offset rjsx-mode-markup-indent-offset)))
         ((eq ?\- (plist-get ctx :curr-char))
          (setq offset (+ offset 3)))
         (t
          (setq offset (+ offset 5)))
         ) ;cond
        )
       ) ;cond
      offset
      )
  ))

;; Indentation rules are listed in reverse priority order.
;; TODO(colin): many of these should be split into smaller rules that do just
;; one thing.
(def-indentation-rule
  "Default: indent the same as the last line."
  nil ;; TODO(colin): eventually this should become `t` and be a catch-all.
  (plist-get ctx :prev-indentation))

(def-indentation-rule
  "Javascript indentation."
  t
  ;; TODO(colin): split up the javascript indentation function.
  ;; TODO(colin): the javascript indentation function should perhaps take the
  ;; context?
  (car (rjsx-mode-javascript-indentation
        (plist-get ctx :pos)
        (plist-get ctx :reg-col)
        (plist-get ctx :curr-indentation)
        (plist-get ctx :language)
        (plist-get ctx :reg-beg))))

(def-indentation-rule
  "Indent after a closing paren at a block opening."
  (and (eq (plist-get ctx :prev-char) ?\))
       (rjsx-mode-part-is-opener (plist-get ctx :prev-pos)
                                 (plist-get ctx :reg-beg)))
  (+
   (rjsx-mode-part-is-opener
    (plist-get ctx :prev-pos)
    (plist-get ctx :reg-beg))
   rjsx-mode-code-indent-offset))

(def-indentation-rule
  "Indent after a bare else."
  (string-match-p "^else$" (plist-get ctx :prev-line))
  (+ (plist-get ctx :prev-indentation) rjsx-mode-code-indent-offset))

(def-indentation-rule
  "Indent at the start of an array, argument list, or array element."
  (or (member ?\, (list (plist-get ctx :prev-char) (plist-get ctx :next-char)))
      (member (plist-get ctx :prev-char) '(?\( ?\[)))
  (save-excursion
    (cond
     ;; TODO(colin): checking this condition will move point.  Fix.
     ((not (rjsx-mode-javascript-args-beginning
            (plist-get ctx :pos)
            (plist-get ctx :reg-beg)))
      (rjsx-mode-hypothesize-unused)
      0)
     ((or (not (cdr (assoc "lineup-args" rjsx-mode-indentation-params)))
          (looking-at-p "\n"))
      (let ((reg-col (plist-get ctx :reg-col)))
        (if (and reg-col (> reg-col (current-indentation)))
            (+ reg-col rjsx-mode-code-indent-offset)
          (+ (current-indentation) rjsx-mode-code-indent-offset))))
     ((not (eq (plist-get ctx :curr-char) ?\,))
      (current-column))
     (t
      (let ((initial-offset (current-column)))
        (goto-char (plist-get ctx :pos))
        (looking-at ",[ \t\n]*")
        (- initial-offset (length (match-string-no-properties 0))))))))

(def-indentation-rule
  "Indent after a line ending in an operator."
  ;; TODO(colin): simplify this condition.
  (let ((prev-line (plist-get ctx :prev-line))
        (curr-line (plist-get ctx :curr-line))
        (prev-char (plist-get ctx :prev-char)))
    (and
     (or (string-match-p "[&|?:+-]$" prev-line)
         (string-match-p "^[&|?:+-]" curr-line))
     (not (string-match-p "]:" prev-line))
     (not (and (eq prev-char ?\:)
               (string-match-p "^\\(case\\|default\\)" prev-line)))))
  (max (plist-get ctx :reg-col)
       (cond
        ;; TODO(colin): checking this condition will move point.  Fix.
        ((not (rjsx-mode-javascript-statement-beginning
               (plist-get ctx :pos) (plist-get ctx :reg-beg)))
         (rjsx-mode-hypothesize-unused)
         0)
        ((null (cdr (assoc "lineup-ternary" rjsx-mode-indentation-params)))
         (+ (current-indentation) rjsx-mode-code-indent-offset))
        ((and (member (plist-get ctx :curr-char) '(?\+ ?\- ?\& ?\| ?\? ?\:))
              (not (looking-back "\\(^[ \t]+\\|if[ ]*[(]?\\)" (point-min))))
         (let ((prev-col (current-column)))
           (goto-char (plist-get ctx :pos))
           (looking-at "\\(||\\|&&\\|[&|?:+-]\\)[ \t\n]*")
           (- prev-col (length (match-string-no-properties 0)))))
        (t (current-column)))))

(def-indentation-rule
  "Indent after a line ending in = or =>."
  (string-match-p "=[>]?$" (plist-get ctx :prev-line))
  (+ (plist-get ctx :prev-indentation) rjsx-mode-code-indent-offset))

(def-indentation-rule
  "Concatenation with +."
  (member ?\+ (list (plist-get ctx :prev-char)
                    (plist-get ctx :next-char)))
  (save-excursion
    (cond
     ;; TODO(colin): checking this condition will move point.  Fix.
     ((not (rjsx-mode-javascript-string-beginning
            (plist-get ctx :pos) (plist-get ctx :reg-beg)))
      0)
     ((null (cdr (assoc "lineup-concats" rjsx-mode-indentation-params)))
      (+ (current-indentation) rjsx-mode-code-indent-offset))
     ((not (eq (plist-get ctx :curr-char) ?\+))
      (current-column))
     ((not (looking-back "\\(^[ \t]+\\|if[ ]*[(]?\\)" (point-min)))
      ;; TODO(colin): clean up this condition.
      (goto-char (plist-get ctx :pos))
      (looking-at "\\+[ \t\n]*")
      (- (current-column) (length (match-string-no-properties 0))))
     (t (current-column)))))

(def-indentation-rule
  "Chained function calls starting with `.`."
  (and (member ?\. (list (plist-get ctx :curr-char) (plist-get ctx :prev-char)))
       (not (string-match-p "^\\.\\.\\." (plist-get ctx :curr-line))))
  (save-excursion
    (let ((pair (rjsx-mode-javascript-calls-beginning-position
                 (plist-get ctx :pos)
                 (plist-get ctx :reg-beg))))
      (if pair
          (progn
            (goto-char (car pair))
            (cond
             ((cdr (assoc "lineup-calls" rjsx-mode-indentation-params))
              (if (cdr pair)
                  (let ((offset 0))
                    ;; TODO(colin): simplify this section
                    (goto-char (cdr pair))
                    (setq offset (current-column))
                    (looking-at "\\.\\([ \t\n]*\\)")
                    (setq offset (- offset (length (match-string-no-properties 1))))
                    (unless (eq (plist-get ctx :curr-char) ?\.) (setq offset (1+ offset)))
                    offset)
                ;; TODO(colin): perhaps this should be done in rjsx-mode-javascript-calls-beginning-position.
                (skip-chars-forward " \t\n")
                (+ (current-indentation) rjsx-mode-code-indent-offset))
              )
             (t (+ (current-indentation) rjsx-mode-code-indent-offset))))
        0))))


(def-indentation-rule
  "Closing paren/bracket/brace indentation."
  ;; TODO(colin): verify rule description
  (member (plist-get ctx :curr-char) '(?\} ?\) ?\]))
  ;; TODO(colin): split into multiple rules.
  (save-excursion
    (let ((ori (if (get-text-property (plist-get ctx :pos) 'block-side)
                   (rjsx-mode-block-opening-paren-position
                    (plist-get ctx :pos) (plist-get ctx :reg-beg))
                 (rjsx-mode-part-opening-paren-position
                  (plist-get ctx :pos) (plist-get ctx :reg-beg)))))
      (cond
       ((null ori)
        (plist-get ctx :reg-col))
       ;; TODO(colin): checking this condition will move point.  Fix.
       ;; (consolidate with the next condition once we're sure we don't need the mutation)
       ((and (goto-char ori)
             (looking-back ")[ ]*" (point-min)) ;; TODO(colin): is looking-back needed?
             (re-search-backward ")[ ]*" nil t)
             (rjsx-mode-block-opening-paren (plist-get ctx :reg-beg)))
        (back-to-indentation)
        (current-indentation))
       (t
        (goto-char ori)
        (back-to-indentation)
        (current-indentation))))))

(def-indentation-rule
  "JSX within-element indentation."
  ;; TODO(colin): verify rule description.
  (and (member (plist-get ctx :language) '("jsx"))
       (string= (plist-get ctx :options) "is-html"))
  ;; TODO(colin): split into multiple rules.
  (save-excursion
    (cond
     ((get-text-property (plist-get ctx :pos) 'tag-beg)
      (rjsx-mode-markup-indentation (plist-get ctx :pos)))
     ((and rjsx-mode-indentless-elements
           (not (string= (plist-get ctx :language) "jsx"))
           (null (get-text-property (plist-get ctx :pos) 'block-side))
           (null (get-text-property (plist-get ctx :pos) 'part-side))
           (and (null (get-text-property (plist-get ctx :pos) 'tag-beg))
                (save-excursion
                  (and (rjsx-mode-element-parent)
                       (member (get-text-property (point) 'tag-name) rjsx-mode-indentless-elements))))
           )
      (rjsx-mode-hypothesize-unused)
      0)
     ((or (eq (length (plist-get ctx :curr-line)) 0)
          (= rjsx-mode-indent-style 2)
          (get-text-property (plist-get ctx :pos) 'tag-beg)
          (get-text-property (plist-get ctx :pos) 'reg-beg))
      (rjsx-mode-markup-indentation (plist-get ctx :pos))))))

(def-indentation-rule
  "JSX within-tag indentation."
  ;; TODO(colin): verify rule description.
  (and (member (plist-get ctx :language) '("javascript" "jsx"))
       (get-text-property (plist-get ctx :pos) 'tag-type)
       (not (get-text-property (plist-get ctx :pos) 'tag-beg))
       (or (not (string= (plist-get ctx :language) "jsx"))
           (string= (plist-get ctx :options) "is-html")))
  (save-excursion
    ;; TODO(colin): split into multiple rules
    (cond
     ((and (get-text-property (plist-get ctx :pos) 'tag-attr)
           (get-text-property (1- (plist-get ctx :pos)) 'tag-attr)
           (rjsx-mode-attribute-beginning))
      (cond
       ((eq (logand (get-text-property (point) 'tag-attr-beg) 8) 8)
        0)
       ((and rjsx-mode-attr-value-indent-offset (rjsx-mode-tag-beginning))
        (+ (current-column) rjsx-mode-attr-value-indent-offset))
       (t
        (rjsx-mode-dom-rsf "=[ ]*[\"']?" (plist-get ctx :pos))
        (current-column))
       ) ;cond
      )
     ;; TODO(colin): the next line (rjsx-mode-tag-beginning) may change the
     ;; point and therefore checking this condition may affect others.  Fix.
     ((not (rjsx-mode-tag-beginning))
      (rjsx-mode-hypothesize-unused)
      0)
     ((string-match-p "^/?>" (plist-get ctx :curr-line))
      (- (or (plist-get ctx :prev-indentation) 0) (or rjsx-mode-attr-indent-offset 4)))
     (rjsx-mode-attr-indent-offset
      (+ (current-column) rjsx-mode-attr-indent-offset))
     ((looking-at-p (concat rjsx-mode-start-tag-regexp "[ ]*\n"))
      (if (= (line-number-at-pos) (1- (plist-get ctx :curr-line-number)))
          (+ (or (plist-get ctx :prev-indentation) 0)
             (or rjsx-mode-attr-indent-offset 4))
        (or (plist-get ctx :prev-indentation) 0)))
     ((rjsx-mode-attribute-next)
      (current-column))
     ) ;cond
    )
  )

(def-indentation-rule
  "JSX closing } indentation."
  ;; TODO(colin): verify rule description.
  (and (equal (plist-get ctx :language) "jsx")
       (eq (plist-get ctx :curr-char) ?\})
       (get-text-property (plist-get ctx :pos) 'jsx-end))
  (save-excursion
    (rjsx-mode-go (1- (plist-get ctx :reg-beg)))
    (current-column)))

(def-indentation-rule
  "Tag indentation: self-closing??"
  ;; TODO(colin): is this a correct rule label?
  (and (get-text-property (plist-get ctx :pos) 'tag-beg)
       (eq (get-text-property (plist-get ctx :pos) 'tag-type) 'end))
  (save-excursion
    (rjsx-mode-tag-match)
    (current-indentation)))

(def-indentation-rule
  "Block indentation: closing delimiter??"
  ;; TODO(colin): is this correct that this deals with the closing delimiter?
  (eq (get-text-property (plist-get ctx :pos) 'block-token) 'delimiter-end)
  (save-excursion
    (rjsx-mode-block-beginning)
    (if (< (current-column) (current-indentation)) (current-indentation) (current-column))))

(def-indentation-rule
  "Block indentation."
  ;; TODO(colin): what feature of a block does this deal with exactly?
  (and (get-text-property (plist-get ctx :pos) 'block-beg)
       (or (rjsx-mode-block-close-p (plist-get ctx :pos))
           (rjsx-mode-block-inside-p (plist-get ctx :pos))))
  (save-excursion
    (rjsx-mode-block-match)
    (current-indentation)))

(def-indentation-rule
  "Comment indentation."
  (string= (plist-get ctx :token) "comment")
  (rjsx-mode-comment-indentation ctx))

(def-indentation-rule
  "No offset at beginning of buffer."
  (or (bobp) (= (line-number-at-pos (plist-get ctx :pos)) 1))
  0)

(defun rjsx-mode-indent-line ()
  "Calculate and apply intendation to the line at point."
  (rjsx-mode-propertize)

  (let ((char nil)
        (ctx (save-excursion
               (back-to-indentation)
               (rjsx-mode-point-context (point)))))

    (let ((offset (rjsx-mode-call-matching-rule ctx))
          (diff (- (current-column) (current-indentation))))
      (when (not (= offset (current-indentation)))
        ;; TODO(colin): what is this doing?
        (setq rjsx-mode-change-beg (line-beginning-position)
              rjsx-mode-change-end (+ rjsx-mode-change-beg offset)))
      (setq offset (max 0 offset))
      (indent-line-to offset)
      (if (> diff 0) (move-to-column (+ (current-column) diff))))))

(defun rjsx-mode-bracket-level (pos limit)
  (save-excursion
    (let ((continue t)
          (regexp "[\]\[}{)(]")
          (char nil)
          (map nil)
          (key nil)
          (value 0)
          (open '(?\( ?\{ ?\[)))
      (goto-char pos)
      (while (and continue (re-search-backward regexp limit t))
        (setq char (aref (match-string-no-properties 0) 0))
        (setq key (cond ((eq char ?\)) ?\()
                        ((eq char ?\}) ?\{)
                        ((eq char ?\]) ?\[)
                        (t             char)))
        (setq value (or (plist-get map key) 0))
        (setq value (if (member char open) (1+ value) (1- value)))
        (setq map (plist-put map key value))
        (setq continue (< value 1))
        ;;(message "pos=%S char=%c key=%c value=%S" (point) char key value)
        ) ;while
      (if (>= value 1) (current-indentation) nil)
      )))

(defun rjsx-mode-markup-indentation (pos)
  (let ((offset 0) beg ret depth-beg depth-pos)
    (when (setq beg (rjsx-mode-markup-indentation-origin pos))
      (when (and (setq depth-pos (get-text-property pos 'jsx-depth))
                 (setq depth-beg (get-text-property beg 'jsx-depth))
                 (progn
                   (when (and (get-text-property pos 'jsx-beg)
                              (not (get-text-property pos 'tag-beg)))
                     (setq depth-pos (1- depth-pos)))
                   t)
                 (not (eq depth-beg depth-pos)))
        (setq beg (rjsx-mode-jsx-depth-beginning-position pos)))
      (cond
       ((null (setq ret (rjsx-mode-element-is-opened beg pos)))
        (setq offset (rjsx-mode-indentation-at-pos beg)))
       ((eq ret t)
        (setq offset (+ (rjsx-mode-indentation-at-pos beg) rjsx-mode-markup-indent-offset)))
       (t
        (setq offset ret))
       ) ;cond
      ) ;when beg
    offset))

(defun rjsx-mode-javascript-indentation (pos initial-column language-offset language &optional limit)
  (let (open-ctx indentation offset sub)
    (setq open-ctx (rjsx-mode-bracket-up pos language limit))
    ;;(message "pos(%S) initial-column(%S) language-offset(%S) language(%S) limit(%S)" pos initial-column language-offset language limit)
    ;;(message "javascript-indentation: %S\nchar=%c" open-ctx (plist-get open-ctx :char))
    (setq indentation (plist-get open-ctx :indentation))
    (when (and initial-column (> initial-column indentation))
      (setq indentation initial-column)
      )
    (cond
     ((or (null open-ctx) (null (plist-get open-ctx :pos)))
      (setq offset initial-column))
     ((and (member language '("javascript" "jsx"))
           (eq (plist-get open-ctx :char) ?\{)
           (rjsx-mode-looking-back "switch[ ]*" (plist-get open-ctx :pos))
           ;;(rjsx-mode-looking-back "switch[ ]*(.*)[ ]*" (plist-get open-ctx :pos))
           )
      (setq sub (if (cdr (assoc "case-extra-offset" rjsx-mode-indentation-params)) 0 1))
      (cond
       ((looking-at-p "case\\|default")
        (setq offset (+ indentation (* language-offset (- 1 sub)))))
       (t
        (setq offset (+ indentation (* language-offset (- 2 sub)))))
       ) ;cond switch
      )
     (t
      (setq offset (+ indentation language-offset)))
     ) ;cond
    (cons (if (< offset initial-column) initial-column offset) open-ctx)
    ))

(defun rjsx-mode-bracket-indentation (pos initial-column language-offset language &optional limit)
  (save-excursion
    (let* ((ctx (rjsx-mode-bracket-up pos language limit))
           (char (plist-get ctx :char))
           (pos (plist-get ctx :pos))
           (indentation (plist-get ctx :indentation)))
      ;;(message "pos(%S) initial-column(%S) language-offset(%S) language(%S) limit(%S)" pos initial-column language-offset language limit)
      ;;(message "bracket-up: %S, %c" ctx char)
      (cond
       ((null pos)
        (setq indentation initial-column))
       (t
        (setq indentation (+ indentation language-offset))
        )
       ) ;cond
      (cons (if (< indentation initial-column) initial-column indentation) ctx)
      )))

(defun rjsx-mode-block-previous-live-line ()
  (save-excursion
    (let ((continue t) (line "") (pos (point)))
      (beginning-of-line)
      (while (and continue (not (bobp)) (forward-line -1))
        (when (not (rjsx-mode-block-is-token-line))
          (setq line (rjsx-mode-trim (buffer-substring (point) (line-end-position)))))
        (when (not (string= line ""))
          (setq continue nil))
        ) ;while
      (if (string= line "")
          (progn (goto-char pos) nil)
        (cons line (current-indentation)))
      )))

(defun rjsx-mode-part-is-opener (pos reg-beg)
  (save-excursion
    (save-match-data
      (if (and pos
               (rjsx-mode-go (rjsx-mode-part-opening-paren-position pos))
               (>= (point) reg-beg)
               ;;(progn (message "%S %S" pos (point)))
               (looking-back "\\(^\\|[ \t]\\)\\(if\\|for\\|while\\)[ ]*" (point-min)))
          (current-indentation)
        nil)
      )))

(defun rjsx-mode-part-previous-live-line (reg-beg)
  (unless reg-beg (setq reg-beg (point-min)))
  ;;(message "reg-beg=%S" reg-beg)
  (save-excursion
    (let ((continue (> (point) reg-beg))
          (line "")
          bol-pos
          eol-pos
          pos)
      (beginning-of-line)
      (while (and continue (> (point) reg-beg) (forward-line -1))
        (setq bol-pos (point)
              eol-pos (line-end-position))
        (when (> reg-beg bol-pos)
          (setq bol-pos reg-beg))
        (when (not (rjsx-mode-part-is-token-line bol-pos))
          (setq line (rjsx-mode-trim (buffer-substring bol-pos eol-pos)))
          (when (not (string= line "")) (setq continue nil))
          ) ;when
        ) ;while
      (cond
       ((string= line "")
        nil)
       (t
        (setq continue t)
        (setq pos (1- eol-pos))
        (while (and (>= pos bol-pos) continue)
          (cond
           ((eq (char-after pos) ?\s)
            (setq pos (1- pos)))
           ((get-text-property pos 'part-token)
            (setq pos (1- pos)))
           (t
            (setq continue nil))
           ) ;cond
          ) ;while
        ;;(message "%S %S : %S" bol-pos eol-pos pos)
        (setq line (rjsx-mode-clean-part-line line))
        (list line (current-indentation) pos))
       )
      )))

(defun rjsx-mode-in-code-block (open close &optional prop)
  (save-excursion
    (let ((pos (point)) pos-open pos-close start end ret)
      (when prop
        (setq start pos
              end pos)
        (when (eq (get-text-property pos prop) (get-text-property (1- pos) prop))
          (setq start (or (previous-single-property-change pos prop) (point-min))))
        (when (eq (get-text-property pos prop) (get-text-property (1+ pos) prop))
          (setq end (next-single-property-change pos prop)))
        ;;        (message "start(%S) end(%S)" start end)
        )
      (setq ret (and (rjsx-mode-sb open start t)
                     (setq pos-open (point))
                     (rjsx-mode-sf close end t)
                     (setq pos-close (point))
                     (>= pos-close pos)))
      (if ret
          (cons pos-open pos-close)
        ret)
      )))

(defun rjsx-mode-clean-part-line (input)
  (let ((out "")
        (beg 0)
        (keep t)
        (n (length input)))
    (dotimes (i n)
      (if (or (get-text-property i 'block-side input)
              (eq (get-text-property i 'part-token input) 'comment)
              (eq (get-text-property i 'tag-type input) 'comment))
          (when keep
            (setq out (concat out (substring input beg i))
                  beg 0
                  keep nil))
        (when (null keep)
          (setq beg i
                keep t))
        ) ;if
      ) ;dotimes
    (if (> beg 0) (setq out (concat out (substring input beg n))))
    (setq out (if (= (length out) 0) input out))
    (rjsx-mode-trim out)
    ))

(defun rjsx-mode-clean-block-line (input)
  (let ((out "")
        (beg 0)
        (keep t)
        (n (length input)))
    (dotimes (i n)
      (if (or (not (get-text-property i 'block-side input))
              (member (get-text-property i 'block-token input)
                      '(comment delimiter-beg delimiter-end)))
          (when keep
            (setq out (concat out (substring input beg i))
                  beg 0
                  keep nil))
        (when (null keep)
          (setq beg i
                keep t))
        ) ;if
      ) ;dotimes
    (if (> beg 0) (setq out (concat out (substring input beg n))))
    (setq out (if (= (length out) 0) input out))
    (rjsx-mode-trim out)
    ;;    (message "%S [%s] > [%s]" beg input out)
    ))

(defun rjsx-mode-language-at-pos (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((get-text-property pos 'block-side)
    rjsx-mode-engine)
   ((get-text-property pos 'part-side)
    (symbol-name (get-text-property pos 'part-side)))
   (t
    rjsx-mode-content-type)
   ) ;cond
  )

(defun rjsx-mode-coord-position (line column)
  (save-excursion
    (when (stringp line) (setq line (string-to-number line)))
    (when (stringp column) (setq column (string-to-number column)))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column (1- column))
    (point)))

(defun rjsx-mode-column-at-pos (&optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun rjsx-mode-is-single-line-block (pos)
  (= (rjsx-mode-line-number (rjsx-mode-block-beginning-position pos))
     (rjsx-mode-line-number (rjsx-mode-block-end-position pos))))

(defun rjsx-mode-line-number (&optional pos)
  (unless pos (setq pos (point)))
  (let (ret)
    (setq ret (+ (count-lines 1 pos)
                 (if (= (rjsx-mode-column-at-pos pos) 0) 1 0)))))

(defun rjsx-mode-block-is-control (pos)
  (save-excursion
    (let (control state controls pair)
      (goto-char pos)
      (setq controls (rjsx-mode-block-controls-get pos))
      (setq pair (car controls))
      (cond
       ((eq (car pair) 'inside)
        )
       ((eq (car pair) 'open)
        (setq state t
              control (cdr pair)))
       ((eq (car pair) 'close)
        (setq state nil
              control (cdr pair)))
       ) ;cond
      ;;      (message "engine=%S control=%S state=%S" rjsx-mode-engine control state)
      (if control (cons control state) nil)
      )))

(defun rjsx-mode-block-is-opening-control (pos)
  (save-excursion
    (let (controls pair)
      (goto-char pos)
      (if (and (setq controls (rjsx-mode-block-controls-get pos))
               (= (length controls) 1)
               (setq pair (car controls))
               (eq (car pair) 'open))
          (cdr pair)
        nil)
      )))

(defun rjsx-mode-markup-indentation-origin (pos)
  (save-excursion
    (let* ((continue (not (bobp)))
           (curr-line (rjsx-mode-line-number pos))
           ;;         (pos (point))
           (part-side (not (null (get-text-property pos 'part-side)))) ;part-side at the origin
           (types '(start end void)))
      (while continue
        (forward-line -1)
        (if (bobp)
            (setq pos (point)
                  continue nil)
          (back-to-indentation)
          (setq pos (point))
          (setq curr-line (rjsx-mode-line-number pos))
          ;; we want to pick up indentation of tags that do not start a line,
          ;; so we need to check for a tag on the same line
          (when (= curr-line (rjsx-mode-line-number (rjsx-mode-tag-next-position pos)))
            (setq pos (rjsx-mode-tag-next-position pos)))
          (setq continue (not (or (and (null part-side)
                                       (null (get-text-property pos 'part-side))
                                       (get-text-property pos 'tag-beg)
                                       (member (get-text-property pos 'tag-type) types)
                                       (null (get-text-property (1- pos) 'invisible)))
                                  (and part-side
                                       (get-text-property pos 'part-side)
                                       (get-text-property pos 'tag-beg)
                                       (member (get-text-property pos 'tag-type) types)
                                       (null (get-text-property (1- pos) 'invisible)))
                                  (and (get-text-property pos 'block-beg)
                                       (not (get-text-property pos 'tag-type))
                                       (rjsx-mode-block-is-control pos)
                                       (not (looking-at-p "{% comment"))))))
          ) ;if
        ) ;while
      ;;(message "indent-origin=%S" pos)
      (back-to-indentation)
      (setq pos (point))
      pos)))

;;TODO : prendre en compte part-token
;; state=t <=> start tag
(defun rjsx-mode-element-is-opened (pos limit)
  (let (tag
        last-end-tag
        tag-pos block-pos
        state
        n
        ret
        (continue t)
        controls
        control
        (buffer (current-buffer))
        (h (make-hash-table :test 'equal))
        (h2 (make-hash-table :test 'equal)))

;;    (message "pos-ori=%S limit=%S" pos limit)

    (while continue
      (setq control nil
            controls nil
            last-end-tag nil
            tag nil)

      (cond
       ((get-text-property pos 'tag-beg)
        (when (member (get-text-property pos 'tag-type) '(start end))
          (setq tag (get-text-property pos 'tag-name)
                state (eq (get-text-property pos 'tag-type) 'start))
          (if (null state) (setq last-end-tag (cons tag pos)))
          (setq n (gethash tag h 0))
          (if (null state)
              (progn
                (when (> n 0) (puthash tag (1- n) h))
                (puthash tag (1- n) h2))
            (puthash tag (1+ n) h)
            (puthash tag (1+ n) h2))
          ) ;when
        (when (setq pos (rjsx-mode-tag-end-position pos))
          (setq tag-pos nil)
          (when (and block-pos (> pos block-pos))
            (setq block-pos nil))
          ) ;when
        )
       ((and rjsx-mode-enable-control-block-indentation
             (get-text-property pos 'block-beg))
        (when (setq controls (rjsx-mode-block-controls-get pos))
          (dolist (control controls)
            (setq tag (cdr control))
            (setq n (gethash tag h 0))
            (cond
             ((eq (car control) 'inside)
              )
             ((eq (car control) 'open)
              (puthash tag (1+ n) h))
             ((> n 0)
              (puthash tag (1- n) h))
             ) ;cond
            ) ;dolist
          )
        (when (setq pos (rjsx-mode-block-end-position pos))
          (setq block-pos nil)
          (when (and tag-pos (> pos tag-pos))
            (setq tag-pos nil))
          )
        )
       ) ;cond

;;      (message "tag=%S end-pos=%S" tag pos)

      (when (and pos (< pos limit))
        (when (or (null tag-pos) (>= pos tag-pos))
          (setq tag-pos (rjsx-mode-tag-next-position pos limit))
;;          (message "from=%S tag-next-pos=%S" pos tag-pos)
          )
        (when (or (null block-pos) (>= pos block-pos))
          (setq block-pos (rjsx-mode-block-next-position pos limit))
;;          (message "from=%S block-next-pos=%S" pos block-pos)
          )
        )

      (cond
       ((null pos)
        )
       ((and (null tag-pos)
             (null block-pos))
        (setq pos nil))
       ((and tag-pos block-pos)
        (if (< tag-pos block-pos)
            (progn
              (setq pos tag-pos)
              (setq tag-pos nil))
          (setq pos block-pos)
          (setq block-pos nil))
        )
       ((null tag-pos)
        (setq pos block-pos)
        (setq block-pos nil))
       (t
        (setq pos tag-pos)
        (setq tag-pos nil))
       )

      (when (or (null pos)
                (>= pos limit))
        (setq continue nil))
      ) ;while

;;    (message "hashtable=%S" h)
    (maphash (lambda (k v) (if (> v 0) (setq ret t))) h)

    (when (and (null ret)
               last-end-tag
               (> (hash-table-count h2) 1)
               (< (gethash (car last-end-tag) h2) 0))
;;      (message "last-end-tag=%S" last-end-tag)
      (save-excursion
        (goto-char (cdr last-end-tag))
        (rjsx-mode-tag-match)
        (when (not (= (point) (cdr last-end-tag)))
          (setq n (point))
          (back-to-indentation)
          (if (= n (point)) (setq ret (current-indentation))))
        ))

    ret))

(defun rjsx-mode-previous-line (pos limit)
  (save-excursion
    (let (beg end line (continue t))
      (goto-char pos)
      (while continue
        (forward-line -1)
        (setq end (line-end-position))
        (setq line (buffer-substring-no-properties (point) end))
        (when (or (not (string-match-p "^[ \t]*$" line))
                  (bobp)
                  (<= (point) limit))
          (setq continue nil))
        )
      (if (<= (point) limit)
          ;;todo : affiner (le + 3 n est pas générique cf. <% <%- etc.)
          (setq beg (if (< (+ limit 3) end) (+ limit 3) end))
        (setq beg (line-beginning-position))
        ) ;if
      (setq line (buffer-substring-no-properties beg end))
      (cons line (current-indentation))
      )))

(defun rjsx-mode-bracket-up (pos language &optional limit)
  (unless limit (setq limit nil))
  ;;(message "pos(%S) language(%S) limit(%S)" pos language limit)
  (save-excursion
    (goto-char pos)
    (let ((continue t)
          (regexp "[\]\[}{)(]")
          (char nil)
          (column nil)
          (indentation nil)
          (map nil)
          (key nil)
          (value 0)
          (open '(?\( ?\{ ?\[))
          (searcher nil)
          (opener nil))
      (cond
       ((get-text-property pos 'block-side)
        (setq searcher 'rjsx-mode-block-rsb
              opener 'rjsx-mode-block-opening-paren-position))
       (t
        (setq searcher 'rjsx-mode-part-rsb
              opener 'rjsx-mode-part-opening-paren-position))
       )
      (while (and continue (funcall searcher regexp limit))
        (setq char (aref (match-string-no-properties 0) 0))
        (setq key (cond ((eq char ?\)) ?\()
                        ((eq char ?\}) ?\{)
                        ((eq char ?\]) ?\[)
                        (t             char)))
        (setq value (or (plist-get map key) 0))
        (setq value (if (member char open) (1+ value) (1- value)))
        (setq map (plist-put map key value))
        (setq continue (< value 1))
        ;;(message "pos=%S char=%c key=%c value=%S map=%S" (point) char key value map)
        ) ;while
      (setq column (current-column)
            indentation (current-indentation))
      (when (and (> value 0)
                 (eq char ?\{)
                 (looking-back ")[ ]*" (point-min)))
        (search-backward ")")
        (when (setq pos (funcall opener (point) limit))
          (goto-char pos)
          ;;(message "pos=%S" pos)
          (setq indentation (current-indentation)))
        ) ;when
      (list :pos (if (> value 0) (point) nil)
            :char char
            :column column
            :indentation indentation)
      ) ;let
    ))

(defun rjsx-mode-count-char-in-string (char string)
  (let ((n 0))
    (dotimes (i (length string))
      (if (eq (elt string i) char)
          (setq n (1+ n))))
    n))

(defun rjsx-mode-mark-and-expand ()
  "Mark and expand."
  (interactive)
  (rjsx-mode-mark (point)))

(defun rjsx-mode-mark (pos)
  (let ((beg pos) (end pos) prop reg-beg boundaries)

    (if mark-active
        (setq reg-beg (region-beginning))
      (setq rjsx-mode-expand-initial-pos (point)
            rjsx-mode-expand-initial-scroll (window-start))
      )

    ;; (message "regs=%S %S %S %S" (region-beginning) (region-end) (point-min) (point-max))
    ;; (message "before=%S" rjsx-mode-expand-previous-state)

    (cond

     ((and mark-active
           (= (region-beginning) (point-min))
           (or (= (region-end) (point-max))
               (= (1+ (region-end)) (point-max))))
      (deactivate-mark)
      (goto-char (or rjsx-mode-expand-initial-pos (point-min)))
      (setq rjsx-mode-expand-previous-state nil)
      (when rjsx-mode-expand-initial-scroll
        (set-window-start (selected-window) rjsx-mode-expand-initial-scroll))
      )

     ((string= rjsx-mode-expand-previous-state "elt-content")
      (rjsx-mode-element-parent)
      ;;(message "pos=%S" (point))
      (rjsx-mode-element-select)
      (setq rjsx-mode-expand-previous-state "html-parent"))

     ((and (member (get-text-property pos 'block-token) '(comment string))
           (not (member rjsx-mode-expand-previous-state '("block-token" "block-body" "block-side"))))
      (when (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token))
        (setq beg (or (previous-single-property-change pos 'block-token) (point-min))))
      (when (eq (get-text-property pos 'block-token) (get-text-property (1+ pos) 'block-token))
        (setq end (next-single-property-change pos 'block-token)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq rjsx-mode-expand-previous-state "block-token"))

     ((and (get-text-property pos 'block-side)
           (not (member rjsx-mode-expand-previous-state '("block-body" "block-side")))
           (not (member rjsx-mode-engine '(django go)))
           (setq boundaries (rjsx-mode-in-code-block "{" "}" 'block-side)))
      (set-mark (car boundaries))
      (goto-char (cdr boundaries))
      (exchange-point-and-mark)
      (setq rjsx-mode-expand-previous-state "block-body"))

     ((and (get-text-property pos 'block-side)
           (not (member rjsx-mode-expand-previous-state '("block-side"))))
      (set-mark (rjsx-mode-block-beginning-position pos))
      (goto-char (1+ (rjsx-mode-block-end-position pos)))
      (exchange-point-and-mark)
      (setq rjsx-mode-expand-previous-state "block-side"))

     ((and (get-text-property pos 'part-token)
           (not (string= rjsx-mode-expand-previous-state "part-token")))
      (when (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token))
        (setq beg (previous-single-property-change pos 'part-token)))
      (when (eq (get-text-property pos 'part-token) (get-text-property (1+ pos) 'part-token))
        (setq end (next-single-property-change pos 'part-token)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq rjsx-mode-expand-previous-state "part-token"))

     ((and (get-text-property pos 'part-side)
           (not (string= rjsx-mode-expand-previous-state "client-part"))
           (setq boundaries (rjsx-mode-in-code-block "{" "}" 'part-side)))
      (set-mark (car boundaries))
      (goto-char (cdr boundaries))
      (exchange-point-and-mark)
      (setq rjsx-mode-expand-previous-state "client-part"))

     ((and (get-text-property pos 'part-side)
           (not (string= rjsx-mode-expand-previous-state "part-side")))
      (when (eq (get-text-property pos 'part-side) (get-text-property (1- pos) 'part-side))
        (setq beg (previous-single-property-change pos 'part-side)))
      (when (eq (get-text-property pos 'part-side) (get-text-property (1+ pos) 'part-side))
        (setq end (next-single-property-change pos 'part-side)))
      (when (eq (char-after beg) ?\n)
        (setq beg (1+ beg)))
      (set-mark beg)
      (goto-char end)
      (when (looking-back "^[ \t]+" (point-min))
        (beginning-of-line))
      (exchange-point-and-mark)
      (setq rjsx-mode-expand-previous-state "part-side"))

     ((and (get-text-property pos 'tag-attr)
           (not (member rjsx-mode-expand-previous-state '("html-attr" "html-tag"))))
      (rjsx-mode-attribute-select pos)
      (setq rjsx-mode-expand-previous-state "html-attr"))

     ((and (eq (get-text-property pos 'tag-type) 'comment)
           (not (member rjsx-mode-expand-previous-state '("html-tag" "html-comment" "html-elt" "html-parent"))))
      (rjsx-mode-tag-select)
      (setq rjsx-mode-expand-previous-state "html-comment"))

     ((and (get-text-property pos 'tag-name)
           (not (member rjsx-mode-expand-previous-state '("html-tag" "html-elt" "html-parent"))))
      (rjsx-mode-tag-select)
      (setq rjsx-mode-expand-previous-state "html-tag"))

     ((and (get-text-property pos 'tag-beg)
           (string= rjsx-mode-expand-previous-state "html-tag"))
      (rjsx-mode-element-select)
      (setq rjsx-mode-expand-previous-state "html-elt"))

     (t
      (cond
       ((not (rjsx-mode-element-parent))
        (push-mark (point))
        (push-mark (point-max) nil t)
        (goto-char (point-min))
        (setq rjsx-mode-expand-previous-state "mark-whole"))
       ((not (= (rjsx-mode-tag-end-position (point)) (1- beg)))
        (rjsx-mode-element-content-select)
        (setq rjsx-mode-expand-previous-state "elt-content"))
       (t
        (rjsx-mode-element-select)
        (setq rjsx-mode-expand-previous-state "html-parent"))
       )
      ) ;t

     ) ;cond

    ;;(message "w=%S" (window-end))
    ;;(message "after=%S" rjsx-mode-expand-previous-state)

    ))

(defun rjsx-mode-block-kill ()
  "Kill the current block."
  (interactive)
  (rjsx-mode-block-select)
  (when mark-active
    (kill-region (region-beginning) (region-end))))

(defun rjsx-mode-block-select ()
  "Select the current block."
  (interactive)
  (let (beg)
    (when (setq beg (rjsx-mode-block-beginning-position (point)))
      (goto-char beg)
      (set-mark (point))
      (rjsx-mode-block-end)
      (exchange-point-and-mark))
    beg))

(defun rjsx-mode-tag-select ()
  "Select the current html tag."
  (interactive)
  (let (beg)
    (when (setq beg (rjsx-mode-tag-beginning-position (point)))
      (goto-char beg)
      (set-mark (point))
      (rjsx-mode-tag-end)
      (exchange-point-and-mark))
    beg))

(defun rjsx-mode-element-content-select ()
  "Select the content of a html element."
  (interactive)
  (let (pos beg end)
    (rjsx-mode-element-select)
    (when mark-active
      (setq pos (point))
      (deactivate-mark)
      (rjsx-mode-tag-match)
      (setq end (point))
      (goto-char pos)
      (rjsx-mode-tag-end)
      (set-mark (point))
      (goto-char end)
      (exchange-point-and-mark)
      )))

(defun rjsx-mode-element-select ()
  "Select the current html element (including opening and closing tags)."
  (interactive)
  (let* ((pos (point))
         (type (get-text-property pos 'tag-type)))
    (if type
        (cond
         ((member type '(start void))
          (rjsx-mode-tag-beginning)
          (set-mark (point))
          (rjsx-mode-tag-match)
          (rjsx-mode-tag-end)
          (exchange-point-and-mark))
         (t
          (rjsx-mode-tag-match)
          (set-mark (point))
          (rjsx-mode-tag-match)
          (rjsx-mode-tag-end)
          (exchange-point-and-mark))
         ) ;cond
      (rjsx-mode-element-parent)
      (unless (= (point) pos) (rjsx-mode-element-select))
      ) ;if
    ))

(defun rjsx-mode-element-is-collapsed (&optional pos)
  (unless pos (setq pos (point)))
  (let (boundaries)
    (and (setq boundaries (rjsx-mode-element-boundaries pos))
         (or (= (car (car boundaries)) (car (cdr boundaries)))
             (= (cdr (car boundaries)) (1- (car (cdr boundaries)))))
         )))

(defun rjsx-mode-element-transpose ()
  "Transpose two html elements."
  (interactive)
  (let (pos start1 end1 start2 end2)
    (save-excursion
      (setq pos (point))
      (cond
       ((get-text-property pos 'tag-type)
        (setq start1 (rjsx-mode-element-beginning-position pos)
              end1 (1+ (rjsx-mode-element-end-position pos)))
        )
       ((setq start1 (rjsx-mode-element-parent-position pos))
        (setq end1 (1+ (rjsx-mode-element-end-position pos)))
        )
       ) ;cond
      (when (and start1 end1 (> end1 0))
        (goto-char end1)
        (unless (get-text-property (point) 'tag-beg)
          (skip-chars-forward "\n\t "))
        (when (get-text-property (point) 'tag-beg)
          (setq start2 (rjsx-mode-element-beginning-position (point))
                end2 (1+ (rjsx-mode-element-end-position (point))))
          )
        )
      (transpose-regions start1 end1 start2 end2)
      ) ;save-excursion
    start2))

(defun rjsx-mode-element-children-fold-or-unfold (&optional pos)
  "Fold/Unfold all the children of the current html element."
  (interactive)
  (unless pos (setq pos (point)))
  (save-excursion
    (dolist (child (reverse (rjsx-mode-element-children pos)))
      (goto-char child)
      (rjsx-mode-fold-or-unfold))
    ))

(defun rjsx-mode-element-mute-blanks ()
  "Mute blanks."
  (interactive)
  (let (pos parent beg end children elt)
    (setq pos (point))
    (save-excursion
      (when (and (setq parent (rjsx-mode-element-boundaries pos))
                 (rjsx-mode-element-child-position (point)))
        (setq children (reverse (rjsx-mode-element-children)))
        (goto-char (car (cdr parent)))
        (dolist (child children)
          (setq elt (rjsx-mode-element-boundaries child))
          (when (> (point) (1+ (cddr elt)))
            (when (and (not (eq (get-text-property (point) 'part-token) 'comment))
                       (not (eq (get-text-property (1+ (cddr elt)) 'part-token) 'comment)))
              (rjsx-mode-insert-text-at-pos "-->" (point))
              (rjsx-mode-insert-text-at-pos "<!--" (1+ (cddr elt))))
            )
          (goto-char child)
          )
        (when (and (> (point) (1+ (cdr (car parent))))
                   (not (eq (get-text-property (point) 'part-token) 'comment))
                   (not (eq (get-text-property (1+ (cdr (car parent))) 'part-token) 'comment)))
          (rjsx-mode-insert-text-at-pos "-->" (point))
          (rjsx-mode-insert-text-at-pos "<!--" (1+ (cdr (car parent)))))
        ) ;when
      )))

(defun rjsx-mode-element-children (&optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t) (i 0) child children)
    (save-excursion
      (when (and (member (get-text-property pos 'tag-type) '(start end))
                 (setq child (rjsx-mode-element-child-position pos)))
        (while continue
          (cond
           ((> (setq i (1+ i)) 100)
            (setq continue nil)
            (message "element-children ** warning **"))
           ((= i 1)
            (goto-char child))
           ((rjsx-mode-element-sibling-next)
            )
           (t
            (setq continue nil))
           ) ;cond
          (when continue
            (setq children (append children (list (point)))))
          ) ;while
        ) ;when
      ) ;save-excursion
    ;;(message "%S" children)
    children))

(defun rjsx-mode-element-boundaries (&optional pos)
  "Return ((start-tag-beg . start-tag-end) . (end-tag-beg . end-tag-end))
First level car and cdr are the same with void elements.
Pos should be in a tag."
  (unless pos (setq pos (point)))
  (let (start-tag-beg start-tag-end end-tag-beg end-tag-end)
    (cond
     ((eq (get-text-property pos 'tag-type) 'start)
      (setq start-tag-beg (rjsx-mode-tag-beginning-position pos)
            start-tag-end (rjsx-mode-tag-end-position pos))
      (when (setq pos (rjsx-mode-tag-match-position pos))
        (setq end-tag-beg pos
              end-tag-end (rjsx-mode-tag-end-position pos)))
      )
     ((eq (get-text-property pos 'tag-type) 'end)
      (setq end-tag-beg (rjsx-mode-tag-beginning-position pos)
            end-tag-end (rjsx-mode-tag-end-position pos))
      (when (setq pos (rjsx-mode-tag-match-position pos))
        (setq start-tag-beg pos
              start-tag-end (rjsx-mode-tag-end-position pos)))
      )
     ((eq (get-text-property pos 'tag-type) 'void)
      (setq start-tag-beg (rjsx-mode-tag-beginning-position pos)
            start-tag-end (rjsx-mode-tag-end-position pos))
      (setq end-tag-beg start-tag-beg
            end-tag-end start-tag-end)
      )
     ) ;cond
    (if (and start-tag-beg start-tag-end end-tag-beg end-tag-end)
        (cons (cons start-tag-beg start-tag-end) (cons end-tag-beg end-tag-end))
      nil)
    ))

(defun rjsx-mode-surround ()
  "Surround each line of the current REGION with a start/end tag."
  (interactive)
  (when mark-active
    (let (beg end line-beg line-end pos tag tag-start tag-end)
      (save-excursion
        (setq tag (read-from-minibuffer "Tag name? ")
              tag-start (concat "<" tag ">")
              tag-end (concat "</" tag ">")
              pos (point)
              beg (region-beginning)
              end (region-end)
              line-beg (rjsx-mode-line-number beg)
              line-end (rjsx-mode-line-number end))
        (goto-char end)
        (unless (bolp)
          (insert tag-end)
          (back-to-indentation)
          (when (> beg (point))
            (goto-char beg))
          (insert tag-start))
        (while (> line-end line-beg)
          (forward-line -1)
          (setq line-end (1- line-end))
          (unless (looking-at-p "[[:space:]]*$")
            (end-of-line)
            (insert tag-end)
            (back-to-indentation)
            (when (> beg (point))
              (goto-char beg))
            (insert tag-start))
          ) ;while
        (deactivate-mark)
        ))))

(defun rjsx-mode-element-wrap (&optional tag-name)
  "Wrap current REGION with start and end tags.
Prompt user if TAG-NAME isn't provided."
  (interactive)
  (let (beg end pos tag sep)
    (save-excursion
      (setq tag (or tag-name (read-from-minibuffer "Tag name? ")))
      (setq pos (point))
      (cond
       (mark-active
        (setq beg (region-beginning)
              end (region-end)))
       ((get-text-property pos 'tag-type)
        (setq beg (rjsx-mode-element-beginning-position pos)
              end (1+ (rjsx-mode-element-end-position pos))))
       ((setq beg (rjsx-mode-element-parent-position pos))
        (setq end (1+ (rjsx-mode-element-end-position pos))))
       )
      ;;      (message "beg(%S) end(%S)" beg end)
      (when (and beg end (> end 0))
        (setq sep (if (get-text-property beg 'tag-beg) "\n" ""))
        (rjsx-mode-insert-text-at-pos (concat sep "</" tag ">") end)
        (rjsx-mode-insert-text-at-pos (concat "<" tag ">" sep) beg)
        (when (string= sep "\n") (indent-region beg (+ end (* (+ 3 (length tag)) 2))))
        )
      ) ;save-excursion
    (rjsx-mode-go beg)))

(defun rjsx-mode-element-vanish (&optional arg)
  "Vanish the current html element. The content of the element is kept."
  (interactive "p")
  (let (type (pos (point)) start-b start-e end-b end-e)
    (while (>= arg 1)
      (setq type (get-text-property pos 'tag-type))
      (when type
        (cond
         ((member type '(void))
          (rjsx-mode-element-kill)
          (set-mark (point))
          (rjsx-mode-tag-match)
          (rjsx-mode-tag-end)
          (exchange-point-and-mark))
         ((member type '(start))
          (setq start-b (rjsx-mode-tag-beginning-position)
                start-e (rjsx-mode-tag-end-position))
          (when (rjsx-mode-tag-match)
            (setq end-b (rjsx-mode-tag-beginning-position)
                  end-e (rjsx-mode-tag-end-position)))
          )
         (t
          (setq end-b (rjsx-mode-tag-beginning-position)
                end-e (rjsx-mode-tag-end-position))
          (when (rjsx-mode-tag-match)
            (setq start-b (rjsx-mode-tag-beginning-position)
                  start-e (rjsx-mode-tag-end-position)))
          ) ;t
         ) ;cond
        (when (and start-b end-b)
          (goto-char end-b)
          (delete-region end-b (1+ end-e))
          (delete-blank-lines)
          (goto-char start-b)
          (delete-region start-b (1+ start-e))
          (delete-blank-lines)
          (rjsx-mode-buffer-indent)
          )
        ;;        (message "start %S %S - end %S %S" start-b start-e end-b end-e))
        ) ;when
      (skip-chars-forward "[:space:]\n")
      (setq arg (1- arg))
      ) ;while
    ) ;let
  )

(defun rjsx-mode-element-kill (&optional arg)
  "Kill the current html element."
  (interactive "p")
  (while (>= arg 1)
    (setq arg (1- arg))
    (rjsx-mode-element-select)
    (when mark-active
      (kill-region (region-beginning) (region-end)))
    ) ;while
  )

(defun rjsx-mode-element-clone (&optional arg)
  "Clone the current html element."
  (interactive "p")
  (let (col pos)
    (while (>= arg 1)
      (setq arg (1- arg)
            col 0)
      (rjsx-mode-element-select)
      (when mark-active
        (save-excursion
          (goto-char (region-beginning))
          (setq col (current-column)))
        (kill-region (region-beginning) (region-end))
        (yank)
        (newline)
        (indent-line-to col)
        (setq pos (point))
        (yank)
        (goto-char pos))
      )
    ) ;let
  )

(defun rjsx-mode-element-insert ()
  "Insert an html element."
  (interactive)
  (let (tag-name)
    (cond
     ((and (get-text-property (point) 'tag-type)
           (not (get-text-property (point) 'tag-beg)))
      (message "element-insert ** invalid context **"))
     ((not (and (setq tag-name (read-from-minibuffer "Tag name? "))
                (> (length tag-name) 0)))
      (message "element-insert ** failure **"))
     ((rjsx-mode-element-is-void tag-name)
      (insert (concat "<" tag-name "/>"))
      )
     (mark-active
      (let ((beg (region-beginning)) (end (region-end)))
        (deactivate-mark)
        (goto-char end)
        (insert "</" tag-name ">")
        (goto-char beg)
        (insert "<" tag-name ">")
        )
      )
     (t
      (insert (concat "<" tag-name ">" "</" tag-name ">"))
      (rjsx-mode-sb "</")
      )
     ) ;cond
    ))

(defun rjsx-mode-element-rename (&optional tag-name)
  "Rename the current html element."
  (interactive)
  (save-excursion
    (let (pos)
      (unless tag-name (setq tag-name (read-from-minibuffer "New tag name? ")))
      (when (and (> (length tag-name) 0)
                 (rjsx-mode-element-beginning)
                 (looking-at "<\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)?\\)"))
        (setq pos (point))
        (unless (rjsx-mode-element-is-void)
            (save-match-data
              (rjsx-mode-tag-match)
              (if (looking-at "</[ ]*\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)?\\)")
                  (replace-match (concat "</" tag-name))
                )))
        (goto-char pos)
        (replace-match (concat "<" tag-name))
        ))))

(defun rjsx-mode-current-trimmed-line ()
  (rjsx-mode-trim (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))

(defun rjsx-mode-trim (string)
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun rjsx-mode-block-is-token-line ()
  (save-excursion
    (let ((continue t) (counter 0))
      (beginning-of-line)
      (back-to-indentation)
      (while (and continue (not (eolp)))
        (cond
         ((get-text-property (point) 'block-token)
          (setq counter (1+ counter)))
         ((not (eq ?\s (following-char)))
          (setq continue nil
                counter 0))
         ) ;cond
        (forward-char)
        ) ;while
      (> counter 0)
      )))

(defun rjsx-mode-part-is-token-line (pos)
  (save-excursion
    (let ((continue t)
          (counter 0))
      (goto-char pos)
      (setq continue (not (eolp)))
      (while continue
        (forward-char)
        (cond
         ((eolp)
          (setq continue nil))
         ((or (get-text-property (point) 'block-side)
              (member (get-text-property (point) 'part-token) '(comment string)))
          (setq counter (1+ counter)))
         ((eq ?\s (following-char))
          )
         (t
          (setq continue nil
                counter 0))
         )
        ) ;while
      (> counter 0))))

(defun rjsx-mode-is-content (&optional pos)
  (unless pos (setq pos (point)))
  (not (or (get-text-property pos 'part-side)
           (get-text-property pos 'tag-type)
           (get-text-property pos 'block-side)
           )))

(defun rjsx-mode-is-comment-or-string (&optional pos)
  (unless pos (setq pos (point)))
  (not (null (or (eq (get-text-property pos 'tag-type) 'comment)
                 (member (get-text-property pos 'block-token) '(comment string))
                 (member (get-text-property pos 'part-token) '(comment string))))))

;; NOTE: we look at the firt one
(defun rjsx-mode-block-open-p (&optional pos)
  (unless pos (setq pos (point))))

;; NOTE: we look at the last one
(defun rjsx-mode-block-close-p (&optional pos)
  (unless pos (setq pos (point)))
  (and (get-text-property pos 'block-side)
       (eq (caar (rjsx-mode-block-controls-get pos)) 'close)))

;; NOTE: we look at the first one
(defun rjsx-mode-block-inside-p (&optional pos)
  (unless pos (setq pos (point)))
  (and (get-text-property pos 'block-side)
       (eq (caar (rjsx-mode-block-controls-get pos)) 'inside)))

(defun rjsx-mode-element-is-void (&optional tag)
  (cond
   ((and tag (member tag '("div" "li" "a" "p")))
    nil)
   ((and tag (string= rjsx-mode-content-type "jsx"))
    (member (downcase tag) '("img" "br" "hr")))
   (tag
    (car (member (downcase tag) rjsx-mode-void-elements)))
   (t
    (eq (get-text-property (point) 'tag-type) 'void))
   ))

(defun rjsx-mode-toggle-current-element-highlight ()
  "Toggle highlighting of the current html element."
  (interactive)
  (if rjsx-mode-enable-current-element-highlight
      (progn
        (rjsx-mode-delete-tag-overlays)
        (setq rjsx-mode-enable-current-element-highlight nil))
    (setq rjsx-mode-enable-current-element-highlight t)
    ))

(defun rjsx-mode-fold-or-unfold (&optional pos)
  "Toggle folding on an html element or a control block."
  (interactive)
  (rjsx-mode-propertize)
  (with-silent-modifications
   (save-excursion
     (if pos (goto-char pos))
     (let (beg-inside beg-outside end-inside end-outside overlay overlays regexp)
       (when (looking-back "^[\t ]*" (point-min))
         (back-to-indentation))
       (setq overlays (overlays-at (point)))
       (dolist (elt overlays)
         (when (and (not overlay)
                    (eq (overlay-get elt 'font-lock-face) 'rjsx-mode-folded-face))
           (setq overlay elt)))
       (cond
        ;; *** unfolding
        (overlay
         (setq beg-inside (overlay-start overlay)
               end-inside (overlay-end overlay))
         (remove-overlays beg-inside end-inside)
         (put-text-property beg-inside end-inside 'invisible nil)
         )
        ;; *** tag folding
        ((member (get-text-property (point) 'tag-type) '(start end))
         (when (not (rjsx-mode-element-is-collapsed (point)))
           (rjsx-mode-tag-beginning)
           (when (eq (get-text-property (point) 'tag-type) 'end)
             (rjsx-mode-tag-match))
           (setq beg-outside (point))
           (rjsx-mode-tag-end)
           (setq beg-inside (point))
           (goto-char beg-outside)
           (when (rjsx-mode-tag-match)
             (setq end-inside (point))
             (rjsx-mode-tag-end)
             (setq end-outside (point)))
           )
         )
        ;; *** block folding
        ((cdr (rjsx-mode-block-is-control (point)))
         (setq beg-outside (rjsx-mode-block-beginning-position (point)))
         (setq beg-inside (1+ (rjsx-mode-block-end-position (point))))
         (when (rjsx-mode-block-match)
           (setq end-inside (point))
           (setq end-outside (1+ (rjsx-mode-block-end-position (point)))))
         )
        ) ;cond
       (when (and beg-inside beg-outside end-inside end-outside)
         (setq overlay (make-overlay beg-outside end-outside))
         (overlay-put overlay 'font-lock-face 'rjsx-mode-folded-face)
         (put-text-property beg-inside end-inside 'invisible t))
       ))))

(defun rjsx-mode-toggle-comments ()
  "Toggle comments visbility."
  (interactive)
  (with-silent-modifications
   (save-excursion
     (if rjsx-mode-comments-invisible
         (remove-overlays))
     (setq rjsx-mode-comments-invisible (null rjsx-mode-comments-invisible))
     (let ((continue t)
           (pos (point-min))
           (visibility rjsx-mode-comments-invisible)
           overlay end)
       (while continue
         (setq pos (next-single-property-change pos 'font-lock-face))
         (if (null pos)
             (setq continue nil)
           (when (eq (get-text-property pos 'font-lock-face) 'rjsx-mode-comment-face)
             (setq end (next-single-property-change pos 'font-lock-face))
             (put-text-property pos end 'invisible visibility)
             (when visibility
               (setq overlay (make-overlay pos end)))
             (goto-char pos)
             )
           )
         )
       ) ;let
     )))

(defun rjsx-mode-comment-or-uncomment-region (beg end &optional arg)
  (interactive)
  (save-excursion
    (push-mark end)
    (goto-char beg)
    (setq mark-active t)
    (rjsx-mode-comment-or-uncomment)
    (pop-mark)))

(defun rjsx-mode-comment-or-uncomment ()
  "Comment or uncomment line(s), block or region at POS."
  (interactive)
  ;; TODO : if mark is at eol, mark--
  (if (and (not mark-active) (looking-at-p "[[:space:]]*$"))
      (rjsx-mode-comment-insert)
    (when (and (use-region-p) (eq (point) (region-end)))
      (if (bolp) (backward-char))
      (exchange-point-and-mark))
    (skip-chars-forward "[:space:]" (line-end-position))
    (cond
     ((or (eq (get-text-property (point) 'tag-type) 'comment)
          (eq (get-text-property (point) 'block-token) 'comment)
          (eq (get-text-property (point) 'part-token) 'comment))
      (rjsx-mode-uncomment (point)))
     (t
      (rjsx-mode-comment (point)))
     )
    ) ;if
  )

(defun rjsx-mode-comment-insert ()
  (cond
   ((get-text-property (point) 'block-side)
    (insert "/*  */")
    (search-backward " */"))
   ((get-text-property (point) 'part-side)
    (insert "/*  */")
    (search-backward " */"))
   (t
    (insert "<!--  -->")
    (search-backward " -->"))
   )
  )

(defun rjsx-mode-comment-indent-new-line (&optional soft)
  (interactive)
  (let (ctx)
    (setq ctx (rjsx-mode-comment-context))
    (if (null ctx) nil
      ;; (message "ctx=%S" ctx)
      (newline 1)
      (indent-line-to (plist-get ctx :col))
      (insert (concat (plist-get ctx :prefix) ""))
      ) ;if
    ))

(defun rjsx-mode-comment-context (&optional pos)
  (cond
   (pos
    )
   ((and (eolp) (not (bobp)))
    (setq pos (1- (point))))
   (t
    (setq pos (point)))
   ) ;cond
  (let (beg col prefix type format)
    (cond
     ((eq (get-text-property pos 'block-token) 'comment)
      (setq type "block"))
     ((eq (get-text-property pos 'tag-type) 'comment)
      (setq type "tag"))
     ((eq (get-text-property pos 'part-token) 'comment)
      (setq type "part"))
     )
    (if (null type) nil
      (save-excursion
        (goto-char pos)
        (rjsx-mode-comment-beginning)
        (setq beg (point)
              col (current-column))
        (cond
         ((looking-at-p "/\\*")
          (setq format "/*"
                prefix " * "))
         ((looking-at-p "//")
          (setq format "//"
                prefix "//"))
         ((looking-at-p "#")
          (setq format "#"
                prefix "#"))
         ((looking-at-p ";")
          (setq format ";"
                prefix ";"))
         ) ;cond
        (list :beg beg :col col :prefix prefix :type type :format format)))))

(defun rjsx-mode-comment (pos)
  (let (ctx language col sel beg end tmp block-side single-line-block pos-after content)

    (setq pos-after pos)

    (setq block-side (get-text-property pos 'block-side))
    (setq single-line-block (rjsx-mode-is-single-line-block pos))

    (cond

     ((and block-side (string= rjsx-mode-engine "erb"))
      (rjsx-mode-comment-erb-block pos)
      )

     ((and single-line-block block-side
           (intern-soft (concat "rjsx-mode-comment-" rjsx-mode-engine "-block")))
        (funcall (intern (concat "rjsx-mode-comment-" rjsx-mode-engine "-block")) pos)
        )

     (t
      (setq ctx (rjsx-mode-point-context
                 (if mark-active (region-beginning) (line-beginning-position))))
      (setq language (plist-get ctx :language))
      (setq col (current-column))
      (cond
       (mark-active
        ;;(message "%S %S" (point) col)
        )
       ((and (member language '("html" "xml"))
             (get-text-property (progn (back-to-indentation) (point)) 'tag-beg))
        (rjsx-mode-element-select))
       (t
        (end-of-line)
        (set-mark (line-beginning-position)))
       ) ;cond

      (setq beg (region-beginning)
            end (region-end))

      (when (> (point) (mark))
        (exchange-point-and-mark))

      (if (and (eq (char-before end) ?\n)
               (not (eq (char-after end) ?\n)))
          (setq end (1- end)))

      (setq sel (buffer-substring-no-properties beg end))

      (cond

       ((member language '("html" "xml"))
        (cond
           (t
            (setq content (concat "<!-- " sel " -->"))
            (when (< (length sel) 1)
              (search-backward " -->")
              (setq pos-after nil))
            ))
        ) ;case html

       ((member language '("javascript" "jsx"))
        (let (alt)
          (cond
           ((get-text-property pos 'jsx-depth)
            (setq content (concat "{/* " sel " */}")))
           ((and (setq alt (cdr (assoc language rjsx-mode-comment-formats)))
                 (string= alt "//"))
            (setq content (replace-regexp-in-string (concat "\n[ ]\\{" (number-to-string col) "\\}") "\n// " sel))
            (setq content (concat "// " content))
            ;;(setq content (replace-regexp-in-string "^[ ]*" alt sel))
            )
           (rjsx-mode-comment-prefixing
            (setq content (replace-regexp-in-string (concat "\n[ ]\\{" (number-to-string col) "\\}") "\n* " sel))
            (setq content (concat "/* " content "*/")))
           (t
            (setq content (concat "/* " sel " */")))
           ) ;cond
          ) ;let
        )

       (t
        (setq content (concat "/* " sel " */")))

       ) ;cond

      (when content
        (delete-region beg end)
        (deactivate-mark)
        (let (beg end)
          (setq beg (point-at-bol))
          (insert content)
          (setq end (point-at-eol))
          (indent-region beg end)
          )
        ) ;when

      ) ;t
     ) ;cond

    (when pos-after (goto-char pos-after))

    ))

(defun rjsx-mode-comment-boundaries (&optional pos)
  (interactive)
  (unless pos (setq pos (point)))
  (let ((beg pos) (end pos) prop)
    (save-excursion
      (goto-char pos)
      (setq prop
            (cond
             ((eq (get-text-property pos 'block-token) 'comment) 'block-token)
             ((eq (get-text-property pos 'tag-type) 'comment) 'tag-type)
             ((eq (get-text-property pos 'part-token) 'comment) 'part-token)
             (t nil)
             ))
      (if (null prop)
          (setq beg nil
                end nil)
        (when (and (not (bobp))
                   (eq (get-text-property pos prop) (get-text-property (1- pos) prop)))
          (setq beg (or (previous-single-property-change pos prop) (point-min))))
        (when (and (not (eobp))
                   (eq (get-text-property pos prop) (get-text-property (1+ pos) prop)))
          (setq end (or (next-single-property-change pos prop) (point-max)))))
      (when (and beg (string= (buffer-substring-no-properties beg (+ beg 2)) "//"))
        (goto-char end)
        (while (and (looking-at-p "\n[ ]*//")
                    (not (eobp)))
          (search-forward "//")
          (backward-char 2)
          ;;(message "%S" (point))
          (setq end (next-single-property-change (point) prop))
          (goto-char end)
          ;;(message "%S" (point))
          ) ;while
        ) ;when
      (when end (setq end (1- end)))
      ) ;save-excursion
    ;;(message "beg=%S end=%S" beg end)
    (if (and beg end) (cons beg end) nil)
    ))

(defun rjsx-mode-uncomment (pos)
  (let ((beg pos) (end pos) (sub2 "") comment boundaries)
    (save-excursion
      (cond
       ((and (get-text-property pos 'block-side)
             (intern-soft (concat "rjsx-mode-uncomment-" rjsx-mode-engine "-block")))
        (funcall (intern (concat "rjsx-mode-uncomment-" rjsx-mode-engine "-block")) pos))
       ((and (setq boundaries (rjsx-mode-comment-boundaries pos))
             (setq beg (car boundaries))
             (setq end (1+ (cdr boundaries)))
             (> (- end beg) 4))
        ;;(message "beg(%S) end(%S)" beg end)
        (setq comment (buffer-substring-no-properties beg end))
        (setq sub2 (substring comment 0 2))
        (cond
         ((member sub2 '("<!" "<%"))
          (setq comment (replace-regexp-in-string "\\(^<[!%]--[ ]?\\|[ ]?--[%]?>$\\)" "" comment)))
         ((string= sub2 "{#")
          (setq comment (replace-regexp-in-string "\\(^{#[ ]?\\|[ ]?#}$\\)" "" comment)))
         ((string= sub2 "{/") ;jsx comments
          (setq comment (replace-regexp-in-string "\\(^{/\\*[ ]?\\|[ ]?\\*/}$\\)" "" comment)))
         ((string= sub2 "/*")
          (setq comment (replace-regexp-in-string "\\(^/\\*[ ]?\\|[ ]?\\*/$\\|^[ \t]*\\*\\)" "" comment)))
         ((string= sub2 "//")
          (setq comment (replace-regexp-in-string "\\(//\\)" "" comment)))
         ) ;cond
        (delete-region beg end)
        (rjsx-mode-insert-and-indent comment)
        (goto-char beg)
        )
       ) ;cond
      (indent-according-to-mode)
      )))

(defun rjsx-mode-looking-at (regexp pos)
  (save-excursion
    (goto-char pos)
    (looking-at regexp)))

(defun rjsx-mode-looking-at-p (regexp pos)
  (save-excursion
    (goto-char pos)
    (looking-at-p regexp)))

(defun rjsx-mode-looking-back (regexp pos &optional limit greedy)
  (save-excursion
    (goto-char pos)
    (if limit
        (looking-back regexp limit greedy)
      (looking-back regexp (point-min)))))

(defun rjsx-mode-insert-text-at-pos (text pos)
  (let ((mem rjsx-mode-enable-auto-pairing))
    (setq rjsx-mode-enable-auto-pairing nil)
    (save-excursion
      (goto-char pos)
      (insert text)
      (setq rjsx-mode-enable-auto-pairing mem)
      )))

(defun rjsx-mode-remove-text-at-pos (n &optional pos)
  (unless pos (setq pos (point)))
  (delete-region pos (+ pos n)))

(defun rjsx-mode-insert-and-indent (text)
  (let (beg end)
    (setq beg (point-at-bol))
    (insert text)
    (setq end (point-at-eol))
    (indent-region beg end)
    ))

(defun rjsx-mode-indentation-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (current-indentation)))

(defun rjsx-mode-navigate (&optional pos)
  "Move point to the matching opening/closing tag/block."
  (interactive)
  (unless pos (setq pos (point)))
  (let (init)
    (goto-char pos)
    (setq init (point))
    (when (> (current-indentation) (current-column))
      (back-to-indentation))
    (setq pos (point))
    (cond
     ((and (get-text-property pos 'block-side)
           (rjsx-mode-block-beginning)
           (rjsx-mode-block-controls-get (point)))
      (rjsx-mode-block-match))
     ((member (get-text-property pos 'tag-type) '(start end))
      (rjsx-mode-tag-beginning)
      (rjsx-mode-tag-match))
     (t
      (goto-char init))
     )
    ))

(defun rjsx-mode-block-match (&optional pos)
  (unless pos (setq pos (point)))
  (let (pos-ori controls control (counter 1) type (continue t) pair)
    (setq pos-ori pos)
    (goto-char pos)
    (setq controls (rjsx-mode-block-controls-get pos))
    ;;(message "controls=%S" controls)
    (cond
     (controls
      (setq pair (car controls))
      (setq control (cdr pair))
      (setq type (car pair))
      (when (eq type 'inside) (setq type 'close))
      (while continue
        (cond
         ((and (> pos-ori 1) (bobp))
          (setq continue nil))
         ((or (and (eq type 'open) (not (rjsx-mode-block-next)))
              (and (eq type 'close) (not (rjsx-mode-block-previous))))
          (setq continue nil)
          )
         ((null (setq controls (rjsx-mode-block-controls-get (point))))
          )
         (t
          ;;TODO : est il nécessaire de faire un reverse sur controls si on doit matcher backward
          (dolist (pair controls)
            (cond
             ((not (string= (cdr pair) control))
              )
             ((eq (car pair) 'inside)
              )
             ((eq (car pair) type)
              (setq counter (1+ counter)))
             (t
              (setq counter (1- counter)))
             )
            ) ;dolist
          (when (= counter 0)
            (setq continue nil))
          ) ;t
         ) ;cond
        ) ;while
      (if (= counter 0) (point) nil)
      ) ;controls
     (t
      (goto-char pos-ori)
      nil
      ) ;controls = nul
     ) ;conf
    ))

(defun rjsx-mode-tag-match (&optional pos)
  "Move point to the matching opening/closing tag."
  (interactive)
  (unless pos (setq pos (point)))
  (let (regexp)
    (cond
     ((eq (get-text-property pos 'tag-type) 'void)
      (rjsx-mode-tag-beginning))
     (t
      (setq regexp (concat "</?" (get-text-property pos 'tag-name)))
      (when (member (get-text-property pos 'tag-type) '(start end))
        (rjsx-mode-tag-beginning)
        (setq pos (point)))
      (if (eq (get-text-property pos 'tag-type) 'end)
          (rjsx-mode-tag-fetch-opening regexp pos)
        (rjsx-mode-tag-fetch-closing regexp pos))
      ) ;t
     ) ;cond
    t))

(defun rjsx-mode-tag-fetch-opening (regexp pos)
  (let ((counter 1) (n 0) (type nil))
    (goto-char pos)
    (while (and (> counter 0) (re-search-backward regexp nil t))
      (when (and (get-text-property (point) 'tag-beg)
                 (member (get-text-property (point) 'tag-type) '(start end)))
        (setq n (1+ n))
        (cond
         ((eq (get-text-property (point) 'tag-type) 'end)
          (setq counter (1+ counter)))
         (t
          (setq counter (1- counter))
          )
         )
        )
      )
    (if (= n 0) (goto-char pos))
    ))

(defun rjsx-mode-tag-fetch-closing (regexp pos)
  (let ((counter 1) (n 0))
    (goto-char pos)
    (rjsx-mode-tag-end)
    (while (and (> counter 0) (re-search-forward regexp nil t))
      (when (get-text-property (match-beginning 0) 'tag-beg)
        (setq n (1+ n))
        (if (eq (get-text-property (point) 'tag-type) 'end)
            (setq counter (1- counter))
          (setq counter (1+ counter))))
      )
    (if (> n 0)
        (rjsx-mode-tag-beginning)
      (goto-char pos))
    ))

(defun rjsx-mode-element-tag-name (&optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (if (and (rjsx-mode-tag-beginning)
             (looking-at "</?\\([[:alpha:]][[:alnum:]:-]*\\)"))
        (match-string-no-properties 1)
      nil)))

(defun rjsx-mode-element-close ()
  "Close html element."
  (interactive)
  (let (jump epp ins tag)

    (if (and (eq (char-before) ?\>)
             (rjsx-mode-element-is-void (get-text-property (1- (point)) 'tag-name)))
        (unless (eq (char-before (1- (point))) ?\/)
          (backward-char)
          (insert "/")
          (forward-char))
      (setq epp (rjsx-mode-element-parent-position)))

    ;;(message "epp=%S" epp)
    (when epp
      (setq tag (get-text-property epp 'tag-name))
      (setq tag (rjsx-mode-element-tag-name epp))
      ;;(message "tag=%S %c" tag (char-before))
      (cond
       ((or (null tag) (rjsx-mode-element-is-void tag))
        (setq epp nil))
       ((looking-back "</" (point-min))
        (setq ins tag))
       ((looking-back "<" (point-min))
        (setq ins (concat "/" tag)))
       (t
        ;;auto-close-style = 2
        ;;(message "%S %c" (point) (char-after))
        (when (and (looking-at-p "[[:alpha:]]") (> (length tag) 4))
          (dolist (elt '("div" "span" "strong" "pre" "li"))
            (when (and (string-match-p (concat "^" elt) tag) (not (string= tag elt)))
              (setq tag elt)
              (put-text-property epp (point) 'tag-name tag))
            )
          ) ;when
        (if (rjsx-mode-element-is-void (get-text-property (point) 'tag-name))
            (setq ins nil
                  epp nil)
          (setq ins (concat "</" tag)))
        )
       ) ;cond
      (when ins
        (unless (looking-at-p "[ ]*>")
          (setq ins (concat ins ">")))
        (insert ins)
        (setq tag (downcase tag))
        (save-excursion
          (search-backward "<")
          (setq jump (and (eq (char-before) ?\>)
                          (string= (get-text-property (1- (point)) 'tag-name) tag)))
          (if jump (setq jump (point)))
          ) ;save-excursion
        (if jump (goto-char jump))
        ) ;when not ins
      ) ;when epp
    epp))

(defun rjsx-mode-detect-content-type ()
  (cond
   ((and (string= rjsx-mode-content-type "javascript")
         (< (point) rjsx-mode-chunk-length)
         (eq (char-after (point-min)) ?\/)
         (string-match-p "@jsx" (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
    (rjsx-mode-set-content-type "jsx"))
   ))

(defun rjsx-mode-on-after-change (beg end len)
 ;;(message "after-change: pos=%d, beg=%d, end=%d, len=%d, ocmd=%S, cmd=%S" (point) beg end len this-original-command this-command)
 ;;(backtrace)
 ;;(message "this-command=%S" this-command)
  (when (eq this-original-command 'yank)
    (setq rjsx-mode-inhibit-fontification t))
  (when (or (null rjsx-mode-change-beg) (< beg rjsx-mode-change-beg))
    (setq rjsx-mode-change-beg beg))
  (when (or (null rjsx-mode-change-end) (> end rjsx-mode-change-end))
    (setq rjsx-mode-change-end end))
  )

(defun rjsx-mode-complete ()
  "Autocomple at point."
  (interactive)
  (let ((pos (point))
        (char (char-before))
        (chunk (buffer-substring-no-properties (- (point) 2) (point)))
        (auto-closed   nil)
        (auto-expanded nil)
        (auto-paired   nil)
        (auto-quoted   nil)
        expanders)

    ;;-- auto-closing
    (when (and rjsx-mode-enable-auto-closing
               (>= pos 4)
               (or (string= "</" chunk)
                   ;;(progn (message "%c" char) nil)
                   (and (= rjsx-mode-auto-close-style 2)
                        (or (string= rjsx-mode-content-type "jsx")
                            (not (get-text-property pos 'part-side)))
                        (string-match-p "[[:alnum:]'\"]>" chunk)))
               (not (get-text-property (- pos 2) 'block-side))
               (rjsx-mode-element-close))
      (setq auto-closed t))

    ;;-- auto-pairing
    (when (and rjsx-mode-enable-auto-pairing
               (>= pos 4)
               (not auto-closed))
      (let ((i 0) expr after pos-end (l (length rjsx-mode-auto-pairs)))
        (setq pos-end (if (> (+ pos 32) (line-end-position))
                          (line-end-position)
                        (+ pos 10)))
        (setq chunk (buffer-substring-no-properties (- pos 3) pos)
              after (buffer-substring-no-properties pos pos-end))
        (while (and (< i l) (not auto-paired))
          (setq expr (elt rjsx-mode-auto-pairs i)
                i (1+ i))
          ;;(message "chunk=%S expr=%S after=%S" chunk expr after)
          (when (and (string= (car expr) chunk)
                     (not (string-match-p (regexp-quote (cdr expr)) after)))
            (setq auto-paired t)
            (insert (cdr expr))
            (if (string-match-p "|" (cdr expr))
                (progn
                  (search-backward "|")
                  (delete-char 1))
              (goto-char pos))
            ) ;when
          ) ;while
        ) ;let
      )

    ;;-- auto-expanding
    (when (and rjsx-mode-enable-auto-expanding
               (not auto-closed)
               (not auto-paired)
               (eq char ?\/)
               (looking-back "\\(^\\|[[:punct:][:space:]>]\\)./" (point-min))
               (or (rjsx-mode-jsx-is-html (1- pos))
                   (and (not (get-text-property (1- pos) 'tag-type))
                        (not (get-text-property (1- pos) 'part-side))))
               (not (get-text-property (1- pos) 'block-side))
               )
      (setq expanders (append rjsx-mode-expanders rjsx-mode-extra-expanders))
      (let ((i 0) pair (l (length expanders)))
        (setq chunk (buffer-substring-no-properties (- pos 2) pos))
        ;;(message "%S" chunk)
        (while (and (< i l) (not auto-expanded))
          (setq pair (elt expanders i)
                i (1+ i))
          (when (string= (car pair) chunk)
            (setq auto-expanded t)
            (delete-char -2)
            (insert (cdr pair))
            (when (string-match-p "|" (cdr pair))
              (search-backward "|")
              (delete-char 1))
            ) ;when
          ) ;while
        ) ;let
      )

    ;;-- auto-quoting
    (when (and rjsx-mode-enable-auto-quoting
               (>= pos 4)
               (not (get-text-property pos 'block-side))
               (not auto-closed)
               (not auto-paired)
               (not auto-expanded)
               (get-text-property (- pos 2) 'tag-attr))
      (cond
       ((and (eq char ?\=)
             (not (looking-at-p "[ ]*[\"']")))
        (if (= rjsx-mode-auto-quote-style 2)
            (insert "''")
          (insert "\"\""))
        (backward-char)
        (setq auto-quoted t))
       ((and (eq char ?\")
             (looking-back "=[ ]*\"" (point-min))
             (not (looking-at-p "[ ]*[\"]")))
        (insert-and-inherit "\"")
        (backward-char)
        (setq auto-quoted t))
       ((and (eq char ?\')
             (looking-back "=[ ]*'" (point-min))
             (not (looking-at-p "[ ]*[']")))
        (insert-and-inherit "'")
        (backward-char)
        (setq auto-quoted t))
       ((and (eq char ?\{)
             (eq (get-text-property pos 'part-side) 'jsx)
             (looking-back "=[ ]*{" (point-min))
             (not (looking-at-p "[ ]*[}]")))
        (insert-and-inherit "}")
        (backward-char)
        (setq auto-quoted t))
       ((and (eq char ?\")
             (eq (char-after) ?\"))
        (delete-char 1)
        (cond
         ((looking-back "=\"\"" (point-min))
          (backward-char))
         ((eq (char-after) ?\s)
          (forward-char))
         (t
          (insert " "))
         ) ;cond
        )
       ) ;cond
      ) ;when

    ;;--
    (cond
     ((or auto-closed auto-paired auto-expanded auto-quoted)
      (when (and rjsx-mode-change-end
                 (>= (line-end-position) rjsx-mode-change-end))
        (setq rjsx-mode-change-end (line-end-position)))
      (list :auto-closed auto-closed
            :auto-paired auto-paired
            :auto-expanded auto-expanded
            :auto-quoted auto-quoted))
     (t
      nil)
     )

    ))

(defun rjsx-mode-on-post-command ()
  (let (ctx n char)

    ;;(message "this-command=%S (%S)" this-command rjsx-mode-expand-previous-state)
    ;;(message "%S: %S %S" this-command rjsx-mode-change-beg rjsx-mode-change-end)

    (when (and rjsx-mode-expand-previous-state
               (not (member this-command rjsx-mode-commands-like-expand-region)))
      (when (eq this-command 'keyboard-quit)
        (goto-char rjsx-mode-expand-initial-pos))
      (deactivate-mark)
      (when rjsx-mode-expand-initial-scroll
        (set-window-start (selected-window) rjsx-mode-expand-initial-scroll)
        )
      (setq rjsx-mode-expand-previous-state nil
            rjsx-mode-expand-initial-pos nil
            rjsx-mode-expand-initial-scroll nil))

    (when (member this-command '(yank))
      (let ((beg rjsx-mode-change-beg) (end rjsx-mode-change-end))
        (setq rjsx-mode-inhibit-fontification nil)
        (when (and rjsx-mode-change-beg rjsx-mode-change-end)
          (save-excursion
            (font-lock-fontify-region rjsx-mode-change-beg rjsx-mode-change-end))
          (when rjsx-mode-enable-auto-indentation
            (indent-region beg end))
          ) ;and
        )
      )

    (when (< (point) 16)
      (rjsx-mode-detect-content-type))

    (when (> (point) 1)
      (setq char (char-before)))

    (cond

     ((null char)
      )

     ((and (>= (point) 3)
           (member this-command '(self-insert-command))
           (not (member (get-text-property (point) 'part-token) '(comment string))))
      (setq ctx (rjsx-mode-complete)))

     ((and rjsx-mode-enable-auto-opening
           (member this-command '(newline electric-newline-and-maybe-indent))
           (or (and (not (eobp))
                    (eq (char-after) ?\<)
                    (eq (get-text-property (point) 'tag-type) 'end)
                    (looking-back ">\n[ \t]*" (point-min))
                    (setq n (length (match-string-no-properties 0)))
                    (eq (get-text-property (- (point) n) 'tag-type) 'start)
                    (string= (get-text-property (- (point) n) 'tag-name)
                             (get-text-property (point) 'tag-name))
                    )
               nil))
      (newline-and-indent)
      (forward-line -1)
      (indent-according-to-mode)
      )
     ) ;cond

    (when (and rjsx-mode-enable-auto-indentation
               (member this-command '(self-insert-command))
               (or (and ctx
                        (or (plist-get ctx :auto-closed)
                            (plist-get ctx :auto-expanded)))
                   (and (> (point) (point-min))
                        (get-text-property (1- (point)) 'tag-end)
                        (get-text-property (line-beginning-position) 'tag-beg))))
      (indent-according-to-mode)
      (when (and rjsx-mode-change-end (> rjsx-mode-change-end (point-max)))
        (message "post-command: enlarge rjsx-mode-change-end")
        (setq rjsx-mode-change-end (point-max))
        )
      ) ;when auto-indent

    (when (and rjsx-mode-enable-auto-indentation
               (member this-command '(self-insert-command))
               (member (get-text-property (point) 'part-side) '(javascript jsx))
               (looking-back "^[ \t]+[]})]"))
      (indent-according-to-mode)
      ;;(message "%S" (point))
      (when (and rjsx-mode-change-end (> rjsx-mode-change-end (point-max)))
        (message "post-command: enlarge rjsx-mode-change-end")
        (setq rjsx-mode-change-end (point-max))
        )
      )

    (when rjsx-mode-enable-current-element-highlight
      (rjsx-mode-highlight-current-element))

    (when (and rjsx-mode-enable-current-column-highlight
               (not (rjsx-mode-buffer-narrowed-p)))
      (rjsx-mode-column-show))

    ;;(message "post-command (%S) (%S)" rjsx-mode-change-end rjsx-mode-change-end)

    ))

(defun rjsx-mode-dom-apostrophes-replace ()
  "Replace char(') with char(’) in the html contents of the buffer."
  (interactive)
  (save-excursion
    (let ((min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (while (rjsx-mode-content-rsf "\\([[:alpha:]]\\)'\\([[:alpha:]]\\)" max)
        (replace-match "\\1’\\2"))
      )))

(defun rjsx-mode-dom-entities-encode ()
  (save-excursion
    (let (regexp ms elt (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (setq regexp "[")
      (dolist (pair rjsx-mode-html-entities)
        (setq regexp (concat regexp (char-to-string (cdr pair))))
        )
      (setq regexp (concat regexp "]"))
      (while (rjsx-mode-content-rsf regexp max)
        (setq elt (match-string-no-properties 0))
        (setq elt (aref elt 0))
        (setq elt (car (rassoc elt rjsx-mode-html-entities)))
        (replace-match (concat "&" elt ";"))
        ) ;while
      )))

(defun rjsx-mode-dom-entities-replace ()
  "Replace html entities (e.g. &eacute; &#233; or &#x00E9; become é)"
  (interactive)
  (save-excursion
    (let (ms pair elt (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (while (rjsx-mode-content-rsf "&\\([#]?[[:alnum:]]\\{2,8\\}\\);" max)
        (setq elt nil)
        (setq ms (match-string-no-properties 1))
        (cond
         ((not (eq (aref ms 0) ?\#))
          (and (setq pair (assoc ms rjsx-mode-html-entities))
               (setq elt (cdr pair))
               (setq elt (char-to-string elt))))
         ((eq (aref ms 1) ?x)
          (setq elt (substring ms 2))
          (setq elt (downcase elt))
          (setq elt (string-to-number elt 16))
          (setq elt (char-to-string elt)))
         (t
          (setq elt (substring ms 1))
          (setq elt (char-to-string (string-to-number elt))))
         ) ;cond
        (when elt (replace-match elt))
        ) ;while
      )))

(defun rjsx-mode-dom-xml-replace ()
  "Replace &, > and < in html content."
  (interactive)
  (save-excursion
    (let (expr (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (while (rjsx-mode-content-rsf "[&<>]" max)
        (replace-match (cdr (assq (char-before) rjsx-mode-xml-chars)) t t))
      )))

(defun rjsx-mode-dom-quotes-replace ()
  "Replace dumb quotes."
  (interactive)
  (save-excursion
    (let (expr (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (setq expr (concat (car rjsx-mode-smart-quotes) "\\2" (cdr rjsx-mode-smart-quotes)))
      (while (rjsx-mode-content-rsf "\\(\"\\)\\(.\\{1,200\\}\\)\\(\"\\)" max)
        (replace-match expr)
        ) ;while
      )))

(defun rjsx-mode-dom-xpath (&optional pos)
  "Display html path."
  (interactive)
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (let (path)
      (while (rjsx-mode-element-parent)
        (setq path (cons (get-text-property (point) 'tag-name) path))
        )
      (message "/%s" (mapconcat 'identity path "/"))
      )))

(defun rjsx-mode-block-ends-with (regexp &optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (save-match-data
      (if (stringp regexp)
          (and (rjsx-mode-block-end)
               (progn (backward-char) t)
               (rjsx-mode-block-skip-blank-backward)
               (progn (forward-char) t)
               (looking-back regexp (point-min)))
        (let ((pair regexp)
              (block-beg (rjsx-mode-block-beginning-position pos))
              (block-end (rjsx-mode-block-end-position pos)))
          (and (rjsx-mode-block-end)
               (rjsx-mode-block-sb (car pair) block-beg)
               (not (rjsx-mode-sf (cdr pair) block-end)))
          ) ;let
        ) ;if
      )))

(defun rjsx-mode-block-token-starts-with (regexp &optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (and (goto-char pos)
         (rjsx-mode-block-token-beginning)
         (skip-chars-forward "[\"']")
         (looking-at regexp))
    ))

(defun rjsx-mode-block-starts-with (regexp &optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (and (rjsx-mode-block-beginning)
         (rjsx-mode-block-skip-blank-forward)
         (looking-at regexp))
    ))

(defun rjsx-mode-block-skip-blank-backward (&optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t))
    (goto-char pos)
    (while continue
      (if (and (get-text-property (point) 'block-side)
               (not (bobp))
               (or (member (char-after) '(?\s ?\n))
                   (member (get-text-property (point) 'block-token)
                           '(delimiter-beg delimiter-end comment))))
          (backward-char)
        (setq continue nil))
      ) ;while
    (point)))

(defun rjsx-mode-block-skip-blank-forward (&optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t))
    (goto-char pos)
    (while continue
      (if (and (get-text-property (point) 'block-side)
               (or (member (char-after) '(?\s ?\n ?\t))
                   (member (get-text-property (point) 'block-token)
                           '(delimiter-beg delimiter-end comment))))
          (forward-char)
        (setq continue nil))
      ) ;while
;;    (message "pt=%S" (point))
    (point)))

(defun rjsx-mode-tag-attributes-sort (&optional pos)
  "Sort the attributes inside the current html tag."
  (interactive)
  (unless pos (setq pos (point)))
  (save-excursion
    (let (attrs (continue t) min max tag-beg tag-end attr attr-name attr-beg attr-end indent indentation sorter ins)
      (if (not (member (get-text-property pos 'tag-type) '(start void)))
          nil
        (setq tag-beg (rjsx-mode-tag-beginning-position pos)
              tag-end (rjsx-mode-tag-end-position))
;;        (message "%S %S" tag-beg tag-end)
        (goto-char tag-beg)
        (while continue
          (if (or (not (rjsx-mode-attribute-next))
                  (>= (point) tag-end))
              (setq continue nil)
            ;;(message "attr=%S" (point))
            (setq attr-beg (rjsx-mode-attribute-beginning-position)
                  attr-end (1+ (rjsx-mode-attribute-end-position)))
            (when (null min)
              (setq min attr-beg))
            (setq max attr-end)
            (goto-char attr-beg)
            (setq attr (buffer-substring-no-properties attr-beg attr-end))
            (if (string-match "^\\([[:alnum:]-]+\\)=" attr)
                (setq attr-name (match-string-no-properties 1 attr))
              (setq attr-name attr))
            (setq indent (looking-back "^[ \t]*" (point-min)))
            (setq attrs (append attrs (list (list attr-beg attr-end attr-name attr indent))))
            ) ;if
          ) ;while
        ) ;if in tag
      (when attrs
        (setq sorter (function
                      (lambda (elt1 elt2)
                        (string< (nth 2 elt1) (nth 2 elt2))
                        )))
        (setq attrs (sort attrs sorter))
        (delete-region (1- min) max)
        (setq ins "")
        (dolist (elt attrs)
          (if (and (nth 4 elt) (> (length ins) 1))
              (setq ins (concat ins "\n"))
            (setq ins (concat ins " ")))
          (setq ins (concat ins (nth 3 elt)))
          )
        (goto-char (1- min))
        (insert ins)
        (rjsx-mode-tag-beginning)
        (setq min (line-beginning-position))
        (rjsx-mode-tag-end)
        (setq max (line-end-position))
        (indent-region min max)
        )
      ;;(message "attrs=%S" attrs)
      )))

(defun rjsx-mode-attribute-insert ()
  "Insert an attribute inside current tag."
  (interactive)
  (let (attr attr-name attr-value)
    (cond
     ((not (eq (get-text-property (point) 'tag-type) 'start))
      (message "attribute-insert ** invalid context **"))
     ((not (and (setq attr-name (read-from-minibuffer "Attribute name? "))
                (> (length attr-name) 0)))
      (message "attribute-insert ** failure **"))
     (t
      (setq attr (concat " " attr-name))
      (when (setq attr-value (read-from-minibuffer "Attribute value? "))
        (setq attr (concat attr "=\"" attr-value "\"")))
      (rjsx-mode-tag-end)
      (re-search-backward "/?>")
      (insert attr)
      )
     ) ;cond
    ))

(defun rjsx-mode-attribute-transpose (&optional pos)
  "Transpose the current html attribute."
  (interactive)
  (unless pos (setq pos (point)))
  (let (ret attr-beg attr-end next-beg next-end tag-end)
    (when (and (get-text-property pos 'tag-attr)
               (setq next-beg (rjsx-mode-attribute-next-position pos))
               (setq next-end (rjsx-mode-attribute-end-position next-beg))
               (setq tag-end (rjsx-mode-tag-end-position pos))
               (> tag-end next-end))
      (setq attr-beg (rjsx-mode-attribute-beginning-position pos)
            attr-end (rjsx-mode-attribute-end-position pos))
      ;;      (message "%S %S - %S %S" attr-beg attr-end next-beg next-end)
      (transpose-regions attr-beg (1+ attr-end) next-beg (1+ next-end))
      )))

(defun rjsx-mode-attribute-select (&optional pos)
  "Select the current html attribute."
  (interactive)
  (unless pos (setq pos (point)))
  (if (null (get-text-property pos 'tag-attr))
      nil
    (goto-char pos)
    (rjsx-mode-attribute-beginning)
    (set-mark (point))
    (rjsx-mode-attribute-end)
    (exchange-point-and-mark)
    (point)
    ))

(defun rjsx-mode-attribute-kill (&optional arg)
  "Kill the current html attribute."
  (interactive "p")
  (unless arg (setq arg 1))
  (while (>= arg 1)
    (setq arg (1- arg))
    (rjsx-mode-attribute-select)
    (when mark-active
      (let ((beg (region-beginning)) (end (region-end)))
        (save-excursion
          (goto-char end)
          (when (looking-at "[ \n\t]*")
            (setq end (+ end (length (match-string-no-properties 0)))))
          ) ;save-excursion
        (kill-region beg end)
        ) ;let
      ) ;when
    ) ;while
  )

(defun rjsx-mode-block-close (&optional pos)
  "Close the first unclosed control block."
  (interactive)
  (unless pos (setq pos (point)))
  (let ((continue t)
        (h (make-hash-table :test 'equal)) ctx ctrl n closing-block)
    (save-excursion
      (while (and continue (rjsx-mode-block-previous))
        (when (setq ctx (rjsx-mode-block-is-control (point)))
          (setq ctrl (car ctx))
          (setq n (gethash ctrl h 0))
          (if (cdr ctx)
              (puthash ctrl (1+ n) h)
            (puthash ctrl (1- n) h))
          (when (> (gethash ctrl h) 0)
            (setq continue nil))
          )
        ) ;while
      ) ;save-excursion
    (when (and (null continue)
               (setq closing-block (rjsx-mode-closing-block ctrl)))
      (insert closing-block)
      (indent-according-to-mode)
      ;;      (indent-for-tab-command)
      )
    ))

(defun rjsx-mode-closing-block (type)
  (cond
   (t nil)
   ) ;cond
  )

;;---- POSITION ----------------------------------------------------------------

(defun rjsx-mode-comment-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (car (rjsx-mode-comment-boundaries pos)))

(defun rjsx-mode-comment-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cdr (rjsx-mode-comment-boundaries pos)))

(defun rjsx-mode-part-opening-paren-position (pos &optional limit)
  (save-restriction
    (unless limit (setq limit nil))
    (goto-char pos)
    (let* ((n -1)
           (paren (char-after))
           (pairs '((?\) . "[)(]")
                    (?\] . "[\]\[]")
                    (?\} . "[}{]")
                    (?\> . "[><]")))
           (regexp (cdr (assoc paren pairs)))
           (continue (not (null regexp)))
           (counter 0))
      (while (and continue (re-search-backward regexp limit t))
        (cond
         ((> (setq counter (1+ counter)) 500)
          (message "part-opening-paren-position ** warning **")
          (setq continue nil))
         ((or (rjsx-mode-is-comment-or-string)
              (get-text-property (point) 'block-side))
          )
         ((eq (char-after) paren)
          (setq n (1- n)))
         (t
          (setq n (1+ n))
          (setq continue (not (= n 0))))
         )
        ) ;while
      (if (= n 0) (point) nil)
      )))

(defun rjsx-mode-token-opening-paren-position (pos limit context)
  (save-restriction
    (unless limit (setq limit nil))
    (goto-char pos)
    (let* ((n -1)
           (paren (char-after))
           (pairs '((?\) . "[)(]")
                    (?\] . "[\]\[]")
                    (?\} . "[}{]")
                    (?\> . "[><]")))
           (regexp (cdr (assoc paren pairs)))
           (continue (not (null regexp)))
           (counter 0))
      (while (and continue (re-search-backward regexp limit t))
        (cond
         ((> (setq counter (1+ counter)) 200)
          (message "token-opening-paren-position ** warning **")
          (setq continue nil))
         ((get-text-property (point) 'block-side)
          )
         ((eq (char-after) paren)
          (setq n (1- n)))
         (t
          (setq n (1+ n))
          (setq continue (not (= n 0))))
         )
        ) ;while
      (if (= n 0) (point) nil)
      )))

(defun rjsx-mode-closing-paren-position (&optional pos limit)
  (save-excursion
    (unless pos (setq pos (point)))
    (unless limit (setq limit nil))
    (goto-char pos)
    (let* ((n 0)
           (block-side (and (get-text-property pos 'block-side)
                            t))
           (paren (char-after))
           (pairs '((?\( . "[)(]")
                    (?\[ . "[\]\[]")
                    (?\{ . "[}{]")
                    (?\< . "[><]")))
           (regexp (cdr (assoc paren pairs)))
           (continue (not (null regexp))))
      (while (and continue (re-search-forward regexp limit t))
        (cond
         ((or (rjsx-mode-is-comment-or-string (1- (point)))
              (and block-side (not (get-text-property (point) 'block-side))))
          ;;(message "pt=%S" (point))
          )
         ((eq (char-before) paren)
          (setq n (1+ n)))
         (t
          (setq n (1- n))
          (setq continue (not (= n 0)))
          )
         ) ;cond
        ) ;while
      (if (= n 0) (1- (point)) nil)
      )))

(defun rjsx-mode-closing-delimiter-position (delimiter &optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit nil))
  (save-excursion
    (goto-char pos)
    (setq pos nil)
    (let ((continue t))
      (while (and continue (re-search-forward delimiter limit t))
        (setq continue nil
              pos (1- (point)))
        ) ;while
      pos)))

(defun rjsx-mode-tag-match-position (&optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (rjsx-mode-tag-match pos)
    (if (= pos (point)) nil (point))))

(defun rjsx-mode-tag-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (let (beg end depth)
    (setq depth (get-text-property pos 'jsx-depth))
    (when (and depth (get-text-property pos 'tag-attr-beg))
       (setq depth (get-text-property (1- pos) 'jsx-depth)))
    (cond
     ((null pos)
      (setq end nil))
     ((get-text-property pos 'tag-beg)
      (setq beg pos))
     ((and (> pos 1) (get-text-property (1- pos) 'tag-beg))
      (setq beg (1- pos)))
     ((get-text-property pos 'tag-type)
      (setq beg (previous-single-property-change pos 'tag-beg))
      (when beg (setq beg (1- beg)))
      (cond
       ((not (get-text-property beg 'tag-beg))
        (setq beg nil))
       ((and depth (not (eq depth (get-text-property beg 'jsx-depth))))
        (let ((continue (> beg (point-min))))
          (while continue
            (setq beg (previous-single-property-change beg 'tag-beg))
            (when beg (setq beg (1- beg)))
            (cond
             ((null beg)
              (setq continue nil))
             ((not (get-text-property beg 'tag-beg))
              (setq continue nil
                    beg nil))
             ((eq depth (get-text-property beg 'jsx-depth))
              (setq continue nil))
             ) ;cond
            ) ;while
          ) ;let
        )
       ) ;cond
      )
     (t
      (setq beg nil))
     ) ;cond
    beg))

(defun rjsx-mode-tag-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (let (end depth)
    (setq depth (get-text-property pos 'jsx-depth))
    (when (and depth (get-text-property pos 'tag-attr-beg))
      (setq depth (get-text-property (1- pos) 'jsx-depth)))
    (cond
     ((null pos)
      (setq end nil))
     ((get-text-property pos 'tag-end)
      (setq end pos))
     ((get-text-property pos 'tag-type)
      (setq end (next-single-property-change pos 'tag-end))
      (cond
       ((not (get-text-property end 'tag-end))
        (setq end nil))
       ((and depth (not (eq depth (get-text-property end 'jsx-depth))))
        (let ((continue (< end (point-max))))
          (while continue
            (setq end (1+ end))
            (setq end (next-single-property-change end 'tag-end))
            (cond
             ((null end)
              (setq continue nil))
             ((not (get-text-property end 'tag-end))
              (setq continue nil
                    end nil))
             ((eq depth (get-text-property end 'jsx-depth))
              (setq continue nil))
             ) ;cond
            ) ;while
          ) ;let
        )
       ) ;cond
      )
     (t
      (setq end nil))
     ) ;cond
    end))

;; TODO: prendre en compte jsx-depth
(defun rjsx-mode-tag-next-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-max)))
  (cond
   ((or (>= pos (point-max)) (>= pos limit)) nil)
   (t
    (when (get-text-property pos 'tag-beg) (setq pos (1+ pos)))
    (setq pos (next-single-property-change pos 'tag-beg))
    (if (and pos (<= pos limit)) pos nil))
   ))

;; TODO: prendre en compte jsx-depth
(defun rjsx-mode-tag-previous-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-min)))
  (cond
   ((or (<= pos (point-min)) (<= pos limit)) nil)
   (t
    (when (get-text-property pos 'tag-beg) (setq pos (1- pos)))
    (rjsx-mode-go (previous-single-property-change pos 'tag-beg) -1))
   ))

;; TODO: prendre en compte jsx-depth
(defun rjsx-mode-attribute-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((null (get-text-property pos 'tag-attr))
    nil)
   ((get-text-property pos 'tag-attr-beg)
    pos)
   ((and (> pos (point-min)) (get-text-property (1- pos) 'tag-attr-beg))
    (1- pos))
   (t
    (setq pos (previous-single-property-change pos 'tag-attr-beg))
    (setq pos (1- pos)))
   ))

;; TODO: retoucher en incluant un param limit et en s'inspirant de
;;       rjsx-mode-attribute-next-position
(defun rjsx-mode-attribute-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (let (beg end depth)
    ;;(message "pos=%S" pos)
    (setq depth (get-text-property pos 'jsx-depth))
    (cond
     ((null pos)
      (setq end nil))
     ((get-text-property pos 'tag-attr-end)
      (setq end pos))
     ((get-text-property pos 'tag-attr)
      (setq end (next-single-property-change pos 'tag-attr-end))
      (when (and depth
                 end
                 (setq beg (rjsx-mode-attribute-beginning-position end))
                 (eq (logand (get-text-property pos 'tag-attr-beg) 4) 4))
        (setq depth (1- (get-text-property beg 'jsx-depth)))
        ;;(message "%S %S" beg end)
        )
      (cond
       ((not (get-text-property end 'tag-attr-end))
        (setq end nil))
       ((and depth
             (eq depth (get-text-property end 'jsx-depth))
             (not (eq depth (get-text-property end 'jsx-end))))
        )
       ((and depth (eq (1+ depth) (get-text-property end 'jsx-depth)))
        )
       ((and depth (not (eq (1+ depth) (get-text-property end 'jsx-depth))))
        (let ((continue (< end (point-max))))
          (while continue
            (setq end (1+ end))
            (setq end (next-single-property-change end 'tag-attr-end))
            (cond
             ((null end)
              (setq continue nil))
             ((not (get-text-property end 'tag-attr-end))
              (setq continue nil
                    end nil))
             ((eq (1+ depth) (get-text-property end 'jsx-depth))
              (setq continue nil))
             ) ;cond
            ) ;while
          ) ;let
        )
       ) ;cond
      )
     (t
      (setq end nil))
     ) ;cond
    end))

;; attention si pos est au debut d'un spread attributes, cela
;; risque de poser pb
(defun rjsx-mode-attribute-next-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-max)))
  (let (continue depth)
    (when (get-text-property pos 'tag-attr-beg)
      (setq pos (1+ pos)))
    (if (< pos limit)
        (setq continue t
              depth (get-text-property pos 'jsx-depth))
      (setq continue nil
            pos nil))
    (while continue
      (setq pos (next-single-property-change pos 'tag-attr-beg))
      (cond
       ((null pos)
        (setq continue nil))
       ((>= pos limit)
        (setq continue nil
              pos nil))
       ((null depth)
        (setq continue nil))
       ((and (eq (get-text-property pos 'tag-attr-beg) 4)
             (eq (1+ depth) (get-text-property pos 'jsx-depth)))
        (setq continue nil))
       ((eq depth (get-text-property pos 'jsx-depth))
        (setq continue nil))
       (t
        (setq pos (1+ pos)
              continue (< pos limit)))
       )
      ) ;while
    pos))

(defun rjsx-mode-attribute-previous-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-min)))
  (let (continue depth)
    (cond
     ((and (> pos (point-min)) (get-text-property (1- pos) 'tag-attr-beg))
      (setq pos (1- pos)
            continue nil))
     (t
      (when (get-text-property pos 'tag-attr-beg)
        (setq pos (1- pos)))
      (if (> pos limit)
          (setq continue t
                depth (get-text-property pos 'jsx-depth))
        (setq continue nil
              pos nil))
      ) ;t
     ) ;cond
    (while continue
      (setq pos (previous-single-property-change pos 'tag-attr-beg))
      (cond
       ((null pos)
        (setq continue nil))
       ((< pos limit)
        (setq continue nil
              pos nil))
       ;;((null depth)
       ;; (setq continue nil))
       ((and depth (eq depth (get-text-property pos 'jsx-depth)))
        (setq  pos (1- pos)
               continue nil))
       (depth
        (setq pos nil
              continue (> pos limit)))
       (t
        (setq pos (1- pos)
              continue nil))
       ) ;cond
      ) ;while
    pos))

;; TODO: prendre en compte jsx-depth
(defun rjsx-mode-element-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((null (get-text-property pos 'tag-type))
    (setq pos (rjsx-mode-element-parent-position)))
   ((eq (get-text-property pos 'tag-type) 'end)
    (setq pos (rjsx-mode-tag-match-position pos))
    (setq pos (if (get-text-property pos 'tag-beg) pos nil)))
   ((member (get-text-property pos 'tag-type) '(start void))
    (setq pos (rjsx-mode-tag-beginning-position pos)))
   (t
    (setq pos nil))
   ) ;cond
  pos)

;; TODO: prendre en compte jsx-depth
(defun rjsx-mode-element-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((null (get-text-property pos 'tag-type))
    (setq pos (rjsx-mode-element-parent-position pos))
    (when pos
      (setq pos (rjsx-mode-tag-match-position pos))
      (when pos (setq pos (rjsx-mode-tag-end-position pos)))
      )
    )
   ((member (get-text-property pos 'tag-type) '(end void))
    (setq pos (rjsx-mode-tag-end-position pos))
    )
   ((member (get-text-property pos 'tag-type) '(start))
    (setq pos (rjsx-mode-tag-match-position pos))
    (when pos (setq pos (rjsx-mode-tag-end-position pos))))
   (t
    (setq pos nil))
   ) ;cond
  pos)

(defun rjsx-mode-element-child-position (&optional pos)
  (save-excursion
    (let (child close)
      (unless pos (setq pos (point)))
      (goto-char pos)
      (cond
       ((eq (get-text-property pos 'tag-type) 'start)
        (rjsx-mode-tag-match)
        (setq close (point))
        (goto-char pos)
        )
       ((eq (get-text-property pos 'tag-type) 'void)
        )
       ((eq (get-text-property pos 'tag-type) 'end)
        (rjsx-mode-tag-beginning)
        (setq close (point))
        (rjsx-mode-tag-match)
        )
       ((rjsx-mode-element-parent-position pos)
        (setq pos (point))
        (rjsx-mode-tag-match)
        (setq close (point))
        (goto-char pos)
        )
       ) ;cond
      (when (and close
                 (rjsx-mode-element-next)
                 (< (point) close))
        (setq child (point))
        )
      child)))

(defun rjsx-mode-element-parent-position (&optional pos)
  (let (n tag-type tag-name (continue t) (tags (make-hash-table :test 'equal)))
    (save-excursion
      (if pos (goto-char pos))
      (while (and continue (rjsx-mode-tag-previous))
        (setq pos (point)
              tag-type (get-text-property pos 'tag-type)
              tag-name (get-text-property pos 'tag-name)
              n (gethash tag-name tags 0))
        (when (member tag-type '(end start))
          (if (eq tag-type 'end)
              (puthash tag-name (1- n) tags)
            (puthash tag-name (1+ n) tags)
            (when (= n 0) (setq continue nil))
            ) ;if
          ) ;when
        ) ;while
      ) ;save-excursion
    (if (null continue) pos nil)))

(defun rjsx-mode-element-previous-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-min)))
  (save-excursion
    (goto-char pos)
    (let ((continue (not (bobp)))
          (props '(start void comment)))
      (while continue
        (setq pos (rjsx-mode-tag-previous))
        (cond
         ((or (null pos) (< (point) limit))
          (setq continue nil
                pos nil))
         ((member (get-text-property (point) 'tag-type) props)
          (setq continue nil))
         )
        ) ;while
      pos)))

(defun rjsx-mode-element-next-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-max)))
  (save-excursion
    (goto-char pos)
    (let ((continue (not (eobp)))
          (props '(start void comment)))
      (while continue
        (setq pos (rjsx-mode-tag-next))
        (cond
         ((or (null pos) (> (point) limit))
          (setq continue nil
                pos nil))
         ((member (get-text-property (point) 'tag-type) props)
          (setq continue nil))
         )
        ) ;while
;;      (message "pos=%S" pos)
      pos)))

(defun rjsx-mode-part-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((member rjsx-mode-content-type rjsx-mode-part-content-types)
    (setq pos (point-max)))
   ((not (get-text-property pos 'part-side))
    (setq pos nil))
   ((= pos (point-max))
    (setq pos nil))
   ((not (get-text-property (1+ pos) 'part-side))
    pos)
   (t
    (setq pos (next-single-property-change pos 'part-side)))
   ) ;cond
  pos)

(defun rjsx-mode-part-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((member rjsx-mode-content-type rjsx-mode-part-content-types)
    (setq pos (point-min)))
   ((not (get-text-property pos 'part-side))
    (setq pos nil))
   ((= pos (point-min))
    (setq pos nil))
   ((not (get-text-property (1- pos) 'part-side))
    pos)
   (t
    (setq pos (previous-single-property-change pos 'part-side)))
   ) ;cond
  pos)

(defun rjsx-mode-part-next-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((and (= pos (point-min)) (get-text-property pos 'part-side))
    )
   ((not (get-text-property pos 'part-side))
    (setq pos (next-single-property-change pos 'part-side)))
   ((and (setq pos (rjsx-mode-part-end-position pos)) (>= pos (point-max)))
    (setq pos nil))
   ((and (setq pos (1+ pos)) (not (get-text-property pos 'part-side)))
    (setq pos (next-single-property-change pos 'part-side)))
   ) ;cond
  pos)

(defun rjsx-mode-block-match-position (&optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (rjsx-mode-block-match pos)
    (if (= pos (point)) nil (point))))

(defun rjsx-mode-block-control-previous-position (type &optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t) controls)
    (while continue
      (setq pos (rjsx-mode-block-previous-position pos))
      (cond
       ((null pos)
        (setq continue nil
              pos nil))
       ((and (setq controls (rjsx-mode-block-controls-get pos))
             (eq (car (car controls)) type))
        (setq continue nil))
       ) ;cond
      ) ;while
    pos))

(defun rjsx-mode-block-opening-paren-position (pos limit)
  (save-excursion
    (when (> limit pos)
      (message "block-opening-paren-position: limit(%S) > pos(%S)" limit pos))
    (goto-char pos)
    (let (c
          n
          pt
          (continue (> pos limit))
          (pairs '((?\) . ?\()
                   (?\] . ?\[)
                   (?\} . ?\{)))
          (h (make-hash-table :test 'equal))
          (regexp "[\]\[)(}{]"))
      (while (and continue (re-search-backward regexp limit t))
        (cond
         ((rjsx-mode-is-comment-or-string)
          )
         (t
          (setq c (char-after))
          (cond
           ((member c '(?\( ?\{ ?\[))
            (setq n (gethash c h 0))
            (if (= n 0)
                (setq continue nil
                      pt (point))
              (puthash c (1+ n) h)
              ))
           (t
            (setq c (cdr (assoc c pairs)))
            (setq n (gethash c h 0))
            (puthash c (1- n) h))
           ) ;cond
          ) ;t
         ) ;cond
        ) ;while
      pt)))

(defun rjsx-mode-block-code-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (when (and (setq pos (rjsx-mode-block-beginning-position pos))
             (eq (get-text-property pos 'block-token) 'delimiter-beg))
    (setq pos (next-single-property-change pos 'block-token)))
  pos)

(defun rjsx-mode-block-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((or (and (get-text-property pos 'block-side) (= pos (point-min)))
        (get-text-property pos 'block-beg))
    )
   ((and (> pos (point-min)) (get-text-property (1- pos) 'block-beg))
    (setq pos (1- pos)))
   ((get-text-property pos 'block-side)
    (setq pos (previous-single-property-change pos 'block-beg))
    (setq pos (if (and pos (> pos (point-min))) (1- pos) (point-min))))
   (t
    (setq pos nil))
   ) ;cond
  pos)

(defun rjsx-mode-block-string-beginning-position (pos &optional block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (rjsx-mode-block-beginning-position pos)))
  (let (char (ori pos) (continue (not (null pos))))
    (while continue
      (setq char (char-after pos))
      (cond
       ((< pos block-beg)
        (setq continue nil
              pos block-beg))
       ((and (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (rjsx-mode-block-token-beginning-position pos))
        )
       ((member char '(?\) ?\]))
        (setq pos (rjsx-mode-block-opening-paren-position pos block-beg))
        (setq pos (1- pos))
        )
       ((and (> ori pos) (member char '(?\( ?\= ?\[ ?\? ?\: ?\; ?\, ?\`)))
        (setq continue nil)
        (rjsx-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0))))
        )
       ((rjsx-mode-looking-back "\\_<\\(return\\|echo\\|include\\|print\\)[ \n\t]*" pos)
        (setq continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    ;;(message "pos=%S" pos)
    pos))

(defun rjsx-mode-block-statement-beginning-position (pos &optional block-beg)
  (unless pos (setq pos (point)))
  (setq pos (1- pos))
  (unless block-beg (setq block-beg (rjsx-mode-block-beginning-position pos)))
  (let (char (continue (not (null pos))))
    (while continue
      (setq char (char-after pos))
      (cond
       ((< pos block-beg)
        (setq continue nil
              pos block-beg))
       ((and (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (rjsx-mode-block-token-beginning-position pos)))
       ((member char '(?\) ?\] ?\}))
        (setq pos (rjsx-mode-block-opening-paren-position pos block-beg))
        (setq pos (1- pos)))
       ((and (eq char ?\=)
             (rjsx-mode-looking-back "[<>!=]+" pos block-beg t))
        (setq pos (- pos 1 (length (match-string-no-properties 0))))
        ;;(setq pos (1- pos))
        ;;(message "%S pos=%S" (match-string-no-properties 0) pos)
        )
       ((member char '(?\( ?\[ ?\{ ?\=))
        (setq continue nil)
        (rjsx-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((rjsx-mode-looking-back "\\_<\\(return\\|echo\\|include\\|print\\)[ \n\t]*" pos)
        (setq continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    pos))

(defun rjsx-mode-block-args-beginning-position (pos &optional block-beg)
  (unless pos (setq pos (point)))
  (setq pos (1- pos)) ;#512
  (unless block-beg (setq block-beg (rjsx-mode-block-beginning-position pos)))
  (let (char (continue (not (null pos))))
    (while continue
      (setq char (char-after pos))
      (cond
       ((< pos block-beg)
        (message "block-args-beginning-position ** failure **")
        (setq continue nil
              pos block-beg))
       ((and (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (rjsx-mode-block-token-beginning-position pos)))
       ((member char '(?\) ?\] ?\}))
        (setq pos (rjsx-mode-block-opening-paren-position pos block-beg))
        (setq pos (1- pos)))
       ((member char '(?\( ?\[ ?\{))
        (setq continue nil)
        (rjsx-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    pos))

(defun rjsx-mode-block-calls-beginning-position (pos &optional block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (rjsx-mode-block-beginning-position pos)))
  (let (char (continue (not (null pos))))
    (while continue
      (setq char (char-after pos))
      (cond
       ((< pos block-beg)
        (message "block-calls-beginning-position ** failure **")
        (setq continue nil
              pos block-beg))
       ((and (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (rjsx-mode-block-token-beginning-position pos)))
       ((member char '(?\) ?\]))
        (setq pos (rjsx-mode-block-opening-paren-position pos block-beg))
        (setq pos (1- pos)))
       ((member char '(?\( ?\[ ?\{ ?\} ?\= ?\? ?\: ?\; ?\,))
        (setq continue nil)
        (rjsx-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((rjsx-mode-looking-back "\\(return\\|else\\)[ \n\t]*" pos)
        (setq ;;pos (point)
              continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    pos))

(defun rjsx-mode-javascript-string-beginning-position (pos &optional reg-beg)
  (unless pos (setq pos (point)))
  (let ((char nil)
        (blockside (get-text-property pos 'block-side))
        (i 0)
        (continue (not (null pos))))
    (unless reg-beg
      (if blockside
          (setq reg-beg (rjsx-mode-block-beginning-position pos))
        (setq reg-beg (rjsx-mode-part-beginning-position pos)))
      )
    (while continue
      (setq char (char-after pos))
      (cond
       ((> (setq i (1+ i)) 20000)
        (message "javascript-string-beginning-position ** warning (%S) **" pos)
        (setq continue nil
              pos nil))
       ((null pos)
        (message "javascript-string-beginning-position ** invalid pos **")
        (setq continue nil))
       ((< pos reg-beg)
        (message "javascript-string-beginning-position ** failure **")
        (setq continue nil
              pos reg-beg))
       ((and blockside
             (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (rjsx-mode-block-token-beginning-position pos)))
       ((and (not blockside)
             (member (get-text-property pos 'part-token) '(string comment))
             (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token)))
        (setq pos (rjsx-mode-part-token-beginning-position pos)))
       ((and (not blockside)
             (get-text-property pos 'block-side))
        (when (setq pos (rjsx-mode-block-beginning-position pos))
          (setq pos (1- pos))))
       ((member char '(?\) ?\] ?\}))
        (setq pos (rjsx-mode-part-opening-paren-position pos reg-beg))
        (setq pos (1- pos)))
       ((member char '(?\( ?\{ ?\[ ?\= ?\? ?\: ?\; ?\, ?\& ?\|))
        (setq continue nil)
        (rjsx-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((rjsx-mode-looking-back "\\(return\\)[ \n\t]*" pos)
        (setq continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    ;;(message "js-statement-beg:%S" pos)
    pos))

;; TODO: reg-beg : jsx-beg
;; TODO: skipper les expr dont la depth est superieure

;; NOTE: blockside is useful for ejs
(defun rjsx-mode-javascript-statement-beginning-position (pos &optional reg-beg)
  (unless pos (setq pos (point)))
  (setq pos (1- pos))
  (let ((char nil)
        (blockside (get-text-property pos 'block-side))
        (i 0)
        (is-jsx (string= rjsx-mode-content-type "jsx"))
        (depth-o nil) (depth-l nil)
        (continue (not (null pos))))
    (setq depth-o (get-text-property pos 'jsx-depth))
    (unless reg-beg
      (cond
       (blockside
        (setq reg-beg (rjsx-mode-block-beginning-position pos)))
       (is-jsx
        (setq reg-beg (rjsx-mode-jsx-depth-beginning-position pos)))
       (t
        (setq reg-beg (rjsx-mode-part-beginning-position pos)))
       ) ;cond
      ) ;unless
    (while continue
      (setq char (char-after pos))
      (cond
       ((> (setq i (1+ i)) 20000)
        (message "javascript-statement-beginning-position ** warning (%S) **" pos)
        (setq continue nil
              pos nil))
       ((null pos)
        (message "javascript-statement-beginning-position ** invalid pos **")
        (setq continue nil))
       ((< pos reg-beg)
        (when (not is-jsx)
          (message "javascript-statement-beginning-position ** failure **"))
        (setq continue nil
              pos reg-beg))
       ((and is-jsx
             (progn (setq depth-l (get-text-property pos 'jsx-depth)))
             (not (eq depth-l depth-o)))
        ;;(message "%S > depth-o(%S) depth-l(%S)" pos depth-o depth-l)
        (setq pos (previous-single-property-change pos 'jsx-depth))
        (setq pos (1- pos))
        ;;(message "--> %S %S" pos (get-text-property pos 'jsx-depth))
        )
       ((and blockside
             (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (rjsx-mode-block-token-beginning-position pos)))
       ((and (not blockside)
             (member (get-text-property pos 'part-token) '(string comment))
             (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token)))
        (setq pos (rjsx-mode-part-token-beginning-position pos)))
       ((and (not blockside)
             (get-text-property pos 'block-side))
        (when (setq pos (rjsx-mode-block-beginning-position pos))
          (setq pos (1- pos))))
       ((member char '(?\) ?\] ?\}))
        (setq pos (rjsx-mode-part-opening-paren-position pos reg-beg))
        (setq pos (1- pos)))
       ((and (eq char ?\=)
             (rjsx-mode-looking-back "[<>!=]+" pos reg-beg t))
        (setq pos (- pos 1 (length (match-string-no-properties 0))))
        ;;(setq pos (1- pos))
        ;;(message "%S pos=%S" (match-string-no-properties 0) pos)
        )
       ((member char '(?\( ?\{ ?\[ ?\=))
        (setq continue nil)
        (rjsx-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((rjsx-mode-looking-back "\\_<\\(return\\)[ \n\t]*" pos)
        (setq continue nil)
        (rjsx-mode-looking-at "[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((rjsx-mode-looking-back "[{,][ \t\n]*[[:alnum:]_]+[ ]*:[ ]*" pos)
        (setq continue nil)
        (rjsx-mode-looking-at "[ ]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    ;;(message "%S -------" pos)
    pos))

(defun rjsx-mode-javascript-args-beginning-position (pos &optional reg-beg)
  (unless pos (setq pos (point)))
  (setq pos (1- pos))
  (let ((char nil)
        (blockside (get-text-property pos 'block-side))
        (i 0)
        (continue (not (null pos))))
    (unless reg-beg
      (if blockside
          (setq reg-beg (rjsx-mode-block-beginning-position pos))
        (setq reg-beg (rjsx-mode-part-beginning-position pos)))
      )
    (while continue
      (setq char (char-after pos))
      (cond
       ((> (setq i (1+ i)) 20000)
        (message "javascript-args-beginning-position ** warning (%S) **" pos)
        (setq continue nil
              pos nil))
       ((null pos)
        (message "javascript-args-beginning-position ** invalid pos **")
        (setq continue nil))
       ((< pos reg-beg)
        (message "javascript-args-beginning-position ** failure **")
        (setq continue nil
              pos reg-beg))
       ((and blockside
             (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (rjsx-mode-block-token-beginning-position pos)))
       ((and (not blockside)
             (member (get-text-property pos 'part-token) '(string comment))
             (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token)))
        (setq pos (rjsx-mode-part-token-beginning-position pos)))
       ((and (not blockside)
             (get-text-property pos 'block-side))
        (when (setq pos (rjsx-mode-block-beginning-position pos))
          (setq pos (1- pos)))
        )
       ((member char '(?\) ?\] ?\}))
        (when (setq pos (rjsx-mode-part-opening-paren-position pos reg-beg))
          (setq pos (1- pos))))
       ((member char '(?\( ?\[ ?\{))
;;        (rjsx-mode-looking-at ".[ \t\n]*" pos)
        (rjsx-mode-looking-at ".[ ]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))
              continue nil)
;;        (message "=>%S" pos)
        )
       ((rjsx-mode-looking-back "\\_<\\(var\\|let\\|return\\|const\\)[ \n\t]+" pos)
;;        (rjsx-mode-looking-at "[ \t\n]*" pos)
        (rjsx-mode-looking-at "[ \t]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))
              continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    ;;(message "=%S" pos)
    pos))

(defun rjsx-mode-javascript-calls-beginning-position (pos &optional reg-beg)
  (unless pos (setq pos (point)))
  ;;(message "pos=%S" pos)
  (let ((char nil)
        (dot-pos nil)
        (blockside (get-text-property pos 'block-side))
        (i 0)
        (continue (not (null pos))))
    (unless reg-beg
      (if blockside
          (setq reg-beg (rjsx-mode-block-beginning-position pos))
        (setq reg-beg (rjsx-mode-part-beginning-position pos)))
      )
    (while continue
      (setq char (char-after pos))
      ;;(message "%S| %S=%c" reg-beg pos char)
      (cond
       ((> (setq i (1+ i)) 20000)
        (message "javascript-calls-beginning-position ** warning (%S) **" pos)
        (setq continue nil
              pos nil))
       ((null pos)
        (message "javascript-calls-beginning-position ** invalid pos **")
        (setq continue nil))
       ((< pos reg-beg)
        ;;(forward-char)
        ;;(skip-chars-forward " \t")
        ;;(message "pos(%S) reg-beg(%S)" pos reg-beg)
        ;;(message "javascript-calls-beginning-position ** failure **")
        (setq continue nil
              pos reg-beg))
       ((and blockside
             (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (rjsx-mode-block-token-beginning-position pos)))
       ((and (not blockside)
             (member (get-text-property pos 'part-token) '(string comment))
             (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token)))
        (setq pos (rjsx-mode-part-token-beginning-position pos)))
       ((and (not blockside)
             (get-text-property pos 'block-side))
        (when (setq pos (rjsx-mode-block-beginning-position pos))
          (setq pos (1- pos))))
       ;;((member char '(?\) ?\] ?\}))
       ;;((member char '(?\s ?\t))
       ;; (skip-chars-backward " \t" reg-beg))
       ((and (member char '(?\.)) (> i 1))
        (setq dot-pos pos
              pos (1- pos)))
       ((member char '(?\) ?\]))
        (when (setq pos (rjsx-mode-part-opening-paren-position pos reg-beg))
          (setq pos (1- pos)))
        ;;(message "pos=%S" pos)
        )
       ((member char '(?\( ?\{ ?\} ?\[ ?\= ?\? ?\: ?\; ?\, ?\& ?\| ?\>))
        ;;(message "1--> %S" pos)
        (setq continue nil)
        (rjsx-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((rjsx-mode-looking-back "\\_<\\(return\\|else\\)[ \n\t]*" pos)
        ;;(message "2--> %S" pos)
        (setq continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    ;;(message "pos=%S dot-pos=%S" pos dot-pos)
    (if (null pos) pos (cons pos dot-pos))
    ))

(defun rjsx-mode-part-token-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((not (get-text-property pos 'part-token))
    nil)
   ((or (= pos (point-min))
        (and (> pos (point-min))
             (not (get-text-property (1- pos) 'part-token))))
    pos)
   (t
    (setq pos (previous-single-property-change pos 'part-token))
    (if (and pos (> pos (point-min))) pos (point-min)))
   ))

(defun rjsx-mode-part-token-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((not (get-text-property pos 'part-token))
    nil)
   ((or (= pos (point-max))
        (not (get-text-property (1+ pos) 'part-token)))
    pos)
   (t
    (1- (next-single-property-change pos 'part-token)))
   ))

(defun rjsx-mode-block-token-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((not (get-text-property pos 'block-token))
    nil)
   ((or (= pos (point-min))
        (and (> pos (point-min))
             (not (get-text-property (1- pos) 'block-token))))
    pos)
   (t
    (setq pos (previous-single-property-change pos 'block-token))
    (if (and pos (> pos (point-min))) pos (point-min)))
   ))

(defun rjsx-mode-block-token-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((not (get-text-property pos 'block-token))
    nil)
   ((or (= pos (point-max))
        (not (get-text-property (1+ pos) 'block-token)))
    pos)
   (t
    (1- (next-single-property-change pos 'block-token)))
   ))

(defun rjsx-mode-block-code-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (setq pos (rjsx-mode-block-end-position pos))
  (cond
   ((not pos)
    nil)
   ((and (eq (get-text-property pos 'block-token) 'delimiter-end)
         (eq (get-text-property (1- pos) 'block-token) 'delimiter-end))
    (previous-single-property-change pos 'block-token))
   ((= pos (1- (point-max))) ;; TODO: comparer plutot avec line-end-position
    (point-max))
   (t
    pos)
   ))

(defun rjsx-mode-block-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((get-text-property pos 'block-end)
    pos)
   ((get-text-property pos 'block-side)
    (or (next-single-property-change pos 'block-end)
        (point-max)))
   (t
    nil)
   ))

(defun rjsx-mode-block-previous-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((= pos (point-min))
    (setq pos nil))
   ((get-text-property pos 'block-side)
    (setq pos (rjsx-mode-block-beginning-position pos))
    (cond
     ((or (null pos) (= pos (point-min)))
      (setq pos nil)
      )
     ((and (setq pos (previous-single-property-change pos 'block-beg))
           (> pos (point-min)))
      (setq pos (1- pos))
      )
     )
    ) ;block-side
   ((get-text-property (1- pos) 'block-side)
    (setq pos (rjsx-mode-block-beginning-position (1- pos)))
    )
   (t
    (setq pos (previous-single-property-change pos 'block-side))
    (cond
     ((and (null pos) (get-text-property (point-min) 'block-beg))
      (setq pos (point-min)))
     ((and pos (> pos (point-min)))
      (setq pos (rjsx-mode-block-beginning-position (1- pos))))
     )
    )
   ) ;conf
  pos)

(defun rjsx-mode-block-next-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-max)))
  (cond
   ((and (get-text-property pos 'block-side)
         (setq pos (rjsx-mode-block-end-position pos))
         (< pos (point-max))
         (setq pos (1+ pos)))
    (unless (get-text-property pos 'block-beg)
      (setq pos (next-single-property-change pos 'block-side)))
    )
   (t
    (setq pos (next-single-property-change pos 'block-side)))
   ) ;cond
  (if (and pos (<= pos limit)) pos nil))

;;---- EXCURSION ---------------------------------------------------------------

(defun rjsx-mode-backward-sexp (n)
  (interactive "p")
  (if (< n 0) (rjsx-mode-forward-sexp (- n))
    (let (pos)
      (dotimes (_ n)
        (skip-chars-backward "[:space:]")
        (setq pos (point))
        (cond
         ((bobp) nil)
         ((get-text-property (1- pos) 'block-end)
          (backward-char 1)
          (rjsx-mode-block-beginning))
         ((get-text-property (1- pos) 'block-token)
          (backward-char 1)
          (rjsx-mode-block-token-beginning))
         ((get-text-property (1- pos) 'part-token)
          (backward-char 1)
          (rjsx-mode-part-token-beginning))
         ((get-text-property (1- pos) 'tag-end)
          (backward-char 1)
          (rjsx-mode-element-beginning))
         ((get-text-property (1- pos) 'tag-attr)
          (backward-char 1)
          (rjsx-mode-attribute-beginning))
         ((get-text-property (1- pos) 'tag-type)
          (backward-char 1)
          (rjsx-mode-tag-beginning))
         ((get-text-property (1- pos) 'jsx-end)
          (backward-char 1)
          (rjsx-mode-jsx-beginning))
         (t
          (let ((forward-sexp-function nil))
            (backward-sexp))
          ) ;case t
         ) ;cond
        ) ;dotimes
      ))) ;let if defun

(defun rjsx-mode-forward-sexp (n)
  (interactive "p")
  (if (< n 0) (rjsx-mode-backward-sexp (- n))
    (let (pos)
      (dotimes (_ n)
        (skip-chars-forward "[:space:]")
        (setq pos (point))
        (cond
         ((eobp) nil)
         ((get-text-property pos 'block-beg)
          (rjsx-mode-block-end))
         ((get-text-property pos 'block-token)
          (rjsx-mode-block-token-end))
         ((get-text-property pos 'part-token)
          (rjsx-mode-part-token-end))
         ((get-text-property pos 'tag-beg)
          (rjsx-mode-element-end))
         ((get-text-property pos 'tag-attr)
          (rjsx-mode-attribute-end))
         ((get-text-property pos 'tag-type)
          (rjsx-mode-tag-end))
         ((get-text-property pos 'jsx-beg)
          (rjsx-mode-jsx-end))
         (t
          (let ((forward-sexp-function nil))
            (forward-sexp))
          ) ;case t
         ) ;cond
        ) ;dotimes
      ))) ;let if defun

(defun rjsx-mode-comment-beginning ()
  "Fetch current comment beg."
  (interactive)
  (rjsx-mode-go (rjsx-mode-comment-beginning-position (point))))

(defun rjsx-mode-comment-end ()
  "Fetch current comment end."
  (interactive)
  (rjsx-mode-go (rjsx-mode-comment-end-position (point)) 1))

(defun rjsx-mode-tag-beginning ()
  "Fetch current html tag beg."
  (interactive)
  (rjsx-mode-go (rjsx-mode-tag-beginning-position (point))))

(defun rjsx-mode-tag-end ()
  "Fetch current html tag end."
  (interactive)
  (rjsx-mode-go (rjsx-mode-tag-end-position (point)) 1))

(defun rjsx-mode-tag-previous ()
  "Fetch previous tag."
  (interactive)
  (rjsx-mode-go (rjsx-mode-tag-previous-position (point))))

(defun rjsx-mode-tag-next ()
  "Fetch next tag. Might be html comment or server tag (e.g. jsp)."
  (interactive)
  (rjsx-mode-go (rjsx-mode-tag-next-position (point))))

(defun rjsx-mode-attribute-beginning ()
  "Fetch html attribute beginning."
  (interactive)
  (rjsx-mode-go (rjsx-mode-attribute-beginning-position (point))))

(defun rjsx-mode-attribute-end ()
  "Fetch html attribute end."
  (interactive)
  (rjsx-mode-go (rjsx-mode-attribute-end-position (point)) 1))

(defun rjsx-mode-attribute-next (&optional arg)
  "Fetch next attribute."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((= arg 1) (rjsx-mode-go (rjsx-mode-attribute-next-position (point))))
   ((< arg 1) (rjsx-mode-element-previous (* arg -1)))
   (t
    (while (>= arg 1)
      (setq arg (1- arg))
      (rjsx-mode-go (rjsx-mode-attribute-next-position (point)))
      )
    )
   )
  )

(defun rjsx-mode-attribute-previous (&optional arg)
  "Fetch previous attribute."
  (interactive "p")
  (unless arg (setq arg 1))
  (unless arg (setq arg 1))
  (cond
   ((= arg 1) (rjsx-mode-go (rjsx-mode-attribute-previous-position (point))))
   ((< arg 1) (rjsx-mode-element-next (* arg -1)))
   (t
    (while (>= arg 1)
      (setq arg (1- arg))
      (rjsx-mode-go (rjsx-mode-attribute-previous-position (point)))
      )
    )
   )
  )

(defun rjsx-mode-element-previous (&optional arg)
  "Fetch previous element."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((= arg 1) (rjsx-mode-go (rjsx-mode-element-previous-position (point))))
   ((< arg 1) (rjsx-mode-element-next (* arg -1)))
   (t
    (while (>= arg 1)
      (setq arg (1- arg))
      (rjsx-mode-go (rjsx-mode-element-previous-position (point)))
      ) ;while
    ) ;t
   ) ;cond
  )

(defun rjsx-mode-element-next (&optional arg)
  "Fetch next element."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((= arg 1) (rjsx-mode-go (rjsx-mode-element-next-position (point))))
   ((< arg 1) (rjsx-mode-element-previous (* arg -1)))
   (t
    (while (>= arg 1)
      (setq arg (1- arg))
      (rjsx-mode-go (rjsx-mode-element-next-position (point)))
      ) ;while
    ) ;t
   ) ;cond
  )

(defun rjsx-mode-element-sibling-next ()
  "Fetch next sibling element."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      (cond
       ((not (get-text-property pos 'tag-type))
        (if (and (rjsx-mode-element-parent)
                 (rjsx-mode-tag-match)
                 (rjsx-mode-tag-next)
                 (member (get-text-property (point) 'tag-type) '(start void comment)))
            (setq pos (point))
          (setq pos nil))
        )
       ((member (get-text-property pos 'tag-type) '(start void))
        (if (and (rjsx-mode-tag-match)
                 (rjsx-mode-tag-next)
                 (member (get-text-property (point) 'tag-type) '(start void comment)))
            (setq pos (point))
          (setq pos nil))
        )
       ((and (rjsx-mode-tag-next)
             (member (get-text-property (point) 'tag-type) '(start void comment)))
        (setq pos (point)))
       (t
        (setq pos nil))
       ) ;cond
      ) ;save-excursion
    (rjsx-mode-go pos)))

(defun rjsx-mode-element-sibling-previous ()
  "Fetch previous sibling element."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      (cond
       ((not (get-text-property pos 'tag-type))
        (if (and (rjsx-mode-element-parent)
                 (rjsx-mode-tag-previous)
                 (rjsx-mode-element-beginning))
            (setq pos (point))
          (setq pos nil))
        )
       ((eq (get-text-property pos 'tag-type) 'start)
        (if (and (rjsx-mode-tag-beginning)
                 (rjsx-mode-tag-previous)
                 (rjsx-mode-element-beginning))
            (setq pos (point))
          (setq pos nil))
        )
       ((and (rjsx-mode-element-beginning)
             (rjsx-mode-tag-previous)
             (rjsx-mode-element-beginning))
        (setq pos (point)))
       (t
        (setq pos nil))
       ) ;cond
      ) ;save-excursion
    (rjsx-mode-go pos)))

(defun rjsx-mode-element-beginning ()
  "Move to beginning of element."
  (interactive)
  (rjsx-mode-go (rjsx-mode-element-beginning-position (point))))

(defun rjsx-mode-element-end ()
  "Move to end of element."
  (interactive)
  (rjsx-mode-go (rjsx-mode-element-end-position (point)) 1))

(defun rjsx-mode-element-parent ()
  "Fetch parent element."
  (interactive)
  (rjsx-mode-go (rjsx-mode-element-parent-position (point))))

(defun rjsx-mode-element-child ()
  "Fetch child element."
  (interactive)
  (rjsx-mode-go (rjsx-mode-element-child-position (point))))

(defun rjsx-mode-dom-traverse ()
  "Traverse html dom tree."
  (interactive)
  (cond
   ((rjsx-mode-element-child)
    )
   ((rjsx-mode-element-sibling-next)
    )
   ((and (rjsx-mode-element-parent)
         (not (rjsx-mode-element-sibling-next)))
    (goto-char (point-min)))
   (t
    (goto-char (point-min)))
   ) ;cond
  )

(defun rjsx-mode-closing-paren (limit)
  (let ((pos (rjsx-mode-closing-paren-position (point) limit)))
    (if (or (null pos) (> pos limit))
        nil
      (goto-char pos)
      pos)
    ))

(defun rjsx-mode-part-next ()
  "Move point to the beginning of the next part."
  (interactive)
  (rjsx-mode-go (rjsx-mode-part-next-position (point))))

(defun rjsx-mode-part-beginning ()
  "Move point to the beginning of the current part."
  (interactive)
  (rjsx-mode-go (rjsx-mode-part-beginning-position (point))))

(defun rjsx-mode-part-end ()
  "Move point to the end of the current part."
  (interactive)
  (rjsx-mode-go (rjsx-mode-part-end-position (point)) 1))

(defun rjsx-mode-block-previous ()
  "Move point to the beginning of the previous block."
  (interactive)
  (rjsx-mode-go (rjsx-mode-block-previous-position (point))))

(defun rjsx-mode-block-next ()
  "Move point to the beginning of the next block."
  (interactive)
  (rjsx-mode-go (rjsx-mode-block-next-position (point))))

(defun rjsx-mode-block-beginning ()
  "Move point to the beginning of the current block."
  (interactive)
  (rjsx-mode-go (rjsx-mode-block-beginning-position (point))))

(defun rjsx-mode-block-end ()
  "Move point to the end of the current block."
  (interactive)
  (rjsx-mode-go (rjsx-mode-block-end-position (point)) 1))

(defun rjsx-mode-block-token-beginning ()
  (rjsx-mode-go (rjsx-mode-block-token-beginning-position (point))))

(defun rjsx-mode-block-token-end ()
  (rjsx-mode-go (rjsx-mode-block-token-end-position (point)) 1))

(defun rjsx-mode-part-token-beginning ()
  (rjsx-mode-go (rjsx-mode-part-token-beginning-position (point))))

(defun rjsx-mode-part-token-end ()
  (rjsx-mode-go (rjsx-mode-part-token-end-position (point)) 1))

(defun rjsx-mode-block-opening-paren (limit)
  (rjsx-mode-go (rjsx-mode-block-opening-paren-position (point) limit)))

(defun rjsx-mode-block-string-beginning (&optional pos block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (rjsx-mode-block-beginning-position pos)))
  (rjsx-mode-go (rjsx-mode-block-string-beginning-position pos block-beg)))

(defun rjsx-mode-block-statement-beginning (&optional pos block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (rjsx-mode-block-beginning-position pos)))
  (rjsx-mode-go (rjsx-mode-block-statement-beginning-position pos block-beg)))

(defun rjsx-mode-block-args-beginning (&optional pos block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (rjsx-mode-block-beginning-position pos)))
  (rjsx-mode-go (rjsx-mode-block-args-beginning-position pos block-beg)))

(defun rjsx-mode-block-calls-beginning (&optional pos block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (rjsx-mode-block-beginning-position pos)))
  (rjsx-mode-go (rjsx-mode-block-calls-beginning-position pos block-beg)))

(defun rjsx-mode-javascript-string-beginning (&optional pos reg-beg)
  (unless pos (setq pos (point)))
  (unless reg-beg
    (if (get-text-property pos 'block-side)
        (setq reg-beg (rjsx-mode-block-beginning-position pos))
      (setq reg-beg (rjsx-mode-part-beginning-position pos))))
  (rjsx-mode-go (rjsx-mode-javascript-string-beginning-position pos reg-beg)))

(defun rjsx-mode-javascript-statement-beginning (&optional pos reg-beg)
  (unless pos (setq pos (point)))
  (unless reg-beg
    (if (get-text-property pos 'block-side)
        (setq reg-beg (rjsx-mode-block-beginning-position pos))
      (setq reg-beg (rjsx-mode-part-beginning-position pos))))
  (rjsx-mode-go (rjsx-mode-javascript-statement-beginning-position pos reg-beg)))

(defun rjsx-mode-javascript-args-beginning (&optional pos reg-beg)
  (unless pos (setq pos (point)))
  (unless reg-beg
    (if (get-text-property pos 'block-side)
        (setq reg-beg (rjsx-mode-block-beginning-position pos))
      (setq reg-beg (rjsx-mode-part-beginning-position pos))))
  (rjsx-mode-go (rjsx-mode-javascript-args-beginning-position pos reg-beg)))

(defun rjsx-mode-javascript-calls-beginning (&optional pos reg-beg)
  (unless pos (setq pos (point)))
  (unless reg-beg
    (if (get-text-property pos 'block-side)
        (setq reg-beg (rjsx-mode-block-beginning-position pos))
      (setq reg-beg (rjsx-mode-part-beginning-position pos))))
  (let (pair)
    (setq pair (rjsx-mode-javascript-calls-beginning-position pos reg-beg))
    (when pair (rjsx-mode-go (car pair)))
    ))

(defun rjsx-mode-go (pos &optional offset)
  (unless offset (setq offset 0))
  (when pos
    (cond
     ((and (> offset 0) (<= (+ pos offset) (point-max)))
      (setq pos (+ pos offset)))
     ((and (< offset 0) (>= (+ pos offset) (point-min)))
      (setq pos (+ pos offset)))
     ) ;cond
    (goto-char pos))
  pos)

;;---- SEARCH ------------------------------------------------------------------

(defun rjsx-mode-rsf-balanced (regexp-open regexp-close &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t)
        (level 1)
        (pos (point))
        ret
        (regexp (concat regexp-open "\\|" regexp-close)))
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (cond
       ((null ret)
        (setq continue nil)
        )
       (t
        (if (string-match-p regexp-open (match-string-no-properties 0))
            (setq level (1+ level))
          (setq level (1- level)))
        (when (< level 1)
          (setq continue nil)
          )
        ) ;t
       ) ;cond
      ) ;while
    (when (not (= level 0)) (goto-char pos))
    ret))

(defun rjsx-mode-block-sb (expr &optional limit noerror)
  (unless limit (setq limit (rjsx-mode-block-beginning-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-backward expr limit noerror))
      (when (or (null ret)
                (not (get-text-property (point) 'block-token)))
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun rjsx-mode-block-sf (expr &optional limit noerror)
  (unless limit (setq limit (rjsx-mode-block-end-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (when (or (null ret)
                (not (get-text-property (point) 'block-token)))
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun rjsx-mode-block-rsb (regexp &optional limit noerror)
  (unless limit (setq limit (rjsx-mode-block-beginning-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (when (or (null ret)
                (not (get-text-property (point) 'block-token)))
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun rjsx-mode-block-rsf (regexp &optional limit noerror)
  (unless limit (setq limit (rjsx-mode-block-end-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (when (or (null ret)
                (not (get-text-property (point) 'block-token)))
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun rjsx-mode-part-sb (expr &optional limit noerror)
  (unless limit (setq limit (rjsx-mode-part-beginning-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-backward expr limit noerror))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'block-side)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun rjsx-mode-part-sf (expr &optional limit noerror)
  (unless limit (setq limit (rjsx-mode-part-end-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'block-side)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun rjsx-mode-part-rsb (regexp &optional limit noerror)
  (unless limit (setq limit (rjsx-mode-part-beginning-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'block-side)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun rjsx-mode-part-rsf (regexp &optional limit noerror)
  (unless limit (setq limit (rjsx-mode-part-end-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit t))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'block-side)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun rjsx-mode-javascript-rsb (regexp &optional limit noerror)
  (unless limit (setq limit (rjsx-mode-part-beginning-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'block-side))
                     (not (get-text-property (point) 'jsx-depth)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun rjsx-mode-javascript-rsf (regexp &optional limit noerror)
  (unless limit (setq limit (rjsx-mode-part-end-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit t))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'block-side))
                     (not (get-text-property (point) 'jsx-depth)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun rjsx-mode-dom-sf (expr &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (if (or (null ret)
              (not (get-text-property (- (point) (length expr)) 'block-side)))
          (setq continue nil))
      )
    ret))

(defun rjsx-mode-dom-rsf (regexp &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) (ret nil))
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      ;;      (message "ret=%S point=%S limit=%S i=%S" ret (point) limit 0)
      (cond
       ((null ret)
        (setq continue nil))
       ((or (get-text-property (match-beginning 0) 'block-side)
            (get-text-property (match-beginning 0) 'part-token))
        )
       (t
        (setq continue nil))
       ) ;cond
      ) ;while
    ret))

(defun rjsx-mode-rsb (regexp &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (if (or (null ret)
              (not (rjsx-mode-is-comment-or-string)))
          (setq continue nil)))
    ret))

(defun rjsx-mode-rsf (regexp &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (if (or (null ret)
              (not (rjsx-mode-is-comment-or-string)))
          (setq continue nil))
      )
    ret))

(defun rjsx-mode-sb (expr &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-backward expr limit noerror))
      (if (or (null ret)
              (not (rjsx-mode-is-comment-or-string)))
          (setq continue nil)))
    ret))

(defun rjsx-mode-sf (expr &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (if (or (null ret)
              (not (rjsx-mode-is-comment-or-string)))
          (setq continue nil)))
    ret))

(defun rjsx-mode-content-rsf (regexp &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret beg end)
    (while continue
      (setq ret (re-search-forward regexp limit noerror)
            beg (if (null ret) (point) (match-beginning 0))
            end (if (null ret) (point) (1- (match-end 0))))
      (if (or (null ret)
              (and (rjsx-mode-is-content beg)
                   (rjsx-mode-is-content end)))
          (setq continue nil)))
    ret))

;;---- ADVICES -----------------------------------------------------------------

(defadvice ac-start (before rjsx-mode-set-up-ac-sources activate)
  "Set `ac-sources' based on current language before running auto-complete."
  (when (equal major-mode 'rjsx-mode)
    ;; set ignore each time to nil. User has to implement a hook to change it
    ;; for each completion
    (setq rjsx-mode-ignore-ac-start-advice nil)
    (run-hooks 'rjsx-mode-before-auto-complete-hooks)
    (unless rjsx-mode-ignore-ac-start-advice
      (when rjsx-mode-ac-sources-alist
        (let ((new-rjsx-mode-ac-sources
               (assoc (rjsx-mode-language-at-pos)
                      rjsx-mode-ac-sources-alist)))
          (setq ac-sources (cdr new-rjsx-mode-ac-sources)))))))

;;---- UNIT TESTING ------------------------------------------------------------

(defun rjsx-mode-test ()
  "Executes rjsx-mode unit tests. See `rjsx-mode-tests-directory'."
  (interactive)
  (let (files ret regexp)
    (setq regexp "^[[:alnum:]][[:alnum:]._]+\\'")
    (setq files (directory-files rjsx-mode-tests-directory t regexp))
    (dolist (file files)
      (cond
       ((eq (string-to-char (file-name-nondirectory file)) ?\_)
        (delete-file file))
       (t
        (setq ret (rjsx-mode-test-process file)))
       ) ;cond
      ) ;dolist
    ))

(defun rjsx-mode-test-process (file)
  (with-temp-buffer
    (let (out sig1 sig2 success err)
      (setq-default indent-tabs-mode nil)
      (if (string-match-p "sql" file)
          (setq rjsx-mode-enable-sql-detection t)
        (setq rjsx-mode-enable-sql-detection nil))
      (insert-file-contents file)
      (set-visited-file-name file)
      (rjsx-mode)
      (setq sig1 (md5 (current-buffer)))
      (delete-horizontal-space)
      (while (not (eobp))
        (forward-line)
        (delete-horizontal-space)
        (end-of-line))
      (rjsx-mode-buffer-indent)
      (setq sig2 (md5 (current-buffer)))
      (setq success (string= sig1 sig2))
      (setq out (concat (if success "ok" "ko") " : " (file-name-nondirectory file)))
      ;(message out)
      (setq err (concat (file-name-directory file) "_err." (file-name-nondirectory file)))
      (if success
          (when (file-readable-p err)
            (delete-file err))
        (write-file err)
        ;(message "[%s]" (buffer-string))
        ) ;if
      out)))

;;---- MISC --------------------------------------------------------------------

(defun rjsx-mode-set-engine (engine)
  "Set the engine for the current buffer."
  (interactive
   (list (completing-read
          "Engine: "
          (let (engines)
            (dolist (elt rjsx-mode-engines)
              (setq engines (append engines (list (car elt)))))
            engines))))
  (setq rjsx-mode-content-type "html"
        rjsx-mode-engine (rjsx-mode-engine-canonical-name engine)
        rjsx-mode-minor-engine engine)
  (rjsx-mode-on-engine-setted)
  (rjsx-mode-buffer-highlight))

(defun rjsx-mode-set-content-type (content-type)
  "Set the content-type for the current buffer"
  (interactive (list (completing-read "Content-type: " rjsx-mode-part-content-types)))
  (setq rjsx-mode-content-type content-type)
  (when (called-interactively-p 'any)
    )
  (rjsx-mode-buffer-highlight))

(defun rjsx-mode-on-engine-setted ()
  nil ;; TODO(colin): remove
    )

(defun rjsx-mode-detect-engine ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "-\\*- engine:[ ]*\\([[:alnum:]-]+\\)[ ]*-\\*-" rjsx-mode-chunk-length t)
      (setq rjsx-mode-minor-engine (match-string-no-properties 1))
      (setq rjsx-mode-engine (rjsx-mode-engine-canonical-name rjsx-mode-minor-engine)))
    rjsx-mode-minor-engine))

(defun rjsx-mode-guess-engine-and-content-type ()
  (let (buff-name elt found)

    (setq buff-name (buffer-file-name))
    (unless buff-name (setq buff-name (buffer-name)))
    (setq rjsx-mode-is-scratch (string= buff-name "*scratch*"))
    (setq rjsx-mode-content-type nil)

    (when (boundp 'rjsx-mode-content-types-alist)
      (setq found nil)
      (dolist (elt rjsx-mode-content-types-alist)
        (when (and (not found) (string-match-p (cdr elt) buff-name))
          (setq rjsx-mode-content-type (car elt)
                found t))
        ) ;dolist
      ) ;when

    (unless rjsx-mode-content-type
      (setq found nil)
      (dolist (elt rjsx-mode-content-types)
        (when (and (not found) (string-match-p (cdr elt) buff-name))
          (setq rjsx-mode-content-type (car elt)
                found t))
        ) ;dolist
      ) ;unless

    (when (boundp 'rjsx-mode-engines-alist)
      (setq found nil)
      (dolist (elt rjsx-mode-engines-alist)
        (cond
         ((stringp (cdr elt))
          (when (string-match-p (cdr elt) buff-name)
            (setq rjsx-mode-engine (car elt))))
         ((functionp (cdr elt))
          (when (funcall (cdr elt))
            (setq rjsx-mode-engine (car elt))))
         ) ;cond
        ) ;dolist
      ) ;when

    (when (and (string= rjsx-mode-content-type "javascript")
               (string-match-p "@jsx"
                               (buffer-substring-no-properties
                                (point-min)
                                (if (< (point-max) rjsx-mode-chunk-length)
                                    (point-max)
                                  rjsx-mode-chunk-length)
                                )))
      (setq rjsx-mode-content-type "jsx"))

    (when rjsx-mode-engine
      (setq rjsx-mode-minor-engine rjsx-mode-engine
            rjsx-mode-engine (rjsx-mode-engine-canonical-name rjsx-mode-engine))
      )
    ))

(defun rjsx-mode-on-after-save ()
  (when rjsx-mode-is-scratch
    (rjsx-mode-guess-engine-and-content-type)
    (rjsx-mode-buffer-highlight))
  nil)

(defun rjsx-mode-on-exit ()
  (with-silent-modifications
   (put-text-property (point-min) (point-max) 'invisible nil)
   (remove-overlays)
   (remove-hook 'change-major-mode-hook 'rjsx-mode-on-exit t)
   ))

(defun rjsx-mode-file-link (file)
  "Insert a link to a file in html document. This function can be
extended to support more filetypes by customizing
`rjsx-mode-links'."
  (interactive
   (list (file-relative-name (read-file-name "Link file: "))))
  (let ((matched nil)
        (point-line (line-number-at-pos))
        (point-column (current-column)))
    (dolist (type rjsx-mode-links)
      (when (string-match (car type) file)
        (setq matched t)
        (when (nth 2 type)
          (goto-char (point-min))
          (search-forward "</head>")
          (backward-char 7)
          (open-line 1))
        (insert (format (cadr type) file))
        (indent-for-tab-command)
        (when (nth 2 type)
          ;; return point where it was and fix indentation
          (forward-line)
          (indent-for-tab-command)
          (if (> point-line (- (line-number-at-pos) 2))
              (forward-line (+ (- point-line (line-number-at-pos)) 1))
            (forward-line (- point-line (line-number-at-pos))))
          (move-to-column point-column))
        ;; move point back if needed
        (backward-char (nth 3 type))))
    (when (not matched)
      (user-error "Unknown file type"))))

(defun rjsx-mode-reload ()
  "Reload rjsx-mode."
  (interactive)
  (with-silent-modifications
    (put-text-property (point-min) (point-max) 'invisible nil)
    (remove-overlays)
    (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region)
    (load "rjsx-mode.el")
    (setq rjsx-mode-change-beg nil
          rjsx-mode-change-end nil)
    (rjsx-mode)
    ))

(defun rjsx-mode-trace (msg)
  (let (sub)
    ;;      (when (null rjsx-mode-time) (setq rjsx-mode-time (current-time)))
    (setq sub (time-subtract (current-time) rjsx-mode-time))
    (when nil
      (save-excursion
        (let ((n 0))
          (goto-char (point-min))
          (while (rjsx-mode-tag-next)
            (setq n (1+ n))
            )
          (message "%S tags found" n)
          )))
    (message "%18s: time elapsed = %Ss %9Sµs" msg (nth 1 sub) (nth 2 sub))
    ))

(defun rjsx-mode-reveal ()
  "Display text properties at point."
  (interactive)
  (let (symbols out)
    (setq out (format
               "[point=%S engine=%S minor=%S content-type=%S language-at-pos=%S]\n"
               (point)
               rjsx-mode-engine
               rjsx-mode-minor-engine
               rjsx-mode-content-type
               (rjsx-mode-language-at-pos (point))))
    (setq symbols (append rjsx-mode-scan-properties '(font-lock-face face)))
    (dolist (symbol symbols)
      (when symbol
        (setq out (concat out (format "%s(%S) " (symbol-name symbol) (get-text-property (point) symbol)))))
      )
    (message "%s\n" out)
    ;;(message "syntax-class=%S" (syntax-class (syntax-after (point))))
    (message nil)))

(defun rjsx-mode-debug ()
  "Display informations useful for debugging."
  (interactive)
  (let ((modes nil)
        (customs '(rjsx-mode-enable-current-column-highlight rjsx-mode-enable-current-element-highlight indent-tabs-mode))
        (ignore '(abbrev-mode auto-composition-mode auto-compression-mode auto-encryption-mode auto-insert-mode blink-cursor-mode column-number-mode delete-selection-mode display-time-mode electric-indent-mode file-name-shadow-mode font-lock-mode global-font-lock-mode global-hl-line-mode line-number-mode menu-bar-mode mouse-wheel-mode recentf-mode show-point-mode tool-bar-mode tooltip-mode transient-mark-mode)))
    (message "\n")
    (message "--- RJSX-MODE DEBUG BEG ---")
    (message "versions: emacs(%S.%S) rjsx-mode(%S)"
             emacs-major-version emacs-minor-version rjsx-mode-version)
    (message "vars: engine(%S) minor(%S) content-type(%S) file(%S)"
             rjsx-mode-engine
             rjsx-mode-minor-engine
             rjsx-mode-content-type
             (or (buffer-file-name) (buffer-name)))
    (message "system: window(%S) config(%S)" window-system system-configuration)
    (message "colors: fg(%S) bg(%S) "
             (cdr (assoc 'foreground-color default-frame-alist))
             (cdr (assoc 'background-color default-frame-alist)))
    (mapc (lambda (mode)
            (condition-case nil
                (if (and (symbolp mode) (symbol-value mode) (not (member mode ignore)))
                    (add-to-list 'modes mode))
              (error nil))
            ) ;lambda
          minor-mode-list)
    (message "minor modes: %S" modes)
    (message "vars:")
    (dolist (custom customs)
      (message (format "%s=%S " (symbol-name custom) (symbol-value custom))))
    (message "--- RJSX-MODE DEBUG END ---")
    (switch-to-buffer "*Messages*")
    (goto-char (point-max))
    (recenter)
  ))

(provide 'rjsx-mode)

;;; rjsx-mode.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
