;;; reverso.el --- Translation, grammar checking, context search -*- lexical-binding: t -*-

;; Copyright (C) 2023 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.2
;; Package-Requires: ((emacs "27.1") (transient "0.3.7") (request "0.3.2"))
;; Homepage: https://github.com/SqrtMinusOne/reverso.el
;; Published-At: 2022-08-28

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs client for the https://reverso.net service.  The service
;; doesn't offer an official API, so this package accesses it with
;; whatever means possible.
;;
;; The implemented features are as follows:
;; - Translation (run `reverso-translate')
;; - "Context" or bilingual concordances (run `reverso-context')
;; - Grammar check (run `reverso-grammar')
;; - Synonyms search (run `reverso-synonyms')
;; - Verb conjugation (run `reverso-conjugation')
;; There's also `reverso-grammar-buffer', which does grammar check in
;; the current buffer and displays the result with overlays.
;;
;; The `reverso' command is the entrypoint to all the functionality.
;;
;; The Elisp API of the listed features is as follows:
;; - `reverso--translate'
;; - `reverso--get-context'
;; - `reverso--get-grammar'
;; - `reverso--get-context'
;; - `reverso--get-synonyms'
;; - `reverso--get-conjugation'
;;
;; Also check out the README file at
;; <https://github.com/SqrtMinusOne/reverso.el>

;;; Code:
(require 'request)
(require 'transient)
(require 'url-util)
(require 'dom)
(require 'widget)
(require 'wid-edit)

;; XXX Compatibility with evil
(declare-function evil-define-key* "evil-core")

(defgroup reverso nil
  "Translation, grammar checking, context search."
  :group 'applications)

(defface reverso-highlight-face
  '((t (:inherit underline)))
  "Face for highlighting selected words in translation."
  :group 'reverso)

(defface reverso-error-face
  '((t (:inherit error)))
  "Face for highlighting errors in grammar check."
  :group 'reverso)

(defface reverso-heading-face
  '((t (:inherit transient-heading)))
  "Face for headings in reverso buffers."
  :group 'reverso)

(defface reverso-keyword-face
  '((t (:inherit transient-value)))
  "Face for various keywords in reverso buffers."
  :group 'reverso)

(defface reverso-definition-face
  '((t (:inherit italic)))
  "Face for word definitions in reverso buffers."
  :group 'reverso)

(defface reverso-history-item-face nil
  "Face for history items in `reverso-history'."
  :group 'reverso)

(defcustom reverso-max-display-lines-in-input 5
  "Maximum number of lines to display in input."
  :type 'integer
  :group 'reverso)

(defcustom reverso-language-completing-read-threshold 4
  "Minimum number of languages to choose with `completing-read'."
  :type 'integer
  :group 'reverso)

;; This mapping is also the source of all available languages
(defconst reverso--language-mapping
  '((arabic . ara)
    (chinese . chi)
    (czech . cze)
    (danish . dan)
    (dutch . dut)
    (english . eng)
    (french . fra)
    (german . ger)
    (greek . gre)
    (hebrew . heb)
    (hindi . hin)
    (hungarian . hun)
    (italian . ita)
    (japanese . jpn)
    (korean . kor)
    (persian . per)
    (polish . pol)
    (portuguese . por)
    (romanian . rum)
    (russian . rus)
    (slovak . slo)
    (spanish . spa)
    (swedish . swe)
    (thai . tha)
    (turkish . tur)
    (ukrainian . ukr))
  "Mapping from long language names to short ones.

This one is used for the translation queries.")

(defconst reverso--language-mapping-1
  '((english . en)
    (german . de)
    (arabic . ar)
    (french . fr)
    (spanish . es)
    (french . fr)
    (hebrew . he)
    (italian . it)
    (japanese . ja)
    (dutch . nl)
    (polish . pl)
    (portuguese . pt)
    (romanian . ro)
    (russian . ru))
  "Another mapping from long language names to short ones.

This one is used for the synonym queries.")

(defconst reverso--right-to-left-languages
  '(arabic hebrew persian)
  "List of languages that are written from right to left.")

(defcustom reverso-languages (mapcar #'car reverso--language-mapping)
  "Subset of languages to use."
  :type `(set ,@(cl-loop for cell in reverso--language-mapping
                         collect (list 'const (car cell)))))

(defconst reverso--languages
  `((translation . ,(mapcar #'car reverso--language-mapping))
    (context . (arabic german english spanish french hebrew italian
                       japanese dutch polish portuguese romanian
                       russian swedish turkish ukrainian chinese
                       korean))
    (grammar . (english french spanish italian))
    (synonyms . (arabic german english spanish french hebrew italian
                        japanese dutch polish portuguese romanian
                        russian))
    (conjugation . (english french spanish german italian portuguese
                            hebrew russian arabic japanese)))
  "Available languages for diferent operations.")

(defconst reverso--languages-compatible
  `((context
     . ((arabic . (english german spanish french hebrew italian
                           portuguese russian turkish ukrainian))
        (german . (arabic english spanish french hebrew italian
                          japanese korean dutch polish portuguese
                          romanian russian swedish turkish ukrainian))
        (english . (arabic german spanish french hebrew italian
                           japanese korean dutch polish portuguese
                           romanian russian swedish turkish ukrainian chinese))
        (spanish . (arabic german english french hebrew italian
                           japanese korean dutch polish portuguese
                           romanian russian swedish turkish chinese
                           ukrainian))
        (french . (arabic german spanish english hebrew italian
                          japanese korean dutch polish portuguese
                          romanian russian swedish turkish chinese
                          ukrainian))
        (hebrew . (arabic german english spanish french italian korean
                          dutch portuguese russian ukrainian))
        (italian . (arabic german english spanish french hebrew korean
                           japanese dutch polish portuguese romanian
                           russian swedish turkish ukrainian))
        (korean . (english ukrainian))
        (japanese . (german english spanish french italian korean
                            portuguese russian ukrainian))
        (dutch . (german english spanish french hebrew italian korean
                         portuguese russian ukrainian))
        (polish . (german english spanish french italian korean
                          ukrainian))
        (portuguese . (arabic german english spanish french hebrew
                              italian korean japanese dutch russian
                              turkish ukrainian))
        (romanian . (german english spanish french italian korean
                            turkish ukrainian))
        (russian . (arabic german english spanish french hebrew
                           italian japanese korean dutch portuguese
                           ukrainian))
        (swedish . (german english spanish french italian korean
                           ukrainian))
        (turkish . (arabic german english spanish french italian
                           korean portuguese romanian ukrainian))
        (ukrainian . (english korean))
        (chinese . (english french spanish korean ukrainian))))
    ;; They've changed this multiple times while I've been working at
    ;; the package.  Finally it seems like every language is
    ;; compatible with every other, at least for the usual
    ;; translation.
    (translation
     . ,(mapcar
         (lambda (lang)
           (cons (car lang)
                 (mapcar #'car reverso--language-mapping)))
         reverso--language-mapping)))
  "Compatibility of languages for different operations.")

(defun reverso-verify-settings ()
  "Check if the settings are correct."
  (interactive)
  (let ((languages (mapcar #'car reverso--language-mapping)))
    (dolist (cell reverso--languages)
      (dolist (lang (cdr cell))
        (unless (memq lang languages)
          (error "Language %s is not available (reverso--languages)" lang))))
    (dolist (lang reverso-languages)
      (unless (memq lang languages)
        (error "Language %s is not available (reverso-languages)" lang)))
    (dolist (cell-kind reverso--languages-compatible)
      (dolist (cell-lang (cdr cell-kind))
        (unless (memq (car cell-lang) languages)
          (error
           "Language %s is not available (reverso--languages-compatible)"
           (car cell-lang)))
        (dolist (lang (cdr cell-lang))
          (unless (memq lang languages)
            (error
             "Language %s is not available (reverso--languages-compatible)"
             lang))))))
  (message "Everything is OK"))

;;; API

(defconst reverso--urls
  '((translation . "https://api.reverso.net/translate/v1/translation")
    (context . "https://context.reverso.net/translation/")
    (grammar . "https://orthographe.reverso.net/api/v1/Spelling")
    (synonyms . "https://synonyms.reverso.net/synonym/")
    (conjugation . "https://conjugator.reverso.net/"))
  "URLs with reverso endpoints.")

(defconst reverso--user-agents
  '("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.0.0 Safari/537.36"
                          "Mozilla/5.0 (X11; Linux x86_64; rv:103.0) Gecko/20100101 Firefox/103.0"
                          "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:103.0) Gecko/20100101 Firefox/103.0"
                          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.0.0 Safari/537.36"
                          "Mozilla/5.0 (Windows NT 10.0; rv:103.0) Gecko/20100101 Firefox/103.0"
                          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36"
                          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.6 Safari/605.1.15"
                          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:103.0) Gecko/20100101 Firefox/103.0")
  "User-Agents to use for reverso.el requests.

A random one is be picked at package initialization.")

(defvar reverso--user-agent
  (nth (random (length reverso--user-agents))
       reverso--user-agents)
  "User-Agent to use for reverso.el requests.")

(defvar reverso--operation-hook nil
  "Hook run after an operation.

The operations are:
- `reverso--translate'
- `reverso--get-context'
- `reverso--get-synonyms'
- `reverso--get-grammar'

The hook is called with the following arguments:
- the operation name
- the result of the operation, that is the value passed to the
  callback
- parameters of the operation")

(defun reverso--translate (text source target cb)
  "Translate TEXT from language SOURCE to TARGET.

SOURCE and TARGET are keys of `reverso--languages'.  CB is called with
the result.

The result is an alist with the following keys:
- `:corrected-text': corrected version of SOURCE (if available)
- `:language-from': the source language
- `:language-to': the target language
- `:detected-language': the detected source language
- `:translation': a string with translated text
- `:context-results': a list with found contexts.
  One item of the list is an alist with the keys:
  - `:source': string in the source language
  - `:target': string in the target language"
  (when (string-empty-p text)
    (user-error "Empty input!"))
  (unless (and (alist-get source reverso--language-mapping)
               (member source
                       (alist-get 'translation reverso--languages)))
    (error "Wrong language: %s" source))
  (unless (and (alist-get target reverso--language-mapping)
               (member source
                       (alist-get 'translation reverso--languages)))
    (error "Wrong language: %s" target))
  (unless (member target
                  (alist-get source
                             (alist-get 'translation reverso--languages-compatible)))
    (error "Language %s is not compatible with %s" target source))
  (request (alist-get 'translation reverso--urls)
    :type "POST"
    :data (json-encode
           `((format . "text")
             (from . ,(alist-get source reverso--language-mapping))
             (input . ,text)
             (options . ((contextResults . t)
                         (languageDetection . t)
                         (origin . "reversomobile")
                         (sentenceSpliiter . :json-false)))
             (to . ,(alist-get target reverso--language-mapping))))
    :headers `(("Content-Type" . "application/json")
               ("Accept" . "*/*")
               ("Connection" . "keep-alive")
               ("User-Agent" . ,reverso--user-agent))
    :parser 'json-read
    :encoding 'utf-8
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((res (reverso--alist-remove-empty-values
                            (reverso--translate-parse data))))
                  (run-hook-with-args
                   'reverso--operation-hook
                   'reverso--translate res
                   `((:text . ,text)
                     (:source . ,source)
                     (:target . ,target)))
                  (funcall cb res))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun reverso--convert-string (dom)
  "Convert DOM from the reverso API to fontified string.

reverso.net uses tags to highlight relevant words, e.g. <em> for the
selected word in the context search.  This function fontifies words
that are in tags with `reverso-highlight-face'"
  (thread-last
    (mapconcat (lambda (node)
                 (let ((text (if (listp node) (dom-texts node) node))
                       (is-special (listp node)))
                   (if is-special
                       (propertize text 'face 'reverso-highlight-face)
                     text)))
               (dom-children dom)
               "")
    (string-trim)
    (replace-regexp-in-string
     (rx (+ (syntax whitespace))",") ",")
    (replace-regexp-in-string
     (rx (+ (syntax whitespace))) " ")))

(defun reverso--convert-string-html (html)
  "Convert an HTML string from the reverso API to fontified string."
  (with-temp-buffer
    (insert "<html><body>" html "<body></html>")
    (reverso--convert-string
     (car
      (dom-by-tag
       (libxml-parse-html-region (point-min) (point-max))
       'body)))))

(defun reverso--alist-remove-empty-values (alist)
  "Remove empty values from ALIST."
  (cl-loop for (key . value) in alist
           if value
           collect (cons key value)))

(defun reverso--alist-get-inv (alist lookup-value)
  "Like `alist-get', but with `car' and `cdr' swapped.

ALIST is an alist, LOOKUP-VALUE is a value to look in `cdr'."
  (cl-loop for (key . value) in alist
           if (equal lookup-value value)
           return key))

(defun reverso--translate-parse (response)
  "Convert RESPONSE from the reverso translation API into an alist."
  (let ((corrected-text (alist-get 'correctedText response))
        (language-from (reverso--alist-get-inv
                        reverso--language-mapping
                        (intern (alist-get 'from response))))
        (language-to (reverso--alist-get-inv
                      reverso--language-mapping
                      (intern (alist-get 'to response))))
        (detected-language (reverso--alist-get-inv
                            reverso--language-mapping
                            (intern (or
                                     (alist-get 'detectedLanguage
                                                (alist-get 'languageDetection response))
                                     "nil"))))
        (translation (seq-elt (alist-get 'translation response) 0))
        (context-results
         (cl-loop for r across (alist-get 'results (alist-get 'contextResults response))
                  collect
                  `((:translation . ,(alist-get 'translation r))
                    (:context
                     . ,(cl-loop for source across (alist-get 'sourceExamples r)
                                 for target across (alist-get 'targetExamples r)
                                 collect
                                 `((:source . ,(reverso--convert-string-html source))
                                   (:target . ,(reverso--convert-string-html target)))))))))
    `((:corrected-text . ,corrected-text)
      (:language-from . ,language-from)
      (:language-to . ,language-to)
      (:detected-language . ,detected-language)
      (:translation . ,translation)
      (:context-results . ,context-results))))

(defun reverso--get-context (text source target cb)
  "Find bilingual concordances for TEXT.

SOURCE and TARGET are keys of `reverso--languages'.  CB is called with
the result.

The result is a list of alists with the keys:
- `:source': a string in the source language
- `:target': the same string in the target language"
  (unless (and (alist-get source reverso--language-mapping)
               (member source
                       (alist-get 'context reverso--languages)))
    (error "Wrong language: %s" source))
  (unless (and (alist-get target reverso--language-mapping)
               (member source
                       (alist-get 'context reverso--languages)))
    (error "Wrong language: %s" target))
  (unless (member target
                  (alist-get source
                             (alist-get 'context reverso--languages-compatible)))
    (error "Language %s is not compatible with %s" target source))
  (request (concat (alist-get 'context reverso--urls)
                   (symbol-name source) "-" (symbol-name target) "/"
                   (replace-regexp-in-string
                    "%20" "+" (url-hexify-string text) t t))
    :type "GET"
    :headers `(("Accept" . "*/*")
               ("Connection" . "keep-alive")
               ("User-Agent" . ,reverso--user-agent))
    :parser 'buffer-string
    :encoding 'utf-8
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((res (reverso--alist-remove-empty-values
                            (reverso--get-context-parse data))))
                  (run-hook-with-args
                   'reverso--operation-hook
                   'reverso--get-context res
                   `((:text . ,text)
                     (:source . ,source)
                     (:target . ,target)))
                  (funcall cb res))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun reverso--get-context-parse (data)
  "Parse response from reverso context API.

DATA is an html string."
  (let ((html (with-temp-buffer
                (insert data)
                (libxml-parse-html-region (point-min) (point-max)))))
    (let ((examples (dom-by-id html "examples-content")))
      (cl-loop for child in (dom-non-text-children examples)
               for classes = (alist-get 'class (dom-attributes child))
               when (string-match-p (rx "example") classes)
               collect (let ((src (dom-by-class (dom-by-class child "src") "text"))
                             (trg (dom-by-class (dom-by-class child "trg") "text")))
                         `((:source . ,(reverso--convert-string src))
                           (:target . ,(reverso--convert-string trg))))))))

(defun reverso--get-synonyms (text language cb)
  "Get synonyms for TEXT in LANGUAGE.

CB is called with the result.

The result is a list of alists with the following keys:
- `:kind': part of speech
- `:synonyms': list of alists:
   - `:synonym': word
   - `:relevant': if t, considered a \"good match\" by the service
- `:examples': list of strings with examples
- `:antonyms': list of alists:
   - `:antonym': word"
  (unless (alist-get language reverso--language-mapping-1)
    (error "Wrong language: %s" language))
  (request (concat (alist-get 'synonyms reverso--urls)
                   (symbol-name (alist-get language reverso--language-mapping-1)) "/"
                   (url-hexify-string text))
    :type "GET"
    :headers `(("Accept" . "*/*")
               ("Connection" . "keep-alive")
               ("User-Agent" . ,reverso--user-agent))
    :parser 'buffer-string
    :encoding 'utf-8
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((res (reverso--alist-remove-empty-values
                            (reverso--get-synonyms-parse data))))
                  (run-hook-with-args
                   'reverso--operation-hook
                   'reverso--get-synonyms res
                   `((:text . ,text)
                     (:language . ,language)))
                  (funcall cb res))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun reverso--get-synonyms-parse (html)
  "Parse the reverso synonyms page.

HTML is a string."
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max)))))
    (cl-loop
     for child in (dom-non-text-children
                   (dom-by-id (dom-by-tag dom 'body) "synonyms"))
     if (string-match-p "wrap-hold-prop" (or (dom-attr child 'class) ""))
     collect
     `((:kind . ,(string-trim (dom-texts (dom-by-class child "words-options"))))
       (:synonyms
        . ,(cl-loop
            for synonym in (dom-non-text-children
                            (dom-by-class (car (dom-by-class child "word-opt"))
                                          "word-box"))
            for a = (car (dom-by-tag synonym 'a))
            collect
            `((:synonym . ,(string-trim (dom-texts synonym)))
              (:relevant
               . ,(and (string-match-p "relevant" (or (dom-attr a 'class) "")) t)))))
       (:examples
        . ,(cl-loop
            for example in (dom-non-text-children
                            (dom-by-class child "phrases-examples"))
            for span = (car (dom-by-tag example 'span))
            if span
            collect (reverso--convert-string span)))
       (:antonyms
        . ,(cl-loop
            for antonym in (dom-non-text-children
                            (dom-by-class (car (dom-by-class child "antonyms-wrapper"))
                                          "word-box"))
            for text = (string-trim (dom-texts antonym))
            unless (string-match-p (rx "...") text)
            collect
            `((:antonym . ,text))))))))

(defun reverso--get-grammar (text language cb)
  "Check TEXT in LANGUAGE for errors.

CB is called with the result.

The result is an alist with the following keys:
`:corrected-text': corrected version of TEXT
`:source-text-hl': TEXT with highlighted errors
`:corrections': a list of alists with keys:
  - `:type': error type
  - `:short-description'
  - `:long-description'
  - `:mistake-text': part of TEXT with the error
  - `:correction-text': corrected version of `:mistake-text'
  - `:correction-defition': definiton of `:correction-text', if
    available
  - `:start-index': start index in TEXT
  - `:end-index': end index TEXT
  - `:suggestions': a list of alists with keys:
    - `:text': the suggested text
    - `:definition': definition of `:text', if available
    - `:category': category of the correction, if available"
  (unless (member language (alist-get 'grammar reverso--languages))
    (error "Wrong language: %s" language))
  (request (alist-get 'grammar reverso--urls)
    :type "POST"
    :data (json-encode
           `((IsUserPremium . :json-false)
             (autoReplace . t)
             (englishDialect . "indifferent")
             (getCorrectionDetails . t)
             (interfaceLanguage . "en")
             (isHtml . :json-false)
             (language . ,(alist-get language reverso--language-mapping))
             (locale . "")
             (origin . "interactive")
             (originalText . "")
             (spellingFeedbackOptions
              . ((insertFeedback . t)
                 (userLoggedOn . :json-false)))
             (text . ,text)))
    :headers `(("Content-Type" . "application/json")
               ("Accept" . "*/*")
               ("Connection" . "keep-alive")
               ("User-Agent" . ,reverso--user-agent))
    :parser 'json-read
    :encoding 'utf-8
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((res (reverso--alist-remove-empty-values
                            (reverso--get-grammar-parse text data))))
                  (run-hook-with-args
                   'reverso--operation-hook
                   'reverso--get-grammar res
                   `((:text . ,text)
                     (:language . ,language)))
                  (funcall cb res))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun reverso--get-grammar-parse (source-text data)
  "Parse the reverso grammar API response.

SOURCE-TEXT is the text sent for checking.  DATA is the JSON reply."
  (let* ((corrected-text (alist-get 'text data))
         (source-text-hl
          (with-temp-buffer
            (insert source-text)
            (cl-loop for corr across (alist-get 'corrections data)
                     if (alist-get 'startIndex corr)
                     do (put-text-property (1+ (alist-get 'startIndex corr))
                                           (+ 2 (alist-get 'endIndex corr))
                                           'face 'reverso-error-face))
            (buffer-string)))
         (corrections
          (cl-loop
           for corr across (alist-get 'corrections data)
           collect `((:type . ,(alist-get 'type corr))
                     (:short-description . ,(alist-get 'shortDescription corr))
                     (:long-description . ,(alist-get 'longDescription corr))
                     (:mistake-text . ,(alist-get 'mistakeText corr))
                     (:correction-text . ,(alist-get 'correctionText corr))
                     (:correction-defition . ,(alist-get 'correctionDefinition corr))
                     (:start-index . ,(alist-get 'startIndex corr))
                     (:end-index . ,(1+ (alist-get 'endIndex corr)))
                     (:suggestions
                      . ,(cl-loop for s across (alist-get 'suggestions corr)
                                  collect
                                  `((:text . ,(alist-get 'text s))
                                    (:definition . ,(alist-get 'definition s))
                                    (:category . ,(alist-get 'category s)))))))))
    `((:corrected-text . ,corrected-text)
      (:source-text-hl . ,source-text-hl)
      (:corrections . ,corrections))))

(defun reverso--empty-to-nil (value)
  "If VALUE is nil or empty, return nil, otherwise return VALUE."
  (if (or (null value) (string-empty-p value))
      nil
    value))

(defun reverso--get-conjugation-parse-alternate (data)
  "Parse alternate verb forms in Reverso conjugation page.

DATA is the DOM tree of the page."
  (when-let ((alts (thread-first data
                                 (dom-by-id "ch_divConjugatorHeader")
                                 (dom-by-class "alternate-versions"))))
    `((:model . ,(thread-first alts
                               (dom-by-id (rx bos "ch_lblModel" eos))
                               ;; (dom-by-tag 'a)
                               (dom-texts)
                               (string-trim)
                               (reverso--empty-to-nil)))
      (:auxiliary . ,(thread-first alts
                                   (dom-by-id (rx bos "ch_lblAuxiliary" eos))
                                   (dom-texts)
                                   (string-trim)
                                   (reverso--empty-to-nil)))
      (:other-forms . ,(thread-first alts
                                     (dom-by-id (rx bos "ch_lblAutreForm" eos))
                                     (dom-texts)
                                     (string-trim)
                                     (reverso--empty-to-nil))))))

(defun reverso--get-conjugation-parse-boxes (data)
  "Parse verb conjugation boxes in Reverso conjugation page.

DATA is the DOM tree of the page."
  (when-let ((boxes (thread-first data
                                  (dom-by-class "result-block-api")
                                  (car)
                                  (dom-by-class "blue-box-wrap"))))
    (mapcar
     (lambda (box)
       (let* ((title (dom-attr box 'mobile-title))
              (listing
               (mapcar
                (lambda (item)
                  (let* ((text (string-trim (dom-texts item "")))
                         (verb (thread-first item
                                             (dom-by-class "verb")
                                             (dom-texts)
                                             (string-trim)))
                         (verb-pos (string-match-p (regexp-quote verb) text))
                         (v (dom-attr item 'v)))
                    (when verb-pos
                      (put-text-property
                       verb-pos
                       (+ verb-pos (length verb))
                       'face
                       'reverso-highlight-face
                       text))
                    (cons
                     (replace-regexp-in-string
                      (rx (+ space))
                      " "
                      text)
                     v)))
                (thread-first box
                              (dom-by-class "wrap-verbs-listing")
                              (dom-by-tag 'li))))
              (listing-by-v
               (mapcar (lambda (group)
                         (cons
                          (car group)
                          (mapcar #'car (cdr group))))
                       (seq-group-by #'cdr listing))))
         `((:title . ,title)
           (:listing . ,listing-by-v))))
     boxes)))

(defun reverso--get-conjugation-parse (data)
  "Parse Reverso conjugation page.

DATA is the HTML string of the page.  See `reverso--get-conjugation'
for the return value."
  (let* ((html (with-temp-buffer
                 (insert data)
                 (libxml-parse-html-region (point-min) (point-max))))
         (word (thread-first html
                             (dom-by-id "ch_lblVerb")
                             (dom-texts)
                             (string-trim)))
         (translit-word
          (thread-first html
                        (dom-by-id "ch_lblVerbTranslit")
                        (dom-texts)
                        (string-trim)
                        (reverso--empty-to-nil)))
         (alternate (reverso--get-conjugation-parse-alternate html))
         (boxes (reverso--get-conjugation-parse-boxes html)))
    `((:word . ,word)
      (:translit-word . ,translit-word)
      (:alternate . ,alternate)
      (:boxes . ,boxes))))

(defun reverso--get-conjugation (word language cb)
  "Get conjugation of verb WORD in LANGUAGE.

LANGUAGE is a symbol.  See `reverso--languages' for supported
languages.

CB is a callback function that takes the response as its argument.
The response is an alist with the following keys:
- `:word': the word itself
- `:translit-word': the transliteration of the word, if any
- `:alternate' - alternate forms of the verb.
   - `:model' - a model verb (a verb that cojugates the same way
   - `:auxiliary' - auxiliary verbs (verbs that are used to form
     compound tenses)
   - `:other-forms' - other forms of the verb
- `:boxes' - a list of conjugation boxes
   - `:title' - the title of the box
   - `:listing' - an alist with variant number as the key and a list of
     conjugated forms as the value.  There's usually only one variant."
  (unless (thread-last reverso--languages
                       (alist-get 'conjugation)
                       (alist-get language))
    (request (concat
              (alist-get 'conjugation reverso--urls)
              "conjugation-"
              (symbol-name language)
              "-verb-"
              (url-hexify-string word)
              ".html")
      :type "GET"
      :headers `(("Accept" . "*/*")
                 ("Connection" . "keep-alive")
                 ("User-Agent" . ,reverso--user-agent))
      :parser #'buffer-string
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((res (reverso--alist-remove-empty-values
                              (reverso--get-conjugation-parse data))))
                    (run-hook-with-args
                     'reverso--operation-hook
                     'reverso--get-conjugation res
                     `((:word . ,word)
                       (:language . ,language)))
                    (funcall cb res))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error!: %S" error-thrown))))))

;;; Buffers

(defvar reverso-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (quit-window t)))
    (define-key map (kbd "RET") #'widget-button-press)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal motion) map
        "q" (lambda ()
              (interactive)
              (quit-window t))
        (kbd "RET") #'widget-button-press))
    map)
  "Keymap used in `reverso-result-mode' buffers.")

(defvar-local reverso--input nil
  "Displayed input for `reverso'.")

(defvar-local reverso--output nil
  "Displayed output for `reverso'.")

(defvar-local reverso--data nil
  "Received data for `reverso'.")

(define-derived-mode reverso-result-mode special-mode "Reverso results"
  "Major mode to display results of `reverso'."
  :group 'reverso)

(defun reverso--translate-render (text data)
  "Render the translation results.

DATA is an alist as defined in `reverso--translate'.  TEXT is the
source text."
  (let ((multiline (string-match-p "\n" text)))
    (insert (propertize
             (symbol-name (alist-get :language-from data))
             'face 'reverso-keyword-face)
            " -> "
            (propertize
             (symbol-name (alist-get :language-to data))
             'face 'reverso-keyword-face))
    (when (alist-get :detected-language data)
      (insert " [detected: "
              (propertize
               (symbol-name (alist-get :detected-language data))
               'face 'reverso-keyword-face)
              "]"))
    (insert "\n\n")

    (insert (propertize
             "Source text: "
             'face 'reverso-heading-face))
    (when multiline
      (insert "\n")
      (when (memq (alist-get :language-from data)
                  reverso--right-to-left-languages)
        (insert "\n")))
    (insert text "\n\n")
    (setq-local reverso--input text)

    (when (alist-get :corrected-text data)
      (insert (propertize
               "Corrected text: "
               'face 'reverso-heading-face))
      (when (memq (alist-get :language-from data)
                  reverso--right-to-left-languages)
        (insert "\n"))
      (insert (alist-get :corrected-text data) "\n\n"))

    (if (alist-get :translation data)
        (progn
          (setq-local reverso--output (alist-get :translation data))
          (insert
           (propertize
            "Translation: "
            'face 'reverso-heading-face))
          (when multiline
            (insert "\n")
            (when (memq (alist-get :language-to data)
                        reverso--right-to-left-languages)
              (insert "\n")))
          (insert (alist-get :translation data) "\n\n"))
      (insert "No results!"))
    (when (alist-get :context-results data)
      (insert (propertize
               "Context results: "
               'face 'reverso-heading-face)
              "\n")
      (cl-loop for result in (alist-get :context-results data)
               for translation = (alist-get :translation result)
               when (not (string-empty-p translation))
               do (insert
                   (propertize
                    translation
                    'face 'reverso-highlight-face)
                   "\n")
               do (reverso--context-render-list
                   (alist-get :context result)
                   (alist-get :language-to data)
                   (or (alist-get :detected-language data)
                       (alist-get :language-from data)))))))

(defun reverso--context-render-list (data lang-to lang-from)
  "Render a list of context translation results.

DATA is a list of alists with the following keys:
- `:source': string in the source language (LANG-FROM)
- `:target': string in the target language (LANG-TO)"
  (cl-loop with lang-to-name = (symbol-name lang-to)
           with lang-from-name = (symbol-name lang-from)
           with lang-length = (max (length lang-to-name) (length lang-from-name))
           for datum in data
           for source = (alist-get :source datum)
           for target = (alist-get :target datum)
           do (insert (propertize
                       (format (format "%%-%ds: " lang-length) lang-from-name)
                       'face 'reverso-keyword-face)
                      source "\n"
                      (propertize
                       (format (format "%%%ds: " lang-length) lang-to-name)
                       'face 'reverso-keyword-face)
                      target "\n\n")))

(defun reverso--translate-render-brief (text data)
  "Render the translation results in brief format.

DATA is an alist as defined in `reverso--translate'.  TEXT is the
source text."
  (setq-local reverso--input text)
  (if (alist-get :translation data)
      (progn
        (setq-local reverso--output (alist-get :translation data))
        (insert (alist-get :translation data)))
    (insert "No results!")))

(defun reverso--context-render (input data lang-to lang-from)
  "Render context translation results.

INPUT is the input string.  DATA is a list as defined in
`reverso--get-context'.  LANG-TO and LANG-FROM and the target and
source languages."
  (setq-local reverso--input input)
  (insert (propertize
           "Context results: "
           'face 'reverso-heading-face)
          "\n")
  (reverso--context-render-list data lang-to lang-from))

(defun reverso--synonyms-render (input data)
  "Render synonym search results.

INPUT is the input string.  DATA is a list as defined in
`reverso--get-synonyms'."
  (setq-local reverso--input input)
  (dolist (datum data)
    (when (alist-get :kind datum)
      (insert (propertize
               "Part of speech: "
               'face 'reverso-keyword-face)
              (alist-get :kind datum)
              "\n"))
    (when (alist-get :synonyms datum)
      (insert (propertize
               "Synonyms: "
               'face 'reverso-heading-face)
              "\n")
      (dolist (synonym (alist-get :synonyms datum))
        (when (alist-get :relevant synonym)
          (insert "- " (alist-get :synonym synonym) "\n")))
      (insert "\n"))
    (when (alist-get :examples datum)
      (insert (propertize
               "Examples: "
               'face 'reverso-heading-face)
              "\n")
      (dolist (example (alist-get :examples datum))
        (insert "- " example "\n"))
      (insert "\n"))
    (when (alist-get :antonyms datum)
      (insert (propertize
               "Antonyms: "
               'face 'reverso-heading-face)
              "\n")
      (dolist (antonym (alist-get :antonyms datum))
        (insert "- " (alist-get :antonym antonym) "\n"))
      (insert "\n"))))

(defun reverso--grammar-render (data)
  "Render grammar checking results.

DATA is an alist as defined in `reverso--get-grammar'."
  (insert
   (propertize "Source text: " 'face 'reverso-heading-face)
   "\n"
   (alist-get :source-text-hl data)
   "\n\n"
   (propertize "Corrected text: " 'face 'reverso-heading-face)
   "\n"
   (alist-get :corrected-text data)
   "\n\n")
  (when (alist-get :corrections data)
    (insert
     (propertize "Corrections: " 'face 'reverso-heading-face)
     "\n")
    (dolist (corr (alist-get :corrections data))
      (reverso--grammar-render-error corr))))

(defun reverso--grammar-render-error (corr)
  "Render one error from grammar checking results.

CORR is one element of the `:corrections' list, as defined in
`reverso--get-grammar'."
  (insert
   (propertize (or (alist-get :type corr) "")
               'face 'reverso-keyword-face))
  (when (alist-get :short-description corr)
    (insert ": " (alist-get :short-description corr)))
  (insert "\n")
  (when (alist-get :long-description corr)
    (insert (alist-get :long-description corr))
    (insert "\n"))
  (insert "\n")
  (insert
   (propertize
    (or (alist-get :mistake-text corr) "")
    'face 'reverso-highlight-face)
   " -> "
   (propertize
    (or (alist-get :correction-text corr) "")
    'face 'reverso-highlight-face))
  (when (alist-get :correction-defition corr)
    (insert " [" (propertize
                  (alist-get :correction-defition corr)
                  'face 'reverso-definition-face)
            "]"))
  (insert "\n\n")
  (when (alist-get :suggestions corr)
    (insert "Suggestions:\n")
    (dolist (sg (alist-get :suggestions corr))
      (insert "- ")
      (when (alist-get :category sg)
        (insert (alist-get :category sg) ": "))
      (insert (propertize
               (alist-get :text sg)
               'face 'reverso-highlight-face))
      (when (alist-get :definition sg)
        (insert
         " ["
         (propertize
          (alist-get :definition sg)
          'face 'reverso-definition-face)
         "]"))
      (insert "\n"))
    (insert "\n")))

(defun reverso--conjugation-render (input data)
  "Render conjugation results.

DATA is a form as described in `reverso--get-conjugation'.  INPUT is
the input string."
  (setq-local reverso--input input)
  (insert (propertize "Word: " 'face 'reverso-heading-face)
          (alist-get :word data)
          "\n\n")
  (let ((alt (alist-get :alternate data)))
    (when (or (alist-get :model alt)
              (alist-get :auxiliary alt)
              (alist-get :other-forms alt))
      (insert (propertize "Alternate forms: " 'face 'reverso-heading-face)
              "\n")
      (when-let ((model (alist-get :model alt)))
        (insert (propertize "Model" 'face 'reverso-keyword-face)
                ": "
                model "\n"))
      (when-let ((auxiliary (alist-get :auxiliary alt)))
        (insert (propertize "Auxiliary" 'face 'reverso-keyword-face)
                ": " auxiliary "\n"))
      (when-let ((other-forms (alist-get :other-forms alt)))
        (insert (propertize "Other forms" 'face 'reverso-keyword-face)
                ": " other-forms "\n"))
      (insert "\n")))
  (when-let ((boxes (alist-get :boxes data)))
    ;; (insert (propertize "Conjugation: " 'face 'reverso-heading-face)
    ;;         "\n")
    (dolist (box boxes)
      (insert (propertize (alist-get :title box)
                          'face 'reverso-heading-face)
              "\n")
      (dolist (variant (alist-get :listing box))
        (when (car variant)
          (insert (propertize (concat (alist-get :title box) " (" (car variant) ")")
                              'face 'reverso-heading-face)
                  "\n"))
        (dolist (line (cdr variant))
          (insert "- " line "\n"))
        (insert "\n")))))

(defmacro reverso--with-buffer (&rest body)
  "Execute BODY in a clean `reverso' results buffer."
  (declare (indent 0))
  `(progn
     (let ((buffer (get-buffer-create
                    (generate-new-buffer-name "*Reverso*"))))
       (with-current-buffer buffer
         (unless (derived-mode-p 'reverso-result-mode)
           (reverso-result-mode))
         (let ((inhibit-read-only t))
           (erase-buffer)
           ,@body)
         (goto-char (point-min)))
       (switch-to-buffer-other-window buffer))))

;;; In-buffer correction
(defun reverso-check-clear (&optional region-start region-end)
  "Remove reverso grammar check overlays.

If a region is active, remove overlays only in that region.

REGION-START and REGION-END are borders of the region."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (dolist (ov (overlays-in region-start region-end))
    (when (overlay-get ov 'reverso-correction)
      (delete-overlay ov))))

(defun reverso--check-make-overlays (region-start region-end data)
  "Put reverso grammar check overlays into region.

REGION-START and REGION-END and borders of the region.  DATA is an
alist as defined in `reverso--get-grammar'."
  (reverso-check-clear region-start region-end)
  (dolist (corr (alist-get :corrections data))
    (let* ((start (+ region-start (alist-get :start-index corr)))
           (end (+ region-start (alist-get :end-index corr)))
           (ov (make-overlay start end)))
      (overlay-put ov 'reverso-correction corr)
      (overlay-put ov 'priority 1)
      (overlay-put ov 'help-echo
                   (concat
                    (alist-get :type corr)
                    ". " (alist-get :short-description corr)
                    (when (alist-get :long-description corr)
                      (concat ". " (alist-get :long-description corr)))))
      (overlay-put ov 'face 'reverso-error-face))))

(defun reverso-check-next-error ()
  "Jump to next reverso grammar check error."
  (interactive)
  (let ((ov (cl-loop with point = (1+ (point))
                     with point-max = (point-max)
                     with ov = nil
                     while (and (not (= point point-max))
                                (not ov))
                     do (setq ov
                              (cl-loop for ov-cand in (overlays-at point)
                                       if (and (overlay-get ov-cand 'reverso-correction)
                                               (= point (overlay-start ov-cand)))
                                       return ov-cand)
                              point (next-overlay-change point))
                     if ov return ov)))
    (if ov
        (goto-char (overlay-start ov))
      (message "No errors left!"))))

(defun reverso-check-prev-error ()
  "Jump to previous reverso grammar check error."
  (interactive)
  (let ((ov (cl-loop with point = (1- (point))
                     with point-prev = (1+ (point-max))
                     with ov = nil
                     while (and (not (= point point-prev))
                                (not ov))
                     do (setq ov
                              (cl-loop for ov-cand in (overlays-at point)
                                       if (and (overlay-get ov-cand 'reverso-correction)
                                               (= point (overlay-start ov-cand)))
                                       return ov-cand)
                              point-prev point
                              point (previous-overlay-change point))
                     if ov return ov)))
    (if ov
        (goto-char (overlay-start ov))
      (message "No errors left!"))))

(defun reverso-check-first-error ()
  "Jump to the first reverso grammar check error."
  (interactive)
  (goto-char
   (save-excursion
     (goto-char (point-min))
     (if (reverso--get-error-at-point)
         (point)
       (reverso-check-next-error)
       (if (not (bobp))
           (point)
         (user-error "No errors left!"))))))

(defun reverso-check-last-error ()
  "Jump to the last reverso grammar check error."
  (interactive)
  (goto-char
   (save-excursion
     (goto-char (point-max))
     (if (reverso--get-error-at-point)
         (point)
       (reverso-check-prev-error)
       (if (not (eobp))
           (point)
         (user-error "No errors left!"))))))

(defun reverso--get-error-at-point ()
  "Return reverso error at point."
  (cl-loop for ov in (overlays-at (point))
           if (overlay-get ov 'reverso-correction)
           return (cons ov (overlay-get ov 'reverso-correction))))

(defun reverso-check-ignore-error ()
  "Remove error at point."
  (interactive)
  (let ((err (reverso--get-error-at-point)))
    (unless err
      (user-error "No error at point!"))
    (delete-overlay (car err))
    (when transient-current-command
      (reverso-check-next-error))))

(defun reverso-describe-error-at-point ()
  "Describe reverso error at point."
  (interactive)
  (let ((err (reverso--get-error-at-point)))
    (unless err
      (user-error "No error at point!"))
    (reverso--with-buffer
      (reverso--grammar-render-error (cdr err)))))

(defun reverso-check-fix-at-point ()
  "Fix reverso error at point."
  (interactive)
  (let ((err (reverso--get-error-at-point)))
    (unless err
      (user-error "No error at point!"))
    (unless (alist-get :suggestions (cdr err))
      (user-error "No suggestions!"))
    (let ((correction
           (completing-read
            "Fix: "
            (mapcar
             (lambda (sugg)
               (alist-get :text sugg))
             (alist-get :suggestions (cdr err)))
            nil t))
          (start (overlay-start (car err)))
          (end (overlay-end (car err))))
      (delete-overlay (car err))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert correction)))))

(defun reverso-check-buffer (language region-start region-end &optional string-join)
  "Check for grammar errors in buffer.

If a region is active, restrict the action to that region.

LANGUAGE is a language from the `reverso--languages' list.
REGION-START and REGION-END are the borders of the region.

If STRING-JOIN is non-nil, remove linebreaks from the string."
  (interactive (append
                (list (intern
                       (completing-read
                        "Language: "
                        (seq-intersection
                         reverso-languages
                         (alist-get 'grammar reverso--languages))
                        nil t)))
                (if (use-region-p)
                    (list (region-beginning) (region-end))
                  (list (point-min) (point-max)))))
  (let ((string (buffer-substring-no-properties region-start region-end)))
    (when string-join
      (setq string (replace-regexp-in-string "\n" " " string)))
    (reverso--get-grammar
     string language
     (lambda (data)
       (reverso--check-make-overlays region-start region-end data)
       (message "Check complete!")))))

;;; History
(defvar reverso--history nil
  "History of the last queries.

Each item is a list as described in `reverso--operation-hook', plus
the timestamp as the fourth element.")

(defcustom reverso-history-size 200
  "Maximum number of items in `reverso--history'."
  :type 'integer
  :group 'reverso)

(defcustom reverso-timestamp-format "%Y-%m-%d %H:%M:%S"
  "Format string for timestamps in `reverso--history'."
  :type 'string
  :group 'reverso)

(defun reverso--history-add (op data params)
  "Add an item to `reverso--history'.

OP, DATA and PARAMS are as described in `reverso--operation-hook'."
  (push (list op data params (time-convert nil 'integer)) reverso--history)
  (when (> (length reverso--history) reverso-history-size)
    (setq reverso--history (butlast reverso--history))))

;;;###autoload
(define-minor-mode reverso-history-mode
  "Minor mode for recording history of reverso queries."
  :lighter " Reverso History"
  :global t
  (if reverso-history-mode
      (add-hook 'reverso--operation-hook #'reverso--history-add)
    (remove-hook 'reverso--operation-hook #'reverso--history-add)))

(defun reverso--history-format-item (item)
  "Format a history ITEM for display."
  (apply
   #'format
   "%-20s %-20s %s"
   (format-time-string
    reverso-timestamp-format
    (seconds-to-time (nth 3 item)))
   (pcase (nth 0 item)
     ('reverso--translate
      (list
       "Translate"
       (format "%s -> %s: %s -> %s"
               (propertize
                (symbol-name (alist-get :language-from (nth 1 item)))
                'face 'reverso-keyword-face)
               (propertize
                (symbol-name (alist-get :language-to (nth 1 item)))
                'face
                'reverso-keyword-face)
               (alist-get :text (nth 2 item))
               (alist-get :translation (nth 1 item)))))
     ('reverso--get-grammar
      (list
       "Grammar check"
       (format "%s: %s -> %s"
               (propertize
                (symbol-name (alist-get :language (nth 2 item)))
                'face 'reverso-keyword-face)
               (alist-get :source-text-hl (nth 1 item))
               (alist-get :corrected-text (nth 1 item)))))
     ('reverso--get-context
      (list
       "Context"
       (format "%s -> %s: %s (%d results)"
               (propertize
                (symbol-name (alist-get :source (nth 2 item)))
                'face 'reverso-keyword-face)
               (propertize
                (symbol-name (alist-get :target (nth 2 item)))
                'face
                'reverso-keyword-face)
               (alist-get :text (nth 2 item))
               (length (nth 1 item)))))
     ('reverso--get-synonyms
      (list
       "Synonyms"
       (format
        "%s: %s (%d results)"
        (propertize
         (symbol-name (alist-get :language (nth 2 item)))
         'face 'reverso-keyword-face)
        (alist-get :text (nth 2 item))
        (cl-reduce (lambda (acc x)
                     (+ acc (length (alist-get :synonyms x))))
                   (nth 1 item)
                   :initial-value 0))))
     ('reverso--get-conjugation
      (list
       "Conjugation"
       (format
        "%s: %s"
        (propertize
         (symbol-name (alist-get :language (nth 2 item)))
         'face 'reverso-keyword-face)
        (alist-get :word (nth 2 item))))))))

(defun reverso--history-display (widget &rest _)
  "Action for displaying a history item in a WIDGET."
  (let ((item (widget-get widget :item)))
    (reverso--with-buffer
      (setq-local reverso--data (nth 1 item))
      (pcase (nth 0 item)
        ('reverso--translate
         (reverso--translate-render
          (alist-get :text (nth 2 item))
          (nth 1 item)))
        ('reverso--get-grammar
         (reverso--grammar-render (nth 1 item)))
        ('reverso--get-context
         (reverso--context-render
          (alist-get :text (nth 2 item))
          (nth 1 item)
          (alist-get :source (nth 2 item))
          (alist-get :target (nth 2 item))))
        ('reverso--get-synonyms
         (reverso--synonyms-render
          (alist-get :text (nth 2 item))
          (nth 1 item)))
        ('reverso--get-conjugation
         (reverso--conjugation-render
          (alist-get :word (nth 2 item))
          (nth 1 item)))))))

(defun reverso-history ()
  "Display the history of reverso queries.

Enable `reverso-history-mode' to record history."
  (interactive)
  (when (and (seq-empty-p reverso--history)
             (not reverso-history-mode))
    (user-error "Enable `reverso-history-mode' to record history"))
  (reverso--with-buffer
    (insert
     (format
      "%-20s %-20s %s\n"
      (propertize
       "Timestamp" 'face 'reverso-heading-face)
      (propertize
       "Operation" 'face 'reverso-heading-face)
      (propertize
       "Parameters" 'face 'reverso-heading-face)))
    (setq-local widget-push-button-prefix "")
    (setq-local widget-push-button-suffix "")
    (dolist (item reverso--history)
      (widget-create 'push-button
                     :notify #'reverso--history-display
                     :item item
                     :button-face 'reverso-history-item-face
                     (reverso--history-format-item item))
      (insert "\n"))))

;;; Transient

(defclass reverso--transient-input (transient-infix)
  ((format :initform " %k %d: %v"))
  "Class used for retrieving the input string.")

(defvar reverso--use-buffer-as-input nil
  "Whether to use the current buffer as input.

That parameter is normally set by the prefix argument, but as it
doesn't persist between parent and child invocations, this variable is
used instead.")

(cl-defmethod transient-init-value ((obj reverso--transient-input))
  "Initialize the value of `reverso--transient-input'.

OBJ is an instance of the class."
  (oset obj value
        (cond
         ((and (slot-boundp obj 'value) (oref obj value))
          (oref obj value))
         ((region-active-p)
          (buffer-substring-no-properties (region-beginning) (region-end)))
         ((or (equal current-prefix-arg '(4))
              reverso--use-buffer-as-input)
          (if reverso--output
              reverso--output
            (buffer-substring-no-properties (point-min) (point-max))))
         (reverso--input reverso--input)
         (t ""))))

(cl-defmethod transient-infix-read ((obj reverso--transient-input))
  "Read input string from the minibuffer.

OBJ is an instance of the `reverso--transient-input'."
  (read-from-minibuffer "Input: " (oref obj value)))

(cl-defmethod transient-format-value ((obj reverso--transient-input))
  "Format the input string for display in the transient buffer.

OBJ is an instance of `reverso--transient-input'."
  (propertize
   (with-temp-buffer
     (insert (oref obj value))
     (goto-char (point-min))
     (let ((lines (count-lines (point-min) (point-max))))
       (cond
        ((= (buffer-size) 0) "(empty)")
        ((= lines 1) (buffer-string))
        ((> lines reverso-max-display-lines-in-input)
         (concat
          "\n"
          (buffer-substring
           (point-min)
           (save-excursion
             (goto-char (point-min))
             (forward-line (1- reverso-max-display-lines-in-input))
             (point)))
          "\n... (truncated "
          (number-to-string
           (- lines reverso-max-display-lines-in-input))
          " more lines)"))
        (t (concat "\n" (buffer-string))))))
   'face 'transient-value))

(defclass reverso--transient-language (transient-infix)
  ((format :initform " %k %d %v")
   (languages :initarg :languages :initform nil)
   (target-languages :initarg :target-languages :initform nil)
   (is-target :initarg :is-target :initform nil))
  "Class used for switching the language.")

(defvar reverso--source-value nil
  "Selected source language in `reverso'.

Used for persistence between invocations of transient buffers.")

(defvar reverso--target-value nil
  "Selected target language in `reverso'.

Used for persistence between invocations of transient buffers.")

(defvar reverso--prefer-brief nil
  "If non-nil, select brief translation display by default.

Used for persistence between invocations of transient buffers.")

(defun reverso--get-available-languages (obj)
  "Get available languages for the operation.

OBJ is an instance of transient infix.  It needs to have the following
slots:
- `languages': list of available languages
- `is-target': if OBJ selects a target language
- `target-languages': alist, where the key is a source language, and
  the value is a list of available target languages for that source
  language.

If `is-target' is non-nil, then selection of languages has to be
limited.  In that case, `reverso--source-value' is used to get the
source language."
  (let* ((all-languages
          (seq-sort
           (lambda (a b) (string-lessp (symbol-name a)
                                       (symbol-name b)))
           (or (oref obj languages)
               (mapcar #'car reverso--language-mapping))))
         (is-target (oref obj is-target))
         (source-language (when is-target
                            (or reverso--source-value
                                (car all-languages))))
         (target-languages (when (and is-target (oref obj target-languages))
                             (alist-get source-language (oref obj target-languages))))
         (languages (cl-loop for lang in all-languages
                             if (or (not source-language)
                                    (not (eq source-language lang)))
                             collect lang)))
    (seq-sort
     (lambda (a b) (string-lessp (symbol-name a)
                                 (symbol-name b)))
     (let ((intersection-1 (seq-intersection languages reverso-languages)))
       (if (not target-languages)
           intersection-1
         (seq-intersection intersection-1 target-languages))))))

(defun reverso--get-language-variable (obj)
  "Get the name of the variable that stores the selected language.

OBJ is an instance of a transient infix, that has to have a slot
called `is-target'."
  (if (oref obj is-target)
      'reverso--target-value
    'reverso--source-value))

(cl-defmethod transient-init-value ((obj reverso--transient-language))
  "Initialize the value of the language picker.

OBJ is an instance of `reverso--transient-language'."
  (let ((value
         (cond
          ((and (slot-boundp obj 'value) (oref obj value))
           (oref obj value))
          ((and (symbol-value (reverso--get-language-variable obj)))
           (symbol-value (reverso--get-language-variable obj)))
          (t (car (reverso--get-available-languages obj))))))
    (set (reverso--get-language-variable obj) value)
    (oset obj value value)))

(cl-defmethod transient-format-value ((obj reverso--transient-language))
  "Format the value of the language picker.

OBJ is an instance of `reverso--transient-language'."
  (let ((value (transient-infix-value obj)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat
      (lambda (choice)
        (propertize (symbol-name choice) 'face
                    (if (eq choice value)
                        'transient-value
                      'transient-inactive-value)))
      (reverso--get-available-languages obj)
      (propertize "|" 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-read ((obj reverso--transient-language))
  "Pick a value in the language picker.

OBJ is an instance of `reverso--transient-language'."
  (let* ((choices (reverso--get-available-languages obj))
         (current-idx (or (cl-position (transient-infix-value obj) choices) -1))
         (next-idx (% (1+ current-idx) (length choices)))
         (next-choice
          (if (> (length choices) reverso-language-completing-read-threshold)
              (let ((lang (intern
                           (completing-read
                            "Language: "
                            (mapcar #'symbol-name choices) nil t))))
                (unless (member lang choices)
                  (user-error "Bad language: %s" lang))
                lang)
            (nth next-idx choices))))
    (set (reverso--get-language-variable obj) next-choice)
    next-choice))

(cl-defmethod transient-infix-value ((obj reverso--transient-language))
  "Get the current value of the language picker.

OBJ is an instance of `reverso--transient-language'."
  (let* ((choices (reverso--get-available-languages obj))
         (current-idx (or (cl-position (oref obj value) choices) -1)))
    (nth current-idx choices)))

(defclass reverso--transient-brief (transient-switch) ()
  "Toggle brief output.")

(cl-defmethod transient-init-value ((obj reverso--transient-brief))
  "Initialize the value of the brief output switcher.

OBJ is an instance of `reverso--transient-brief'."
  (oset obj value reverso--prefer-brief))

(cl-defmethod transient-infix-read ((obj reverso--transient-brief))
  "Toggle the switch on or off.

OBJ is an instance of `reverso--transient-brief'."
  (setq reverso--prefer-brief
        (null (oref obj value))))

(transient-define-infix reverso--transient-input-infix ()
  :class 'reverso--transient-input
  :description "Input")

(transient-define-infix reverso--transient-translate-language-source ()
  :class 'reverso--transient-language
  :description "Source language"
  :key "s"
  ;; XXX not sure why `:argument' is necessary :(
  :argument "-s"
  :languages (alist-get 'translation reverso--languages))

(transient-define-infix reverso--transient-translate-language-target ()
  :class 'reverso--transient-language
  :description "Target language"
  :key "t"
  :argument "-t"
  :languages (alist-get 'translation reverso--languages)
  :target-languages (alist-get 'translation reverso--languages-compatible)
  :is-target t)

(transient-define-suffix reverso--transient-swap-languages ()
  :transient t
  :key "S"
  :description "Swap languages"
  (interactive)
  (let* ((suffixes (transient-suffixes transient-current-command))
         (source (seq-find (lambda (suffix)
                             (and (reverso--transient-language-p suffix)
                                  (not (oref suffix is-target))))
                           suffixes))
         (target (seq-find (lambda (suffix)
                             (and (reverso--transient-language-p suffix)
                                  (oref suffix is-target)))
                           suffixes))
         (source-value (transient-infix-value source))
         (target-value (transient-infix-value target)))
    (oset source value target-value)
    (oset target value source-value)
    (setq reverso--source-value target-value)
    (setq reverso--target-value source-value)))

(transient-define-infix reverso--transient-brief-infix ()
  :transient t
  :class 'reverso--transient-brief
  :key "b"
  :argument "--brief"
  :description "Brief translation output")

(transient-define-suffix reverso--translate-exec-suffix (input source target &optional is-brief)
  :key "e"
  :description "Translate"
  (interactive (transient-args transient-current-command))
  (reverso--translate
   input source target
   (lambda (data)
     (reverso--with-buffer
       (if is-brief
           (reverso--translate-render-brief input data)
         (reverso--translate-render input data))))))

;;;###autoload (autoload 'reverso-translate "reverso" nil t)
(transient-define-prefix reverso-translate ()
  "Translate text.

In normal buffers, if launched with a region selected, use that
region.  Otherwise, if launched with \\[universal-argument], use the
current buffer as input.

In launched in a `reverso-result-mode' buffer, use the current input
string as input.  If launched there with \\[universal-argument] and
the reverso buffer has an output string, use that output string as
input."
  ["Input"
   ("i" "Input" reverso--transient-input-infix)]
  ["Parameters"
   (reverso--transient-translate-language-source)
   (reverso--transient-translate-language-target)
   (reverso--transient-swap-languages)
   (reverso--transient-brief-infix)]
  ["Actions"
   (reverso--translate-exec-suffix)
   ("q" "Quit" transient-quit-one)])

(transient-define-infix reverso--transient-context-language-source ()
  :class 'reverso--transient-language
  :description "Source language"
  :key "s"
  :argument "-s"
  :languages (alist-get 'context reverso--languages))

(transient-define-infix reverso--transient-context-language-target ()
  :class 'reverso--transient-language
  :description "Target language"
  :key "t"
  :argument "-t"
  :languages (alist-get 'translation reverso--languages)
  :target-languages (alist-get 'context reverso--languages-compatible)
  :is-target t)

(transient-define-suffix reverso--context-exec-suffix (input source target)
  :key "e"
  :description "Find context"
  (interactive (transient-args transient-current-command))
  (reverso--get-context
   input source target
   (lambda (data)
     (reverso--with-buffer
       (reverso--context-render input data source target)
       (setq-local reverso--data data)))))

;;;###autoload (autoload 'reverso-context "reverso" nil t)
(transient-define-prefix reverso-context ()
  "Find bilingual concordances for text.

A bilingual concordance is a pair of strings of the same text in
different languages.  This works well for a comparatively short
inputs."
  ["Input"
   ("i" "Input" reverso--transient-input-infix)]
  ["Parameters"
   (reverso--transient-context-language-source)
   (reverso--transient-context-language-target)
   (reverso--transient-swap-languages)]
  ["Actions"
   (reverso--context-exec-suffix)
   ("q" "Quit" transient-quit-one)])

(transient-define-infix reverso--transient-synonyms-language ()
  :class 'reverso--transient-language
  :description "Language"
  :key "s"
  :argument "-s"
  :languages (alist-get 'synonyms reverso--languages))

(transient-define-suffix reverso--synonyms-exec-suffix (input source)
  :key "e"
  :description "Find synonyms"
  (interactive (transient-args transient-current-command))
  (reverso--get-synonyms
   input source
   (lambda (data)
     (reverso--with-buffer
       (reverso--synonyms-render input data)
       (setq-local reverso--data data)))))

;;;###autoload (autoload 'reverso-synonyms "reverso" nil t)
(transient-define-prefix reverso-synonyms ()
  "Find synonyms."
  ["Input"
   ("i" "Input" reverso--transient-input-infix)]
  ["Parameters"
   (reverso--transient-synonyms-language)]
  ["Actions"
   (reverso--synonyms-exec-suffix)
   ("q" "Quit" transient-quit-one)])

(transient-define-infix reverso--transient-conjugation-language ()
  :class 'reverso--transient-language
  :description "Language"
  :key "s"
  :argument "-s"
  :languages (alist-get 'conjugation reverso--languages))

(transient-define-suffix reverso--conjugation-exec-suffix (word language)
  :key "e"
  :description "Find conjugations"
  (interactive (transient-args transient-current-command))
  (reverso--get-conjugation
   word language
   (lambda (data)
     (reverso--with-buffer
       (reverso--conjugation-render word data)
       (setq-local reverso--data data)))))

;;;###autoload (autoload 'reverso-conjugation "reverso" nil t)
(transient-define-prefix reverso-conjugation ()
  "Find conjugations."
  ["Input"
   ("i" "Input" reverso--transient-input-infix)]
  ["Parameters"
   (reverso--transient-conjugation-language)]
  ["Actions"
   (reverso--conjugation-exec-suffix)
   ("q" "Quit" transient-quit-one)])

(transient-define-infix reverso--transient-grammar-language ()
  :class 'reverso--transient-language
  :description "Language"
  :key "s"
  :argument "-s"
  :languages (alist-get 'grammar reverso--languages))

(transient-define-suffix reverso--grammar-exec-suffix (input source)
  :key "e"
  :description "Check grammar"
  (interactive (transient-args transient-current-command))
  (reverso--get-grammar
   input source
   (lambda (data)
     (reverso--with-buffer
       (reverso--grammar-render data)
       (setq-local reverso--data data)))))

;;;###autoload (autoload 'reverso-grammar "reverso" nil t)
(transient-define-prefix reverso-grammar ()
  "Check grammar."
  ["Input"
   ("i" "Input" reverso--transient-input-infix)]
  ["Parameters"
   (reverso--transient-grammar-language)]
  ["Actions"
   (reverso--grammar-exec-suffix)
   ("q" "Quit" transient-quit-one)])

(defclass reverso--transient-curent-error (transient-suffix)
  ((transient :initform t))
  "A class to display reverso error point.")

(cl-defmethod transient-init-value ((_ reverso--transient-curent-error))
  "A dummy method for `reverso--transient-curent-error'.

The class doesn't actually have any value, but this is necessary for transient."
  nil)

(defvar reverso--current-grammar-check-buffer nil)

(cl-defmethod transient-format ((_ reverso--transient-curent-error))
  "Format reverso error at point."
  (when reverso--current-grammar-check-buffer
    (if-let ((err
              (with-current-buffer reverso--current-grammar-check-buffer
                (reverso--get-error-at-point))))
        (with-temp-buffer
          (reverso--grammar-render-error (cdr err))
          (string-trim (buffer-string)))
      "No error at point")))

(transient-define-infix reverso--transient-current-error-infix ()
  :class 'reverso--transient-curent-error
  ;; A dummy key. Seems to be necessary for transient.
  ;; Just don't press ~ while in the buffer.
  :key "~~1")

(transient-define-suffix reverso--grammar-buffer-exec-suffix
  (language region-start region-end string-join)
  :key "e"
  :description "Check grammar"
  (interactive (append
                (list (car (transient-args transient-current-command)))
                (if (use-region-p)
                    (list (region-beginning) (region-end))
                  (list (point-min) (point-max)))
                (list (cadr (transient-args transient-current-command)))))
  (when (use-region-p)
    (deactivate-mark))
  (reverso-check-buffer language region-start region-end string-join))

(defun reverso--check-fix-at-point-transient ()
  "Fix error at point, switch to next error and return to transient."
  (interactive)
  (condition-case err
      (progn
        (reverso-check-fix-at-point)
        (reverso-check-next-error))
    (error (message "Error: %s" err)))
  (call-interactively #'reverso-grammar-buffer))

(transient-define-infix reverso--transient-join-string ()
  :class 'transient-switch
  :description "Remove linebreaks"
  :key "l"
  :argument "--remove-line-breaks")

;;;###autoload (autoload 'reverso-grammar-buffer "reverso" nil t)
(transient-define-prefix reverso-grammar-buffer ()
  "Check grammar current in buffer.

If launched with a region selected, restrict the operation to that
region.  Otherwise, use the entire buffer."
  ["Information"
   (reverso--transient-current-error-infix)]
  ["Parameters"
   (reverso--transient-grammar-language)
   (reverso--transient-join-string)]
  ["Errors"
   :class transient-row
   ("f" "Fix error" reverso--check-fix-at-point-transient)
   ("i" "Ignore error" reverso-check-ignore-error :transient t)
   ("p" "Previous error" reverso-check-prev-error :transient t)
   ("P" "First error" reverso-check-first-error :transient t)
   ("n" "Next error" reverso-check-next-error :transient t)
   ("N" "Last error" reverso-check-last-error :transient t)]
  ["Actions"
   :class transient-row
   ("e" "Check grammar" reverso--grammar-buffer-exec-suffix :transient t)
   ("c" "Clear" reverso-check-clear :transient t)
   ("q" "Quit" transient-quit-one)]
  (interactive)
  (setq reverso--current-grammar-check-buffer (current-buffer))
  (transient-setup 'reverso-grammar-buffer))

;;;###autoload (autoload 'reverso "reverso" nil t)
(transient-define-prefix reverso ()
  "Reverso translation service.

The following features are implemented as nested transient buffers:
- `reverso-translate': translation
- `reverso-context': context (bilingual concordances)
- `reverso-synonyms': synonyms
- `reverso-grammar': grammar check
- `reverso-grammar-buffer': grammar check in buffer"
  ["Commands"
   ("t" "Translation" reverso-translate)
   ("c" "Context" reverso-context)
   ("s" "Synonyms" reverso-synonyms)
   ("o" "Conjugation" reverso-conjugation)
   ("g" "Grammar check" reverso-grammar)
   ("b" "Grammar check in buffer" reverso-grammar-buffer)]
  ["Other"
   ("h" "History" reverso-history)]
  ["Actions"
   ("q" "Quit" transient-quit-one)]
  (interactive)
  (setq-local reverso--use-buffer-as-input (equal current-prefix-arg '(4)))
  (transient-setup 'reverso))

(provide 'reverso)

;;; reverso.el ends here
