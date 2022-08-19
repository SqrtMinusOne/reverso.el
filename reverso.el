;;; reverso.el --- Client for the https://reverso.net translation service -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.7") (request "0.3.2"))
;; Homepage: https://github.com/SqrtMinusOne/reverso.el

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

;; TODO

;;; Code:
(require 'request)
(require 'transient)
(require 'url-util)

(defgroup reverso nil
  "Client for the https://reverso.net translation service."
  :group 'applications)

(defface reverso-highlight-face
  '((t (:inherit bold)))
  "Face for highlighting selected words in translation."
  :group 'reverso)

(defface reverso-error-face
  '((t (:inherit error)))
  "Face for highlighting errors in grammar check."
  :group 'reverso)

(defconst reverso--languages
  '((translation . (arabic chinese dutch english french german hebrew
                           italian japanese korean polish portugese
                           romanian russian spanish swedish turkish
                           ukrainian))
    (context . (arabic german english spanish french hebrew italian
                       japanese dutch polish portugese romanian
                       russian swedish turkish ukrainian chinese))
    (grammar . (english french))
    (synonyms . (arabic german english spanish french hebrew italian
                        japanese dutch polish portugese romanian
                        russian)))
  "Available languages for diferent operations.")

(defconst reverso--language-mapping
  '((arabic . ara)
    (german . ger)
    (spanish . spa)
    (french . fra)
    (hebrew . heb)
    (italian . ita)
    (japanese . jpn)
    (dutch . dut)
    (polish . pol)
    (portuguese . por)
    (romanian . rum)
    (russian . rus)
    (ukrainian . ukr)
    (turkish . tur)
    (chinese . chi)
    (english . eng))
  "Mapping from long language names to short ones.

This one is used for the translation queries.")

(defconst reverso--language-mapping-1
  '((arabic . ar)
    (german . de)
    (english . en)
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
  "Mapping from long language names to short ones.

This one is used for the synonyms queries.")

(defconst reverso--languages-compatible
  `((context
     . ((english . (arabic german spanish french hebrew italian
                           japanese dutch polish portuguese romanian
                           russian turkish chinese))
        (arabic . (english german spanish french hebrew italian
                           portuguese russian turkish))
        (german . (arabic english spanish french hebrew italian
                          japanese dutch polish portuguese romanian
                          russian turkish))
        (spanish . (arabic german english french hebrew italian
                           japanese dutch polish portuguese romanian
                           russian turkish chinese))
        (french . (arabic german spanish english hebrew italian
                          japanese dutch polish portuguese romanian
                          russian turkish chinese))
        (hebrew . (arabic german spanish french english italian dutch
                          portuguese russian))
        (italian . (arabic german spanish french hebrew english
                           japanese dutch polish portuguese romanian
                           russian turkish chinese))
        (japanese . (german spanish french italian english portuguese
                            russian))
        (dutch . (german spanish french hebrew italian english
                         portuguese russian))
        (polish . (german spanish french italian english))
        (portuguese . (arabic german spanish french hebrew italian
                              japanese dutch english russian turkish))
        (romanian . (german spanish french italian english turkish))
        (russian . (arabic german spanish french hebrew italian
                           japanese dutch portuguese english))
        (turkish . (arabic german spanish french italian portuguese
                           romanian english))
        (chinese . (english french spanish)))))
  "Which languages are compatible with which.")

(defconst reverso--urls
  '((translation . "https://api.reverso.net/translate/v1/translation")
    (context . "https://context.reverso.net/translation/")
    (grammar . "https://orthographe.reverso.net/api/v1/Spelling")
    (synomyms . "https://synonyms.reverso.net/synonym/"))
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
  "User-Agent to use for reverse.el requests.")

(defun reverso--translate (text source target cb)
  "Translate TEXT from language SOURCE to TARGET.

SOURCE and TARGET are keys of `reverso--languages'.  CB is a function
that is called with the result."
  (unless (and (alist-get source reverso--language-mapping)
               (member source
                       (alist-get 'translation reverso--languages)))
    (error "Wrong language: %s" source))
  (unless (and (alist-get target reverso--language-mapping)
               (member source
                       (alist-get 'translation reverso--languages)))
    (error "Wrong language: %s" target))
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
                (funcall cb (reverso--translate-parse data))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun reverso--convert-string (dom)
  "Convert html DOM from the reverso API to fontified string.

reverso.net uses tags to highlight relevant works, e.g. <em> for the
selected word in the context search.  This function fontifies words
that are in tags with `reverso-highlight-face'"
  (thread-last
    (mapconcat (lambda (node)
                 (let ((text (string-trim (if (listp node) (dom-texts node) node)))
                       (is-special (listp node)))
                   (if is-special
                       (propertize text 'face 'reverso-highlight-face)
                     text)))
               (dom-children dom)
               " ")
    (string-trim)
    (replace-regexp-in-string
     (rx (+ (syntax whitespace))",") ",")
    (replace-regexp-in-string
     (rx (+ (syntax whitespace))) " ")))

(defun reverso--convert-string-html (html)
  "Convert HTML string from the reverso API to fontified string."
  (with-temp-buffer
    (insert "<html><body>" html "<body></html>")
    (reverso--convert-string
     (car
      (dom-by-tag
       (libxml-parse-html-region (point-min) (point-max))
       'body)))))

(defun reverso--alist-remove-empty-values (alist)
  (cl-loop for (key . value) in alist
           if value
           collect (cons key value)))

(defun reverso--translate-parse (response)
  "Convert RESPONSE from the reverso translation API into an alist."
  (let ((corrected-text (alist-get 'correctedText response))
        (language-from (alist-get 'from response))
        (language-to (alist-get 'to response))
        (detected-language (alist-get 'detectedLanguage
                                      (alist-get 'languageDetection response)))
        (translation (alist-get 'translation response))
        (context-results
         (cl-loop for r across (alist-get 'results (alist-get 'contextResults response))
                  collect
                  `((translation . ,(alist-get 'translation r))
                    (context
                     . ,(cl-loop for source across (alist-get 'sourceExamples r)
                                 for target across (alist-get 'targetExamples r)
                                 collect
                                 `((source . ,(reverso--convert-string-html source))
                                   (target . ,(reverso--convert-string-html target)))))))))
    `((corrected-text . ,corrected-text)
      (language-from . ,language-from)
      (language-to . ,language-to)
      (detected-language
       . ,(when (and detected-language
                     (not (string= detected-language language-from)))
            detected-language))
      (translation . ,translation)
      (context-results . ,context-results))))

(defun reverso--get-context (text source target cb)
  "Do a context translation for TEXT from SOURCE to TARGET.

SOURCE and TARGET are keys of `reverso--languages'.  CB is a function
that is called with the result."
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
                (funcall cb (reverso--alist-remove-empty-values
                             (reverso--get-context-parse data)))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun reverso--get-context-parse (data)
  "Parse response from reverso context API into an alist.

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
                         `((source . ,(reverso--convert-string src))
                           (target . ,(reverso--convert-string trg))))))))

(defun reverso--get-synomyms (text language cb)
  "Get synomyms for TEXT in LANGUAGE.

CB is a function that is called with the result."
  (unless (alist-get language reverso--language-mapping-1)
    (error "Wrong language: %s" language))
  (request (concat (alist-get 'synomyms reverso--urls)
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
                (funcall cb (reverso--alist-remove-empty-values
                             (reverso--get-synomyms-parse data)))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun reverso--get-synomyms-parse (html)
  "Parse the reverso synomyms page.

HTML is a string."
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max)))))
    (cl-loop
     for child in (dom-non-text-children
                   (dom-by-id (dom-by-tag dom 'body) "synonyms"))
     if (string-match-p "wrap-hold-prop" (or (dom-attr child 'class) ""))
     collect
     `((kind . ,(string-trim (dom-texts (dom-by-class child "words-options"))))
       (synonyms
        . ,(cl-loop
            for synonym in (dom-non-text-children
                            (dom-by-class (car (dom-by-class child "word-opt"))
                                          "word-box"))
            for a = (car (dom-by-tag synonym 'a))
            collect
            `((synonym . ,(string-trim (dom-texts synonym)))
              (relevant
               . ,(and (string-match-p "relevant" (or (dom-attr a 'class) "")) t)))))
       (examples
        . ,(cl-loop
            for example in (dom-non-text-children
                            (dom-by-class child "phrases-examples"))
            for span = (car (dom-by-tag example 'span))
            if span
            collect (reverso--convert-string span)))
       (antonyms
        . ,(cl-loop
            for antonym in (dom-non-text-children
                            (dom-by-class (car (dom-by-class child "antonyms-wrapper"))
                                          "word-box"))
            for a = (car (dom-by-tag antonym 'a))
            for text = (string-trim (dom-texts antonym))
            unless (string-match-p (rx "...") text)
            collect
            `((antonym . ,text))))))))

(defun reverso--get-grammar (text language cb)
  (unless (member language (alist-get 'grammar reverso--languages))
    (error "Wrong language: %s" language))
  (request (concat (alist-get 'grammar reverso--urls)
                   "?text=" (url-hexify-string text)
                   "&language=" (symbol-name
                                 (alist-get language reverso--language-mapping))
                   "&getCorrectionDetails=true")
    :type "GET"
    :headers `(("Accept" . "*/*")
               ("Connection" . "keep-alive")
               ("User-Agent" . ,reverso--user-agent))
    :parser 'json-read
    :encoding 'utf-8
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall cb (reverso--alist-remove-empty-values
                             (reverso--get-grammar-parse text data)))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun reverso--get-grammar-parse (source-text data)
  (let* ((corrected-text (alist-get 'text data))
         (source-text-hl
          (with-temp-buffer
            (insert source-text)
            (cl-loop for corr across (alist-get 'corrections data)
                     if (alist-get 'startIndex corr)
                     do (put-text-property (alist-get 'startIndex corr)
                                           (alist-get 'endIndex corr)
                                           'face 'reverso-error-face))
            (buffer-string)))
         (corrections
          (cl-loop
           for corr across (alist-get 'corrections data)
           collect `((type . ,(alist-get 'type corr))
                     (short-description . ,(alist-get 'shortDescription corr))
                     (long-description . ,(alist-get 'longDescription corr))
                     (mistake-text . ,(alist-get 'mistakeText corr))
                     (correction-text . ,(alist-get 'correctionText corr))
                     (correction-defition . ,(alist-get 'correctionDefinition corr))
                     (suggestions
                      . ,(cl-loop for s across (alist-get 'suggestions corr)
                                  collect
                                  `((text . ,(alist-get 'text s))
                                    (definition . ,(alist-get 'definition s))
                                    (category . ,(alist-get 'category s)))))))))
    `((corrected-text . ,corrected-text)
      (source-text . ,source-text-hl)
      (corrections . ,corrections))))

(provide 'reverso)
;;; reverso.el ends here
