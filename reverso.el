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

(defconst reverso--languages
  '((translation . (arabic chinese dutch english french german hebrew
                           italian japanese korean polish portugese
                           romanian russian spanish swedish turkish
                           ukrainian))
    (context . (arabic german english spanish french hebrew italian
                       japanese dutch polish portugese romanian
                       russian swedish turkish ukrainian chinese))
    (grammar . (english french))
    (synonims . (arabic german english spanish french hebrew italian
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
    (english . eng)))

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
  (unless (alist-get source reverso--language-mapping)
    (error "Wrong language: %s" source))
  (unless (alist-get target reverso--language-mapping)
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
                (funcall cb data)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

;; (reverso--translate "Your god is war" 'english 'german (lambda (&rest kek) (setq my/test kek)))


(provide 'reverso)
;;; reverso.el ends here
