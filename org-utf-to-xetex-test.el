;;; org-utf-to-xetex-test.el --- Tests for org-utf-to-xetex.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Grant Rettke

;; Author: Grant Rettke <gcr@wisdomandwonder.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for org-utf-to-xetex.

;;; Code:

(require 'ert)
(require 'org-utf-to-xetex)

(ert-deftest org-utf-to-xetex--block-to-friendly-name ()
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name nil)))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name " ")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "  ")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "   ")))

  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "-")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "--")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "---")))

  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "_")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "__")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "___")))

  (should (equal "" (org-utf-to-xetex--block-to-friendly-name " -_")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "- _")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "_ -")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name " _-")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "-_ ")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "_- ")))

  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "   - _")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name " -   _")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name " _   -")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name "   _ -")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name " - _  ")))
  (should (equal "" (org-utf-to-xetex--block-to-friendly-name " _ -  ")))

  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name "A B-C_")))
  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name "A-B C_")))
  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name "A_B C-")))
  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name "A B_C-")))
  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name "A-B_C ")))
  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name "A_B-C ")))

  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name " A-B_C")))
  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name "-A B_C")))
  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name "_A B-C")))
  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name " A_B-C")))
  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name "-A_B C")))
  (should (equal "ABC" (org-utf-to-xetex--block-to-friendly-name "_A-B C"))))

(ert-deftest org-utf-to-xetex--block-to-newfontfamily ()
  (should (equal "\\newfontfamily\\A{font}"
                 (org-utf-to-xetex--block-to-newfontfamily "A")))
  (should (equal "\\newfontfamily\\A{B}"
                 (org-utf-to-xetex--block-to-newfontfamily "A" "B"))))

(ert-deftest org-utf-to-xetex--block-to-textfontcommand ()
  (should (equal "\\textABC"
                 (org-utf-to-xetex--block-to-textfontcommand "ABC"))))

(ert-deftest org-utf-to-xetex--block-to-declaretextfontcommand ()
  (should (equal "\\DeclareTextFontCommand{\\textEmoticons}{\\Emoticons}"
                 (org-utf-to-xetex--block-to-declaretextfontcommand
                  "Emoticons"))))

(ert-deftest org-utf-to-xetex--valid-char ()
  (should (equal nil
                 (org-utf-to-xetex--valid-char nil)))
  (should (equal nil
                 (org-utf-to-xetex--valid-char "")))
  (should (equal nil
                 (org-utf-to-xetex--valid-char "  ")))
  (should (equal t
                 (org-utf-to-xetex--valid-char " ")))
  (should (equal t
                 (org-utf-to-xetex--valid-char "A")))
  (should (equal t
                 (org-utf-to-xetex--valid-char "😀")))
  (should (equal t
                 (org-utf-to-xetex--valid-char "⍋")))
  (should (equal t
                 (org-utf-to-xetex--valid-char "我"))))

(ert-deftest org-utf-to-xetex--char-to-block-def ()
  ;; no argument
  (should (equal nil (org-utf-to-xetex--char-to-block-def nil)))
  ;; Emoticons
  (should (equal "Emoticons" (car (org-utf-to-xetex--char-to-block-def "😀"))))
  (should (equal "Emoticons" (car (org-utf-to-xetex--char-to-block-def "😨"))))
  (should (equal "Emoticons" (car (org-utf-to-xetex--char-to-block-def "🙏"))))
  ;; Miscellaneous Symbols
  (should (equal "Miscellaneous Symbols" (car (org-utf-to-xetex--char-to-block-def "☀"))))
  ;; Miscellaneous Symbols
  (should (equal "Miscellaneous Technical" (car (org-utf-to-xetex--char-to-block-def "⍋"))))
  ;; "CJK Unified Ideographs"
  (should (equal "CJK Unified Ideographs" (car (org-utf-to-xetex--char-to-block-def "我"))))
  (should (equal "CJK Unified Ideographs" (car (org-utf-to-xetex--char-to-block-def "人"))))
  (should (equal "CJK Unified Ideographs" (car (org-utf-to-xetex--char-to-block-def "日")))))

(ert-deftest org-utf-to-xetex--char-to-xetex ()
  (should (equal "\\textEmoticons{😈}"
                 (org-utf-to-xetex--char-to-xetex "😈")))
  (should (equal "\\textCJKUnifiedIdeographs{我}"
                 (org-utf-to-xetex--char-to-xetex "我"))))

(ert-deftest org-utf-to-xetex-string-to-xetex ()
  (should (equal
           "A \\textCJKUnifiedIdeographs{我}-\\textMiscellaneousTechnical{⍋}+\\textMiscellaneousSymbols{☀}APPLE\\textEmoticons{🙋}ZEBRA"
           (org-utf-to-xetex-string-to-xetex "A 我-⍋+☀APPLE🙋ZEBRA"))))

(ert-deftest org-utf-to-xetex-test-existence-of-expected-functions ()
  (should (fboundp 'org-utf-to-xetex--block-to-newfontfamily))
  (should (fboundp 'org-utf-to-xetex--block-to-textfontcommand))
  (should (fboundp 'org-utf-to-xetex--block-to-declaretextfontcommand))
  (should (fboundp 'org-utf-to-xetex--valid-char))
  (should (fboundp 'org-utf-to-xetex--char-to-block-def))
  (should (fboundp 'org-utf-to-xetex--char-to-xetex))
  (should (fboundp 'org-utf-to-xetex-command-for-every-block))
  (should (fboundp 'org-utf-to-xetex-string-to-xetex))
  (should (fboundp 'org-utf-to-xetex-prettify))
  (should (fboundp 'org-utf-to-xetex-insert-or-wrap-with-macro))
  (should (fboundp 'org-utf-to-xetex-get-unicode-block-for-string))
  (should (fboundp 'org-utf-to-xetex-get-unicode-block-for-string-char-after))
  (should (fboundp 'org-utf-to-xetex-insert-setup-file-line))
  (should (fboundp 'org-utf-to-xetex--get-local-macro))
  (should (fboundp 'org-utf-to-xetex-use-local-macro))
  (should (fboundp 'org-utf-to-xetex-use-custom-macro))
  ) ;; intentional line break

(provide 'org-utf-to-xetex-test)
;;; org-utf-to-xetex-test.el ends here

;;  LocalWords:  textfontcommand textABC declaretextfontcommand textEmoticons
;;  LocalWords:  textCJKUnifiedIdeographs textMiscellaneousTechnical
;;  LocalWords:  textMiscellaneousSymbols
