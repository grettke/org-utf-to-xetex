;;; org-utf-to-xetex-test.el --- Tests for org-utf-to-xetex.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Grant Rettke

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

(ert-deftest org-utf-to-xetex-test-existence-1 ()
  (should (fboundp 'org-utf-to-xetex--block-to-newfontfamily)))

(ert-deftest org-utf-to-xetex-test-existence-2 ()
  (should (fboundp 'org-utf-to-xetex--block-to-textfontcommand)))

(ert-deftest org-utf-to-xetex-test-existence-3 ()
  (should (fboundp 'org-utf-to-xetex--block-to-declaretextfontcommand)))

(ert-deftest org-utf-to-xetex-test-existence-4 ()
  (should (fboundp 'org-utf-to-xetex--valid-char)))

(ert-deftest org-utf-to-xetex-test-existence-5 ()
  (should (fboundp 'org-utf-to-xetex--char-to-block-def)))

(ert-deftest org-utf-to-xetex-test-existence-6 ()
  (should (fboundp 'org-utf-to-xetex--char-to-xetex)))

(ert-deftest org-utf-to-xetex-test-existence-7 ()
  (should (fboundp 'org-utf-to-xetex-command-for-every-block)))

(ert-deftest org-utf-to-xetex-test-existence-8 ()
  (should (fboundp 'org-utf-to-xetex-string-to-xetex)))

(ert-deftest org-utf-to-xetex-test-existence-9 ()
  (should (fboundp 'org-utf-to-xetex-prettify)))

(ert-deftest org-utf-to-xetex-test-existence-10 ()
  (should (fboundp 'org-utf-to-xetex-insert-or-wrap-with-macro)))

(ert-deftest org-utf-to-xetex-test-existence-11 ()
  (should (fboundp 'org-utf-to-xetex-get-unicode-block-for-string)))

(ert-deftest org-utf-to-xetex-test-existence-12 ()
  (should (fboundp 'org-utf-to-xetex-get-unicode-block-for-string-char-after)))

(ert-deftest org-utf-to-xetex-test-existence-13 ()
  (should (fboundp 'org-utf-to-xetex-insert-setup-file-line)))

(ert-deftest org-utf-to-xetex-test-existence-14 ()
  (should (fboundp 'org-utf-to-xetex--get-local-macro)))

(ert-deftest org-utf-to-xetex-test-existence-15 ()
  (should (fboundp 'org-utf-to-xetex-use-local-macro)))

(ert-deftest org-utf-to-xetex-test-existence-16 ()
  (should (fboundp 'org-utf-to-xetex-use-remote-macro)))

(ert-deftest org-utf-to-xetex-test-existence-17 ()
  (should (fboundp 'org-utf-to-xetex-use-custom-macro)))

(ert-deftest org-utf-to-xetex-test-existence-18 ()
  (should-not (fboundp 'foo-bar-baz)))

(provide 'org-utf-to-xetex-test)
;;; org-utf-to-xetex-test.el ends here

;;  LocalWords:  textfontcommand textABC declaretextfontcommand textEmoticons
;;  LocalWords:  textCJKUnifiedIdeographs textMiscellaneousTechnical
;;  LocalWords:  textMiscellaneousSymbols
