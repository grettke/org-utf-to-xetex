;;; org-utf-to-xetex.el --- Org XeTeX Automate Font Specification By Unicode Block -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Grant Rettke

;; Author: Grant Rettke <grant@wisdomandwonder.com>
;; Maintainer: Grant Rettke <grant@wisdomandwonder.com>
;; Version: 0.92
;; Package-Requires: ((emacs "29.4"))
;; Keywords: convenience, extensions, i18n, languages, tex, tools
;; Homepage: https://github.com/grettke/org-utf-to-xetex

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

;; See README.org.

;;; Code:

(require 'map)
(require 'cl-lib)

;; Constants

(defconst org-utf-to-xetex-blocks
  '(
    ;; ("Basic Latin" #x0000 #x007F) ;; Ignore: presumably covered by your main font
    ;; ("Latin-1 Supplement" #x0080 #x00FF) ;; Ignore: presumably covered by your main font
    ;; ("Latin Extended-A" #x0100 #x017F) ;; Ignore: presumably covered by your main font
    ;; ("Latin Extended-B" #x0180 #x024F) ;; Ignore: presumably covered by your main font
    ("IPA Extensions" #x0250 #x02AF)
    ;; ("Spacing Modifier Letters" #x02B0 #x02FF) ;; Ignore: presumably covered by your main font
    ("Combining Diacritical Marks" #x0300 #x036F)
    ("Greek and Coptic" #x0370 #x03FF)
    ("Cyrillic" #x0400 #x04FF)
    ("Cyrillic Supplement" #x0500 #x052F)
    ("Armenian" #x0530 #x058F)
    ("Hebrew" #x0590 #x05FF)
    ("Arabic" #x0600 #x06FF)
    ("Syriac" #x0700 #x074F)
    ("Arabic Supplement" #x0750 #x077F)
    ("Thaana" #x0780 #x07BF)
    ("NKo" #x07C0 #x07FF)
    ("Samaritan" #x0800 #x083F)
    ("Mandaic" #x0840 #x085F)
    ("Syriac Supplement" #x0860 #x086F)
    ("Arabic Extended-A" #x08A0 #x08FF)
    ("Devanagari" #x0900 #x097F)
    ("Bengali" #x0980 #x09FF)
    ("Gurmukhi" #x0A00 #x0A7F)
    ("Gujarati" #x0A80 #x0AFF)
    ("Oriya" #x0B00 #x0B7F)
    ("Tamil" #x0B80 #x0BFF)
    ("Telugu" #x0C00 #x0C7F)
    ("Kannada" #x0C80 #x0CFF)
    ("Malayalam" #x0D00 #x0D7F)
    ("Sinhala" #x0D80 #x0DFF)
    ("Thai" #x0E00 #x0E7F)
    ("Lao" #x0E80 #x0EFF)
    ("Tibetan" #x0F00 #x0FFF)
    ("Myanmar" #x1000 #x109F)
    ("Georgian" #x10A0 #x10FF)
    ("Hangul Jamo" #x1100 #x11FF)
    ("Ethiopic" #x1200 #x137F)
    ("Ethiopic Supplement" #x1380 #x139F)
    ("Cherokee" #x13A0 #x13FF)
    ("Unified Canadian Aboriginal Syllabics" #x1400 #x167F)
    ("Ogham" #x1680 #x169F)
    ("Runic" #x16A0 #x16FF)
    ("Tagalog" #x1700 #x171F)
    ("Hanunoo" #x1720 #x173F)
    ("Buhid" #x1740 #x175F)
    ("Tagbanwa" #x1760 #x177F)
    ("Khmer" #x1780 #x17FF)
    ("Mongolian" #x1800 #x18AF)
    ("Unified Canadian Aboriginal Syllabics Extended" #x18B0 #x18FF)
    ("Limbu" #x1900 #x194F)
    ("Tai Le" #x1950 #x197F)
    ("New Tai Lue" #x1980 #x19DF)
    ("Khmer Symbols" #x19E0 #x19FF)
    ("Buginese" #x1A00 #x1A1F)
    ("Tai Tham" #x1A20 #x1AAF)
    ("Combining Diacritical Marks Extended" #x1AB0 #x1AFF)
    ("Balinese" #x1B00 #x1B7F)
    ("Sundanese" #x1B80 #x1BBF)
    ("Batak" #x1BC0 #x1BFF)
    ("Lepcha" #x1C00 #x1C4F)
    ("Ol Chiki" #x1C50 #x1C7F)
    ("Cyrillic Extended-C" #x1C80 #x1C8F)
    ("Georgian Extended" #x1C90 #x1CBF)
    ("Sundanese Supplement" #x1CC0 #x1CCF)
    ("Vedic Extensions" #x1CD0 #x1CFF)
    ("Phonetic Extensions" #x1D00 #x1D7F)
    ("Phonetic Extensions Supplement" #x1D80 #x1DBF)
    ("Combining Diacritical Marks Supplement" #x1DC0 #x1DFF)
    ;; ("Latin Extended Additional" #x1E00 #x1EFF) ;; Ignore: presumably covered by your main font
    ("Greek Extended" #x1F00 #x1FFF)
    ("General Punctuation" #x2000 #x206F)
    ("Superscripts and Subscripts" #x2070 #x209F)
    ("Currency Symbols" #x20A0 #x20CF)
    ("Combining Diacritical Marks for Symbols" #x20D0 #x20FF)
    ("Letterlike Symbols" #x2100 #x214F)
    ("Number Forms" #x2150 #x218F)
    ("Arrows" #x2190 #x21FF)
    ("Mathematical Operators" #x2200 #x22FF)
    ("Miscellaneous Technical" #x2300 #x23FF)
    ("Control Pictures" #x2400 #x243F)
    ("Optical Character Recognition" #x2440 #x245F)
    ("Enclosed Alphanumerics" #x2460 #x24FF)
    ("Box Drawing" #x2500 #x257F)
    ("Block Elements" #x2580 #x259F)
    ("Geometric Shapes" #x25A0 #x25FF)
    ("Miscellaneous Symbols" #x2600 #x26FF)
    ("Dingbats" #x2700 #x27BF)
    ("Miscellaneous Mathematical Symbols-A" #x27C0 #x27EF)
    ("Supplemental Arrows-A" #x27F0 #x27FF)
    ("Braille Patterns" #x2800 #x28FF)
    ("Supplemental Arrows-B" #x2900 #x297F)
    ("Miscellaneous Mathematical Symbols-B" #x2980 #x29FF)
    ("Supplemental Mathematical Operators" #x2A00 #x2AFF)
    ("Miscellaneous Symbols and Arrows" #x2B00 #x2BFF)
    ("Glagolitic" #x2C00 #x2C5F)
    ("Latin Extended-C" #x2C60 #x2C7F)
    ("Coptic" #x2C80 #x2CFF )
    ("Georgian Supplement" #x2D00 #x2D2F)
    ("Tifinagh" #x2D30 #x2D7F)
    ("Ethiopic Extended" #x2D80 #x2DDF)
    ("Cyrillic Extended-A" #x2DE0 #x2DFF)
    ("Supplemental Punctuation" #x2E00 #x2E7F)
    ("CJK Radicals Supplement" #x2E80 #x2EFF)
    ("Kangxi Radicals" #x2F00 #x2FDF)
    ("Ideographic Description Characters" #x2FF0 #x2FFF)
    ("CJK Symbols and Punctuation" #x3000 #x303F)
    ("Hiragana" #x3040 #x309F)
    ("Katakana" #x30A0 #x30FF)
    ("Bopomofo" #x3100 #x312F)
    ("Hangul Compatibility Jamo" #x3130 #x318F)
    ("Kanbun" #x3190 #x319F)
    ("Bopomofo Extended" #x31A0 #x31BF)
    ("CJK Strokes" #x31C0 #x31EF)
    ("Katakana Phonetic Extensions" #x31F0 #x31FF)
    ("Enclosed CJK Letters and Months" #x3200 #x32FF)
    ("CJK Compatibility" #x3300 #x33FF)
    ("CJK Unified Ideographs Extension A" #x3400 #x4DBF)
    ("Yijing Hexagram Symbols" #x4DC0 #x4DFF)
    ("CJK Unified Ideographs" #x4E00 #x9FFF)
    ("Yi Syllables" #xA000 #xA48F)
    ("Yi Radicals" #xA490 #xA4CF)
    ("Lisu" #xA4D0 #xA4FF)
    ("Vai" #xA500 #xA63F)
    ("Cyrillic Extended-B" #xA640 #xA69F)
    ("Bamum" #xA6A0 #xA6FF)
    ("Modifier Tone Letters" #xA700 #xA71F)
    ("Latin Extended-D" #xA720 #xA7FF)
    ("Syloti Nagri" #xA800 #xA82F)
    ("Common Indic Number Forms" #xA830 #xA83F)
    ("Phags-pa" #xA840 #xA87F)
    ("Saurashtra" #xA880 #xA8DF)
    ("Devanagari Extended" #xA8E0 #xA8FF)
    ("Kayah Li" #xA900 #xA92F)
    ("Rejang" #xA930 #xA95F)
    ("Hangul Jamo Extended-A" #xA960 #xA97F)
    ("Javanese" #xA980 #xA9DF)
    ("Myanmar Extended-B" #xA9E0 #xA9FF)
    ("Cham" #xAA00 #xAA5F)
    ("Myanmar Extended-A" #xAA60 #xAA7F)
    ("Tai Viet" #xAA80 #xAADF)
    ("Meetei Mayek Extensions" #xAAE0 #xAAFF)
    ("Ethiopic Extended-A" #xAB00 #xAB2F)
    ("Latin Extended-E" #xAB30 #xAB6F)
    ("Cherokee Supplement" #xAB70 #xABBF)
    ("Meetei Mayek" #xABC0 #xABFF)
    ("Hangul Syllables" #xAC00 #xD7AF)
    ("Hangul Jamo Extended-B" #xD7B0 #xD7FF)
    ;; ("High Surrogates" #xD800 #xDB7F) ; no displayable characters
    ;; ("High Private Use Surrogates" #xDB80 #xDBFF) ; no displayable characters
    ;; ("Low Surrogates" #xDC00 #xDFFF) ; no displayable characters
    ("Private Use Area" #xE000 #xF8FF)
    ("CJK Compatibility Ideographs" #xF900 #xFAFF)
    ("Alphabetic Presentation Forms" #xFB00 #xFB4F)
    ("Arabic Presentation Forms-A" #xFB50 #xFDFF)
    ("Variation Selectors" #xFE00 #xFE0F)
    ("Vertical Forms" #xFE10 #xFE1F)
    ("Combining Half Marks" #xFE20 #xFE2F)
    ("CJK Compatibility Forms" #xFE30 #xFE4F)
    ("Small Form Variants" #xFE50 #xFE6F)
    ("Arabic Presentation Forms-B" #xFE70 #xFEFF)
    ("Halfwidth and Fullwidth Forms" #xFF00 #xFFEF)
    ("Specials" #xFFF0 #xFFFF)
    ("Linear B Syllabary" #x10000 #x1007F)
    ("Linear B Ideograms" #x10080 #x100FF)
    ("Aegean Numbers" #x10100 #x1013F)
    ("Ancient Greek Numbers" #x10140 #x1018F)
    ("Ancient Symbols" #x10190 #x101CF)
    ("Phaistos Disc" #x101D0 #x101FF)
    ("Lycian" #x10280 #x1029F)
    ("Carian" #x102A0 #x102DF)
    ("Coptic Epact Numbers" #x102E0 #x102FF)
    ("Old Italic" #x10300 #x1032F)
    ("Gothic" #x10330 #x1034F)
    ("Old Permic" #x10350 #x1037F)
    ("Ugaritic" #x10380 #x1039F)
    ("Old Persian" #x103A0 #x103DF)
    ("Deseret" #x10400 #x1044F)
    ("Shavian" #x10450 #x1047F)
    ("Osmanya" #x10480 #x104AF)
    ("Osage" #x104B0 #x104FF)
    ("Elbasan" #x10500 #x1052F)
    ("Caucasian Albanian" #x10530 #x1056F)
    ("Linear A" #x10600 #x1077F)
    ("Cypriot Syllabary" #x10800 #x1083F)
    ("Imperial Aramaic" #x10840 #x1085F)
    ("Palmyrene" #x10860 #x1087F)
    ("Nabataean" #x10880 #x108AF)
    ("Hatran" #x108E0 #x108FF)
    ("Phoenician" #x10900 #x1091F)
    ("Lydian" #x10920 #x1093F)
    ("Meroitic Hieroglyphs" #x10980 #x1099F)
    ("Meroitic Cursive" #x109A0 #x109FF)
    ("Kharoshthi" #x10A00 #x10A5F)
    ("Old South Arabian" #x10A60 #x10A7F)
    ("Old North Arabian" #x10A80 #x10A9F)
    ("Manichaean" #x10AC0 #x10AFF)
    ("Avestan" #x10B00 #x10B3F)
    ("Inscriptional Parthian" #x10B40 #x10B5F)
    ("Inscriptional Pahlavi" #x10B60 #x10B7F)
    ("Psalter Pahlavi" #x10B80 #x10BAF)
    ("Old Turkic" #x10C00 #x10C4F)
    ("Old Hungarian" #x10C80 #x10CFF)
    ("Hanifi Rohingya" #x10D00 #x10D3F)
    ("Rumi Numeral Symbols" #x10E60 #x10E7F)
    ("Old Sogdian" #x10F00 #x10F2F)
    ("Sogdian" #x10F30 #x10F6F)
    ("Brahmi" #x11000 #x1107F)
    ("Kaithi" #x11080 #x110CF)
    ("Sora Sompeng" #x110D0 #x110FF)
    ("Chakma" #x11100 #x1114F)
    ("Mahajani" #x11150 #x1117F)
    ("Sharada" #x11180 #x111DF)
    ("Sinhala Archaic Numbers" #x111E0 #x111FF)
    ("Khojki" #x11200 #x1124F)
    ("Multani" #x11280 #x112AF)
    ("Khudawadi" #x112B0 #x112FF)
    ("Grantha" #x11300 #x1137F)
    ("Newa" #x11400 #x1147F)
    ("Tirhuta" #x11480 #x114DF)
    ("Siddham" #x11580 #x115FF)
    ("Modi" #x11600 #x1165F)
    ("Mongolian Supplement" #x11660 #x1167F)
    ("Takri" #x11680 #x116CF)
    ("Ahom" #x11700 #x1173F)
    ("Dogra" #x11800 #x1184F)
    ("Warang Citi" #x118A0 #x118FF)
    ("Zanabazar Square" #x11A00 #x11A4F)
    ("Soyombo" #x11A50 #x11AAF)
    ("Pau Cin Hau" #x11AC0 #x11AFF)
    ("Bhaiksuki" #x11C00 #x11C6F)
    ("Marchen" #x11C70 #x11CBF)
    ("Masaram Gondi" #x11D00 #x11D5F)
    ("Gunjala Gondi" #x11D60 #x11DAF)
    ("Makasar" #x11EE0 #x11EFF)
    ("Cuneiform" #x12000 #x123FF)
    ("Cuneiform Numbers and Punctuation" #x12400 #x1247F)
    ("Early Dynastic Cuneiform" #x12480 #x1254F)
    ("Egyptian Hieroglyphs" #x13000 #x1342F)
    ("Anatolian Hieroglyphs" #x14400 #x1467F)
    ("Bamum Supplement" #x16800 #x16A3F)
    ("Mro" #x16A40 #x16A6F)
    ("Bassa Vah" #x16AD0 #x16AFF)
    ("Pahawh Hmong" #x16B00 #x16B8F)
    ("Medefaidrin" #x16E40 #x16E9F)
    ("Miao" #x16F00 #x16F9F)
    ("Ideographic Symbols and Punctuation" #x16FE0 #x16FFF)
    ("Tangut" #x17000 #x187FF)
    ("Tangut Components" #x18800 #x18AFF)
    ("Kana Supplement" #x1B000 #x1B0FF)
    ("Kana Extended-A" #x1B100 #x1B12F)
    ("Nushu" #x1B170 #x1B2FF)
    ("Duployan" #x1BC00 #x1BC9F)
    ;; ("Shorthand Format Controls" #x1BCA0 #x1BCAF) ; no displayable characters
    ("Byzantine Musical Symbols" #x1D000 #x1D0FF)
    ("Musical Symbols" #x1D100 #x1D1FF)
    ("Ancient Greek Musical Notation" #x1D200 #x1D24F)
    ("Mayan Numerals" #x1D2E0 #x1D2FF)
    ("Tai Xuan Jing Symbols" #x1D300 #x1D35F)
    ("Counting Rod Numerals" #x1D360 #x1D37F)
    ("Mathematical Alphanumeric Symbols" #x1D400 #x1D7FF)
    ("Sutton SignWriting" #x1D800 #x1DAAF)
    ("Glagolitic Supplement" #x1E000 #x1E02F)
    ("Mende Kikakui" #x1E800 #x1E8DF)
    ("Adlam" #x1E900 #x1E95F)
    ("Indic Siyaq Numbers" #x1EC70 #x1ECBF)
    ("Arabic Mathematical Alphabetic Symbols" #x1EE00 #x1EEFF)
    ("Mahjong Tiles" #x1F000 #x1F02F)
    ("Domino Tiles" #x1F030 #x1F09F)
    ("Playing Cards" #x1F0A0 #x1F0FF)
    ("Enclosed Alphanumeric Supplement" #x1F100 #x1F1FF)
    ("Enclosed Ideographic Supplement" #x1F200 #x1F2FF)
    ("Miscellaneous Symbols and Pictographs" #x1F300 #x1F5FF)
    ("Emoticons" #x1F600 #x1F64F)
    ("Ornamental Dingbats" #x1F650 #x1F67F)
    ("Transport and Map Symbols" #x1F680 #x1F6FF)
    ("Alchemical Symbols" #x1F700 #x1F77F)
    ("Geometric Shapes Extended" #x1F780 #x1F7FF)
    ("Supplemental Arrows-C" #x1F800 #x1F8FF)
    ("Supplemental Symbols and Pictographs" #x1F900 #x1F9FF)
    ("Chess Symbols" #x1FA00 #x1FA6F)
    ("CJK Unified Ideographs Extension B" #x20000 #x2A6DF)
    ("CJK Unified Ideographs Extension C" #x2A700 #x2B73F)
    ("CJK Unified Ideographs Extension D" #x2B740 #x2B81F)
    ("CJK Unified Ideographs Extension E" #x2B820 #x2CEAF)
    ("CJK Unified Ideographs Extension F" #x2CEB0 #x2EBEF)
    ("CJK Compatibility Ideographs Supplement" #x2F800 #x2FA1F)
    ("Tags" #xE0000 #xE007F)
    ("Variation Selectors Supplement" #xE0100 #xE01EF)
    ("Supplementary Private Use Area-A" #xF0000 #xFFFFF)
    ("Supplementary Private Use Area-B" #x100000 #x10FFFF))
  "Alist of Unicode 11.0 blocks. See charts here URL `https://www.unicode.org/charts/'.

The block name values must remain in the same case you found them.

For example: do not capitalize \"and\"!")

(defconst org-utf-to-xetex-setup-file
  "#+SETUPFILE: https://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/org-utf-to-xetex.setup

"
  "The org-utf-to-xetex export macro.")

;; Variables

(defvar org-utf-to-xetex-macro-name "utf2xtx"
  "The package macro name to be prettified by pretty-symbols.")

(defvar org-utf-to-xetex-macro-pretty-name ?⋅
  "The pretty character name for the macro.")

;; Functions

(defun org-utf-to-xetex--block-to-friendly-name (name)
  "Generate a LaTeX friendly name for block NAME."
  (let* ((rep "")
         (name (or name ""))
         (result name))
    (setq result (replace-regexp-in-string "\\s-" rep result))
    (setq result (replace-regexp-in-string "-" rep result))
    (setq result (replace-regexp-in-string "_" rep result))
    result))

(defun org-utf-to-xetex--block-to-newfontfamily (name &optional font)
  "Generate a LaTeX newfontfamily command for block NAME using FONT if provided."
  (let* ((name (org-utf-to-xetex--block-to-friendly-name name))
         (font (or font "font"))
         (result (format "\\newfontfamily\\%s{%s}" name font)))
    result))

(defun org-utf-to-xetex--block-to-textfontcommand (name)
  "Generate a LaTeX text command name for block NAME."
  (let* ((name (org-utf-to-xetex--block-to-friendly-name name))
         (result (format "\\text%s" name)))
    result))

(defun org-utf-to-xetex--block-to-declaretextfontcommand (name)
  "Generate a LaTeX DeclareTextFontCommand for block NAME."
  (let* ((textfontcommand (org-utf-to-xetex--block-to-textfontcommand name))
         (name (org-utf-to-xetex--block-to-friendly-name name))
         (result (format "\\DeclareTextFontCommand{%s}{\\%s}"
                         textfontcommand name)))
    result))

(defun org-utf-to-xetex--valid-char (str)
  "Return non-nil if STR is a single character string."
  (and str (= (length str) 1)))

(defun org-utf-to-xetex--char-to-block-def (str)
  "Return the Unicode block definition containing STR.

If STR is not found, return nil."
  (let ((def nil)
        (maxucschar #x110000))
    (catch 'nilarg
      (when (not (org-utf-to-xetex--valid-char str))
        (throw 'nilarg nil))
      (setq def
            (catch 'result
              (dolist (it org-utf-to-xetex-blocks)
                (let* ((beg (nth 1 it))
                       (end (nth 2 it))
                       (codepoint (with-temp-buffer
                                    (insert str)
                                    (goto-char (point-min))
                                    (char-after))))
                  (cond
                   ((not codepoint)
                    (message
                     "org-utf-to-xetex--char-to-block-def: '%s' has no codepoint" str))
                   ((not (characterp codepoint))
                    (message
                     "org-utf-to-xetex--char-to-block-def: '%s' is not character" str))
                   ((>= codepoint maxucschar)
                    (message
                     "org-utf-to-xetex--char-to-block-def: '%s' (codepoint '%s') exceeds the Unicode codespace" str codepoint))
                   (t
                    (when (and (>= codepoint beg)
                               (<= codepoint end))
                      (throw 'result it)))))))))
    def))

(defun org-utf-to-xetex--char-to-xetex (str)
  "Find the block containing STR.

Return the font encoded LaTeX string for that block. On no
match, or any error, return STR."
  (let ((result))
    (catch 'nilarg
      (when (not (org-utf-to-xetex--valid-char str))
        (message "org-utf-to-xetex--char-to-xetex CHAR is either nil, empty, or
greater than one character, exiting.")
        (throw 'nilarg str)))
    (setq result
          (catch 'result
            (let* ((def (org-utf-to-xetex--char-to-block-def str))
                   (_ (when (not def) (throw 'result str)))
                   (block (nth 0 def))
                   (cmd (org-utf-to-xetex--block-to-textfontcommand block))
                   (string (format "%s{%s}" cmd str)))
              string)))
    result))

;; Features

(defun org-utf-to-xetex-command-for-every-block ()
  "List configuration commands for every block."
  (interactive)
  (let* ((name "*ORG-UTF-TO-XETEX*")
         (buf (get-buffer-create name))
         (blocks (length org-utf-to-xetex-blocks)))
    (switch-to-buffer buf)
    (dolist (block org-utf-to-xetex-blocks)
      (let ((name (org-utf-to-xetex--block-to-friendly-name (nth 0 block))))
        (insert "% ")
        (insert (org-utf-to-xetex--block-to-newfontfamily name))
        (insert "\n")
        (insert "% ")
        (insert (org-utf-to-xetex--block-to-declaretextfontcommand name))
        (insert "\n")))
    (help-mode)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (let ((lines (count-lines (point-min) (point-max))))
      (when (not (equal lines (* 2 blocks)))
        (display-warning
         :error
         (format "org-utf-to-xetex-command-for-every-block expected to create %s commands, but only created %s. Please report this." blocks
                 lines))))))

(defun org-utf-to-xetex-string-to-xetex (str)
  "Return LaTeX string with correct font environment for STR."
  (let* ((string str)
         (codepoints (string-to-list string))
         (strings (mapcar #'string codepoints))
         (result (mapconcat #'org-utf-to-xetex--char-to-xetex strings "")))
    result))

(defun org-utf-to-xetex-prettify ()
  "Set up function `prettify-symbols-mode'."
  (when (boundp 'prettify-symbols-mode)
    (setf (map-elt prettify-symbols-alist org-utf-to-xetex-macro-name)
          org-utf-to-xetex-macro-pretty-name)
    (prettify-symbols-mode)
    (prettify-symbols-mode))
  (setq org-hide-macro-markers t)
  (font-lock-mode)
  (font-lock-mode))

(defun org-utf-to-xetex-insert-or-wrap-with-macro ()
  "Insert the macro maybe around a region."
  (interactive)
  (let* ((bounds (and (use-region-p) (list (region-beginning) (region-end))))
         (left (concat "{{{" org-utf-to-xetex-macro-name "("))
         (right ")}}}")
         (backup (length right))
         (body (if bounds (buffer-substring-no-properties (cl-first bounds)
                                                          (cl-second bounds))
                 ""))
         (string (concat left body right)))
    (when bounds (delete-region (cl-first bounds) (cl-second bounds)))
    (insert string)
    (backward-char backup)))

(defun org-utf-to-xetex-get-unicode-block-for-string (str)
  "In what Unicode block does STR live in? Answered here."
  (let* ((block (org-utf-to-xetex--char-to-block-def str))
         (msg (if block (cl-first block)
                "This package doesn't handle this block. If it isn't one of the ignored Latin blocks then please report it. ")))
    (message "Unicode Block Name For '%s': %s" str msg)))

(defun org-utf-to-xetex-get-unicode-block-for-string-char-after ()
  "In what Unicode block does the character after the cursor live in?

Answered here."
  (interactive)
  (if (char-after)
      (org-utf-to-xetex-get-unicode-block-for-string
       (format "%c" (char-after)))
    (message "First place the cursor on a char to inspect it.")))

(defun org-utf-to-xetex-insert-setup-file-line ()
  "Insert the org-utf-to-xetex export macro SETUPFILE line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert org-utf-to-xetex-setup-file)
    (save-buffer)))

(provide 'org-utf-to-xetex)

;;; org-utf-to-xetex.el ends here
