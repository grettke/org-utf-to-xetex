;;; org-utf-to-xetex.el --- Org-Mode Print Unicode Characters Directly To PDF -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Grant Rettke

;; Author: Grant Rettke <gcr@wisdomandwonder.com>
;; Homepage: https://github.com/grettke/org-utf-to-xetex
;; URL: http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/org-utf-to-xetex.el
;; Version: 0.9
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, extensions, i18n, languages, tex, tools

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

;; Read Me First:
;;
;; You wrote `Have a great day 😄' in your Org-Mode file. Then you export it
;; to PDF. Instead of seeing nice smiling face, you see a white box instead:
;; `Have a great day □'. If you are in this situation then you _might_ be
;; interested in reading on.
;;
;; Org-Mode exports to many formats. Each format has different strengths. When
;; Org-Mode exports your document it needs to leverage the strengths of the
;; destination format by abstracting away the difference so that you can
;; specify the _intended_ character in Org-Mode's markup and let Org-Mode
;; choose the right one for your destination format. Org-Mode facilitates this
;; abstraction using it's [Symbols](https://orgmode.org/worg/org-symbols.html) mechanism. A great example here is the smiley face.
;;
;; In Org-Mode the smiley face is represented by the string `\smiley'. When your
;; document is exported it gets converted to the correct representation for
;; it's destination. Here are some example conversions (language, output):
;;
;; * LaTeX: `\smiley{}'
;;
;; * HTML: `&#9786;'
;;
;; * ASCII: `:-)'
;;
;; * UTF-8: `☺'
;;
;; You will get a smiley face in every export. This mechanism _should_ get you
;; what you want. For my computer, this wasn't enough.
;;
;; On my computer I am pretty lazy in that I want to write a document once and
;; have it look pretty close to it's final version. I don't want to have to
;; use a Symbol for every Non-Latin character, and there aren't enough symbols
;; anyway. Using UTF-8 files makes it easy to use whatever character you want
;; so that is the right place to start.
;;
;; List of characters supported is simple: you can use any Unicode character. Once
;; you find a good font, then it looks good as a text file just how you would
;; expect it. When you export to HTML, it just works. On my box though it
;; didn't just work for PDF. The solution is to switch to a different LaTeX
;; compiler, and it lets you generate PDFs just as easily as with every other
;; Org-Mode export: all of your characters show up correctly.
;;
;; **Org-Mode makes it easy to use the same "Unicode Everywhere"** workflow by
;; switching from the PDFLaTeX compiler to the XeTeX compiler. In order to use
;; this package you must have already switched over to XeTeX. Most of us are
;; switching here _from_ PDFLaTeX.
;;
;; **Once you make the switch to XeTeX**, _and_ configure your system using this package, your
;; PDFs print the same characters as your text files. If you are interested in
;; that workflow then you might be interested in this package.

;; Overview:
;;
;; `org-utf-to-xetex' is a little package that teaches [Org-Mode](https://orgmode.org/) and [XeTeX](http://xetex.sourceforge.net/)
;; what fonts to use for a few of the Unicode characters in your document. This
;; is a _little_ package because most of the steps happen in your LaTeX
;; configuration. This package provides helpful functionality for setting up your
;; LaTeX compiler and most importantly integrating it with your Org LaTeX
;; exporter. The critical functionality though is provided by your LaTeX
;; compiler, as you will see in the [workflow steps below](#workflow). Read on
;; to see the empty white box problem and how this package resolves it.
;;
;; You probably set up XeTeX with three font settings like this
;;
;; * `\setmainfont{DejaVu Serif}'
;;
;; * `\setsansfont{DejaVu Sans}[Scale=MatchLowercase]'
;;
;; * `\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]'
;;
;; That probably makes most of your PDF documents look great because 99% of
;; the characters that you use are [Latin](https://en.wikipedia.org/wiki/List_of_languages_by_writing_system#Latin_script) and your `mainfont' supports all of them.
;; The problem is the 1% that it doesn't, which are probably Non-Latin characters.
;; Instead of your Unicode character that you expected to see, you see a white box. Here is an
;; example of the Org-Mode document, the intermediate LaTeX,  and resulting PDF
;;
;; <table border="1">
;;   <caption>Workflow <i>Without</i> This Package</caption>
;;   <tr>
;;     <th>File In Workflow</th>
;;     <th>Screenshot</th>
;;   </tr>
;;   <tr>
;;     <td>Org-Mode Source</td>
;;     <td><img src="./images/orgfile.png" alt="Org-Mode Source File"></td>
;;   </tr>
;;   <tr>
;;     <td>LaTeX Source</td>
;;     <td><img src="./images/latexfile.png" alt="LaTeX Source File"></td>
;;   </tr>
;;   <tr>
;;     <td>Generated PDF</td>
;;     <td><img src="./images/pdffile.png" alt="PDF Without Using Package" width="62%" height="62%"></td>
;;   </tr>
;; </table>
;;
;; That clearly isn't what you expected.
;;
;; When your LaTeX compiler created the PDF, it used your mainfont. But that font
;; doesn't handle the Unicode character you wrote. Your PDF wants you to that
;; _it tried_ to show you something for that character but could not. It tells
;;  you by showing you an empty white box.
;;
;; This is pretty common because fonts don't and _cannot_ cover all of the
;; Unicode symbols out there (there are too many). The solution is to specify a different font to
;; handle the characters that your main font doesn't know about.
;;
;; This package teaches Org-Mode how to ask XeTeX to use a different font for
;; some characters.
;;
;; Now your Org-Mode document and generated PDF should look something like
;; this
;;
;; <table border="1">
;;   <caption>Workflow <i>With</i> This Package</caption>
;;   <tr>
;;     <th>File In Workflow</th>
;;     <th><Screenshot></th>
;;   </tr>
;;   <tr>
;;     <td>Org-Mode Source <i>Without</i> Prettification</td>
;;     <td><img src="./images/orgfiledonenotpretty.png" alt="Org-Mode Source File Without Any  Prettification"></td>
;;   </tr>
;;   <tr>
;;     <td>Org-Mode Source <i>With</i> Prettification</td>
;;     <td><img src="./images/orgfiledonepretty.png" alt="Org-Mode Source File With Prettification"></td>
;;   </tr>
;;   <tr>
;;     <td>LaTeX Source With Font Commands</td>
;;     <td><img src="./images/latexfiledone.png" alt="LaTeX Source With Font Commands"></td>
;;   </tr>
;;   <tr>
;;     <td>PDF With Correct Fonts</td>
;;     <td><img src="./images/pdffiledone.png" alt="PDF Using This Package" width="62%" height="62%"></td>
;;   </tr>
;; </table>

;; Examples:
;;
;; * 5 examples from `view-hello-file' [as a PDF](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/view-hello-file-five.pdf) along with [the source Org-Mode file](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/view-hello-file-five.org)
;;
;; * Entire Emoticon block [as a PDF](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/Emoticons.pdf) along with [the source Org-Mode file](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/Emoticons.org)

;; Alternatives:
;;
;; **Before** you go any further with this package, **please** read
;; about the alternatives listed on [this post](https://tex.stackexchange.com/questions/21046/change-xetex-fonts-automatically-depending-on-unicode-blocks)
;;
;;
;; * [xecjk – Support for CJK documents in XeLaTeX](https://www.ctan.org/pkg/xecjk)
;;
;; * [fontwrap – Bind fonts to specific unicode blocks](https://www.ctan.org/pkg/fontwrap)
;;
;; * [polyglossia – An alternative to babel for XeLaTeX and LuaLaTeX](https://www.ctan.org/pkg/polyglossia)
;;
;; * [ucharclasses – sets up XeTeX character classes based on which  unicode block a character is found in](https://www.ctan.org/tex-archive/macros/xetex/latex/ucharclasses)
;;
;; This package duplicates `ucharclasses'. It was exactly what I wanted but I
;; couldn't get it working. It is also unmaintained. So I figured I would
;; write something to do the same thing using Org-Mode and Elisp.

;; Requirements And Compatibility:
;;
;; A LaTeX Distribution And Compiler
;;
;; * Compiler: XeTeX
;;
;; * Distribution: An OS Specific TeX Distribution
;;
;;       * Windows Uses [MiKTeX](https://miktex.org/) (I've used it and it is
;;         great) or [TeXLive on Windows](https://www.tug.org/texlive/windows.html)
;;
;;       * Linux: [TeXLive](https://www.tug.org/texlive/)
;;
;;       * macOS: [MacTeX](https://www.tug.org/mactex/) (I've used it
;;         and it is great)
;;
;; * Version
;;
;;       * You've got two considerations here: the version of the compiler
;;         and the version of the packages. When I setup this package, I used the
;;         2016 distribution and the packages that came with it. I haven't updated
;;         either the compiler or packages since then. My experience is that once
;;         you've got it working, don't upgrade anything until you absolutely need
;;         to. If you do need to update something then take a backup of your system
;;         first because things can unexpectedly change leaving you without
;;         a working system. Yes this is totally obvious, but you don't want
;;         to break your system when you are in the middle of writing and
;;         publishing. Instead break it later when you are willing to cleans
;;         things at ease.
;;
;; Org-Mode
;;
;;
;; * `Org mode version 9.x: yes'
;; * `Org mode version 8.x: yes'
;; * `Org mode version below 8.x: no (8.0 introduced the new exporter framework with which you would use this package)'
;;
;; Emacs
;;
;; * `GNU Emacs version 26.1: yes'
;;
;; * `GNU Emacs version 25.x: yes'
;;
;; * `GNU Emacs version 25.x and lower: no'

;; Installation:
;;
;; Download it to _~/src_.
;;
;; Add the following to your init file to
;;
;; * Add it to your load path
;;
;; * Load it
;;
;; * Add it to your Org-Mode hook
;;
;;     (add-to-list 'load-path "~/src/org-utf-to-xetex")
;;     (require 'org-utf-to-xetex)
;;     (add-hook 'org-mode-hook #'org-utf-to-xetex-prettify)

;; Verification:
;;
;; This package is probably working correctly when:
;;
;; * All of the tests pass
;;
;; * You've configured enough font blocks to cover the characters in your
;;   source document and they appear correctly in the PDF
;;
;; Here is how to run the tests:
;;
;; * Go to your command line
;;
;; * Verify that Emacs is in your path
;;
;; * Run: `emacs -batch -l ert -l ~/src/org-utf-to-xetex/org-utf-to-xetex.el -l ~/src/org-utf-to-xetex/org-utf-to-xetex-test.el -f ert-run-tests-batch-and-exit'
;;
;; The test report should say that all of the rests ran as expected.
;;
;; For example, if there are eight tests then all eight should pass as
;; expected. That number will probably change over time.
;;
;; `Ran 8 tests, 8 results as expected (2018-06-26 21:16:34-0500)'

;; Usage Notes:
;;
;; Character Support:
;;
;; This package assumes that 99% of your document uses [Latin Characters](https://en.wikipedia.org/wiki/List_of_languages_by_writing_system#Latin_script)
;; so this package doesn't specify a font for them—it totally ignores them. The LaTeX compiler will
;; use the `mainfont' that you specified, there is no need to look up a
;; font for their Unicode block.
;;
;; If you need to handle switching fonts for large blocks of text then read about
;; the [alternatives](#alternatives).
;;
;; Performance:
;;
;; Compiling the entire Emoticon block ([as a PDF](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/Emoticons.pdf) along with [the source Org-Mode file](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/Emoticons.org))
;; with or without this macro takes virtually the same amount of time. However
;; when I add characters that require nine other fonts compiles takes ten
;; times as long.
;;
;; Since only plan to use this for documents that are mostly Latin characters
;; I have not researched this any further.
;;
;; Unicode And You:
;;
;; Learning more about Unicode will serve you well beyond using this
;; package. Here are some fun ways to explore Unicode.
;;
;; * [Code Charts](https://www.unicode.org/charts/): Click on a code block and
;;   see the characters that live there. This is useful when you find the
;;   block for characters that you are not familiar with an you want to see
;;   what other characters are in there. Remember that you can use
;;   `org-utf-to-xetex-get-unicode-block-for-string' to get the block for any
;;   Non-Latin character. It was fun to see the APL Symbols in the [Miscellaneous Technical Block](https://www.unicode.org/charts/PDF/U2300.pdf).
;;
;; * [The Story Of A Unicode Emoji](https://unicode.org/emoji/slides.html) is ostensibly only about about Unicode Emoji
;;   but serves as a great introduction to just about every interesting aspect
;;   of Unicode.
;;
;; * The [unicode-fonts](https://github.com/rolandwalker/unicode-fonts)
;;   package configures Emacs with the font to use for each Unicode block. Its
;;   default configuration chooses good defaults so your job is only to
;;   install the fonts themselves. After you have found fonts that you like,
;;   you can use _this_ package to specify the same font for XeTeX, resulting in a
;;   "What You See Is What You Get" experience from Emacs to PDF.
;;
;; * Call the `view-hello-file' function to "Display the HELLO file, which
;;   lists languages and characters." This is a fun way to learn more about
;;   characters using `describe-char' and `org-utf-to-xetex-get-unicode-block-for-string-char-after'.
;;
;; Intended Users:
;;
;; If you are reading this then it is safe to say that you are an Org-Mode
;; user. Org-Mode makes it _so_ easy to create documents that you inevitably
;; want to use some Unicode characters directly instead of using
;; [Symbols](https://orgmode.org/worg/org-symbols.html). And that is how you
;; ran into this problem. You probably fit into one of the following profiles:
;;
;; * You are not a LaTeX and XeTeX user but you are willing to set up
;;   Org-Mode for both and get very comfortable with them
;;
;; * You are already a LaTeX and XeTeX user and have already set up Org-Mode
;;   for both. You _very_ comfortable with both.
;;
;; This guide is written for experienced Org-Mode, LaTeX, and XeTeX users. If
;; you aren't yet then please know that:
;;
;; * It is worth learning because you will use it for the rest of your life.
;;
;; * It is pretty easy to learn.
;;
;; Once you get comfortable with the tools then workflow for this package will
;; feel simple to you. Until you reach that point please take your time and
;; learn at your ownpace. You can see how my system works
;; [here](https://github.com/grettke/help/blob/master/.emacs.el) and you'll
;; find that it is pretty easy to follow. Don't hesitate to contact me with
;; any questions or concerns.

;; Public API Features:
;;
;; First play around with them. See what you can do with them.
;;
;; Second use them to configure your system.
;;
;; <table border="1">
;;   <caption>API</caption>
;;   <tr>
;;     <th>Goal</th>
;;     <th>Function</th>
;;     <th>Documentation</th>
;;   </tr>
;;   <tr>
;;     <td>What Unicode block does the character after the cursor live in?</td>
;;     <td>`org-utf-to-xetex-get-unicode-block-for-string-char-after'</td>
;;     <td>This is Unicode block name for this character.</td>
;;   </tr>
;;   <tr>
;;     <td>What Unicode block does this character live in?</td>
;;     <td>`org-utf-to-xetex-get-unicode-block-for-string', `str'</td>
;;     <td>This Unicode block name is used for the LaTeX fontcommands.</td>
;;   </tr>
;;   <tr>
;;     <td>Tell XeTeX about the Unicode block for some characters (so this package knows what font to use)</td>
;;     <td>`org-utf-to-xetex-string-to-xetex', `str'</td>
;;     <td>Provides a LaTeX string with the font environment you want</td>
;;   </tr>
;;   <tr>
;;     <td>Wrap some text with the package macro, or just insert it</td>
;;     <td>`org-utf-to-xetex-insert-or-wrap-with-macro'</td>
;;     <td>See goal</td>
;;   </tr>
;;   <tr>
;;     <td>Make the Org-Mode markup for this package easier to read</td>
;;     <td>`org-utf-to-xetex-prettify'</td>
;;     <td>Use `prettify-symbols-mode' and `org-hide-macro-markers' to hide parentheses. Add to `org-mode-hook'.</td>
;;   </tr>
;;   <tr>
;;     <td>Tell what fonts to use for what kinds of characters.</td>
;;     <td>`org-utf-to-xetex-command-for-every-block'</td>
;;     <td>Pop up a window with commands necessary for <i>every</i> Unicode block</td>
;;   </tr>
;; </table>

;; Workflow:
;;
;; Here are the steps to use this package starting from the top layer with
;; Org-Mode all the way up to the bottom layer with XeTeX.
;;
;; * In Org-Mode change the LaTeX compiler and engine to XeTeX. Force Org-Mode
;;   to produce PDFs. Use `latexmk' because it is easier. Like [this
;;   article](https://tex.stackexchange.com/questions/2984/frequently-loaded-packages-differences-between-pdflatex-and-xelatex)
;;   explains, XeTeX uses the `fontspec' package instead of `inputenc' or
;;   `fontenc' so add `("" "fontspec")' to `org-latex-packages-alist'. Now
;;   choose an existing document to use as your "test file". Compile it. It
;;   will probably compile just fine and if they don't it shouldn't take much
;;   effort to get things working right. Here are the settings that I used.
;;
;;       (setq org-latex-compiler "xelatex")
;;       (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
;;       (setq-default TeX-engine 'xetex)
;;       (setq-default TeX-PDF-mode t)
;;       (add-to-list 'org-latex-packages-alist '("" "fontspec"))
;;
;; * Install this package
;;
;; * Add Unicode to the test document. For example `A 我-⍋+☀APPLE🙋ZEBRA'
;;   Compile it. White boxes will appear for some of the characters you entered.
;;
;; * For every character rendered as a white box, wrap it in the macro from
;;   this package by select it and calling
;;   `org-utf-to-xetex-insert-or-wrap-with-macro'. It is
;;   fine to leave spaces and Latin characters inside of the macro call, they
;;   will be ignored by this package. This makes your text easier to read
;;   instead of breaking it character by character. This macro only runs when you
;;   use the LaTeX exporter with the XeTeX engine so it won't affect any of
;;   your other exporters.
;;
;;       {{{utf2xtx(😄 (Joy))}}}
;;
;; * Install the macro from this package using the `setupfile' line below.
;;   Paste it into your document. Position the cursor on that line and hit _C-c C-c_ so
;;   that Org-Mode will refresh it's setup. Now it can use the macro.
;;
;;       #+SETUPFILE: https://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/org-utf-to-xetex.setup
;;
;; * Identify the Unicode block for the character by again placing the cursor
;;   calling `org-utf-to-xetex-get-unicode-block-for-string-char-after'. The
;;   name of the Unicode block will appear in the Minibuffer and also
;;  `*Messages*'. This package ignores most Latin characters. So if you
;;   inspect a Latin character you will getting message explaining that this
;;   package ignores Latin characters. That means you have nothing more to do
;;   here. There is nothing that you need to do to configure a font for this
;;   character. However if this package cares about that character, then it will
;;   tell you its Unicode block name. Take note of it because you will use it
;;   later.
;;
;; * Find a font that XeTeX should use for rendering this character. An easy
;;   way to find one is to ask Emacs what font that _it is using_ for that
;;   character: Place your cursor on that character and _C-x_ `describe-char'.
;;
;; * Tell XeTeX what font to use for characters in this Unicode block. This
;;   package creates XeTeX commands to help you configure new fontcommands with
;;   the name of the Unicode block. They follow a standard format like you see
;;   in the example below. You can a buffer with commands for _every_ block
;;   name by calling _M-x_ `org-utf-to-xetex-command-for-every-block'.
;;   Find the Unicode block for your character and copy the _newfontfamily_ and
;;   _DeclareTextFontCommand_ commands.
;;
;;       % \newfontfamily\Emoticons{font}
;;       % \DeclareTextFontCommand{\textEmoticons}{\Emoticons}
;;
;; * You need a custom package in which to place these commands. At least,
;;   that is I did. Add these to your custom package and specify what font you
;;   decided to use. Here is an example from my configuration for the Emoticon block:
;;
;;       \newfontfamily\Emoticons{Symbola}
;;       \DeclareTextFontCommand{\textEmoticons}{\Emoticons}
;;
;; * At this point XeTeX should render your characters using the correct font.
;;   Verify this by exporting your document to a buffer calling
;;   _C-c C-e_ `l' RET _L_. Open that buffer and verify that your characters
;;   are wrapped with the correct font, it should like the sample below.
;;   Now export to a PDF and it should now render the characters correctly
;;   instead of using white boxes.
;;
;;       \textEmoticons{😄} (Joy)
;;
;; * This is what it takes to teach Org-Mode and XeTeX to use the
;;   correct font for your Unicode characters. If you got this far then please
;;   tell me what I can do better under this entire section.

;; Credits:
;;
;; * rolandwalker's [unicode-fonts](https://github.com/rolandwalker/unicode-fonts)
;;   Package showed how to utilize Unicode fonts in Emacs. Code showed what font
;;   blocks to ignore. Educational. Sweet. One of a kind package!

;; org-utf-to-xetex In Production:
;;
;; * Cyberdyne Systems
;;
;; * ENCOM
;;
;; * LexCorp
;;
;; * Setec Astronomy
;;
;; * Tyrell Corporation
;;
;; * Wayne Enterprises
;;
;; * Yoyodyne Propulsion Systems

;;; Code:

(require 'map)
(require 'cl-lib)

;; Constants

(defconst org-utf-to-xetex-blocks
  '(
    ;; ("Basic Latin" #x0000 #x007F) ; probably covered by your mainfont
    ;; ("Latin-1 Supplement" #x0080 #x00FF) ; probably covered by your mainfont
    ;; ("Latin Extended-A" #x0100 #x017F) ; probably covered by your mainfont
    ;; ("Latin Extended-B" #x0180 #x024F) ; probably covered by your mainfont
    ("IPA Extensions" #x0250 #x02AF)
    ;; ("Spacing Modifier Letters" #x02B0 #x02FF) ; probably covered by your mainfont
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
    ;; ("Latin Extended Additional" #x1E00 #x1EFF) ; probably covered by your mainfont
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
    ("Supplementary Private Use Area-B" #x100000 #x10FFFF)
    )
  "Alist of Unicode 11.0 blocks. See charts here URL `https://www.unicode.org/charts/'.")

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
  "Return true if STR is a single character string."
  (and str (= (length str) 1)))

(defun org-utf-to-xetex--char-to-block-def (str)
  "Return the Unicode block definition containing STR. If STR is not found, return nil."
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
  "Find the block containing STR. Return the font encoded LaTeX string for that block. On no match, or any error, return STR."
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
         (format "org-utf-to-xetex-command-for-every-block expected to create %s commands, but only created %s. This is probably a bug so report it." blocks
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
    (map-put prettify-symbols-alist org-utf-to-xetex-macro-name
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
                "This package doesn't handle this block. It is probably one of the Latin blocks that this package ignores. If it isn't then please submit an issue.")))
    (message "Unicode Block Name For '%s': %s" str msg)))

(defun org-utf-to-xetex-get-unicode-block-for-string-char-after ()
  "In what Unicode block does the character after the cursor live in? Answered here."
  (interactive)
  (if (char-after)
      (org-utf-to-xetex-get-unicode-block-for-string
       (format "%c" (char-after)))
    (message "First place the cursor on a char to inspect it.")))

(provide 'org-utf-to-xetex)

;;; org-utf-to-xetex.el ends here
