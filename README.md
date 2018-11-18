# org-utf-to-xetex - Org-Mode Print Unicode Characters Directly To PDF

*Author:* Grant Rettke <gcr@wisdomandwonder.com><br>
*Version:* 0.9<br>

## Read Me First

You wrote `Have a great day 😄` in your Org-Mode file. Then you export it
to PDF. Instead of seeing nice smiling face, you see a white box instead:
`Have a great day □`. If you are in this situation then you _might_ be
interested in reading on.

Org-Mode exports to many formats. Each format has different strengths. When
Org-Mode exports your document it needs to leverage the strengths of the
destination format by abstracting away the difference so that you can
specify the _intended_ character in Org-Mode's markup and let Org-Mode
choose the right one for your destination format. Org-Mode facilitates this
abstraction using it's [Symbols](https://orgmode.org/worg/org-symbols.html) mechanism. A great example here is the smiley face.

In Org-Mode the smiley face is represented by the string `\smiley`. When your
document is exported it gets converted to the correct representation for
it's destination. Here are some example conversions (language, output):

* LaTeX: `\smiley{}`
* HTML: `&#9786;`
* ASCII: `:-)`
* UTF-8: `☺`

You will get a smiley face in every export. This mechanism _should_ get you
what you want. For my computer, this wasn't enough.

On my computer I am pretty lazy in that I want to write a document once and
have it look pretty close to it's final version. I don't want to have to
use a Symbol for every Non-Latin character, and there aren't enough symbols
anyway. Using UTF-8 files makes it easy to use whatever character you want
so that is the right place to start.

List of characters supported is simple: you can use any Unicode character. Once
you find a good font, then it looks good as a text file just how you would
expect it. When you export to HTML, it just works. On my box though it
didn't just work for PDF. The solution is to switch to a different LaTeX
compiler, and it lets you generate PDFs just as easily as with every other
Org-Mode export: all of your characters show up correctly.

**Org-Mode makes it easy to use the same "Unicode Everywhere"** workflow by
switching from the PDFLaTeX compiler to the XeTeX compiler. In order to use
this package you must have already switched over to XeTeX. Most of us are
switching here _from_ PDFLaTeX.
**Once you make the switch to XeTeX**, _and_ configure your system using this package, your
PDFs print the same characters as your text files. If you are interested in
that workflow then you might be interested in this package.

## Overview

`org-utf-to-xetex` is a little package that teaches [Org-Mode](https://orgmode.org/) and [XeTeX](http://xetex.sourceforge.net/)
what fonts to use for a few of the Unicode characters in your document. This
is a _little_ package because most of the steps happen in your LaTeX
configuration. This package provides helpful functionality for setting up your
LaTeX compiler and most importantly integrating it with your Org LaTeX
exporter. The critical functionality though is provided by your LaTeX
compiler, as you will see in the [workflow steps below](#workflow). Read on
to see the empty white box problem and how this package resolves it.

You likely set up LaTeX with three font settings like this

* `\setmainfont{DejaVu Serif}`
* `\setsansfont{DejaVu Sans}[Scale=MatchLowercase]`
* `\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]`

That makes most of your PDF documents look great because 99% of
the characters that you use are [Latin](https://en.wikipedia.org/wiki/List_of_languages_by_writing_system#Latin_script) and your `mainfont` supports all of them.
The problem is the 1% that it doesn't, which are most likely Non-Latin characters.
Instead of your Unicode character that you expected to see, you see a white box. Here is an
example of the Org-Mode document, the intermediate LaTeX,  and resulting PDF

<table border="1">
  <caption>Workflow <i>Without</i> This Package</caption>
  <tr>
    <th>File In Workflow</th>
    <th>Screenshot</th>
  </tr>
  <tr>
    <td>Org-Mode Source</td>
    <td><img src="./images/orgfile.png" alt="Org-Mode Source File"></td>
  </tr>
  <tr>
    <td>LaTeX Source</td>
    <td><img src="./images/latexfile.png" alt="LaTeX Source File"></td>
  </tr>
  <tr>
    <td>Generated PDF</td>
    <td><img src="./images/pdffile.png" alt="PDF Without Using Package" width="62%" height="62%"></td>
  </tr>
</table>

That clearly isn't what you expected.

When your LaTeX compiler created the PDF, it used your mainfont. But that font
doesn't handle the Unicode character you wrote. Your PDF wants you to that
_it tried_ to show you something for that character but could not. It tells
 you by showing you an empty white box.

This is pretty common because fonts don't and _cannot_ cover all of the
Unicode symbols out there (there are too many). The solution is to specify a different font to
handle the characters that your main font doesn't know about.

This package teaches Org-Mode how to ask XeTeX to use a different font for
some characters.

Now your Org-Mode document and generated PDF should look something like
this

<table border="1">
  <caption>Workflow <i>With</i> This Package</caption>
  <tr>
    <th>File In Workflow</th>
    <th><Screenshot></th>
  </tr>
  <tr>
    <td>Org-Mode Source <i>Without</i> Prettification</td>
    <td><img src="./images/orgfiledonenotpretty.png" alt="Org-Mode Source File Without Any  Prettification"></td>
  </tr>
  <tr>
    <td>Org-Mode Source <i>With</i> Prettification</td>
    <td><img src="./images/orgfiledonepretty.png" alt="Org-Mode Source File With Prettification"></td>
  </tr>
  <tr>
    <td>LaTeX Source With Font Commands</td>
    <td><img src="./images/latexfiledone.png" alt="LaTeX Source With Font Commands"></td>
  </tr>
  <tr>
    <td>PDF With Correct Fonts</td>
    <td><img src="./images/pdffiledone.png" alt="PDF Using This Package" width="62%" height="62%"></td>
  </tr>
</table>

## Examples

* 5 examples from `view-hello-file` [as a PDF](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/view-hello-file-five.pdf) along with [the source Org-Mode file](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/view-hello-file-five.org)
* Entire Emoticon block [as a PDF](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/Emoticons.pdf) along with [the source Org-Mode file](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/Emoticons.org)

## Alternatives

**Before** you go any further with this package, **please** read
about the alternatives listed on [this post](https://tex.stackexchange.com/questions/21046/change-xetex-fonts-automatically-depending-on-unicode-blocks)
* [xecjk – Support for CJK documents in XeLaTeX](https://www.ctan.org/pkg/xecjk)
* [fontwrap – Bind fonts to specific unicode blocks](https://www.ctan.org/pkg/fontwrap)
* [polyglossia – An alternative to babel for XeLaTeX and LuaLaTeX](https://www.ctan.org/pkg/polyglossia)
* [ucharclasses – sets up XeTeX character classes based on which  unicode block a character is found in](https://www.ctan.org/tex-archive/macros/xetex/latex/ucharclasses)

This package duplicates `ucharclasses`. It was exactly what I wanted but I
couldn't get it working. It is also unmaintained. So I figured I would
write something to do the same thing using Org-Mode and Elisp.

## Requirements And Compatibility

A LaTeX Distribution And Compiler

* Compiler: XeTeX
* Distribution: An OS Specific TeX Distribution

      * Windows Uses [MiKTeX](https://miktex.org/) (I've used it and it is
        great) or [TeXLive on Windows](https://www.tug.org/texlive/windows.html)

      * Linux: [TeXLive](https://www.tug.org/texlive/)

      * macOS: [MacTeX](https://www.tug.org/mactex/) (I've used it
        and it is great)

* Version

      * You've got two considerations here: the version of the compiler
        and the version of the packages. When I setup this package, I used the
        2016 distribution and the packages that came with it. I haven't updated
        either the compiler or packages since then. My experience is that once
        you've got it working, don't upgrade anything until you absolutely need
        to. If you do need to update something then take a backup of your system
        first because things can unexpectedly change leaving you without
        a working system. Yes this is totally obvious, but you don't want
        to break your system when you are in the middle of writing and
        publishing. Instead break it later when you are willing to cleans
        things at ease.

Org-Mode

* `Org mode version 9.x: yes`
* `Org mode version 8.x: yes`
* `Org mode version below 8.x: no (8.0 introduced the new exporter framework with which you would use this package)`

Emacs

* `GNU Emacs version 26.1: yes`
* `GNU Emacs version 25.x: yes`
* `GNU Emacs version 25.x and lower: no`

## Installation

Download it to _~/src_.

Add the following to your init file to

* Add it to your load path
* Load it
* Add it to your Org-Mode hook

        (add-to-list 'load-path "~/src/org-utf-to-xetex")
        (require 'org-utf-to-xetex)
        (add-hook 'org-mode-hook #'org-utf-to-xetex-prettify)

## Verification

This package is working correctly when:

* All of the tests pass
* You've configured enough font blocks to cover the characters in your
  source document and they appear correctly in the PDF

Here is how to run the tests:

* Go to your command line
* Verify that Emacs is in your path
* Run: `emacs -batch -l ert -l ~/src/org-utf-to-xetex/org-utf-to-xetex.el -l ~/src/org-utf-to-xetex/org-utf-to-xetex-test.el -f ert-run-tests-batch-and-exit`

The test report should say that all of the rests ran as expected.

For example

`Ran 8 tests, 8 results as expected (2018-06-26 21:16:34-0500)`

## Usage Notes

### Character Support

This package assumes that 99% of your document uses [Latin Characters](https://en.wikipedia.org/wiki/List_of_languages_by_writing_system#Latin_script)
so this package doesn't specify a font for them—it totally ignores them. The LaTeX compiler will
use the `mainfont` that you specified, there is no need to look up a
font for their Unicode block.

If you need to handle switching fonts for large blocks of text then read about
the [alternatives](#alternatives).

### Performance

Compiling the entire Emoticon block ([as a PDF](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/Emoticons.pdf) along with [the source Org-Mode file](http://raw.githubusercontent.com/grettke/org-utf-to-xetex/master/samples/Emoticons.org))
with or without this macro takes virtually the same amount of time. However
when I add characters that require nine other fonts compiles takes ten
times as long.

Since only plan to use this for documents that are mostly Latin characters
I have not researched this any further.

### Unicode And You

Learning more about Unicode will serve you well beyond using this
package. Here are some fun ways to explore Unicode.

* [Code Charts](https://www.unicode.org/charts/): Click on a code block and
  see the characters that live there. This is useful when you find the
  block for characters that you are not familiar with an you want to see
  what other characters are in there. Remember that you can use
  `org-utf-to-xetex-get-unicode-block-for-string` to get the block for any
  Non-Latin character. It was fun to see the APL Symbols in the [Miscellaneous Technical Block](https://www.unicode.org/charts/PDF/U2300.pdf).
* [The Story Of A Unicode Emoji](https://unicode.org/emoji/slides.html) is ostensibly only about about Unicode Emoji
  but serves as a great introduction to just about every interesting aspect
  of Unicode.
* The [unicode-fonts](https://github.com/rolandwalker/unicode-fonts)
  package configures Emacs with the font to use for each Unicode block. Its
  default configuration chooses good defaults so your job is only to
  install the fonts themselves. After you have found fonts that you like,
  you can use _this_ package to specify the same font for XeTeX, resulting in a
  "What You See Is What You Get" experience from Emacs to PDF.
* Call the `view-hello-file` function to "Display the HELLO file, which
  lists languages and characters." This is a fun way to learn more about
  characters using `describe-char` and `org-utf-to-xetex-get-unicode-block-for-string-char-after`.

### Intended Users

If you are reading this then it is safe to say that you are an Org-Mode
user. Org-Mode makes it _so_ easy to create documents that you inevitably
want to use some Unicode characters directly instead of using
[Symbols](https://orgmode.org/worg/org-symbols.html). And that is how you ran into this problem. You doubtless fit into one of the following profiles:

* You are not a LaTeX and XeTeX user but you are willing to set up
  Org-Mode for both and get very comfortable with them
* You are already a LaTeX and XeTeX user and have already set up Org-Mode
  for both. You are _very_ comfortable with both.

This guide is written for experienced Org-Mode, LaTeX, and XeTeX users. If
you aren't yet then please know that:

* It is worth learning because you will use it for the rest of your life.
* It is pretty easy to learn.

Once you get comfortable with the tools then the workflow for this package will
feel simple to you. Until you reach that point please take your time and
learn at your own pace. You can see how my system is setup
[here](https://github.com/grettke/help/blob/master/.emacs.el) and you'll find that it is pretty easy to follow. Don't hesitate to contact me with
any questions or concerns.

## Public API Features

First play around with them. See what you can do with them.

Second use them to configure your system.

<table border="1">
  <caption>API</caption>
  <tr>
    <th>Goal</th>
    <th>Function</th>
    <th>Documentation</th>
  </tr>
  <tr>
    <td>What Unicode block does the character after the cursor live in?</td>
    <td>`org-utf-to-xetex-get-unicode-block-for-string-char-after`</td>
    <td>This is Unicode block name for this character.</td>
  </tr>
  <tr>
    <td>What Unicode block does this character live in?</td>
    <td>`org-utf-to-xetex-get-unicode-block-for-string`, `str`</td>
    <td>This Unicode block name is used for the LaTeX fontcommands.</td>
  </tr>
  <tr>
    <td>Tell XeTeX about the Unicode block for some characters (so this package knows what font to use)</td>
    <td>`org-utf-to-xetex-string-to-xetex`, `str`</td>
    <td>Provides a LaTeX string with the font environment you want</td>
  </tr>
  <tr>
    <td>Wrap some text with the package macro, or just insert it</td>
    <td>`org-utf-to-xetex-insert-or-wrap-with-macro`</td>
    <td>See goal</td>
  </tr>
  <tr>
    <td>Make the Org-Mode markup for this package easier to read</td>
    <td>`org-utf-to-xetex-prettify`</td>
    <td>Use `prettify-symbols-mode` and `org-hide-macro-markers` to hide parentheses. Add to `org-mode-hook`.</td>
  </tr>
  <tr>
    <td>Tell what fonts to use for what kinds of characters.</td>
    <td>`org-utf-to-xetex-command-for-every-block`</td>
    <td>Pop up a window with commands necessary for <i>every</i> Unicode block</td>
  </tr>
</table>

## Workflow

Here are the steps to use this package starting from the top layer with
Org-Mode all the way up to the bottom layer with XeTeX.

* In Org-Mode change the LaTeX compiler and engine to XeTeX. Force Org-Mode
  to produce PDFs. Use `latexmk` because it is easier. Like [this
  article](https://tex.stackexchange.com/questions/2984/frequently-loaded-packages-differences-between-pdflatex-and-xelatex)
  explains, XeTeX uses the `fontspec` package instead of `inputenc` or
  `fontenc` so add `("" "fontspec")` to `org-latex-packages-alist`. Now
  choose an existing document to use as your test file. Compile it. It
  will compile just fine and if it doesn't then it won't take much
  effort to get things working correctly. Here are the settings that I used.

          (setq org-latex-compiler "xelatex")
          (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
          (setq-default TeX-engine 'xetex)
          (setq-default TeX-PDF-mode t)
          (add-to-list 'org-latex-packages-alist '("" "fontspec"))

* Install this package
* Add Unicode to the test document. For example `A 我-⍋+☀APPLE🙋ZEBRA`
  Compile it. White boxes will appear for some of the characters you entered.
* For every character rendered as a white box, wrap it in the macro from
  this package by select it and calling
  `org-utf-to-xetex-insert-or-wrap-with-macro`. It is
  fine to leave spaces and Latin characters inside of the macro call, they
  will be ignored by this package. This makes your text easier to read
  instead of breaking it character by character. This macro only runs when you
  use the LaTeX exporter with the XeTeX engine so it won't affect any of
  your other exporters.

* Install the macro from this package using the `org-utf-to-xetex-insert-setup-file-line` function
  Position the cursor anywhere at the top of the document.  Call
  `org-utf-to-xetex-insert-setup-file-line`. With the cursor on that line and hit _C-c C-c_ so
  that Org-Mode will refresh it's setup. Now it can use the macro
* Identify the Unicode block for the character by again placing the cursor
  calling `org-utf-to-xetex-get-unicode-block-for-string-char-after`. The
  name of the Unicode block will appear in the Minibuffer and also
 `*Messages*`. This package ignores most Latin characters. So if you
  inspect a Latin character you will getting message explaining that this
  package ignores Latin characters. That means you have nothing more to do
  here. There is nothing that you need to do to configure a font for this
  character. However if this package cares about that character, then it will
  tell you its Unicode block name. Take note of it because you will use it
  later.
* Find a font that XeTeX should use for rendering this character. An easy
  way to find one is to ask Emacs what font that _it is using_ for that
  character: Place your cursor on that character and _C-x_ `describe-char`.
* Tell XeTeX what font to use for characters in this Unicode block. This
  package creates XeTeX commands to help you configure new fontcommands with
  the name of the Unicode block. They follow a standard format like you see
  in the example below. You can create a buffer with commands for _every_ block
  name by calling _M-x_ `org-utf-to-xetex-command-for-every-block`.
  Find the Unicode block for your character and copy the _newfontfamily_ and
  _DeclareTextFontCommand_ commands.

      % \newfontfamily\Emoticons{font}
      % \DeclareTextFontCommand{\textEmoticons}{\Emoticons}

* You need a custom package in which to place these commands. At least,
  that is what I did. Add these to your custom package and specify what font you
  decided to use. Here is an example from my configuration for the Emoticon block:

      \newfontfamily\Emoticons{Symbola}
      \DeclareTextFontCommand{\textEmoticons}{\Emoticons}

* At this point XeTeX should render your characters using the correct font.
  Open that buffer and verify that your characters are wrapped with the
  correct font, it should like the sample below. Verify this by exporting your document to a buffer calling
  _C-c C-e_ _l_ _L_. The should now render the characters correctly
  instead of using white boxes.

      \textEmoticons{😄} (Joy)

* This is what it takes to teach Org-Mode and XeTeX to use the
  correct font for your Unicode characters. If you got this far then please
  tell me what I can do better under this entire section.

## Credits

* rolandwalker's [unicode-fonts](https://github.com/rolandwalker/unicode-fonts)
  Package showed how to utilize Unicode fonts in Emacs. Code showed what font
  blocks to ignore. Educational. Sweet. One of a kind package!

## org-utf-to-xetex In Production

* Cyberdyne Systems
* ENCOM
* LexCorp
* Protovision
* Setec Astronomy
* Tyrell Corporation
* Wayne Enterprises
* Yoyodyne Propulsion Systems


---
Converted from `org-utf-to-xetex.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
