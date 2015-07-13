module Prettier (

  -- pretty printer and document type
  Doc, pPrint, pretty,

  -- basic document combinators
  empty, isEmpty, text, linesep, line, linebreak, softline, softbreak, group,

  -- alignment combinators
  nest, hang, align, indent,

  -- composition combinators
  combineWith, (<>), (<+>), ($$), (<$+>), (</>), (<$$>), (<//>),

  -- list combinators
  compose, hsep, vsep, vsepBlank, fillSep, sep, hcat,
  vcat, fillCat, cat, punctuate, encloseSep, fillEncloseSep,
  list, set, tupled, semiBraces,

  -- bracketing combinators
  enclose, squotes, dquotes, bquotes, parens,
  parensIf, angles, braces, brackets,

  -- fillers
  fill, fillBreak,

  -- primitive type documents
  char, string, int, float,

  -- character documents
  lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
  squote, dquote, bquote, semi, colon, comma, space, dot, backslash, equals,
  larrow, rarrow, doubleArrow, doubleColon, bar, at, tilde

  ) where

import Dequeue as Q (Queue, empty, cons, matchHead, matchLast)

infixl 1 <>, <+>, $$, </>, <$$>, <//>

--- Standard printing with a column length of 80.
pPrint :: Doc -> String
pPrint = pretty 80

--- The abstract data type Doc represents pretty documents.
data Doc
  = Empty
  | Text    Int String
  | Line    Int String
  | Combine Doc Doc
  | Group   Doc
  | Nest    Int Doc
  | Align   Int Doc

--------------------------------------------------------------------------------
-- basic document combinators
--------------------------------------------------------------------------------

--- The empty document
empty :: Doc
empty = Empty

--- Is the document empty?
isEmpty :: Doc -> Bool
isEmpty d = case d of Empty -> True
                      _     -> False

--- The document `(text s)` contains the literal string `s`.
--- The string shouldn't contain any newline ('\n') characters.
--- If the string contains newline characters,
--- the function `string` should be used.
--- @param s - a string without newline (`'\n'`) characters
--- @return a document which contains the literal string
text :: String -> Doc
text s = Text (length s) s

--- The document `(linesep s)` advances to the next line and indents
--- to the current nesting level. Document `(linesep s)`
--- behaves like `(text s)` if the line break is undone by `group`.
--- @param s - a string
--- @return a document which advances to the next line or behaves like `(text s)`
linesep :: String -> Doc
linesep s = Line (length s) s


--- The `line` document advances to the next line and indents to the current
--- nesting level. Document `line` behaves like `(text " ")` if the line break
--- is undone by `group`.
--- @return a document which advances to the next line or behaves
---         like `(text " ")`
line :: Doc
line = linesep " "

--- The `linebreak` document advances to the next line and indents to
--- the current nesting level. Document `linebreak` behaves like `empty`
--- if the line break is undone by `group`.
--- @return a document which advances to the next line or behaves like
---         `(text "")`
linebreak :: Doc
linebreak = linesep ""

--- The document `softline` behaves like `space` if the resulting output
--- fits the page, otherwise it behaves like `line`.<br><br>
--- `softline  = group line`
--- @return a document which behaves like `space` or `line`
softline :: Doc
softline = group line

--- The document `softbreak` behaves like `empty` if the resulting output
--- fits the page, otherwise it behaves like `line`.<br><br>
--- `softbreak  = group linebreak`
--- @return a document which behaves like `empty` or `line`
softbreak :: Doc
softbreak = group linebreak

--- The group combinator is used to specify alternative layouts.
--- The document `(group x)` undoes all line breaks in document `x`.
--- The resulting line is added to the current line if that fits the page.
--- Otherwise, the document `x` is rendered without any changes.
--- @param d - a document
--- @return document d without line breaks if that fits the page.
group :: Doc -> Doc
group = Group

--------------------------------------------------------------------------------
-- alignment combinators
--------------------------------------------------------------------------------

--- The document `(nest i d)` renders document `d` with the current
--- indentation level increased by `i` (See also `hang`,
--- `align` and `indent`).
---
---     nest 2 (text "hello" <$> text "world") <$> text "!"
---
--- outputs as:
---
---     hello
---       world
---     !
---
--- @param i - an integer which increases the indentation level
--- @param d - a document
--- @return document d with an indentation level increased by i
nest :: Int -> Doc -> Doc
nest = Nest

--- The hang combinator implements hanging indentation.
--- The document `(hang i d)` renders document `d` with a nesting level set
--- to the current column plus `i`. The following example uses hanging
--- indentation for some text:
---
---     test = hang 4
---              (fillSep
---                 (map text
---                      (words "the hang combinator indents these words !")))
---
--- Which lays out on a page with a width of 20 characters as:
---
---     the hang combinator
---         indents these
---         words !
---
--- The hang combinator is implemented as:
---
---     hang i x  = align (nest i x)
---
--- @param i - an integer which increases the indentation level
--- @param d - a document
--- @return document d with an indentation level set to the current column plus i
hang :: Int -> Doc -> Doc
hang i d = align (nest i d)

--- The document `(align d)` renders document `d` with the nesting level
--- set to the current column. It is used for example to implement `hang`.
---
--- As an example, we will put a document right above another one,
--- regardless of the current nesting level:
---
---     x $$ y  = align (x <$> y)
---     test    = text "hi" <+> (text "nice" $$ text "world")
---
--- which will be layed out as:
---
---     hi nice
---        world
---
--- @param d - a document
--- @return document d with the nesting level set to the current column
align :: Doc -> Doc
align = Align 0

--- The document `(indent i d)` indents document `d` with `i` spaces.
---
---     test  = indent 4 (fillSep (map text
---             (words "the indent combinator indents these words !")))
---
--- Which lays out with a page width of 20 as:
---
---     the indent
---     combinator
---     indents these
---     words !
---
--- @param i - an integer which increases the indentation level
--- @param d - a document
--- @return document d with an indentation level set to the current column plus i
indent :: Int -> Doc -> Doc
indent i d = hang i (spaces i <> d)

--------------------------------------------------------------------------------
-- composition combinators
--------------------------------------------------------------------------------

--- The document (combineWith c d1 d2) concatenates documents d1 and d2 with c
--- in between, but with identity empty.
--- Thus, the following equations hold:
---
---     combineWith c d1    empty == d1
---     combineWith c empty d2    == d2
---     combineWith c d1    d2    == d1 <> c <> d2 if neither d1 nor d2 are empty
---
--- @param c  - combining document
--- @param d1 - the first document
--- @param d2 - the second document
--- @return concatenation of d1 and d2 with c in between unless one
---         of the documents is empty
combineWith :: Doc -> Doc -> Doc -> Doc
combineWith c d1 d2 | isEmpty d1 = d2
                    | isEmpty d2 = d1
                    | otherwise  = d1 <> c <> d2

--- The document `(x <> y)` concatenates document `x` and document `y`.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y without seperator
(<>) :: Doc -> Doc -> Doc
(<>) = Combine

--- The document `(x <+> y)` concatenates document `x` and `y` with a
--- `space` in between with `empty` as identity.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `space` in between
(<+>) :: Doc -> Doc -> Doc
(<+>) = combineWith space

--- The document `(x $$ y)` concatenates document `x` and `y` with a
--- `line` in between with `empty` as identity.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `line` in between
($$) :: Doc -> Doc -> Doc
($$) = combineWith line

--- The document `(x <$+> y)` concatenates document `x` and `y` with a
--- blank line in between with `empty` as identity.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `line` in between
(<$+>) :: Doc -> Doc -> Doc
(<$+>) = combineWith (line <> linebreak)

--- The document `(x </> y)` concatenates document `x` and `y` with
--- a `softline` in between with `empty` as identity.
--- This effectively puts `x` and `y` either  next to each other
--- (with a `space` in between) or underneath each other.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `softline` in between
(</>) :: Doc -> Doc -> Doc
(</>) = combineWith softline

--- The document `(x <$$> y)` concatenates document `x` and `y` with a
--- `linebreak` in between with `empty` as identity.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `linebreak` in between
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = combineWith linebreak

--- The document `(x <//> y)` concatenates document `x` and `y` with a
--- `softbreak` in between with `empty` as identity.
--- This effectively puts `x` and `y` either right next to each other
--- or underneath each other.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `softbreak` in between
(<//>) :: Doc -> Doc -> Doc
(<//>) = combineWith softbreak

--------------------------------------------------------------------------------
-- list combinators
--------------------------------------------------------------------------------

--- The document `(compose f xs)` concatenates all documents `xs`
--- with function `f`.
--- Function `f` should be like `(<+>)`, `(<$>)` and so on.
--- @param f - a combiner function
--- @param xs - a list of documents
--- @return concatenation of documents
compose :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
compose _ []        = empty
compose op ds@(_:_) = foldr1 op ds -- no seperator at the end

--- The document `(hsep xs)` concatenates all documents `xs`
--- horizontally with `(<+>)`.
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
hsep :: [Doc] -> Doc
hsep = compose (<+>)

--- The document `(vsep xs)` concatenates all documents `xs` vertically with
--- `($$)`. If a group undoes the line breaks inserted by `vsep`,
--- all documents are separated with a `space`.
---
---     someText = map text (words ("text to lay out"))
---     test     = text "some" <+> vsep someText
---
--- This is layed out as:
---
---     some text
---     to
---     lay
---     out
---
--- The `align` combinator can be used to align the documents
--- under their first element:
---
---     test     = text "some" <+> align (vsep someText)
---
--- This is printed as:
---
---     some text
---          to
---          lay
---          out
---
--- @param xs - a list of documents
--- @return vertical concatenation of documents
vsep :: [Doc] -> Doc
vsep = compose ($$)

--- The document `(vsepBlank xs)` concatenates all documents `xs` vertically with
--- `(<$+>)`. If a group undoes the line breaks inserted by `vsepBlank`,
--- all documents are separated with a `space`.
--- @param xs - a list of documents
--- @return vertical concatenation of documents
vsepBlank :: [Doc] -> Doc
vsepBlank = compose (<$+>)

--- The document `(fillSep xs)` concatenates documents `xs` horizontally with
--- `(<+>)` as long as its fits the page, then inserts a
--- `line` and continues doing that for all documents in xs.<br><br>
--- `fillSep xs  = foldr (</>) empty xs`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
fillSep :: [Doc] -> Doc
fillSep = compose (</>)

--- The document `(sep xs)` concatenates all documents `xs` either horizontally
--- with `(<+>)`, if it fits the page, or vertically
--- with `($$)`.<br><br>
--- `sep xs  = group (vsep xs)`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents, if it fits the page,
--- or vertical concatenation else
sep :: [Doc] -> Doc
sep = group . vsep

--- The document `(hcat xs)` concatenates all documents `xs` horizontally
--- with `(<>)`.
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
hcat :: [Doc] -> Doc
hcat = compose (<>)

--- The document `(vcat xs)` concatenates all documents `xs` vertically
--- with `(<$$>)`. If a group undoes the line
--- breaks inserted by `vcat`, all documents are directly concatenated.
--- @param xs - a list of documents
--- @return vertical concatenation of documents
vcat :: [Doc] -> Doc
vcat = compose (<$$>)

--- The document `(fillCat xs)` concatenates documents `xs` horizontally
--- with `(<>)` as long as its fits the page, than inserts
--- a `linebreak` and continues doing that for all documents in `xs`.
--- <br><br>
--- `fillCat xs  = foldr (<//>) empty xs`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
fillCat :: [Doc] -> Doc
fillCat = compose (<//>)

--- The document `(cat xs)` concatenates all documents `xs` either horizontally
--- with `(<>)`, if it fits the page, or vertically with `(<$$>)`.<br><br>
--- `cat xs  = group (vcat xs)`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
cat :: [Doc] -> Doc
cat = group . vcat

--- `(punctuate p xs)` concatenates all documents `xs` with document `p` except
--- for the last document.
---
---     someText = map text ["words","in","a","tuple"]
---     test     = parens (align (cat (punctuate comma someText)))
---
--- This is layed out on a page width of 20 as:
---
---     (words,in,a,tuple)
---
--- But when the page width is 15, it is layed out as:
---
---     (words,
---      in,
---      a,
---      tuple)
---
--- (If you want put the commas in front of their elements instead of at the
--- end, you should use `tupled` or, in general,
--- `encloseSep`.)
--- @param p - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of documents with p in between
punctuate :: Doc -> [Doc] -> [Doc]
punctuate d ds = go ds
 where
  go []           = []
  go [x]          = [x]
  go (x:xs@(_:_)) = (x <> d) : go xs

--- The document `(enclose l r x)` encloses document `x` between
--- documents `l` and `r` using `(<>)`.<br><br>
--- `enclose l r x   = l <> x <> r`
--- @param l - the left document
--- @param r - the right document
--- @param x - the middle document
--- @return concatenation of l, x and r
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r x = l <> x <> r

--- The document `(encloseSep l r sep xs)` concatenates the documents `xs`
--- seperated by `sep` and encloses the resulting document by `l` and `r`.<br>
--- The documents are rendered horizontally if that fits the page. Otherwise
--- they are aligned vertically. All seperators are put in front of the
--- elements.
---
--- For example, the combinator `list` can be defined with `encloseSep`:
---
---     list xs  = encloseSep lbracket rbracket comma xs
---     test     = text "list" <+> (list (map int [10,200,3000]))
---
--- Which is layed out with a page width of 20 as:
---
---     list [10,200,3000]
---
--- But when the page width is 15, it is layed out as:
---
---     list [10
---          ,200
---          ,3000]
---
--- @param l   - left document
--- @param r   - right document
--- @param sep - a document as seperator
--- @param xs  - a list of documents
--- @return concatenation of l, xs (with sep in between) and r
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep l r _ [] = l <> r
encloseSep l r s (d:ds) = align (enclose l r (cat (d:map (s<>) ds)))

--- The document `(fillEncloseSep l r sep xs)` concatenates the documents `xs`
--- seperated by `sep` and encloses the resulting document by `l` and `r`.
---
--- The documents are rendered horizontally if that fits the page.
--- Otherwise they are aligned vertically.
--- All seperators are put in front of the elements.
--- @param l - left document
--- @param r - right document
--- @param sep - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of l, xs (with p in between) and r
fillEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
fillEncloseSep l r _ [] = l <> r
fillEncloseSep l r s (d:ds)
  = align (enclose l r (hcat (d:withSoftBreaks (map (s<>) ds))))
 where
  withSoftBreaks []  = []
  withSoftBreaks [x] = [group (linebreak <> x)]
  withSoftBreaks (x:xs@(_:_))
    = (group (linebreak <> (group (x <> linebreak))) : withSoftBreaks xs)


--- The document `(list xs)` comma seperates the documents `xs` and encloses
--- them in square brackets. The documents are rendered horizontally if
--- that fits the page. Otherwise they are aligned vertically.
--- All comma seperators are put in front of the elements.
--- @param xs - a list of documents
--- @return comma seperated documents xs and enclosed in square brackets
list :: [Doc] -> Doc
list = fillEncloseSep lbracket rbracket comma

--- The document `(set xs)` comma seperates the documents `xs` and encloses
--- them in braces. The documents are rendered horizontally if
--- that fits the page. Otherwise they are aligned vertically.
--- All comma seperators are put in front of the elements.
--- @param xs - a list of documents
--- @return comma seperated documents xs and enclosed in braces
set :: [Doc] -> Doc
set = fillEncloseSep lbrace rbrace comma

--- The document `(tupled xs)` comma seperates the documents `xs` and encloses
--- them in parenthesis. The documents are rendered horizontally if that fits
--- the page. Otherwise they are aligned vertically.
--- All comma seperators are put in front of the elements.
--- @param xs - a list of documents
--- @return comma seperated documents xs and enclosed
--- in parenthesis
tupled :: [Doc] -> Doc
tupled = fillEncloseSep lparen rparen comma

--- The document `(semiBraces xs)` seperates the documents `xs` with semi colons
--- and encloses them in braces. The documents are rendered horizontally
--- if that fits the page. Otherwise they are aligned vertically.
--- All semi colons are put in front of the elements.
--- @param xs - a list of documents
--- @return documents xs seperated with semi colons and enclosed in braces
semiBraces :: [Doc] -> Doc
semiBraces = fillEncloseSep lbrace rbrace semi

--------------------------------------------------------------------------------
-- bracketing combinators
--------------------------------------------------------------------------------

--- Document `(squotes x)` encloses document `x` with single quotes `"'"`.
--- @param x - a document
--- @return document x enclosed by single quotes
squotes :: Doc -> Doc
squotes = enclose squote squote

--- Document `(dquotes x)` encloses document `x` with double quotes.
--- @param x - a document
--- @return document x enclosed by double quotes
dquotes :: Doc -> Doc
dquotes = enclose dquote dquote

--- Document `(bquotes x)` encloses document `x` with back quotes `"\`"`.
--- @param x - a document
--- @return document x enclosed by `\`` quotes
bquotes  :: Doc -> Doc
bquotes = enclose bquote bquote

--- Document `(parens x)` encloses document `x` in parenthesis,
--- `"("` and `")"`.
--- @param x - a document
--- @return document x enclosed in parenthesis
parens :: Doc -> Doc
parens = enclose lparen rparen

--- Document `(parens x)` encloses document `x` in parenthesis,`"("` and `")"`,
--- iff the condition is true.
--- @param x - a document
--- @return document x enclosed in parenthesis iff the condition is true
parensIf :: Bool -> Doc -> Doc
parensIf b s = if b then parens s else s

--- Document `(angles x)` encloses document `x` in angles,
--- `"<"` and `">"`.
--- @param x - a document
--- @return document x enclosed in angles
angles :: Doc -> Doc
angles = enclose langle rangle

--- Document `(braces x)` encloses document `x` in braces,
--- `"{"` and `"}"`.
--- @param x - a document
--- @return document x enclosed in braces
braces :: Doc -> Doc
braces = enclose lbrace rbrace

--- Document `(brackets x)` encloses document `x` in square brackets,
--- `"["` and `"]"`.
--- @param x - a document
--- @return document x enclosed in square brackets
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

--------------------------------------------------------------------------------
-- primitive type documents
--------------------------------------------------------------------------------

--- The document `(char c)` contains the literal character `c`.
--- The character should not be a newline (`\n`),
--- the function `line` should be used for line breaks.
--- @param c - a character
--- @return a document which contains the literal character c
char :: Char -> Doc
char c = text [c]

--- The document `(string s)` concatenates all characters in `s` using
--- `line` for newline characters and `char` for all
--- other characters. It is used instead of `text` whenever the
--- text contains newline characters.
--- @param s - a string
--- @return a document which contains the string s
string :: String -> Doc
string = hcat . map (\c -> if elem c ['\n','\r'] then line else char c)

--- The document `(int i)` shows the literal integer `i` using `text`.
--- @param i - an integer
--- @return a document which contains the integer i
int :: Int -> Doc
int n = text (show n)

--- The document `(float f)` shows the literal float `f` using `text`.
--- @param f - a float
--- @return a document which contains the float f
float :: Float -> Doc
float x = text (show x)

--------------------------------------------------------------------------------
-- character documents
--------------------------------------------------------------------------------

--- The document `lparen` contains a left parenthesis, `"("`.
--- @return a document which contains a left parenthesis
lparen :: Doc
lparen = char '('

--- The document `rparen` contains a right parenthesis, `")"`.
--- @return a document which contains a right parenthesis
rparen :: Doc
rparen = char ')'

--- The document `langle` contains a left angle, `"<"`.
--- @return a document which contains a left angle
langle :: Doc
langle = char '<'

--- The document `rangle` contains a right angle, `">"`.
--- @return a document which contains a right angle
rangle :: Doc
rangle = char '>'

--- The document `lbrace` contains a left brace, `"{"`.
--- @return a document which contains a left brace
lbrace :: Doc
lbrace = char '{'

--- The document `rbrace` contains a right brace, `"}"`.
--- @return a document which contains a right brace
rbrace :: Doc
rbrace = char '}'

--- The document `lbracket` contains a left square bracket, `"["`.
--- @return a document which contains a left square bracket
lbracket :: Doc
lbracket = char '['

--- The document `rbracket` contains a right square bracket, `"]"`.
--- @return a document which contains a right square bracket
rbracket :: Doc
rbracket = char ']'

--- The document `squote` contains a single quote, `"'"`.
--- @return a document which contains a single quote
squote :: Doc
squote = char '\''

--- The document `dquote` contains a double quote.
--- @return a document which contains a double quote
dquote :: Doc
dquote = char '\"'

--- The document `bquote` contains a `'`'` quote.
--- @return a document which contains a `'`'` quote
bquote :: Doc
bquote = char '`'

--- The document `semi` contains a semi colon, `";"`.
--- @return a document which contains a semi colon
semi :: Doc
semi = char ';'

--- The document `colon` contains a colon, `":"`.
--- @return a document which contains a colon
colon :: Doc
colon = char ':'

--- The document `comma` contains a comma, `","`.
--- @return a document which contains a comma
comma :: Doc
comma = char ','

--- The document `space` contains a single space, `" "`.
---
---     x <+> y   = x <> space <> y
---
--- @return a document which contains a single space
space :: Doc
space = char ' '

--- The document (spaces n) contains n spaces, when n is greater than 0.
--- Otherwise the document is empty.
---
--- @return a document which contains n spaces or the empty document,
---  if n <= 0
spaces :: Int -> Doc
spaces n | n <= 0    = empty
         | otherwise = text $ replicate n ' '

--- The document `dot` contains a single dot, `"."`.
--- @return a document which contains a single dot
dot :: Doc
dot = char '.'

--- The document `backslash` contains a back slash, `"\\"`.
--- @return a document which contains a back slash
backslash :: Doc
backslash = char '\\'

--- The document `equals` contains an equal sign, `"="`.
--- @return a document which contains an equal
equals :: Doc
equals = char '='

--- The document `larrow` contains a left arrow sign, `"<-"`.
--- @return a document which contains a left arrow sign
larrow :: Doc
larrow = text "<-"

--- The document `rarrow` contains a right arrow sign, `"->"`.
--- @return a document which contains a right arrow sign
rarrow :: Doc
rarrow = text "->"

--- The document `doubleArrow` contains a double arrow sign, `"=>"`.
--- @return a document which contains a double arrow sign
doubleArrow :: Doc
doubleArrow = text "=>"

--- The document `doubleColon` contains a double colon sign, `"::"`.
--- @return a document which contains a double colon sign
doubleColon :: Doc
doubleColon = text "::"

--- The document `bar` contains a vertical bar sign, `"|"`.
--- @return a document which contains a vertical bar sign
bar :: Doc
bar = char '|'

--- The document `at` contains an at sign, `"@"`.
--- @return a document which contains an at sign
at :: Doc
at = char '@'

--- The document `tilde` contains a tilde sign, `"~"`.
--- @return a document which contains a tilde sign
tilde :: Doc
tilde = char '~'

--------------------------------------------------------------------------------
-- fillers
--------------------------------------------------------------------------------

--- The document `(fillBreak i d)` first renders document `d`. It
--- than appends `space`s until the width is equal to `i`. If the
--- width of `d` is already larger than `i`, the nesting level is
--- increased by `i` and a `line` is appended. When we redefine `ptype`
--- in the previous example to use `fillBreak`, we get a useful
--- variation of the previous output:
---
---     ptype (name,tp)
---          = fillBreak 6 (text name) <+> text "::" <+> text tp
---
--- The output will now be:
---
---     let empty  :: Doc
---         nest   :: Int -> Doc -> Doc
---         linebreak
---                :: Doc
---
fillBreak :: Int -> Doc -> Doc
fillBreak i d = d <> fill'
  where w     = docLength d
        fill' = if w >= i then nest i linebreak
                          else spaces (i - w)

--- The document `(fill i d)` renders document `d`. It than appends
--- `space`s until the width is equal to `i`. If the width of `d` is
--- already larger, nothing is appended. This combinator is quite
--- useful in practice to output a list of bindings. The following
--- example demonstrates this.
---
---     types  = [("empty","Doc")
---              ,("nest","Int -> Doc -> Doc")
---              ,("linebreak","Doc")]
---
---     ptype (name,tp)
---            = fill 6 (text name) <+> text "::" <+> text tp
---
---     test   = text "let" <+> align (vcat (map ptype types))
---
--- Which is layed out as:
---
---     let empty  :: Doc
---         nest   :: Int -> Doc -> Doc
---         linebreak :: Doc
---
fill :: Int -> Doc -> Doc
fill i d = d <> fill'
  where w     = docLength d
        fill' = if w >= i then empty else spaces (i - w)

--------------------------------------------------------------------------------
-- Implementation
--------------------------------------------------------------------------------

--- `(pretty w d)` pretty prints document `d` with a page width of `w` characters
--- @param w - width of page
--- @param d - a document
--- @return pretty printed document
pretty :: Width -> Doc -> String
pretty width d =
  interpret (normalise d) width (\pos qs r i -> "") 0 Q.empty width 0

type Width       = Int
type Position    = Int
type Indentation = Int 
type Horizontal  = Bool
type Remaining   = Int
-- indentation needed here because of align combinator
type Out         = Remaining -> Indentation -> String  
type OutGroup    = Horizontal -> Out -> Out
type TreeCont    = Position -> Q.Queue (Position, OutGroup) -> Out

-- semantic-preserving transformation that ensures that between every end
-- of group and a subsequent line there is no text
normalise :: Doc -> Doc
normalise doc = Combine tdoc sdoc
 where
  (tdoc, sdoc) = norm doc Empty

  -- Assume second argument only built from text, empty and <>.
  -- Ensures first component of result built only from text, empty and <>.
  -- norm doc tt = (tdoc,sdoc) implies doc <> tt and tdoc <> sdoc denote the
  -- same set of layouts.
  norm :: Doc -> Doc -> (Doc, Doc)
  norm Empty           tt = (tt, Empty)
  norm d@(Text _ _)    tt = (Combine d tt, Empty)
  norm d@(Line _ _)    tt = (Empty, Combine d tt)
  norm (Combine d1 d2) tt = let (td1, sd1) = norm d1 td2
                                (td2, sd2) = norm d2 tt
                            in (td1, Combine sd1 sd2)
  norm (Group d)       tt = let (td, sd) = norm d tt
                            in (td, Group sd)
  norm (Nest i d)      tt = let (td, sd) = norm d tt
                            in (td, Nest i sd)
  norm (Align i d)     tt = let (td, sd) = norm d tt
                            in (td, Align (i - docLength td) sd)

  -- Determine length of a document consisting only of text, empty and <>.
  -- To ensure linear complexity for align should actually keep track
  -- of document length within norm function itself.
docLength :: Doc -> Int
docLength Empty           = 0
docLength (Text l _)      = l
docLength (Combine d1 d2) = docLength d1 + docLength d2
-- added for support of fill and fillBreak
docLength (Line l _)      = l
docLength (Group d)       = docLength d
docLength (Nest _ d)      = docLength d
docLength (Align _ d)     = docLength d

interpret :: Doc -> Width -> TreeCont -> TreeCont
interpret Empty _ treeCont pos qs = treeCont pos qs
interpret (Text l s) _ treeCont pos qs =
  extendFrontGroup id prune outText treeCont (pos + l) qs
 where
  outText :: OutGroup
  outText _ cont r i = s ++ cont (r - l) i
interpret (Line l s) width treeCont pos qs =
  extendFrontGroup id prune outLine treeCont (pos + l) qs
 where
  outLine :: OutGroup
  outLine horizontal cont r i
    | horizontal = s ++ cont (r - l) i
    | otherwise  = '\n' : replicate i ' ' ++ cont (width - i) i
interpret (Combine d1 d2) width treeCont pos qs =
  interpret d1 width (interpret d2 width treeCont) pos qs
interpret (Group d) width treeCont pos qs = 
  interpret d width (leaveGroup treeCont) pos (Q.cons (pos, \hz cont -> cont) qs)
interpret (Nest j d) width treeCont pos qs =
  extendFrontGroup (interpret d width) (interpret d width) outNest (unNest treeCont) pos qs
 where
  outNest :: OutGroup
  outNest _ cont r i = cont r (i + j)
  -- Reset indentation level for all documents following a nested document
  unNest :: TreeCont -> TreeCont
  unNest cont p q r i = cont p q r (i - j)
interpret (Align j d) width treeCont pos qs = 
  extendFrontGroup (interpret d width) (interpret d width) outAlign treeCont pos qs
 where
  outAlign :: OutGroup
  outAlign _ cont r _ = cont r (width - r + j)

-- If there are no pending groups, then apply out directly,
-- Otherwise add out to pending group, applying given prune function.
-- This extracts an otherwise repeated pattern of the interpret function.
extendFrontGroup :: (TreeCont -> TreeCont) -> (TreeCont -> TreeCont) -> 
                    OutGroup -> TreeCont -> TreeCont
extendFrontGroup cont1 cont2 out treeCont pos qs =
  case Q.matchHead qs of
    Nothing                 -> out False (cont1 treeCont pos qs)
    Just ((p, outGrp), qs') ->
      cont2 treeCont pos (Q.cons (p, \hz cont -> outGrp hz (out hz cont)) qs')

leaveGroup :: TreeCont -> TreeCont
leaveGroup treeCont pos qs = 
  case Q.matchHead qs of
    Nothing -> treeCont pos qs
    Just ((_, outGrp1), qs1) -> 
      case Q.matchHead qs1 of
        Nothing -> outGrp1 True (treeCont pos Q.empty)
        Just ((s2, outGrp2), qs2) ->
          treeCont pos (Q.cons (s2, \f cont -> outGrp2 f 
            (\r1 -> outGrp1 (pos <= s2 + r1) cont r1)) qs2)

prune :: TreeCont -> TreeCont
prune treeCont pos qs = 
  case Q.matchLast qs of
    Nothing -> treeCont pos qs
    Just ((s, outGrp), qs') ->
      \r -> if pos > s + r then outGrp False (prune treeCont pos qs') r
                           else treeCont pos qs r
