Ekmelily Notation Styles
========================

> Please use instead
> [table.ly](https://github.com/tr-igem/ekmelily/blob/main/docs/table.ly)
> to create the desired lookup tables with [LilyPond](http://lilypond.org/).

This is a collection of files "TUNING/style-NOTATION.pdf" which demonstrate
notation styles in the available tunings of Ekmelily. Each file includes:

- A list of all accidental symbols used in the notation style,
  together with the represented tone fraction,
  its SMuFL code point and glyph name,
  except for the universal notation styles `alteration` and `step`.

- The usage in a LilyPond input file.

- Some sample notes with their note names and tone fractions.

The LilyPond input files "TUNING/style.ly" require
[styledefs.ily](https://github.com/tr-igem/ekmelily/blob/main/styles/styledefs.ily)
as well as [ekmelos-map.ily](https://github.com/tr-igem/ekmelos/blob/main/ly/ekmelos-map.ily)
from [Ekmelos](https://github.com/tr-igem/ekmelos).
