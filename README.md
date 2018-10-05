# minVL - computing minimal voice leadings

This is an R package for computing minimal voice leadings between chords,
after Tymcozko (2006).
Practically all the package is written in C++ for speed.

## Installation

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("pmcharrison/minVL")
```

## Example usage

### min_vl

`min_vl` computes the minimal voice leading between two sets of 
pitch classes or pitches.

```r
library(minVL)

# Compute the minimal voice leading between a G7 chord and a C major chord
G7 <- c(2, 5, 7, 11) # these are pitch classes
C <- c(0, 4, 7)
min_vl(G7, C, elt_type = "pc", norm = "euclidean")
min_vl(G7, C, elt_type = "pc", norm = "taxicab")

# E minor is closer to C major than Eb minor
Emin <- c(4, 7, 11)
Ebmin <- c(3, 6, 10)
min_vl(Emin, C)$dist # distance = 1
min_vl(Ebmin, C)$dist # distance = 4
```

### vl_dist

`vl_dist` computes the voice-leading distance between two sets of 
pitches or pitch classes `s1` and `s2`,
where the `i`th element of `s1` is assumed to move to 
the `i`th element of `s2`.

```r
library(minVL)
Emin <- c(4, 7, 11)
Ebmin <- c(3, 6, 10)
vl_dist(Emin, Ebmin, elt_type = "pc", norm = "euclidean")
```

## Testing 

The package includes regression tests that test the minimal voice-leading implementation
against that on Dmitri Tymoczko's website (http://dmitri.mycpanel.princeton.edu/software.html).

## References

Tymoczko D (2006). “The geometry of musical chords.” 
*Science*, **313**(5783), 72–74. https://doi.org/10.1126/science.1126287
