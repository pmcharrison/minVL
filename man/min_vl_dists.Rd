% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{min_vl_dists}
\alias{min_vl_dists}
\title{Get minimal voice-leading set distances}
\usage{
min_vl_dists(s1_list, s2_list, elt_type = "pc", norm = "taxicab",
  preserve_bass = FALSE)
}
\arguments{
\item{s1_list}{List of numeric vectors corresponding to the
first pitch-class sets to be compared,
with each number corresponding to either a pitch or a pitch class.
Duplicates are permitted, and they will be retained.
Order does not matter.}

\item{s2_list}{List of numeric vectors corresponding to the second
pitch-class sets to be compared; see \code{s1}.}

\item{elt_type}{Can be either \code{pitch} or \code{pc};
determines whether \code{s1} and \code{s2} are interpreted
as pitches or pitch classes.}

\item{norm}{Can be either \code{euclidean}, \code{taxicab},
or \code{infinity}. Each of these identify different norms.}

\item{preserve_bass}{Logical scalar;
if TRUE, the first element of each set is considered to be the bass note,
and the voice leading is constrained to preserve these bass notes.
Only relevant if \code{elt_type == "pc"}.}
}
\description{
Equivalent to \code{\link{min_vl_dist}} but vectorised.
}
