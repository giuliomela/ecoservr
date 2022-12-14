% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_agr_value.R
\name{compute_agr_value}
\alias{compute_agr_value}
\title{Computing average production values of a given agricultural crop (Corine classification)}
\usage{
compute_agr_value(
  nuts = "Italia",
  h = 3,
  last_yr,
  ref_yr = 2019,
  corine_code,
  lang = "it"
)
}
\arguments{
\item{nuts}{A character vector. The names of the the Italian NUTS2 regions of interest. Names
must be in Italian and chosen among the folloqing list: `r knitr::combine_words(ecoservr::nuts2_codes$label)`.
`Bolzano` and `Trento` are treated as separate regions. Data on `Italia` as a whole can be retrieved
as well.}

\item{h}{An integer. A number specifying the number of years over which computing average areas. Default value
is `3`.}

\item{last_yr}{An integer. The latest (meaning most recent) year from which computing average areas.}

\item{ref_yr}{A numeric value. The year at which price levels monetary values must be expressed.}

\item{corine_code}{A numeric vector. Vector of the Corine classes (level 3) for which the average
area must be computed.}

\item{lang}{character string. Either `en` or `it`, to specify in which language Corine classes and crop labels must
be returned. Default value is `it`.}
}
\value{
A tibble with NUTS2 codes, corine3 codes and average production value in the time frame specified
    (expressed in constat million euro, at the prices of `last_yr`).
}
\description{
This function computes the average value of production (at base prices) of a given (agricultural and grassland) Corine class
for each Italian level 2 NUTS regions.
}
