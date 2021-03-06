% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrap_comparis.R
\name{get_volume}
\alias{get_volume}
\title{Get a market volume for a given city}
\usage{
get_volume(city, delay = 1)
}
\arguments{
\item{city}{a length one character vector indicating a city.}

\item{delay}{a length one numeric vector indicating a delay between HTTP
requests in seconds. Default value: \code{1}.}
}
\value{
a sum of prices of all listings.
}
\description{
\code{get_volume()} scrapes a market volume for a given city, which is
defined as a sum of prices of all listings published in the latest week on
\url{https://www.comparis.ch}.
}
\details{
Note that only the latest (no older than one week) items considered.
}
\examples{
pages <- get_volume("bern")

}
\author{
Iegor Rudnytskyi, \email{iegor.rudnytskyi@unil.ch}
}
