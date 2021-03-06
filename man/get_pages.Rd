% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrap_comparis.R
\name{get_pages}
\alias{get_pages}
\title{Get a vector of pages' numbers}
\usage{
get_pages(city)
}
\arguments{
\item{city}{a length one character vector indicating a Swiss city.}
}
\value{
a numeric vector of pages' numbers.
}
\description{
\code{get_pages()} scrapes pages' elements for a given Swiss city from
\url{https://www.comparis.ch} web-page and returns a vector with pages'
numbers.
}
\details{
Note that only the latest (no older than one week) items considered.
\url{https://www.comparis.ch} url's indexing starts from zero, therefore, the
returned sequence also starts from zero.
}
\examples{
pages <- get_pages("bern")

}
\author{
Iegor Rudnytskyi, \email{iegor.rudnytskyi@unil.ch}
}
