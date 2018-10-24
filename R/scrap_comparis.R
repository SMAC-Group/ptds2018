#' Get a vector of pages' numbers
#'
#' \code{get_pages()} scrapes pages' elements for a given Swiss city from
#' \url{https://www.comparis.ch} web-page and returns a vector with pages'
#' numbers.
#'
#' Note that only the latest (no older than one week) items considered.
#' \url{https://www.comparis.ch} url's indexing starts from zero, therefore, the
#' returned sequence also starts from zero.
#'
#' @param city a length one character vector indicating a Swiss city.
#'
#' @return a numeric vector of pages' numbers.
#'
#' @examples
#' pages <- get_pages("bern")
#'
#' @author Iegor Rudnytskyi, \email{iegor.rudnytskyi@unil.ch}
#'
#' @export
get_pages <- function(city) {

  url_path <- paste0(
    "https://en.comparis.ch/immobilien/result/list?requestobject=%7B%22DealTyp",
    "e%22%3A%2210%22%2C%22SiteId%22%3A%220%22%2C%22RootPropertyTypes%22%3A%5B%",
    "5D%2C%22PropertyTypes%22%3A%5B%5D%2C%22RoomsFrom%22%3Anull%2C%22RoomsTo%2",
    "2%3Anull%2C%22FloorSearchType%22%3A%220%22%2C%22LivingSpaceFrom%22%3Anull",
    "%2C%22LivingSpaceTo%22%3Anull%2C%22PriceFrom%22%3Anull%2C%22PriceTo%22%3A",
    "null%2C%22ComparisPointsMin%22%3A%220%22%2C%22AdAgeMax%22%3A%227%22%2C%22",
    "AdAgeInHoursMax%22%3Anull%2C%22Keyword%22%3A%22%22%2C%22WithImagesOnly%22",
    "%3Afalse%2C%22WithPointsOnly%22%3Anull%2C%22Radius%22%3Anull%2C%22MinAvai",
    "lableDate%22%3Anull%2C%22MinChangeDate%22%3A%221753-01-01T00%3A00%3A00%22",
    "%2C%22LocationSearchString%22%3A%22",
    city,
    "%22%2C%22Sort%22%3A%2211%22%2C%22HasBalcony%22%3Afalse%2C%22HasTerrace%22",
    "%3Afalse%2C%22HasFireplace%22%3Afalse%2C%22HasDishwasher%22%3Afalse%2C%22",
    "HasWashingMachine%22%3Afalse%2C%22HasLift%22%3Afalse%2C%22HasParking%22%3",
    "Afalse%2C%22PetsAllowed%22%3Afalse%2C%22MinergieCertified%22%3Afalse%2C%2",
    "2WheelchairAccessible%22%3Afalse%2C%22LowerLeftLatitude%22%3Anull%2C%22Lo",
    "werLeftLongitude%22%3Anull%2C%22UpperRightLatitude%22%3Anull%2C%22UpperRi",
    "ghtLongitude%22%3Anull%7D&sort=11&searchtype=8"
  )

  xml2::read_html(url_path) %>%
    rvest::html_nodes("#resultlist_paging a") %>%
    rvest::html_text() %>%
    as.numeric() %>%
    max(na.rm = TRUE) %>%
    magrittr::subtract(e2 = 1) %>%
    seq(from = 0)
}

#' Get prices for a given city and page
#'
#' \code{get_prices()} scrapes prices elements for a given Swiss city and page
#' from \url{https://www.comparis.ch} web-page and returns a numeric vector of
#' prices.
#'
#' Note that only the latest (no older than one week) items considered.
#' \url{https://www.comparis.ch} url's indexing starts from zero, therefore, the
#' page number should be subtracted by one.
#'
#' @param city a length one character vector indicating a city.
#' @param page a length one numeric vector indicating a page number.
#'
#' @return a numeric vector of prices.
#'
#' @examples
#' prices <- get_prices("bern", 3)
#'
#' @author Iegor Rudnytskyi, \email{iegor.rudnytskyi@unil.ch}
#'
#' @export
get_prices <- function(city, page) {

  url_path <- paste0(
    "https://en.comparis.ch/immobilien/result/list?requestobject={%22DealType%",
    "22:%2210%22,%22SiteId%22:%220%22,%22RootPropertyTypes%22:[],%22PropertyTy",
    "pes%22:[],%22RoomsFrom%22:null,%22RoomsTo%22:null,%22FloorSearchType%22:%",
    "220%22,%22LivingSpaceFrom%22:null,%22LivingSpaceTo%22:null,%22PriceFrom%2",
    "2:null,%22PriceTo%22:null,%22ComparisPointsMin%22:%220%22,%22AdAgeMax%22:",
    "%227%22,%22AdAgeInHoursMax%22:null,%22Keyword%22:%22%22,%22WithImagesOnly",
    "%22:false,%22WithPointsOnly%22:null,%22Radius%22:null,%22MinAvailableDate",
    "%22:null,%22MinChangeDate%22:%221753-01-01T00:00:00%22,%22LocationSearchS",
    "tring%22:%22",
    city,
    "%22,%22Sort%22:%2211%22,%22HasBalcony%22:false,%22HasTerrace%22:false,%22",
    "HasFireplace%22:false,%22HasDishwasher%22:false,%22HasWashingMachine%22:f",
    "alse,%22HasLift%22:false,%22HasParking%22:false,%22PetsAllowed%22:false,%",
    "22MinergieCertified%22:false,%22WheelchairAccessible%22:false,%22LowerLef",
    "tLatitude%22:null,%22LowerLeftLongitude%22:null,%22UpperRightLatitude%22:",
    "null,%22UpperRightLongitude%22:null}&sort=11&page=",
    page
  )

  xml2::read_html(url_path) %>%
    rvest::html_nodes(".item-price strong") %>%
    rvest::html_text() %>%
    substring(first = 5) %>%
    gsub(pattern = ",", replacement = "") %>%
    as.numeric()
}

#' Get a market volume for a given city
#'
#' \code{get_volume()} scrapes a market volume for a given city, which is
#' defined as a sum of prices of all listings published in the latest week on
#' \url{https://www.comparis.ch}.
#'
#' Note that only the latest (no older than one week) items considered.
#'
#' @param city a length one character vector indicating a city.
#'
#' @return a sum of prices of all listings.
#'
#' @examples
#' pages <- get_volume("bern")
#'
#' @author Iegor Rudnytskyi, \email{iegor.rudnytskyi@unil.ch}
#'
#' @export
get_volume <- function(city) {

  sapply(
    X = get_pages(city),
    FUN = get_prices,
    city = city
  ) %>%
    unlist() %>%
    sum(na.rm = TRUE)
}
