#' Adapted from r-bloggers
#' @title Run Shiny Applications for stat297
#' @description
#' Use runShiny('rmd') to call rmd_full.
#' Use runShiny('rmd_mini') to call rmd_min.
#' @param example A \code{string} denoting the Shiny application name.
#' Currently 'rmd' and 'rmd_mini' are available. Default is 'rmd'.
#' @return A running implementation
#' @author Justin Lee
#' @examples
#' \dontrun{
#' runShiny('rmd')
#' runShiny('rmd_mini')
#' }
#' @export
runShiny = function(example){
  # locate all the shiny app examples that exist
  validShiny = list.files(system.file("shiny-examples", package = "ptdspkg"))

  # Prints out valid Shiny apps
  validShinyMsg =
    paste0(
      "Valid examples are: '",
      paste(validShiny, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validShiny) {
    stop(
      'Please run `runShiny()` with a valid Shiny app as an argument.n',
      validShinyMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir = system.file("shiny-examples", example, package = "ptdspkg")
  shiny::runApp(appDir, display.mode = "normal")
}

