#' Start the ovva shiny app with a demonstration data set
#'
#' The demonstration data set is the 2018 women's final (first of 3) from the German Bundesliga: Allianz MTV Stuttgart vs SSC Palmberg Schwerin.
#'
#' @seealso \code{\link{ovva_shiny}}
#'
#' @examples
#' \dontrun{
#'   ovva_shiny_demo()
#' }
#'
#' @export
ovva_shiny_demo <- function() {
   ovva_shiny(data_path = c(Demo = system.file("extdata/demo", package = "ovva")), video_server = "none", dv_read_args = list(skill_evaluation_decode = "german"))
}

