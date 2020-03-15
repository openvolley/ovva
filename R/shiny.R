#' Launch the Shiny app
#'
#' @param data_path character or function: a named character vector of paths to data files. The names will be used as the competition names. So e.g. `c("Competition 1" = "/path/to/dvw/files", "Competition 2" = "/path/to/other/dvw/files")`. If `data_path` is a function, it should return such a character vector
#' @param playlist_handler tibble: a tibble that provides playlist handler capabilities (see \code{\link{ovva_playlist_handler}} for details)
#' @param highlight_handler tibble: a tibble that provides playlist handler capabilities (see \code{\link{ovva_highlight_handler}} for details)
#' @param video_server string or function: if string, either "lighttpd", "servr", or "none". If a function, it will be used to modify the video file path present in each dvw file. Details TBD
#' @param launch_browser logical: if \code{TRUE}, launch the app in the system's default web browser (passed to \code{\link[shiny]{runApp}}'s \code{launch.browser} parameter)
#' @param ... : additional parameters passed to the UI and server functions
#'
#' @export
ovva_shiny <- function(data_path, playlist_handler = ovva_playlist_handler(), highlight_handler = ovva_highlight_handler(), video_server = "lighttpd", launch_browser = TRUE, ...) {
    assert_that(is.flag(launch_browser), !is.na(launch_browser))
    assert_that(is.data.frame(playlist_handler))
    if (!all(c("skill", "specific", "fun") %in% names(playlist_handler))) stop("playlist_handler must have columns 'skill', 'specific', and 'fun'")
    if (any(duplicated(playlist_handler$specific))) stop("playlist_handler cannot have duplicated values of 'specific'")
    if (!is.list(playlist_handler$fun) || !is.function(playlist_handler$fun[[1]])) {
        stop("the 'fun' column of playlist_handler should be a list column of functions")
    }
    assert_that(is.data.frame(highlight_handler))
    if (!all(c("skill", "specific", "fun") %in% names(highlight_handler))) stop("highlight_handler must have columns 'skill', 'specific', and 'fun'")
    if (any(duplicated(highlight_handler$specific))) stop("highlight_handler cannot have duplicated values of 'specific'")
    if (!is.list(highlight_handler$fun) || !is.function(highlight_handler$fun[[1]])) {
        stop("the 'fun' column of highlight_handler should be a list column of functions")
    }
    assert_that(is.string(video_server) || is.function(video_server))
    if (is.string(video_server)) video_server <- match.arg(tolower(video_server), c("lighttpd", "servr", "none"))
    ## check competition data
    if (is.function(data_path)) {
        if (!is.character(data_path())) stop("the data_path function should return a named character vector")
    } else {
        assert_that(is.character(data_path), length(data_path) > 0)
        if (length(names(data_path)) != length(data_path)) stop("data_path must be a named character vector or function that returns one")
        for (z in seq_along(data_path)) {
            if (!dir.exists(data_path[z])) stop("the directory '", data_path[z], "' does not exist")
        }
    }
    ## sort out the video server
    if (is.function(video_server) || (is.string(video_server) && video_server == "none")) {
        video_server_url <- NULL
        video_server_dir <- NULL
        video_serve_method <- video_server
    } else {
        vsrv <- ovva_video_server(method = video_server)
        onStop(function() try({ vsrv$cleanup_fun() }, silent = TRUE))
    }
    app_data <- c(list(data_path = data_path, playlist_handler = playlist_handler, highlight_handler = highlight_handler, video_serve_method = vsrv$method, video_server_dir = vsrv$dir, video_server_url = vsrv$url), list(...))
    this_app <- list(ui = ovva_shiny_ui(app_data = app_data), server = ovva_shiny_server(app_data = app_data))
    shiny::runApp(this_app, display.mode = "normal", launch.browser = launch_browser)
}
