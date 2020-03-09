#' Launch the Shiny app
#'
#' @param data_path character: a named character vector of paths to data files. The names will be used as the competition names. So e.g. `c("Competition 1" = "/path/to/dvw/files", "Competition 2" = "/path/to/other/dvw/files")`
#' @param playlist_handler tibble: a tibble that provides playlist handler capabilities (see \code{\link{ovva_playlist_handler}} for details)
#' @param launch_browser logical: if \code{TRUE}, launch the app in the system's default web browser (passed to \code{\link[shiny]{runApp}}'s \code{launch.browser} parameter)
#' @param ... : additional parameters passed to the UI and server functions
#'
#' @export
ovva_shiny <- function(data_path, playlist_handler = ovva_playlist_handler(), launch_browser = TRUE, ...) {
    assert_that(is.flag(launch_browser), !is.na(launch_browser))
    assert_that(is.data.frame(playlist_handler))
    if (!all(c("skill", "specific", "fun") %in% names(playlist_handler))) stop("playlist_handler must have columns 'skill', 'specific', and 'fun'")
    if (!is.list(playlist_handler$fun) || !is.function(playlist_handler$fun[[1]])) {
        stop("the 'fun' column of playlist_handler should be a list column of functions")
    }
    ## check competition data
    assert_that(is.character(data_path), length(data_path) > 0)
    if (length(names(data_path)) != length(data_path)) stop("data_path must be a named character vector")
    for (z in seq_along(data_path)) {
        if (!dir.exists(data_path[z])) stop("the directory '", data_path[z], "' does not exist")
    }

    ## sort out the video server
    have_lighttpd <- FALSE
    video_server_port <- sample.int(4000, 1) + 8000 ## random port from 8001
    video_server_url <- paste0("http://localhost:", video_server_port, "/")
    if (.Platform$OS.type == "unix") {
        tryCatch({
            chk <- sys::exec_internal("lighttpd", "-version")
            have_lighttpd <- TRUE
        }, error = function(e) warning("could not find the lighttpd executable, install it with e.g. 'apt install lighttpd'. Using \"servr\" video option"))
    }
    video_serve_method <- if (have_lighttpd) "lighttpd" else "servr"
    video_server_dir <- tempfile()
    dir.create(video_server_dir)
    if (video_serve_method == "lighttpd") {
        ## build config file to pass to lighttpd
        lighttpd_conf_file <- tempfile(fileext = ".conf")
        cat("server.document-root = \"", video_server_dir, "\"\nserver.port = \"", video_server_port, "\"\n", sep = "", file = lighttpd_conf_file, append = FALSE)
        lighttpd_pid <- sys::exec_background("lighttpd", c("-D", "-f", lighttpd_conf_file), std_out = FALSE) ## start lighttpd not in background mode
        lighttpd_cleanup <- function() {
            message("cleaning up lighttpd")
            try(tools::pskill(lighttpd_pid), silent = TRUE)
            unlink(video_server_dir, recursive = TRUE)
        }
        onStop(function() try({ lighttpd_cleanup() }, silent = TRUE))
    } else {
        ## start servr instance serving from the video source directory
        servr::httd(dir = video_server_dir, port = video_server_port)
        onStop(function() {
            message("cleaning up servr")
            servr::daemon_stop()
        })
    }
    app_data <- c(list(data_path = data_path, playlist_handler = playlist_handler, video_serve_method = video_serve_method, video_server_dir = video_server_dir, video_server_url = video_server_url), list(...))
    this_app <- list(ui = ovva_shiny_ui(app_data = app_data), server = ovva_shiny_server(app_data = app_data))
    shiny::runApp(this_app, display.mode = "normal", launch.browser = launch_browser)
}
