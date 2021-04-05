#' Start a separate server process for serving video
#'
#' Note that performance with "servr" may not be great, particularly with each new video, because servr has to read the entire video file before it can serve clips from it.
#'
#' @param method string: either "lighttpd" or "servr". If "lighttpd" but the lighttpd executable cannot be found, "servr" will be used as a fallback
#' @param port integer: the port to open the server on. If missing or `NULL`, a random port between 8001 and 12000
#'
#' @return A list with components `method`, `url`, `port`, `dir` (the source dir from which files will be served), and `cleanup_fun` (a function to run when finished with the server, which will stop the server process and clean up its `dir`)
#'
#' @seealso \code{\link{ovva_shiny}}
#'
#' @export
ovva_video_server <- function(method, port) {
    assert_that(is.string(method))
    method <- match.arg(tolower(method), c("lighttpd", "servr"))
    if (missing(port) || is.null(port) || is.na(port)) port <- sample.int(4000, 1) + 8000L ## random port from 8001
    assert_that(is.integer(port), !is.na(port))

    video_server_url <- paste0("http://localhost:", port, "/")
    video_server_dir <- tempfile()
    dir.create(video_server_dir)
    if (method == "lighttpd") {
        lighttpd_path <- ovva_find_lighttpd()
        if (is.null(lighttpd_path)) {
            warning("could not find the lighttpd executable, try `ovva_install_lighttpd()`. Using \"servr\" video option")
            method <- "servr"
        }
    }
    if (method == "lighttpd") {
        ## build config file to pass to lighttpd
        lighttpd_conf_file <- tempfile(fileext = ".conf")
        cat("server.document-root = \"", video_server_dir, "\"\nserver.port = \"", port, "\"\n", sep = "", file = lighttpd_conf_file, append = FALSE)
        lighttpd_pid <- sys::exec_background(lighttpd_path, c("-D", "-f", lighttpd_conf_file), std_out = FALSE) ## start lighttpd not in background mode
        message("Serving the directory ", video_server_dir, " at http://127.0.0.1:", port)
        cleanup_fun <- function() {
            message("cleaning up lighttpd")
            try(tools::pskill(lighttpd_pid), silent = TRUE)
            unlink(video_server_dir, recursive = TRUE)
        }
    } else {
        ## start servr instance
        servr::httd(dir = video_server_dir, port = port, browser = FALSE)
        cleanup_fun <- function() {
            message("cleaning up servr")
            servr::daemon_stop()
            unlink(video_server_dir, recursive = TRUE)
        }
    }
    list(method = method, url = video_server_url, port = port, dir = video_server_dir, cleanup_fun = cleanup_fun)
}

