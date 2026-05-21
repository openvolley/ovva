library(ovva)
options(
  shiny.host = Sys.getenv("SHINY_HOST", unset = "0.0.0.0"),
  shiny.port = as.integer(Sys.getenv("SHINY_PORT", unset = "3838"))
)
ovva_shiny(
  data_path = c(MyData = Sys.getenv("DATA_PATH", unset = "/data")),
  video_server = Sys.getenv("VIDEO_SERVER", unset = "lighttpd"),
  video_server_port = as.integer(Sys.getenv("VIDEO_SERVER_PORT", unset = "8888")),
  alt_video_path = Sys.getenv("ALT_VIDEO_PATH", unset = "/videos"),
  launch_browser = FALSE
)
