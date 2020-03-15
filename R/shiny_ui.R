ovva_shiny_ui <- function(app_data) {
    fluidPage(
        introjsUI(),
        ovideo::ov_video_js(youtube = TRUE),
        useShinyjs(),
        tags$head(
                 tags$style("#subtitle { border: 1px solid black; border-radius: 1px; padding: 5px; margin-left: 6px; background-color: lightblue; font-size: 14px;} #subtitleskill { border: 1px solid black; border-radius: 1px; padding: 5px; margin-left: 6px; background-color: coral; font-size: 14px;}"),
                 tags$style("#headerblock {border-radius:4px; padding:10px; margin-bottom:5px; min-height:120px; color:black;}"),
                 if (!is.null(app_data$css)) tags$style(app_data$css)
             ),
        tags$script("function dvjs_video_onstart() { document.getElementById(\"subtitle\").textContent=dvjs_video_controller.queue[dvjs_video_controller.current].subtitle; document.getElementById(\"subtitleskill\").textContent=dvjs_video_controller.queue[dvjs_video_controller.current].subtitleskill; }"),
        if (!is.null(app_data$ui_header)) app_data$ui_header else tags$h2("Volleyball Video Analysis"),
        tags$hr(),
        ovva_shiny_ui_main(),
    )
}

ovva_shiny_ui_main <- function() {
        sidebarLayout(
            sidebarPanel(
                introBox(selectInput("season", label = tags$h4("Competition directory"),
                                     choices = NULL),
                         data.step = 1,
                         data.intro = "Select volleyball season"),
                uiOutput("no_game_data"),
                introBox(DT::dataTableOutput("game_id_table"),
                         data.step = 2,
                         data.intro = "Search for the Game ID"
                         ),
                fluidRow(
                    tags$h5("Select"),
                    column(4,
                           introBox(
                               selectizeInput(inputId = "team_list",
                                              label = "Team",
                                              choices = NULL,
                                              selected = NULL,
                                              multiple = TRUE),
                               data.step = 3,
                               data.intro = "Input team")),
                    column(4,introBox(
                                 pickerInput(inputId = "player_list",
                                             label = "Player name",
                                             choices = NULL,
                                             options = list(`actions-box` = TRUE),
                                             multiple = TRUE),
                                 data.step = 3,
                                 data.intro = "Choose Player")),
                    column(4, introBox(
                                  selectizeInput(inputId = "skill_list",
                                                 label = "Skill",
                                                 choices = NULL,
                                                 multiple = TRUE),
                                  data.step = 3,
                                  data.intro = "Choose skill"))
                ),
                tags$h5("Optional"),
                fluidRow(
                    tabBox(
                        tabPanel("Filter-based",
                                 column(6,
                                        pickerInput(inputId = "skilltype_list",
                                                    label = "Skill type",
                                                    choices = NULL,
                                                    selected = NULL,
                                                    options = list(`actions-box` = TRUE),
                                                    multiple = TRUE)),
                                 column(6,
                                        pickerInput(inputId = "phase_list",
                                                    label = "Phase",
                                                    choices = NULL,
                                                    selected = NULL,
                                                    options = list(`actions-box` = TRUE),
                                                    multiple = TRUE))),
                        tabPanel("Playlist-based",
                                 uiOutput("playlist_based_ui")
                        ),
                        tabPanel("Highlights",
                                 uiOutput("highlight_based_ui")
                        ),
                        width = NULL, side = "left"
                    )),
                tags$div(style = "border: 1px dashed black; padding: 8px;",
                         tags$h5("Advanced filter"),
                         fluidRow(
                             column(6,introBox(
                                          selectizeInput(inputId = "adFilter_list",
                                                         label = "Advanced filter",
                                                         choices = NULL,
                                                         multiple = FALSE),
                                          data.step = 7,
                                          data.intro = "Filter")),
                             column(6, introBox(pickerInput(inputId = "adFilterValue_list",
                                                            label = "Advanced filter value",
                                                            choices = NULL,
                                                            selected = NULL,
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE),
                                                data.step = 8,
                                                data.intro = "Filter value"))),
                         fluidRow(column(6,introBox(
                                               selectizeInput(inputId = "adFilterB_list",
                                                              label = "2nd advanced filter",
                                                              choices = NULL,
                                                              multiple = FALSE),
                                               data.step = 9,
                                               data.intro = "2nd Filter")),
                                  column(6,introBox(
                                               pickerInput(inputId = "adFilterBValue_list",
                                                           label = "2nd advanced filter value",
                                                           choices = NULL,
                                                           selected = NULL,
                                                           options = list(`actions-box` = TRUE),
                                                           multiple = TRUE),
                                               data.step = 10,
                                               data.intro = "2nd Filter value")))
                         )
            ),
            mainPanel(
                introBox(title = "Video",
                         tabPanel("Video",
                                  tagList(ovideo::ov_video_player(id = "dv_player", type = "local", controls = FALSE, style = "border: 1px solid black; width: 90%;"),
                                          ovideo::ov_video_player(id = "dvyt_player", type = "youtube", controls = FALSE, style = "border: 1px solid black; width: 90%; height: 480px; display:none;"), ## start hidden
                                          uiOutput("player_controls_ui"),
                                          shiny::downloadButton("create_clip_ui", "Download clip")), ##uiOutput("player_ui"),
                                  ##uiOutput("preview_button_ui", inline = TRUE),
                                  ##uiOutput("open_preview_ui", inline = TRUE),
                                  sliderInput("playback_rate", "Playback rate:", min = 0.1, max = 2.0, value = 1.0, step = 0.1)),
                         width = NULL, side = "left"),
                introBox(title = "", tabPanel("Summary of clip",DT::dataTableOutput("official_recap")),
                       uiOutput("chart_ui"),
                       width = NULL, side = "left")
            )
        )
}
