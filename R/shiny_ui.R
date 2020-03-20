ovva_shiny_ui <- function(app_data) {
    fluidPage(
        introjsUI(),
        ovideo::ov_video_js(youtube = TRUE),
        useShinyjs(),
        tags$head(
                 tags$style("#subtitle { border: 1px solid black; border-radius: 1px; padding: 5px; margin-left: 6px; background-color: lightblue; font-size: 14px;} #subtitleskill { border: 1px solid black; border-radius: 1px; padding: 5px; margin-left: 6px; background-color: coral; font-size: 14px;}"),
                 tags$style("#headerblock {border-radius:4px; padding:10px; margin-bottom:5px; min-height:120px; color:black;}"),
                 if (!is.null(app_data$css)) tags$style(app_data$css),
                 tags$script("function dvjs_video_onstart() { document.getElementById(\"subtitle\").textContent=dvjs_video_controller.queue[dvjs_video_controller.current].subtitle; document.getElementById(\"subtitleskill\").textContent=dvjs_video_controller.queue[dvjs_video_controller.current].subtitleskill; }")
             ),
        if (!is.null(app_data$ui_header)) {
            app_data$ui_header
        } else {
            fluidRow(id = "headerblock", column(6, tags$h2("Volleyball Video Analysis")),
                     column(3, offset = 3, tags$div(style = "text-align: center;", "Part of the", tags$br(), tags$img(src = "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMTAiIGhlaWdodD0iMjEwIj48cGF0aCBkPSJNOTcuODMzIDE4Ny45OTdjLTQuNTUtLjM5Ni0xMi44MTItMS44ODYtMTMuNTgxLTIuNDQ5LS4yNDItLjE3Ny0xLjY5Mi0uNzUzLTMuMjIyLTEuMjgxLTI4LjY5Ni05Ljg5NS0zNS4xNy00NS45ODctMTMuODY4LTc3LjMyMyAyLjY3Mi0zLjkzIDIuNTc5LTQuMTktMS4zOTQtMy45MDYtMTIuNjQxLjktMjcuMiA2Ljk1Mi0zMy4wNjYgMTMuNzQ1LTUuOTg0IDYuOTI3LTcuMzI3IDE0LjUwNy00LjA1MiAyMi44NjIuNzE2IDEuODI2LS45MTgtLjE3LTEuODktMi4zMS03LjM1Mi0xNi4xNzQtOS4xODEtMzguNTYtNC4zMzctNTMuMDc0LjY5MS0yLjA3IDEuNDE1LTMuODY2IDEuNjEtMy45ODkuMTk0LS4xMjMuNzgyLTEuMDUzIDEuMzA3LTIuMDY2IDMuOTQ1LTcuNjE3IDkuNDU4LTEyLjg2MiAxNy44MzktMTYuOTcgMTIuMTcyLTUuOTY4IDI1LjU3NS01LjgyNCA0MS40My40NDUgNi4zMSAyLjQ5NSA4LjgwMiAzLjgwMSAxNi4wNDcgOC40MTMgNC4zNCAyLjc2MiA0LjIxMiAyLjg3NCAzLjU5NC0zLjE3My0yLjgyNi0yNy42ODEtMTYuOTA3LTQyLjE4NS0zNi4wNjgtMzcuMTUxLTQuMjU0IDEuMTE3IDUuMjQtMy4zMzggMTEuNjYtNS40NzMgMTMuMTgtNC4zOCAzOC45MzctNS43NzIgNDYuMDc0LTEuNDg4IDEuMjQ3LjU0NyAyLjIyOCAxLjA5NSAzLjI3NSAxLjYzIDQuMjkgMi4xMDcgMTEuNzMzIDcuNjk4IDE0LjI2NSAxMS40MjcuNDA3LjYgMS4yNyAxLjg2NiAxLjkxNyAyLjgxNCAxMS4zMDggMTYuNTY1IDguNjIzIDQxLjkxLTYuODM4IDY0LjU1Mi0zLjI0OSA0Ljc1OC0zLjI1OCA0Ljc0MiAyLjQ1IDQuMDE4IDMyLjQ4Mi00LjEyMiA0OC41MTUtMjEuOTM1IDM5LjU3OC00My45NzQtMS4xNC0yLjgwOSAxLjU2NiAxLjA2IDMuNTE4IDUuMDMyIDI5LjY5MyA2MC40MTctMjIuNTggMTA3Ljg1My03OS40OTggNzIuMTQzLTUuMDg0LTMuMTktNS4xMjMtMy4xNTItMy45MDIgMy44ODMgNC43MjEgMjcuMjIgMjUuNzgzIDQzLjU2MiA0NC4wODkgMzQuMjEgMS4zNjItLjY5NiAyLjIxLS43NSAyLjIxLS4xNDMtNi43NiAzLjg1Ny0xNi4wMTggNi41NTMtMjMuMTI2IDguMDkxLTcuNTU1IDEuNTQ3LTE4LjM2NiAyLjE3Mi0yNi4wMiAxLjUwNnoiIGZpbGw9IiMwMDA3NjYiLz48ZWxsaXBzZSBjeD0iMTA1Ljk3NSIgY3k9IjEwNC40NDEiIHJ4PSI5NC44NCIgcnk9IjkyLjU0MiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwNzY2IiBzdHJva2Utd2lkdGg9IjEwLjc0Ii8+PC9zdmc+", style = "max-height:3em;"), tags$br(), tags$a(href = "https://github.com/openvolley", "openvolley", target = "_blank"), "project")))
        },
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
            uiOutput("processing_note"),
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
                              tagList(ovideo::ov_video_player(id = "dv_player", type = "local", controls = FALSE, poster = "data:image/gif,AAAA", style = "border: 1px solid black; width: 90%;", onloadstart = "set_vspinner();", oncanplay = "remove_vspinner();"),
                                      ovideo::ov_video_player(id = "dvyt_player", type = "youtube", controls = FALSE, style = "border: 1px solid black; width: 90%; height: 480px; display:none;"), ## start hidden
                                      uiOutput("player_controls_ui"),
                                      uiOutput("video_dialog"),
                                      uiOutput("create_clip_button_ui")),
                              ##uiOutput("preview_button_ui", inline = TRUE),
                              ##uiOutput("open_preview_ui", inline = TRUE),
                              sliderInput("playback_rate", "Playback rate:", min = 0.1, max = 2.0, value = 1.0, step = 0.1)),
                     width = NULL, side = "left"),
            introBox(title = "", tabPanel("Summary of clip", DT::dataTableOutput("official_recap")),
                     uiOutput("chart_ui"),
                     width = NULL, side = "left"),
            tags$script("set_vspinner = function() { $('#dv_player').addClass('loading'); }"),
            tags$script("remove_vspinner = function() { $('#dv_player').removeClass('loading'); }"),
            tags$style("video.loading { background: black; }")
        )
    )
}
