ovva_shiny_ui <- function(app_data) {
    fluidPage(
        if (requireNamespace("rintrojs", quietly = TRUE)) rintrojs::introjsUI(),
        ovideo::ov_video_js(youtube = TRUE, twitch = TRUE, version = 2),
        tags$head(
                 tags$style("#subtitle { border: 1px solid black; border-radius: 1px; padding: 5px; margin-left: 6px; background-color: lightblue; font-size: 14px;} #subtitleskill { border: 1px solid black; border-radius: 1px; padding: 5px; margin-left: 6px; background-color: coral; font-size: 14px;}"),
                 tags$style("#headerblock {border-radius:14px; padding:10px; margin-bottom:5px; min-height:120px; color:black; border: 1px solid #000766; background:#000766; background: linear-gradient(90deg, rgba(0,7,102,1) 0%, rgba(255,255,255,1) 65%, rgba(255,255,255,1) 100%);} #headerblock h1, #headerblock h2, #headerblock h3, #headerblock h4 {color:#fff;} h5 {font-weight: bold;} .player_controls button { font-size:large; padding-top:5px; padding-bottom:5px; padding-left:14px; padding-right:14px; border-radius:8px; margin-left:6px;} .player_controls .fa {line-height: inherit;}"),
                 tags$style("#video_overlay, #video_overlay_img { position:absolute; -webkit-backface-visibility: hidden; -webkit-transform: translateZ(0); }"), ## stop chrome putting the overlay underneath the video
                 if (!is.null(app_data$css)) tags$style(app_data$css),
                 tags$script("$(document).on('shiny:sessioninitialized', function() { $('#video_overlay_img').hide(); });")
             ),
        if (!is.null(app_data$ui_header)) {
            app_data$ui_header
        } else {
            fluidRow(id = "headerblock", column(6, tags$h2("Volleyball Video Analysis")),
                     column(3, offset = 3, tags$div(style = "text-align: center;", "Part of the", tags$br(), tags$img(src = "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMTAiIGhlaWdodD0iMjEwIj48cGF0aCBkPSJNOTcuODMzIDE4Ny45OTdjLTQuNTUtLjM5Ni0xMi44MTItMS44ODYtMTMuNTgxLTIuNDQ5LS4yNDItLjE3Ny0xLjY5Mi0uNzUzLTMuMjIyLTEuMjgxLTI4LjY5Ni05Ljg5NS0zNS4xNy00NS45ODctMTMuODY4LTc3LjMyMyAyLjY3Mi0zLjkzIDIuNTc5LTQuMTktMS4zOTQtMy45MDYtMTIuNjQxLjktMjcuMiA2Ljk1Mi0zMy4wNjYgMTMuNzQ1LTUuOTg0IDYuOTI3LTcuMzI3IDE0LjUwNy00LjA1MiAyMi44NjIuNzE2IDEuODI2LS45MTgtLjE3LTEuODktMi4zMS03LjM1Mi0xNi4xNzQtOS4xODEtMzguNTYtNC4zMzctNTMuMDc0LjY5MS0yLjA3IDEuNDE1LTMuODY2IDEuNjEtMy45ODkuMTk0LS4xMjMuNzgyLTEuMDUzIDEuMzA3LTIuMDY2IDMuOTQ1LTcuNjE3IDkuNDU4LTEyLjg2MiAxNy44MzktMTYuOTcgMTIuMTcyLTUuOTY4IDI1LjU3NS01LjgyNCA0MS40My40NDUgNi4zMSAyLjQ5NSA4LjgwMiAzLjgwMSAxNi4wNDcgOC40MTMgNC4zNCAyLjc2MiA0LjIxMiAyLjg3NCAzLjU5NC0zLjE3My0yLjgyNi0yNy42ODEtMTYuOTA3LTQyLjE4NS0zNi4wNjgtMzcuMTUxLTQuMjU0IDEuMTE3IDUuMjQtMy4zMzggMTEuNjYtNS40NzMgMTMuMTgtNC4zOCAzOC45MzctNS43NzIgNDYuMDc0LTEuNDg4IDEuMjQ3LjU0NyAyLjIyOCAxLjA5NSAzLjI3NSAxLjYzIDQuMjkgMi4xMDcgMTEuNzMzIDcuNjk4IDE0LjI2NSAxMS40MjcuNDA3LjYgMS4yNyAxLjg2NiAxLjkxNyAyLjgxNCAxMS4zMDggMTYuNTY1IDguNjIzIDQxLjkxLTYuODM4IDY0LjU1Mi0zLjI0OSA0Ljc1OC0zLjI1OCA0Ljc0MiAyLjQ1IDQuMDE4IDMyLjQ4Mi00LjEyMiA0OC41MTUtMjEuOTM1IDM5LjU3OC00My45NzQtMS4xNC0yLjgwOSAxLjU2NiAxLjA2IDMuNTE4IDUuMDMyIDI5LjY5MyA2MC40MTctMjIuNTggMTA3Ljg1My03OS40OTggNzIuMTQzLTUuMDg0LTMuMTktNS4xMjMtMy4xNTItMy45MDIgMy44ODMgNC43MjEgMjcuMjIgMjUuNzgzIDQzLjU2MiA0NC4wODkgMzQuMjEgMS4zNjItLjY5NiAyLjIxLS43NSAyLjIxLS4xNDMtNi43NiAzLjg1Ny0xNi4wMTggNi41NTMtMjMuMTI2IDguMDkxLTcuNTU1IDEuNTQ3LTE4LjM2NiAyLjE3Mi0yNi4wMiAxLjUwNnoiIGZpbGw9IiMwMDA3NjYiLz48ZWxsaXBzZSBjeD0iMTA1Ljk3NSIgY3k9IjEwNC40NDEiIHJ4PSI5NC44NCIgcnk9IjkyLjU0MiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwNzY2IiBzdHJva2Utd2lkdGg9IjEwLjc0Ii8+PC9zdmc+", style = "max-height:3em;"), tags$br(), tags$a(href = "https://github.com/openvolley", "openvolley", target = "_blank"), "project")))
        },
        tags$hr(),
        ovva_shiny_ui_main(app_data),
    )
}

ovva_shiny_ui_main <- function(app_data = NULL) {
    tagList(
        ## js to track size of video element
        tags$head(tags$script("var vo_rsztmr;
$(document).on('shiny:sessioninitialized', function() {
    $('#playstable_holder').mouseenter(dv_h_suspend); $('#playstable_holder').mouseleave(dv_h_unsuspend);
    Shiny.setInputValue('dv_height', $('#dv_player').innerHeight()); Shiny.setInputValue('dv_width', $('#dv_player').innerWidth()); Shiny.setInputValue('dvyt_height', $('#dvyt_player').innerHeight()); Shiny.setInputValue('dvyt_width', $('#dvyt_player').innerWidth()); Shiny.setInputValue('vo_voffset', $('#video_holder').innerHeight());
    $(window).resize(function() {
      clearTimeout(vo_rsztmr);
      vo_rsztmr = setTimeout(vo_doneResizing, 500); });
    function vo_doneResizing() {
      Shiny.setInputValue('dv_height', $('#dv_player').innerHeight()); Shiny.setInputValue('dv_width', $('#dv_player').innerWidth()); Shiny.setInputValue('dvyt_height', $('#dvyt_player').innerHeight()); Shiny.setInputValue('dvyt_width', $('#dvyt_player').innerWidth()); Shiny.setInputValue('vo_voffset', $('#video_holder').innerHeight());
    }
});
function delete_pl_item(cb) { Shiny.setInputValue('del_plitem', cb.id + '@' + new Date().getTime()); }"),
tags$script("dv_h_ctr = false; dv_h_suspend = function() { if (!dv_h_ctr) { dvpl.suspend(); $('#dv_h_msg').show();; dv_h_ctr = true; return; } else { return; } }; dv_h_unsuspend = function() { dv_h_ctr = false; dvpl.unsuspend(); $('#dv_h_msg').hide(); }; "),
tags$style(".showhide {border-radius: 20px; padding: 6px 9px; background: #668;} .showhide:hover {background: #668;} .showhide:focus {background: #668;} #video_holder:not(:fullscreen) #dvyt_player {height:480px;} #video_holder:fullscreen #dvyt_player {height:100vh;}"),
),
shiny::wellPanel(
fluidRow(column(4, tags$h5("1. Select data"),
                fluidRow(column(6, selectInput("season", label = "Data set", choices = NULL),
                                pickerInput(inputId = "game_table_dropdown", label = "Games", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE),
                                uiOutput("no_game_data"), uiOutput("processing_note")))),
         column(8, tags$h5("2. Apply filters"),
                fluidRow(column(4, pickerInput(inputId = "team_list", label = "Team", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE)),
                         column(4, pickerInput(inputId = "player_list", label = "Player name", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE)),
                         column(4, pickerInput(inputId = "skill_list", label = "Skill", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE))),
                uiOutput("custom_filters_ui"),
                tags$h5("Extra filters", actionButton("collapse_filter2", label = "Show", class = "showhide")),
                tags$div(id = "filter2_panel", style = "display: none;", ## start hidden
                         fluidRow(tabsetPanel(
                             tabPanel("Filter-based",
                                      column(6, pickerInput(inputId = "skilltype_list", label = "Skill type", choices = NULL, selected = NULL, options = list(`actions-box` = TRUE), multiple = TRUE)),
                                      column(6, pickerInput(inputId = "phase_list", label = "Phase", choices = NULL, selected = NULL, options = list(`actions-box` = TRUE), multiple = TRUE))),
                             tabPanel("Playlist-based", uiOutput("playlist_based_ui")),
                             tabPanel("Highlights", uiOutput("highlight_based_ui"))
                         )),
                         tags$div(style = "border: 1px dashed black; padding: 8px; margin-bottom: 20px;",
                                  tags$h5("Advanced filter"),
                                  fluidRow(
                                      column(6,introbox_or_div(selectizeInput(inputId = "adFilter_list",
                                                                              label = "Advanced filter",
                                                                              choices = NULL,
                                                                              multiple = FALSE),
                                                               data.step = 7, data.intro = "Filter")),
                                      column(6, introbox_or_div(pickerInput(inputId = "adFilterValue_list",
                                                                            label = "Advanced filter value",
                                                                            choices = NULL,
                                                                            selected = NULL,
                                                                            options = list(`actions-box` = TRUE),
                                                                            multiple = TRUE),
                                                                data.step = 8, data.intro = "Filter value"))),
                                  fluidRow(column(6, introbox_or_div(selectizeInput(inputId = "adFilterB_list",
                                                                                    label = "2nd advanced filter",
                                                                                    choices = NULL,
                                                                                    multiple = FALSE),
                                                                     data.step = 9, data.intro = "2nd Filter")),
                                           column(6, introbox_or_div(pickerInput(inputId = "adFilterBValue_list",
                                                                                 label = "2nd advanced filter value",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 options = list(`actions-box` = TRUE),
                                                                                 multiple = TRUE),
                                                                     data.step = 10, data.intro = "2nd Filter value")))
                                  )
                         )
                )),
),
tags$hr(),
fluidRow(column(8, tags$div(id = "video_holder", style = "position:relative;",
                            ovideo::ov_video_player(id = "dv_player", type = "local", controls = FALSE, poster = "data:image/gif,AAAA", style = "border: 1px solid black; width: 100%;", onloadstart = "set_vspinner();", oncanplay = "remove_vspinner();", onerror = "dv_player_onerror(event);"),
                            ovideo::ov_video_player(id = "dvyt_player", type = "youtube", controls = FALSE, style = "border: 1px solid black; width: 100%; display:none;"), ## start hidden, note that type = "youtube" also works if we change to twitch ## height: 480px;
                            tags$img(id = "video_overlay_img"),
                            tags$div(id = "vwm", tags$img(id = "vwm_img"))
                            ),
                plotOutput("video_overlay"),
                uiOutput("player_controls_ui", style = "margin-top: 12px;"),
                uiOutput("video_dialog")
                ##uiOutput("preview_button_ui", inline = TRUE),
                ##uiOutput("open_preview_ui", inline = TRUE),
                ),
         column(4, tags$div(id = "playstable_holder", DT::dataTableOutput("playstable")), tags$div(id = "dv_h_msg", style = "color:red; margin: 8px; display:none;", "Playback suspended while mouse is over the table"), uiOutput("chart_ui"))),
tags$hr(),
sliderInput("playback_rate", "Playback rate:", min = 0.1, max = 2.0, value = 1.0, step = 0.1),
tags$hr(),
tags$h5("Clip timing"),
fluidRow(actionButton("timing_all_start_minus", "All start -1"),
         actionButton("timing_all_start_plus", "All start +1"),
         actionButton("timing_all_duration_minus", "All duration -1"),
         actionButton("timing_all_duration_plus", "All duration +1")),
tags$table(tags$tr(tags$th(),
                   tags$th("Serve"),
                   tags$th("Reception"),
                   tags$th("Set", tags$br(), "(in reception)"),
                   tags$th("Set", tags$br(), "(in transition)"),
                   tags$th("Attack", tags$br(), "(in reception)"),
                   tags$th("Attack", tags$br(), "(in transition)"),
                   tags$th("Block", tags$br(), "(in reception)"),
                   tags$th("Block", tags$br(), "(in transition)"),
                   tags$th("Dig", tags$br(), "(in transition)"),
                   tags$th("Freeball", tags$br(), "(in reception)"),
                   tags$th("Freeball", tags$br(), "(in transition)")),
           tags$tr(tags$th("Start offset:"),
                   timing_tstart("Serve", "Serve", timing_df = app_data$video_timing_df),
                   timing_tstart("Reception", "Reception", timing_df = app_data$video_timing_df), ##!!start_offset = -2),
                   timing_tstart("Set", "Reception", timing_df = app_data$video_timing_df),
                   timing_tstart("Set", "Transition", timing_df = app_data$video_timing_df),
                   timing_tstart("Attack", "Reception", timing_df = app_data$video_timing_df),
                   timing_tstart("Attack", "Transition", timing_df = app_data$video_timing_df),
                   timing_tstart("Block", "Reception", timing_df = app_data$video_timing_df),
                   timing_tstart("Block", "Transition", timing_df = app_data$video_timing_df),
                   timing_tstart("Dig", "Transition", timing_df = app_data$video_timing_df),
                   timing_tstart("Freeball", "Reception", timing_df = app_data$video_timing_df),
                   timing_tstart("Freeball", "Transition", timing_df = app_data$video_timing_df)),
           tags$tr(tags$th("Duration:"),
                   timing_tdur("Serve", "Serve", timing_df = app_data$video_timing_df),
                   timing_tdur("Reception", "Reception", timing_df = app_data$video_timing_df),
                   timing_tdur("Set", "Reception", timing_df = app_data$video_timing_df),
                   timing_tdur("Set", "Transition", timing_df = app_data$video_timing_df),
                   timing_tdur("Attack", "Reception", timing_df = app_data$video_timing_df),
                   timing_tdur("Attack", "Transition", timing_df = app_data$video_timing_df),
                   timing_tdur("Block", "Reception", timing_df = app_data$video_timing_df),
                   timing_tdur("Block", "Transition", timing_df = app_data$video_timing_df),
                   timing_tdur("Dig", "Transition", timing_df = app_data$video_timing_df),
                   timing_tdur("Freeball", "Reception", timing_df = app_data$video_timing_df),
                   timing_tdur("Freeball", "Transition", timing_df = app_data$video_timing_df))
           ),
tags$div(style = "display:none;", icon("question-circle")), ## to ensure that font-awesome dependency is included
tags$script("set_vspinner = function() { $('#dv_player').addClass('loading'); }"),
tags$script("remove_vspinner = function() { $('#dv_player').removeClass('loading'); }"),
tags$style("video.loading { background: black; }"),
tags$script("dv_player_onerror = function(e) { $('#dv_player').removeClass('loading'); try { var this_src = btoa(document.getElementById(e.target.id).getAttribute('src')); } catch { var this_src = ''; }; Shiny.setInputValue('video_error', e.target.id + '@' + this_src + '@' + e.target.error.code + '@' + new Date().getTime()); }"),
tags$script("dvpl = new dvjs_controller('dv_player','local',true); dvpl.video_afterpause=function() { Shiny.setInputValue('player_pause_state', dvpl.video_controller.paused); }; dvpl.video_onstart=function() { Shiny.setInputValue('playstable_current_item', dvpl.video_controller.current); el = document.getElementById(\"subtitle\"); if (el !== null) el.textContent=dvpl.video_controller.queue[dvpl.video_controller.current].subtitle; el = document.getElementById(\"subtitleskill\"); if (el !== null) el.textContent=dvpl.video_controller.queue[dvpl.video_controller.current].subtitleskill; if (dvpl.video_controller.type == 'youtube' || dvpl.video_controller.type == 'twitch') { Shiny.setInputValue('dvyt_height', $('#dvyt_player').innerHeight()); Shiny.setInputValue('dvyt_width', $('#dvyt_player').innerWidth()); } else { Shiny.setInputValue('dv_height', $('#dv_player').innerHeight()); Shiny.setInputValue('dv_width', $('#dv_player').innerWidth()); } Shiny.setInputValue('vo_voffset', $('#video_holder').innerHeight()); }")
)
}
