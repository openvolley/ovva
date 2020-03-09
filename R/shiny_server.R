ovva_shiny_server <- function(app_data) {
    function(input, output, session) {

        ## helper function: get the right function from the playlist handler for a given skill and specific
        fun_from_playlist <- function(skill, specific) {
            idx <- which(app_data$playlist_handler$skill %eq% skill & app_data$playlist_handler$specific %eq% specific)
            if (length(idx) != 1) stop("cannot find playlist handler entry with skill '", skill, "' and specific '", specific, "'")
            app_data$playlist_handler$fun[[idx]]
        }

        ## play-by-play data for selected season
        pbp <- reactive({
            if (!is.null(input$season) && input$season %in% names(app_data$data_path)) {
                ## TODO use alldata.rds if available
                myfiles <- dir(app_data$data_path[[input$season]], pattern = "\\.dvw$", ignore.case = TRUE, full.names = TRUE)
                mydat <- bind_rows(lapply(myfiles, function(z) read_dv(z, skill_evaluation_decode = "guess")$plays)) ## other args to read_dv?
                mydat <- ungroup(mutate(group_by(mydat, .data$match_id), game_date = min(as.Date(.data$time), na.rm = TRUE)))
                mutate(mydat, game_id = paste0(.data$game_date,"_", gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1', .data$home_team, perl = TRUE),
                                               "_", gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',.data$visiting_team, perl = TRUE)))
            } else {
                NULL
            }
        })
        ## metadata for selected season matches
        meta <- reactive({
            check_duplicates <- function(metas) {
                if (any(duplicated(lapply(metas, function(z) z$match_id)))) stop("duplicate match_ids")
                metas
            }
            if (!is.null(input$season) && input$season %in% names(app_data$data_path)) {
                ## TODO use allmeta.rds if available
                myfiles <- dir(app_data$data_path[[input$season]], pattern = "\\.dvw$", ignore.case = TRUE, full.names = TRUE)
                check_duplicates(lapply(myfiles, function(z) read_dv(z)$meta))
            } else {
                NULL
            }
        })

        ## Augment pbp with additional covariates
        pbp_augment <- reactive({ 
            preprocess_data(pbp())
        })
        ## Game ID
        game_id_list = reactive({
            unique(na.omit(pbp_augment()$game_id))
        })
        observe({
            updateSelectInput(session, "choose_game_id", choices = game_id_list())
        })
        ## take chosen game_id from DT
        selected_game_id <- reactive({
            if (is.null(input$game_id_table_rows_selected)) {
                NULL
            } else {
                game_table_data()$game_id[input$game_id_table_rows_selected]
            }
        })


        ## Team
        team_list = reactive({
            tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id())
            unique(na.omit(tmp$team))
        })
        observe({
            updateSelectInput(session, "team_list", choices = team_list())
        })

        ## Player ID
        player_list = reactive({
            tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$team %in% input$team_list)
            unique(na.omit(tmp$player_name))
        })
        observe({
            updatePickerInput(session, "player_list", choices = player_list())
        })

        ## Skill
        skill_list = reactive({
            tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list)
            unique(na.omit(tmp$skill))
        })
        observe({
            updateSelectInput(session, "skill_list", choices = skill_list())
        })

        ## Pre-defined playlist
        playlist_list = reactive({
            app_data$playlist_handler$specific[app_data$playlist$skill %in% input$skill_list]
        })
        observe({
            updatePickerInput(session, "playlist_list", choices = playlist_list(), selected = NULL)
        })

        ## Skilltype
        skilltype_list = reactive({
            tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$skill %in% input$skill_list, .data$team %in% input$team_list)
            unique(tmp$skilltype)
        })
        observe({
            updatePickerInput(session, "skilltype_list", choices = skilltype_list(), selected = skilltype_list())
        })
        
        ## Phase
        phase_list = reactive({
            tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list,
                          .data$skill %in% input$skill_list)
            unique(tmp$phase)
        })
        observe({
            updatePickerInput(session, "phase_list", choices = phase_list(), selected = phase_list())
        })

        ## Advanced filter
        adFilter_list = reactive({
            colnames(dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list,
                          .data$skill %in% input$skill_list))
        })
        observe({
            updateSelectInput(session, "adFilter_list", choices = adFilter_list())
        })
        
        ## Advanced filter value
        adFilterValue_list = reactive({
            col_to_select <- input$adFilter_list
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment()) || length(col_to_select) < 1) {
                NULL
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
                unique(tmp[[col_to_select]])
            }
        })
        observe({
            updatePickerInput(session, "adFilterValue_list", choices = adFilterValue_list(), selected = adFilterValue_list())
        })

        ## Advanced filter 2
        adFilterB_list = reactive({
            colnames(dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list,
                          .data$skill %in% input$skill_list))
        })
        observe({
            updateSelectInput(session, "adFilterB_list", choices = adFilterB_list())
        })

        ## Advanced filter 2 value
        adFilterBValue_list = reactive({
            col_to_select <- input$adFilterB_list
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment()) || length(col_to_select) < 1) {
                NULL
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
                unique(tmp[[col_to_select]])
            }
        })
        observe({
            updatePickerInput(session, "adFilterBValue_list", choices = adFilterBValue_list(), selected = adFilterBValue_list())
        })
        
        ## Help
        observeEvent(input$help,
                     rintrojs::introjs(session, options = list("nextLabel"="Next",
                                                              "prevLabel"="Previous",
                                                              "skipLabel"="Skip")
                             )
                     )


        ## Game ID Table
        game_table_data <- reactive({
            pbp <- pbp_augment()
            ## Customize pbp
            pbp_game_id <- dplyr::select(distinct(pbp, .data$game_id, .data$game_date, .data$visiting_team, .data$home_team), "game_id", "game_date", "visiting_team", "home_team")
        })
        output$game_id_table <- DT::renderDataTable({
            DT::datatable(game_table_data(),
                          extensions = "Scroller",
                          filter = "top", options = list(
                                              deferRender = TRUE,
                                              scrollY = 200,
                                              scroller = TRUE
                                          ),
                          rownames = FALSE,
                          colnames = c("Game ID",
                                       "Game date",
                                       "Away",
                                       "Home")
                          )
        })

        ## Table of all actions as per selected_game_id() and player_id() and evaluation()
        recap_dt <- reactive({
            pbp <- pbp_augment()
            ## Customize pbp
            if (is.null(selected_game_id())) {
                NULL
            } else {
                meta <- meta()
                game_select <- selected_game_id()
                team_select <- input$team_list
                player_select <- input$player_list
                skill_select <- input$skill_list
                skilltype_select <- input$skilltype_list
                phase_select <- input$phase_list
                filter_var <- input$adFilter_list
                filter_value_select <- input$adFilterValue_list
                filterB_var <- input$adFilterB_list
                filterB_value_select <- input$adFilterBValue_list
                playlist_select <- input$playlist_list
                if(!is.null(playlist_select) & !is.null(skill_select) & !is.null(game_select) & !is.null(player_select) & !is.null(team_select)) {
                    myfun <- fun_from_playlist(skill_select, playlist_select)
                    if (length(game_select) == 1) {
                        ##pbp_tmp <- app_data$playlist_handler(x = pbp %>% dplyr::filter(game_id %in% game_select),
                        ##                                     team = team_select, player = player_select, skill = skill_select, specific = playlist_select)
                        pbp_tmp <- myfun(x = dplyr::filter(pbp, .data$game_id %in% game_select), team = team_select, player = player_select)
                    } else{
                        ##pbp_tmp <- dplyr::filter(pbp, .data$game_id %in% game_select) %>% split(.$match_id) %>% map_dfr(~app_data$playlist_handler(x = .,
                        ##                                                                                                             team = team_select, player = player_select, skill = skill_select, specific = playlist_select))
                        pbp_tmp <- dplyr::filter(pbp, .data$game_id %in% game_select)
                        pbp_tmp <- bind_rows(lapply(split(pbp_tmp, pbp_tmp$match_id), myfun, team = team_select, player = player_select))
                    }
                } else {
                    pbp_tmp <- dplyr::filter(pbp, .data$player_name %in% input$player_list,
                                             .data$skill %in% input$skill_list,
                                             .data$skilltype %in% input$skilltype_list,
                                             .data$team %in% input$team_list,
                                             .data$phase %in% input$phase_list,
                                             .data$game_id %in% selected_game_id(),
                                             .data[[filter_var]] %in% filter_value_select,
                                             .data[[filterB_var]] %in% filterB_value_select)
                }
                pbp_game_id <- dplyr::summarize(group_by(pbp_tmp, .data$game_id, .data$evaluation_code), N = n())
                DT::datatable(pbp_game_id,
                              extensions = "Scroller",
                              filter = "top", options = list(deferRender = TRUE, scrollY = 200, scroller = TRUE),
                              rownames = FALSE,
                              colnames = c("Game ID", "Evaluation", "N")
                              )
            }
        })
        output$official_recap <- DT::renderDataTable({
            recap_dt()
        })

        playlist <-reactive({
            pbp <- pbp_augment()
            meta <- meta()
            game_select <- selected_game_id()
            team_select <- input$team_list
            player_select <- input$player_list
            skill_select <- input$skill_list
            skilltype_select <- input$skilltype_list
            phase_select <- input$phase_list
            filter_var <- input$adFilter_list
            filter_value_select <- input$adFilterValue_list
            filterB_var <- input$adFilterB_list
            filterB_value_select <- input$adFilterBValue_list
            playlist_select <- input$playlist_list
            ## Customize pbp
            if (is.null(game_select)) {
                NULL
            } else {
                if (!is.null(playlist_select) & !is.null(skill_select) & !is.null(game_select) & !is.null(player_select) & !is.null(team_select)) {
                    myfun <- fun_from_playlist(skill_select, playlist_select)
                    if (length(game_select) == 1) {
                        event_list <- myfun(x = dplyr::filter(pbp, .data$game_id %in% game_select), team = team_select, player = player_select)
                    } else{
                        ##event_list <- pbp %>% dplyr::filter(game_id %in% game_select) %>% split(.$match_id) %>% map_dfr(~app_data$playlist_handler(x = .,
                        ##                                                                                                                team = team_select, player = player_select, skill = skill_select, specific = playlist_select))
                        event_list <- dplyr::filter(pbp, .data$game_id %in% game_select)
                        event_list <- bind_rows(lapply(split(event_list, event_list$match_id), myfun, team = team_select, player = player_select))
                    }
                } else {
                    event_list <- dplyr::filter(pbp, .data$player_name %in% player_select,
                                                .data$skill %in% skill_select,
                                                .data$game_id %in% game_select,
                                                .data$team %in% team_select,
                                                .data$phase %in% phase_select,
                                                .data$skilltype %in% skilltype_select,
                                                .data[[filter_var]] %in% filter_value_select,
                                                .data[[filterB_var]] %in% filterB_value_select)
                }
                match_select <- unique(na.omit(event_list$match_id))
                meta_video <- bind_rows(lapply(meta, function(z) mutate(z$video, match_id = z$match_id)))
                meta_video <- dplyr::filter(meta_video, .data$match_id %in% match_select)

                if (nrow(meta_video) < 1) return(NULL)
                ##if (app_data$video_serve_method == "shiny") {
                ##    ## we are serving the video through the shiny server, so need to make symlinks in the www directory
                ##    vf <- fs::path_norm(meta_video$file)
                ##    if (is.null(vf) || length(vf) < 1) return(NULL)
                ##    ## may have multiple video files at this point
                ##    for (thisf in vf) {
                ##        if (fs::file_exists(thisf)) {
                ##            symlink_abspath <- fs::path_abs(file.path("www", basename(meta_video$file))) ## TODO check that this works when deployed
                ##            suppressWarnings(try(unlink(symlink_abspath), silent = TRUE))
                ##            thisf <- gsub(" ", "\\\\ " , thisf) ## this may not work on Windows
                ##            system2("ln", c("-s", thisf, "www/"))
                ##            onStop(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                ##            onSessionEnded(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                ##        } else {
                ##            ## video file does not exist!
                ##            stop("video file ", thisf, " does not exist, not handled yet")
                ##        }
                ##    }
                ##    meta_video$video_src <- basename(meta_video$file)
                ##} else
                if (app_data$video_serve_method == "lighttpd") {
                    ## we are serving the video through the lighttpd server, so need to make symlinks in its document root directory pointing to the actual video files
                    vf <- fs::path_norm(meta_video$file)
                    if (is.null(vf) || length(vf) < 1) return(NULL)
                    ## may have multiple video files at this point
                    for (thisf in vf) {
                        if (fs::file_exists(thisf)) {
                            symlink_abspath <- fs::path_abs(file.path(app_data$video_server_dir, basename(thisf))) ## TODO check that this works when deployed
                            suppressWarnings(try(unlink(symlink_abspath), silent = TRUE))
                            thisf <- gsub(" ", "\\\\ " , thisf) ## this may not work on Windows
                            system2("ln", c("-s", thisf, symlink_abspath))
                            onStop(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                            onSessionEnded(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                        } else {
                            ## video file does not exist!
                            stop("video file ", thisf, " does not exist, not handled yet")
                        }
                    }
                    meta_video$video_src <- file.path(app_data$video_server_url, basename(meta_video$file))
                ##} else if (app_data$video_serve_method == "standalone") {
                ##    meta_video$video_src <- meta_video$file ## full (local) path
                } else {
                    stop("unrecognized video_serve_method: ", app_data$video_serve_method)
                }
                event_list <- mutate(event_list, skilltype = case_when(.data$skill %in% c("Serve", "Reception", "Dig", "Freeball", "Block") ~ .data$skill_type,
                                                                       .data$skill == "Attack" ~ .data$attack_description),
                                     subtitle = paste("Set", .data$set_number, "-", .data$home_team, .data$home_team_score, "-", .data$visiting_team_score, .data$visiting_team),
                                     subtitleskill = paste(.data$player_name, "-", .data$skilltype, ":", .data$evaluation_code))
                ovideo::ov_video_playlist(x = event_list, meta = meta_video, type= "local", timing = ovideo::ov_video_timing(), extra_cols = c("subtitle", "subtitleskill"))
            }
        })

        observe({
            if (!is.null(playlist())) {
                ## when playlist() changes, push it through to the javascript playlist
                shinyjs::runjs(ovideo::ov_playlist_as_onclick(playlist(), video_id = "dv_player", dvjs_fun = "dvjs_set_playlist_and_play"))
            }
        })
        ## Video of skills
        output$player_ui <- renderUI({
            ##if (app_data$video_serve_method == "standalone") {
            ##    NULL
            ##} else {
                tagList(ovideo::ov_video_player(id = "dv_player", type = "local", controls = FALSE, style = "border: 1px solid black; width: 90%;"),
                        uiOutput("player_controls_ui")) ## deal with controls manually
            ##}
        })
        output$player_controls_ui <- renderUI({
            ##if (is.null(playlist()) || app_data$video_serve_method == "standalone") {
            ##    NULL
            ##} else {
                tags$div(tags$button("Play", onclick = "dvjs_video_play();"),
                         tags$button("Prev", onclick = "dvjs_video_prev();"),
                         tags$button("Next", onclick = "dvjs_video_next();"),
                         tags$button("Pause", onclick = "dvjs_video_pause();"),
                         actionButton("video_back_1s", "Back 1s"),
                         tags$span(id = "subtitle", "Score"),
                         tags$span(id = "subtitleskill", "Skill"))
            ##}
        })
        observeEvent(input$playback_rate, {
            if (!is.null(input$playback_rate)) do_video("playback_rate", input$playback_rate)
        })
        observeEvent(input$video_back_1s, do_video("rew", 1.0))
        ## video helper functions
        do_video <- function(what, ..., id = "dv_player") {
            getel <- paste0("document.getElementById('", id, "')")
            myargs <- list(...)
            if (what == "toggle_pause") {
                shinyjs::runjs(paste0("if (", getel, ".paused == true) { ", getel, ".play(); } else { ", getel, ".pause(); }"))
            } else if (what == "rew") {
                shinyjs::runjs(paste0(getel, ".currentTime=", getel, ".currentTime - ", myargs[[1]], ";"))
            } else if (what == "ff") {
                shinyjs::runjs(paste0(getel, ".currentTime=", getel, ".currentTime + ", myargs[[1]], ";"))
            } else if (what == "playback_rate") {
                shinyjs::runjs(paste0(getel, ".playbackRate=", myargs[[1]], ";"))
            } else {
                NULL
            }
        }

        output$chart_ui <- renderUI(apps_data$chart_renderer)
##        preview_filename <- reactiveVal(tempfile(fileext = ".html"))
##        output$preview_button_ui <- renderUI({
##            if (app_data$video_serve_method != "standalone" || is.null(playlist())) {
##                NULL
##            } else {
##                actionButton("preview_button", "Make preview")
##            }
##        })
##        observeEvent(input$preview_button, {
##            ## make standalone player of current playlist
##            working_dir <- tempfile()
##            dir.create(working_dir)
##            rmd_template <- file.path(working_dir, "player.Rmd")
##            if (!file.copy(from = "standalone_template.Rmd", to = rmd_template)) stop("cannot copy template file to temporary directory")
##            outfile <- preview_filename() ## just use the same file for all previews, so that it can just be refreshed in the browser
##            ## parms to pass to Rmd template
##            vsx <- list(playlist = playlist(),
##                        game_id = selected_game_id(),
##                        skill = input$skill_list,
##                        player = input$player_list,
##                        team = input$team_list,
##                        skilltype = input$skilltype_list,
##                        phase = input$phase_list,
##                        recap_dt = recap_dt(), chart = chart_to_plot(),
##                        banner = fs::path_real("www/banner.png"))
##
##            blah <- knitr::knit_meta(class = NULL, clean = TRUE) ## may help stop memory allocation error
##            render(rmd_template, output_file = outfile)
##            output$open_preview_ui <- renderUI({
##                tags$a(href = sub("file:////", "file:///", paste0("file:///", preview_filename()), fixed = TRUE), target = "_blank", "Open preview (right click -> new tab)")
##            })
##        })
    }
}
