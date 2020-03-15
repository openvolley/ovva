ovva_shiny_server <- function(app_data) {
    function(input, output, session) {
        ## helper function: get the right function from the playlist handler for a given skill and specific
        have_done_startup <- reactiveVal(FALSE)
        funs_from_playlist <- function(specific) {
            ## return a list of functions
            app_data$playlist_handler$fun[which(app_data$playlist_handler$specific %in% specific)]
        }
        funs_from_highlight<- function(specific) {
            ## return a list of functions
            app_data$highlight_handler$fun[which(app_data$highlight_handler$specific %in% specific)]
        }

        get_data_paths <- reactive({
            if (is.function(app_data$data_path)) {
                app_data$data_path()
            } else {
                app_data$data_path
            }
        })
        ## update the season choices
        observe({
            chc <- names(get_data_paths())
            isolate(sel <- input$season)
            if (!sel %in% chc) sel <- chc[1]
            updateSelectInput(session, "season", choices = chc, selected = sel)
        })
        ## play-by-play data for selected season
        pbp <- reactiveVal(NULL)
        pbp_augment <- reactiveVal(NULL)
        ## process metadata for selected season matches and update pbp reactiveVal accordingly
        meta <- reactive({
            if (!is.null(input$season) && input$season %in% names(get_data_paths())) {
                showModal(modalDialog(title = "Processing data ...", footer = NULL, "Please wait"))
                if (file.exists(file.path(get_data_paths()[[input$season]], "allmeta.rds"))) {
                    ## use allmeta.rds if available
                    tmp <- readRDS(file.path(get_data_paths()[[input$season]], "allmeta.rds"))
                    out <- lapply(tmp, function(z) z$meta)
                } else {
                    myfiles <- dir(get_data_paths()[[input$season]], pattern = "\\.dvw$", ignore.case = TRUE, full.names = TRUE)
                    out <- lapply(myfiles, function(z) read_dv(z, metadata_only = TRUE)$meta)
                }
                ## check for duplicate match IDs - these could have different video files, which is too much hassle to handle
                if (any(duplicated(lapply(out, function(z) z$match_id)))) {
                    output$processing_note <- renderUI(tags$div(class = "alert alert-danger", "There are duplicate match IDs"))
                    out <- NULL
                } else {
                    output$processing_note <- renderUI(NULL)
                }
                ## out is a list of metadata objects
                ## prune out any that don't have video
                if (!is.null(out)) out <- Filter(function(z) !is.null(z$video) && nrow(z$video) > 0, out)
                if (length(out) < 1) {
                    pbp(NULL)
                    pbp_augment(NULL)
                    out <- NULL
                } else {
                    ## now process pbp()
                    my_match_ids <- as.character(lapply(out, function(z) z$match_id))
                    showModal(modalDialog(title = "Processing data ...", footer = NULL, "Please wait"))
                    if (file.exists(file.path(get_data_paths()[[input$season]], "alldata.rds"))) {
                        ## use alldata.rds if available
                        mydat <- readRDS(file.path(get_data_paths()[[input$season]], "alldata.rds"))
                    } else {
                        myfiles <- dir(get_data_paths()[[input$season]], pattern = "\\.dvw$", ignore.case = TRUE, full.names = TRUE)
                        mydat <- bind_rows(lapply(myfiles, function(z) read_dv(z, skill_evaluation_decode = "guess")$plays)) ## other args to read_dv?
                    }
                    mydat <- mydat[mydat$match_id %in% my_match_ids, ]
                    mydat <- ungroup(mutate(group_by(mydat, .data$match_id), game_date = min(as.Date(.data$time), na.rm = TRUE)))
                    mydat <- mutate(mydat, game_id = paste0(gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1', .data$home_team, perl = TRUE),
                                                            "_", gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',.data$visiting_team, perl = TRUE)))
                    mydat <- mutate(mydat, game_id = case_when(!is.na(.data$game_date) & !is.infinite(.data$game_date) ~ paste0(.data$game_date, "_", .data$game_id),
                                                               TRUE ~ .data$game_id))
                    pbp(mydat)
                    ## Augment pbp with additional covariates
                    pbp_augment(preprocess_data(mydat))
                }
                removeModal()
                have_done_startup(TRUE)
                out
            } else {
                pbp(NULL)
                pbp_augment(NULL)
                NULL
            }
        })

        ## Game ID
        game_id_list = reactive({
            if (is.null(pbp_augment())) NULL else unique(na.omit(pbp_augment()$game_id))
        })
        observe({
            updateSelectInput(session, "choose_game_id", choices = game_id_list())
        })
        ## take chosen game_id from DT
        selected_game_id <- reactive({
            if (is.null(input$game_id_table_rows_selected) || is.null(game_table_data())) {
                NULL
            } else {
                game_table_data()$game_id[input$game_id_table_rows_selected]
            }
        })


        ## Team
        team_list = reactive({
            if (is.null(pbp_augment())) {
                NULL
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id())
                unique(na.omit(tmp$team))
            }
        })
        observe({
            updateSelectInput(session, "team_list", choices = team_list())
        })

        ## Player ID
        player_list = reactive({
            if (is.null(pbp_augment())) {
                NULL
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$team %in% input$team_list)
                unique(na.omit(tmp$player_name))
            }
        })
        observe({
            updatePickerInput(session, "player_list", choices = player_list())
        })

        ## Skill
        skill_list = reactive({
            if (is.null(pbp_augment())) {
                NULL
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list)
                unique(na.omit(tmp$skill))
            }
        })
        observe({
            updateSelectInput(session, "skill_list", choices = skill_list())
        })

        ## Pre-defined playlist
        playlist_list = reactive({
            tryCatch(app_data$playlist_handler$specific[app_data$playlist_handler$skill %in% input$skill_list],
                     error = function(e) dplyr::tibble(skill = character(), specific = character(), fun = list()))
        })
        output$playlist_based_ui <- renderUI({
            if (length(playlist_list()) < 1) {
                if (length(skill_list()) < 1) {
                    tags$div(class = "alert alert-info", "Choose a skill first")
                } else {
                    tags$div(class = "alert alert-info", "No playlists have been defined for the chosen skill")
                }
            } else {
                ## populate playlist_list options, keeping any existing selections
                isolate(sel <- input$playlist_list)
                if (!is.null(sel)) sel <- intersect(sel, playlist_list())
                pickerInput(inputId = "playlist_list",
                            label = "Playlists",
                            choices = playlist_list(),
                            selected = sel,
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)
            }
        })

        ## Highlights
        highlight_list = reactive({
            app_data$highlight_handler$specific[app_data$highlight$skill %in% "Highlights"]
        })
        output$highlight_based_ui <- renderUI({
                if (length(game_id_list()) < 1) {
                    tags$div(class = "alert alert-info", "Choose a game first")
                } else {
                ## populate highlight_list options, keeping any existing selections
                shiny::isolate(sel <- input$highlight_list)
                if (!is.null(sel)) sel <- intersect(sel, highlight_list())
                pickerInput(inputId = "highlight_list",
                            label = "Highlights",
                            choices = highlight_list(),
                            selected = sel,
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)
            }
        })

        ## Skilltype
        skilltype_list = reactive({
            if (is.null(pbp_augment())) {
                NULL
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$skill %in% input$skill_list, .data$team %in% input$team_list)
                unique(tmp$skilltype)
            }
        })
        observe({
            updatePickerInput(session, "skilltype_list", choices = skilltype_list(), selected = skilltype_list())
        })

        ## Phase
        phase_list = reactive({
            if (is.null(pbp_augment())) {
                NULL
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
                unique(tmp$phase)
            }
        })
        observe({
            updatePickerInput(session, "phase_list", choices = phase_list(), selected = phase_list())
        })

        ## Advanced filter
        adFilter_list = reactive({
            if (is.null(pbp_augment())) {
                NULL
            } else {
                colnames(dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list))
            }
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
            if (is.null(pbp_augment())) {
                NULL
            } else {
                colnames(dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list))
            }
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
            if (is.null(pbp_augment())) {
                output$no_game_data <- renderUI(
                    if (is.null(input$season)) {
                        tags$div(class = "alert alert-info", "No competition data sets. Log in?")
                    } else if (!is.null(meta()) && is.null(pbp())) {
                        tags$div(class = "alert alert-danger", "No matches with video could be found.")
                    } else if (is.null(meta()) && have_done_startup()) {
                        tags$div(class = "alert alert-danger", "Sorry, something went wrong processing this data set.")
                    } else {
                        NULL
                    })
                NULL
            } else {
                output$no_game_data <- renderUI(NULL)
                ## Customize pbp
                dplyr::select(distinct(pbp_augment(), .data$game_id, .data$game_date, .data$visiting_team, .data$home_team), "game_id", "game_date", "visiting_team", "home_team")
            }
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
            ## Customize pbp
            if (is.null(pbp_augment()) || is.null(selected_game_id())) {
                NULL
            } else {
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
                highlight_select <- input$highlight_list

                if (!is.null(playlist_select) & !is.null(skill_select) & !is.null(game_select) & !is.null(player_select) & !is.null(team_select)) {
                    myfuns <- funs_from_playlist(playlist_select)
                    if (length(game_select) == 1) {
                        ## apply each of myfuns in turn and rbind the results
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) myfun(x = dplyr::filter(pbp, .data$game_id %in% game_select), team = team_select, player = player_select)))
                    } else{
                        ##pbp_tmp <- dplyr::filter(pbp, .data$game_id %in% game_select) %>% split(.$match_id) %>% map_dfr(~app_data$playlist_handler(x = .,
                        ##                                                                                                             team = team_select, player = player_select, skill = skill_select, specific = playlist_select))
                        pbp_tmp <- dplyr::filter(pbp, .data$game_id %in% game_select)
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) bind_rows(lapply(split(pbp_tmp, pbp_tmp$match_id), myfun, team = team_select, player = player_select))))
                    }
                } else if (!is.null(highlight_select) & !is.null(game_select)) {
                    myfuns <- funs_from_highlight(highlight_select)
                    if (length(game_select) == 1) {

                        ## apply each of myfuns in turn and rbind the results
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) myfun(x = dplyr::filter(pbp, .data$game_id %in% game_select), team = team_select, player = player_select)))
                    } else{
                        pbp_tmp <- dplyr::filter(pbp, .data$game_id %in% game_select)
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) bind_rows(lapply(split(pbp_tmp, pbp_tmp$match_id), myfun, team = team_select, player = player_select))))
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
                if (!is.null(highlight_select) & !is.null(game_select)) {
                    pbp_game_id <- dplyr::summarize(group_by(pbp_tmp, .data$game_id, .data$point_id), N = n())
                    DT::datatable(pbp_game_id,
                                  extensions = "Scroller",
                                  filter = "top", options = list(deferRender = TRUE, scrollY = 200, scroller = TRUE),
                                  rownames = FALSE,
                                  colnames = c("Game ID", "Point ID", "N"))

                } else {
                    pbp_game_id <- dplyr::summarize(group_by(pbp_tmp, .data$game_id, .data$evaluation_code), N = n())
                    DT::datatable(pbp_game_id,
                                  extensions = "Scroller",
                                  filter = "top", options = list(deferRender = TRUE, scrollY = 200, scroller = TRUE),
                                  rownames = FALSE,
                                  colnames = c("Game ID", "Evaluation", "N")
                                  )
                }
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
            highlight_select <- input$highlight_list
            ## Customize pbp
            if (is.null(pbp) || is.null(meta) || is.null(game_select)) {
                NULL
            } else {
                if (!is.null(playlist_select) & !is.null(skill_select) & !is.null(game_select) & !is.null(player_select) & !is.null(team_select)) {
                    myfuns <- funs_from_playlist(playlist_select)
                    if (length(game_select) == 1) {
                        event_list <- bind_rows(lapply(myfuns, function(myfun) myfun(x = dplyr::filter(pbp, .data$game_id %in% game_select), team = team_select, player = player_select)))
                    } else{
                        event_list <- dplyr::filter(pbp, .data$game_id %in% game_select)
                        event_list <- bind_rows(lapply(myfuns, function(myfun) bind_rows(lapply(split(event_list, event_list$match_id), myfun, team = team_select, player = player_select))))
                    }
                } else if (!is.null(highlight_select) & !is.null(game_select)) {
                    myfuns <- funs_from_highlight(highlight_select)
                    if (length(game_select) == 1) {
                        event_list <- bind_rows(lapply(myfuns, function(myfun) myfun(x = dplyr::filter(pbp, .data$game_id %in% game_select), team = team_select, player = player_select)))
                    } else{
                        ##event_list <- pbp %>% dplyr::filter(game_id %in% game_select) %>% split(.$match_id) %>% map_dfr(~app_data$playlist_handler(x = .,
                        ##                                                                                                                team = team_select, player = player_select, skill = skill_select, specific = playlist_select))
                        event_list <- dplyr::filter(pbp, .data$game_id %in% game_select)
                        event_list <- bind_rows(lapply(myfuns, function(myfun) bind_rows(lapply(split(event_list, event_list$match_id), myfun, team = team_select, player = player_select))))
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
                if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("lighttpd", "servr")) {
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
                        } else if (is_youtube_id(vf) || grepl("https?://", vf)) {
                            ## youtube ID or link to video, don't do anything
                        } else {
                            ## video file does not exist!
                            stop("video file ", thisf, " does not exist, not handled yet")
                        }
                    }
                    meta_video$video_src <- file.path(app_data$video_server_url, basename(meta_video$file))
                    nidx <- is_youtube_id(meta_video$file) | grepl("https?://", meta_video$file)
                    ## replace these with verbatim copy of original info
                    meta_video$video_src[nidx] <- meta_video$file[nidx]
                ##} else if (app_data$video_serve_method == "standalone") {
                    ##    meta_video$video_src <- meta_video$file ## full (local) path
                } else if (is.function(app_data$video_serve_method)) {
                    meta_video$video_src <- vapply(meta_video$file, app_data$video_serve_method, FUN.VALUE = "", USE.NAMES = FALSE)
                } else if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("none")) {
                    ## do nothing except pass the video file info into video_src
                    meta_video$video_src <- meta_video$file
                } else {
                    stop("unrecognized video_serve_method: ", app_data$video_serve_method)
                }
                event_list <- mutate(event_list, skilltype = case_when(.data$skill %in% c("Serve", "Reception", "Dig", "Freeball", "Block", "Set") ~ .data$skill_type,
                                                                       .data$skill == "Attack" ~ .data$attack_description),
                                     subtitle = paste("Set", .data$set_number, "-", .data$home_team, .data$home_team_score, "-", .data$visiting_team_score, .data$visiting_team),
                                     subtitleskill = paste(.data$player_name, "-", .data$skilltype, ":", .data$evaluation_code))
                vpt <- if (all(is_youtube_id(meta_video$video_src))) {
                           "youtube"
                       } else {
                           "local"
                       }
                ## TODO also check for mixed sources, which we can't handle yet
                video_player_type(vpt)
                if (!is.null(highlight_select)) {
                    ovideo::ov_video_playlist_pid(x = event_list, meta = meta_video, type= vpt, extra_cols = c("subtitle"))
                } else {
                    ovideo::ov_video_playlist(x = event_list, meta = meta_video, type= vpt, timing = ovideo::ov_video_timing(), extra_cols = c("subtitle", "subtitleskill"))
                }
            }
        })

        ## video stuff
        video_player_type <- reactiveVal("local") ## the current player type, either "local" or "youtube"
        observe({
            if (!is.null(playlist())) {
                ## when playlist() changes, push it through to the javascript playlist
                if (video_player_type() == "local") {
                    shinyjs::hide("dvyt_player")
                    shinyjs::show("dv_player")
                } else {
                    shinyjs::hide("dv_player")
                    shinyjs::show("dvyt_player")
                }
                ov_video_control("stop")
                shinyjs::runjs(ovideo::ov_playlist_as_onclick(playlist(), video_id = if (video_player_type() == "local") "dv_player" else "dvyt_player", dvjs_fun = "dvjs_set_playlist_and_play"))
            } else {
                ov_video_control("stop") ## empty playlist, so stop the video
            }
        })
        output$player_controls_ui <- renderUI({
            ##if (is.null(playlist()) || app_data$video_serve_method == "standalone") {
            ##    NULL
            ##} else {
                tags$div(tags$button("Play", onclick = "dvjs_video_play();"),
                         tags$button("Prev", onclick = "dvjs_video_prev();"),
                         tags$button("Next", onclick = "dvjs_video_next();"),
                         tags$button("Pause", onclick = "dvjs_video_pause();"),
                         tags$button("Back 1s", onclick = "dvjs_jog(-1);"),
                         tags$span(id = "subtitle", "Score"),
                         tags$span(id = "subtitleskill", "Skill"))
            ##}
        })

        output$create_clip_ui <- shiny::downloadHandler(
            filename = function() {
                #filename <- tempfile(fileext = ".mp4")
                filename <- paste0("Highlights",selected_game_id(),".mp4")
            },
            content = function(file) {
                filename <- tempfile(fileext = ".mp4")
                chk <- sys::exec_internal("ffmpeg", "-version")
                tempfiles <- future.apply::future_lapply(seq_len(nrow(playlist())), function(ri) {
                    outfile <- tempfile(fileext = paste0(".", fs::path_ext(playlist()$video_src[ri])))
                    if (file.exists(outfile)) unlink(outfile)
                    infile <-list.files(path = "/tmp/", pattern = basename(playlist()$video_src[ri]), recursive = TRUE, full.names = TRUE)
                    res <- sys::exec_internal("ffmpeg", c("-ss", playlist()$start_time[ri], "-i", infile, "-strict", "-2", "-t", playlist()$duration[ri], outfile))
                    if (res$status != 0) stop("failed to get video clip, ", rawToChar(res$stderr))
                    outfile
                })
                tempfiles <- unlist(tempfiles)
                cfile <- tempfile(fileext = ".txt")
                on.exit(unlink(c(cfile, tempfiles)))
                cat(paste0("file ", tempfiles), file = cfile, sep = "\n")
                if (file.exists(filename)) unlink(filename)
                res <- sys::exec_internal("ffmpeg", c("-safe", 0, "-f", "concat", "-i", cfile, "-c", "copy", filename))
                if (res$status != 0) stop("failed to combine clips, ", rawToChar(res$stderr))
                file.copy(filename, file)
            },
            contentType = "video/mp4"
        )

        observeEvent(input$playback_rate, {
            if (!is.null(input$playback_rate)) ov_video_control("set_playback_rate", input$playback_rate)
        })

        output$chart_ui <- renderUI(app_data$chart_renderer)
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
