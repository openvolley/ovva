ovva_shiny_server <- function(app_data) {
    function(input, output, session) {
        trace_execution <- FALSE ## for debugging
        allow_item_deletion <- FALSE
        plays_cols_to_show <- c("home_team", "visiting_team", "video_time", "code", "set_number", "home_team_score", "visiting_team_score")
        adfilter_cols_to_show <- c(##"time", "video_time", "code", "team", "player_number",
            "Skill rating code" = "evaluation_code", "Skill rating" = "evaluation",
            "Attack code" = "attack_code", "Attack code description" = "attack_description",
            "Setter call" = "set_code", "Setter call description" = "set_description", ##"set_type",
            "Start zone" = "start_zone", "End zone" = "end_zone", #"End subzone" = "end_subzone",
            "End cone" = "end_cone",
            "Skill subtype" = "skill_subtype", "Number of players" = "num_players", ##"num_players_numeric",
            "Special code" = "special_code",
            "Home team score" = "home_team_score", "Visiting team score" = "visiting_team_score",
            "Home team rotation (setter position)" = "home_setter_position", "Visiting team rotation (setter position)" = "visiting_setter_position",
            "Custom code" = "custom_code",
            ##"attack_phase", ## not really needed with phase already used
            ##"start_coordinate_x", "start_coordinate_y", "mid_coordinate_x", "mid_coordinate_y", "end_coordinate_x", "end_coordinate_y",
            ##home_player_id1", "home_player_id2", "home_player_id3", "home_player_id4", "home_player_id5", "home_player_id6",
            ##"visiting_player_id1", "visiting_player_id2", "visiting_player_id3", "visiting_player_id4", "visiting_player_id5", "visiting_player_id6",
            "Set number" = "set_number", ##"team_touch_id",
            "Home team" = "home_team", "Visiting team" = "visiting_team", "Point won by" = "point_won_by",
            "Receiving team" = "receiving_team", "Serving team" = "serving_team", ##"game_date",
            "Receiving team rotation (setter position)" = "receiving_setter_position", "Serving team rotation (setter position)" = "serving_setter_position",
            "Breakpoint/sideout" = "breakpoint/sideout", "Rotation (setter position)" = "setter_position",
            "Receiving player" = "receiving_player", "Reception grade" = "reception_grade",
            Setter = "setter", "Opposition setter" = "opposition_setter")

        ## some inits
        master_playstable_selected_row <- -99L ## non-reactive
        master_playstable_selected_row_sbs1 <- -99L ## non-reactive
        master_playstable_selected_row_sbs2 <- -99L ## non-reactive
        is_fresh_playlist <- FALSE

        ## helper function: get the right function from the playlist handler for a given skill and specific
        funs_from_playlist <- function(specific) {
            ## return a list of functions
            app_data$playlist_handler$fun[which(app_data$playlist_handler$specific %in% specific)]
        }
        funs_from_highlight<- function(specific) {
            ## return a list of functions
            app_data$highlight_handler$fun[which(app_data$highlight_handler$specific %in% specific)]
        }

        have_done_startup <- reactiveVal(FALSE)

        get_data_paths <- reactive({
            if (is.function(app_data$data_path)) {
                app_data$data_path()
            } else {
                app_data$data_path
            }
        })
        ## update the season choices
        season_choices <- reactive(names(get_data_paths()))
        observe({
            chc <- season_choices()
            isolate(sel <- input$season)
            if (is.null(sel) || !sel %in% chc) sel <- chc[1]
            updateSelectInput(session, "season", choices = chc, selected = sel)
        })
        observe({
            chc <- season_choices()
            isolate(sel <- input$season)
            if (is.null(sel) || !sel %in% chc) sel <- chc[1]
            updateSelectInput(session, "season_sbs1", choices = chc, selected = sel)
        })
        observe({
            chc <- season_choices()
            isolate(sel <- input$season)
            if (is.null(sel) || !sel %in% chc) sel <- chc[1]
            updateSelectInput(session, "season_sbs2", choices = chc, selected = sel)
        })
        ## play-by-play data for selected season
        pbp <- reactiveVal(NULL)
        pbp_augment <- reactiveVal(NULL)
        got_no_video <- reactiveVal(FALSE)
        ## process metadata for selected season matches and update pbp reactiveVal accordingly
        meta <- reactive({
            if (!is.null(input$season) && input$season %in% season_choices()) {
                if (trace_execution) message("recalculating meta")
                showModal(modalDialog(title = "Processing data ...", footer = NULL, "Please wait"))
                if (file.exists(file.path(get_data_paths()[[input$season]], "allmeta.rds"))) {
                    ## use allmeta.rds if available
                    tmp <- readRDS(file.path(get_data_paths()[[input$season]], "allmeta.rds"))
                    out <- lapply(tmp, function(z) z$meta)
                } else {
                    myfiles <- dir(get_data_paths()[[input$season]], pattern = "\\.(dvw|psvb)$", ignore.case = TRUE, full.names = TRUE)
                    dvargs <- if ("dv_read_args" %in% app_data) app_data$dv_read_args else list()
                    dvargs$metadata_only <- TRUE
                    out <- lapply(myfiles, function(z) if (grepl("psvb$", z, ignore.case = TRUE)) {
                                                           pv_read(z)$meta
                                                       } else {
                                                           dvargs$filename <- z
                                                           do.call(dv_read, dvargs)$meta
                                                       })
                }
                if (!is.null(app_data$meta_preprocess) && is.function(app_data$meta_preprocess)) {
                    try(out <- lapply(out, app_data$meta_preprocess))
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
                    got_no_video(TRUE)
                    pbp(NULL)
                    pbp_augment(NULL)
                    out <- NULL
                } else {
                    ## for each video file, check if it exists and try and find it if not
                    for (z in seq_along(out)) {
                        if (is_youtube_id(out[[z]]$video$file) || grepl("^https?://", out[[z]]$video$file, ignore.case = TRUE)) {
                            ## do nothing
                        } else {
                            try({
                                if (isTRUE(app_data$video_subtree_only)) {
                                    out[[z]]$video$file <- find_video_in_subtree(dvw_filename = out[[z]]$filename, video_filename = fs::fs_path(out[[z]]$video$file), subtree_only = TRUE, alt_path = app_data$alt_video_path)
                                } else {
                                    temp <- find_video_in_subtree(dvw_filename = out[[z]]$filename, video_filename = fs::fs_path(out[[z]]$video$file), subtree_only = FALSE, alt_path = app_data$alt_video_path)
                                    out[[z]]$video$file <- ifelse(!fs::file_exists(out[[z]]$video$file) && !is.na(temp), temp, out[[z]]$video$file)
                                }
                            })
                        }
                    }
                    ## remove any files with no associated video
                    out <- Filter(Negate(is.null), lapply(out, function(z) if (nrow(z$video) == 1 && !is.na(z$video$file) && nzchar(z$video$file)) z))
                    if (length(out) < 1) {
                        ## no files with video
                        got_no_video(TRUE)
                        pbp(NULL)
                        pbp_augment(NULL)
                        out <- NULL
                    } else {
                        got_no_video(FALSE)
                        ## now process pbp()
                        my_match_ids <- as.character(lapply(out, function(z) z$match_id))
                        showModal(modalDialog(title = "Processing data ...", footer = NULL, "Please wait"))
                        if (file.exists(file.path(get_data_paths()[[input$season]], "alldata.rds"))) {
                            ## use alldata.rds if available
                            mydat <- readRDS(file.path(get_data_paths()[[input$season]], "alldata.rds"))
                        } else {
                            myfiles <- dir(get_data_paths()[[input$season]], pattern = "\\.(dvw|psvb)$", ignore.case = TRUE, full.names = TRUE)
                            dvargs <- if ("dv_read_args" %in% app_data) app_data$dv_read_args else list()
                            if (!"skill_evaluation_decode" %in% names(dvargs)) dvargs$skill_evaluation_decode <- "guess"
                            mydat <- bind_rows(lapply(myfiles, function(z) if (grepl("psvb$", z)) {
                                                                               pv_read(z)$plays
                                                                           } else {
                                                                               dvargs$filename <- z
                                                                               do.call(dv_read, dvargs)$plays
                                                                           }))
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
                }
                removeModal()
                have_done_startup(TRUE)
                out
            } else {
                got_no_video(FALSE)
                pbp(NULL)
                pbp_augment(NULL)
                NULL
            }
        })

        
        ## play-by-play data sbs 1 for selected season
        pbp_sbs1 <- reactiveVal(NULL)
        pbp_augment_sbs1 <- reactiveVal(NULL)
        got_no_video_sbs1 <- reactiveVal(FALSE)
        ## process metadata for selected season matches and update pbp reactiveVal accordingly
        meta_sbs1 <- reactive({
            if (!is.null(input$season_sbs1) && input$season_sbs1 %in% season_choices()) {
                if (trace_execution) cat("recalculating meta\n")
                showModal(modalDialog(title = "Processing data ...", footer = NULL, "Please wait"))
                if (file.exists(file.path(get_data_paths()[[input$season_sbs1]], "allmeta.rds"))) {
                    ## use allmeta.rds if available
                    tmp <- readRDS(file.path(get_data_paths()[[input$season_sbs1]], "allmeta.rds"))
                    out <- lapply(tmp, function(z) z$meta)
                } else {
                    myfiles <- dir(get_data_paths()[[input$season_sbs1]], pattern = "\\.(dvw|psvb)$", ignore.case = TRUE, full.names = TRUE)
                    out <- lapply(myfiles, function(z) if (grepl("psvb$", z, ignore.case = TRUE)) pv_read(z)$meta else read_dv(z, metadata_only = TRUE)$meta)
                }
                if (!is.null(app_data$meta_preprocess) && is.function(app_data$meta_preprocess)) {
                    try(out <- lapply(out, app_data$meta_preprocess))
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
                    got_no_video_sbs1(TRUE)
                    pbp_sbs1(NULL)
                    pbp_augment_sbs1(NULL)
                    out <- NULL
                } else {
                    ## for each video file, check if it exists and try and find it if not
                    for (z in seq_along(out)) {
                        if (is_youtube_id(out[[z]]$video$file) || grepl("^https?://", out[[z]]$video$file, ignore.case = TRUE)) {
                            ## do nothing
                        } else {
                            try({
                                if (isTRUE(app_data$video_subtree_only)) {
                                    out[[z]]$video$file <- find_video_in_subtree(dvw_filename = out[[z]]$filename, video_filename = fs::fs_path(out[[z]]$video$file), subtree_only = TRUE, alt_path = app_data$alt_video_path)
                                } else {
                                    temp <- find_video_in_subtree(dvw_filename = out[[z]]$filename, video_filename = fs::fs_path(out[[z]]$video$file), subtree_only = FALSE, alt_path = app_data$alt_video_path)
                                    out[[z]]$video$file <- ifelse(!fs::file_exists(out[[z]]$video$file) && !is.na(temp), temp, out[[z]]$video$file)
                                }
                            })
                        }
                    }
                    ## remove any files with no associated video
                    out <- Filter(Negate(is.null), lapply(out, function(z) if (nrow(z$video) == 1 && !is.na(z$video$file) && nzchar(z$video$file)) z))
                    if (length(out) < 1) {
                        ## no files with video
                        got_no_video_sbs1(TRUE)
                        pbp_sbs1(NULL)
                        pbp_augment_sbs1(NULL)
                        out_sbs1 <- NULL
                    } else {
                        got_no_video_sbs1(FALSE)
                        ## now process pbp()
                        my_match_ids <- as.character(lapply(out, function(z) z$match_id))
                        showModal(modalDialog(title = "Processing data ...", footer = NULL, "Please wait"))
                        if (file.exists(file.path(get_data_paths()[[input$season_sbs1]], "alldata.rds"))) {
                            ## use alldata.rds if available
                            mydat <- readRDS(file.path(get_data_paths()[[input$season_sbs1]], "alldata.rds"))
                        } else {
                            myfiles <- dir(get_data_paths()[[input$season_sbs1]], pattern = "\\.(dvw|psvb)$", ignore.case = TRUE, full.names = TRUE)
                            mydat <- bind_rows(lapply(myfiles, function(z) if (grepl("psvb$", z)) pv_read(z)$plays else read_dv(z, skill_evaluation_decode = "guess")$plays)) ## other args to read_dv?
                        }
                        mydat <- mydat[mydat$match_id %in% my_match_ids, ]
                        mydat <- ungroup(mutate(group_by(mydat, .data$match_id), game_date = min(as.Date(.data$time), na.rm = TRUE)))
                        mydat <- mutate(mydat, game_id = paste0(gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1', .data$home_team, perl = TRUE),
                                                                "_", gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',.data$visiting_team, perl = TRUE)))
                        mydat <- mutate(mydat, game_id = case_when(!is.na(.data$game_date) & !is.infinite(.data$game_date) ~ paste0(.data$game_date, "_", .data$game_id),
                                                                   TRUE ~ .data$game_id))
                        pbp_sbs1(mydat)
                        ## Augment pbp with additional covariates
                        pbp_augment_sbs1(preprocess_data(mydat))
                    }
                }
                removeModal()
                have_done_startup(TRUE)
                out
            } else {
                got_no_video_sbs1(FALSE)
                pbp_sbs1(NULL)
                pbp_augment_sbs1(NULL)
                NULL
            }
        })
        
        ## play-by-play data sbs 2 for selected season
        pbp_sbs2 <- reactiveVal(NULL)
        pbp_augment_sbs2 <- reactiveVal(NULL)
        got_no_video_sbs2 <- reactiveVal(FALSE)
        ## process metadata for selected season matches and update pbp reactiveVal accordingly
        meta_sbs2 <- reactive({
            if (!is.null(input$season_sbs2) && input$season_sbs2 %in% season_choices()) {
                if (trace_execution) cat("recalculating meta\n")
                showModal(modalDialog(title = "Processing data ...", footer = NULL, "Please wait"))
                if (file.exists(file.path(get_data_paths()[[input$season_sbs2]], "allmeta.rds"))) {
                    ## use allmeta.rds if available
                    tmp <- readRDS(file.path(get_data_paths()[[input$season_sbs2]], "allmeta.rds"))
                    out <- lapply(tmp, function(z) z$meta)
                } else {
                    myfiles <- dir(get_data_paths()[[input$season_sbs2]], pattern = "\\.(dvw|psvb)$", ignore.case = TRUE, full.names = TRUE)
                    out <- lapply(myfiles, function(z) if (grepl("psvb$", z, ignore.case = TRUE)) pv_read(z)$meta else read_dv(z, metadata_only = TRUE)$meta)
                }
                if (!is.null(app_data$meta_preprocess) && is.function(app_data$meta_preprocess)) {
                    try(out <- lapply(out, app_data$meta_preprocess))
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
                    got_no_video_sbs2(TRUE)
                    pbp_sbs2(NULL)
                    pbp_augment_sbs2(NULL)
                    out <- NULL
                } else {
                    ## for each video file, check if it exists and try and find it if not
                    for (z in seq_along(out)) {
                        if (is_youtube_id(out[[z]]$video$file) || grepl("^https?://", out[[z]]$video$file, ignore.case = TRUE)) {
                            ## do nothing
                        } else {
                            try({
                                if (isTRUE(app_data$video_subtree_only)) {
                                    out[[z]]$video$file <- find_video_in_subtree(dvw_filename = out[[z]]$filename, video_filename = fs::fs_path(out[[z]]$video$file), subtree_only = TRUE, alt_path = app_data$alt_video_path)
                                } else {
                                    temp <- find_video_in_subtree(dvw_filename = out[[z]]$filename, video_filename = fs::fs_path(out[[z]]$video$file), subtree_only = FALSE, alt_path = app_data$alt_video_path)
                                    out[[z]]$video$file <- ifelse(!fs::file_exists(out[[z]]$video$file) && !is.na(temp), temp, out[[z]]$video$file)
                                }
                            })
                        }
                    }
                    ## remove any files with no associated video
                    out <- Filter(Negate(is.null), lapply(out, function(z) if (nrow(z$video) == 1 && !is.na(z$video$file) && nzchar(z$video$file)) z))
                    if (length(out) < 1) {
                        ## no files with video
                        got_no_video_sbs2(TRUE)
                        pbp_sbs2(NULL)
                        pbp_augment_sbs2(NULL)
                        out_sbs2 <- NULL
                    } else {
                        got_no_video_sbs2(FALSE)
                        ## now process pbp()
                        my_match_ids <- as.character(lapply(out, function(z) z$match_id))
                        showModal(modalDialog(title = "Processing data ...", footer = NULL, "Please wait"))
                        if (file.exists(file.path(get_data_paths()[[input$season_sbs2]], "alldata.rds"))) {
                            ## use alldata.rds if available
                            mydat <- readRDS(file.path(get_data_paths()[[input$season_sbs2]], "alldata.rds"))
                        } else {
                            myfiles <- dir(get_data_paths()[[input$season_sbs2]], pattern = "\\.(dvw|psvb)$", ignore.case = TRUE, full.names = TRUE)
                            mydat <- bind_rows(lapply(myfiles, function(z) if (grepl("psvb$", z)) pv_read(z)$plays else read_dv(z, skill_evaluation_decode = "guess")$plays)) ## other args to read_dv?
                        }
                        mydat <- mydat[mydat$match_id %in% my_match_ids, ]
                        mydat <- ungroup(mutate(group_by(mydat, .data$match_id), game_date = min(as.Date(.data$time), na.rm = TRUE)))
                        mydat <- mutate(mydat, game_id = paste0(gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1', .data$home_team, perl = TRUE),
                                                                "_", gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',.data$visiting_team, perl = TRUE)))
                        mydat <- mutate(mydat, game_id = case_when(!is.na(.data$game_date) & !is.infinite(.data$game_date) ~ paste0(.data$game_date, "_", .data$game_id),
                                                                   TRUE ~ .data$game_id))
                        pbp_sbs2(mydat)
                        ## Augment pbp with additional covariates
                        pbp_augment_sbs2(preprocess_data(mydat))
                    }
                }
                removeModal()
                have_done_startup(TRUE)
                out
            } else {
                got_no_video_sbs2(FALSE)
                pbp_sbs2(NULL)
                pbp_augment_sbs2(NULL)
                NULL
            }
        })
        
        ## Game ID
        selected_game_id <- reactive({
            if (trace_execution) message("recalculating game_table")
            if (is.null(input$game_table_dropdown)) {
                NULL
            } else {
                datatble <- dplyr::select(distinct(pbp_augment(), .data$game_id, .data$game_date, .data$visiting_team, .data$home_team), "game_id", "game_date", "visiting_team", "home_team")
                datatble <- mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team))
                datatble$game_id[datatble$display_ID %in% input$game_table_dropdown]
            }
        })

        selected_game_id_sbs1 <- reactive({
            if (is.null(input$game_table_dropdown_sbs1)) {
                NULL
            } else {
                datatble <- dplyr::select(distinct(pbp_augment_sbs1(), .data$game_id, .data$game_date, .data$visiting_team, .data$home_team), "game_id", "game_date", "visiting_team", "home_team")
                datatble <- mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team))
                datatble$game_id[datatble$display_ID %in% input$game_table_dropdown_sbs1]
            }
        })
        
        selected_game_id_sbs2 <- reactive({
            if (is.null(input$game_table_dropdown_sbs2)) {
                NULL
            } else {
                datatble <- dplyr::select(distinct(pbp_augment_sbs2(), .data$game_id, .data$game_date, .data$visiting_team, .data$home_team), "game_id", "game_date", "visiting_team", "home_team")
                datatble <- mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team))
                datatble$game_id[datatble$display_ID %in% input$game_table_dropdown_sbs2]
            }
        })
        
        ## Team
        team_list = reactive({
            if (trace_execution) message("recalculating team_list")
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id())
                sort(unique(na.omit(tmp$team)))
            }
        })
        observe({
            isolate(sel <- intersect(team_list(), input$team_list))
            if (length(sel) < 1) sel <- team_list() ## select all
            updatePickerInput(session, "team_list", choices = team_list(), selected = sel)
        })

        team_list_sbs1 = reactive({
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs1(), .data$game_id %in% selected_game_id_sbs1())
                sort(unique(na.omit(tmp$team)))
            }
        })
        observe({
            isolate(sel_sbs1 <- intersect(team_list_sbs1(), input$team_list_sbs1))
            if (length(sel_sbs1) < 1) sel_sbs1 <- team_list_sbs1() ## select all
            updatePickerInput(session, "team_list_sbs1", choices = team_list_sbs1(), selected = sel_sbs1)
        })
        
        team_list_sbs2 = reactive({
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs2(), .data$game_id %in% selected_game_id_sbs2())
                sort(unique(na.omit(tmp$team)))
            }
        })
        observe({
            isolate(sel_sbs2 <- intersect(team_list_sbs2(), input$team_list_sbs2))
            if (length(sel_sbs2) < 1) sel_sbs2 <- team_list_sbs2() ## select all
            updatePickerInput(session, "team_list_sbs2", choices = team_list_sbs2(), selected = sel_sbs2)
        })
        
        ## Player ID
        player_list = reactive({
            if (trace_execution) message("recalculating player_list")
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$team %in% input$team_list)
                sort(unique(na.omit(tmp$player_name)))
            }
        })
        observe({
            isolate(sel <- intersect(player_list(), input$player_list))
            if (length(sel) < 1) sel <- player_list() ## select all
            updatePickerInput(session, "player_list", choices = player_list(), selected = sel)
        })

        player_list_sbs1 = reactive({
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs1(), .data$game_id %in% selected_game_id_sbs1(), .data$team %in% input$team_list_sbs1)
                sort(unique(na.omit(tmp$player_name)))
            }
        })
        observe({
            isolate(sel_sbs1 <- intersect(player_list_sbs1(), input$player_list_sbs1))
            if (length(sel_sbs1) < 1) sel_sbs1 <- player_list_sbs1() ## select all
            updatePickerInput(session, "player_list_sbs1", choices = player_list_sbs1(), selected = sel_sbs1)
        })
        
        player_list_sbs2 = reactive({
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs2(), .data$game_id %in% selected_game_id_sbs2(), .data$team %in% input$team_list_sbs2)
                sort(unique(na.omit(tmp$player_name)))
            }
        })
        observe({
            isolate(sel_sbs2 <- intersect(player_list_sbs2(), input$player_list_sbs2))
            if (length(sel_sbs2) < 1) sel_sbs2 <- player_list_sbs2() ## select all
            updatePickerInput(session, "player_list_sbs2", choices = player_list_sbs2(), selected = sel_sbs2)
        })
        ## Skill
        skill_list = reactive({
            if (trace_execution) message("recalculating skill_list")
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list)
                sort(unique(na.omit(tmp$skill)))
            }
        })
        observe({
            isolate(sel <- intersect(skill_list(), input$skill_list))
            if (length(sel) < 1) sel <- skill_list() ## select all
            updatePickerInput(session, "skill_list", choices = skill_list(), selected = sel)
        })

        skill_list_sbs1 = reactive({
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs1(), .data$game_id %in% selected_game_id_sbs1(), .data$player_name %in% input$player_list_sbs1, .data$team %in% input$team_list_sbs1)
                sort(unique(na.omit(tmp$skill)))
            }
        })
        observe({
            isolate(sel <- intersect(skill_list_sbs1(), input$skill_list_sbs1))
            if (length(sel) < 1) sel <- skill_list_sbs1() ## select all
            updatePickerInput(session, "skill_list_sbs1", choices = skill_list_sbs1(), selected = sel)
        })
        
        skill_list_sbs2 = reactive({
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs2(), .data$game_id %in% selected_game_id_sbs2(), .data$player_name %in% input$player_list_sbs2, .data$team %in% input$team_list_sbs2)
                sort(unique(na.omit(tmp$skill)))
            }
        })
        observe({
            isolate(sel <- intersect(skill_list_sbs2(), input$skill_list_sbs2))
            if (length(sel) < 1) sel <- skill_list_sbs2() ## select all
            updatePickerInput(session, "skill_list_sbs2", choices = skill_list_sbs2(), selected = sel)
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
                if (length(selected_game_id()) < 1) {
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
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$skill %in% input$skill_list, .data$team %in% input$team_list)
                sort(unique(tmp$skilltype))
            }
        })
        observe({
            ## if the skilltype list changes, then we need to select all, otherwise we may have changes from e.g. just attacks to all skills, but we'll be restricted to just the previously-selected attack skill types
            updatePickerInput(session, "skilltype_list", choices = skilltype_list(), selected = skilltype_list())
        })
        
        skilltype_list_sbs1 = reactive({
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs1(), .data$game_id %in% selected_game_id_sbs1(), .data$player_name %in% input$player_list_sbs1, .data$skill %in% input$skill_list_sbs1, .data$team %in% input$team_list_sbs1)
                sort(unique(tmp$skilltype))
            }
        })
        observe({
            ## if the skilltype list changes, then we need to select all, otherwise we may have changes from e.g. just attacks to all skills, but we'll be restricted to just the previously-selected attack skill types
            updatePickerInput(session, "skilltype_list_sbs1", choices = skilltype_list_sbs1(), selected = skilltype_list_sbs1())
        })

        skilltype_list_sbs2 = reactive({
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs2(), .data$game_id %in% selected_game_id_sbs2(), .data$player_name %in% input$player_list_sbs2, .data$skill %in% input$skill_list_sbs2, .data$team %in% input$team_list_sbs2)
                sort(unique(tmp$skilltype))
            }
        })
        observe({
            ## if the skilltype list changes, then we need to select all, otherwise we may have changes from e.g. just attacks to all skills, but we'll be restricted to just the previously-selected attack skill types
            updatePickerInput(session, "skilltype_list_sbs2", choices = skilltype_list_sbs2(), selected = skilltype_list_sbs2())
        })
        
        ## Phase
        phase_list = reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
                sort(unique(tmp$phase))
            }
        })
        observe({
            isolate(sel <- intersect(phase_list(), input$phase_list))
            if (length(sel) < 1) sel <- phase_list() ## select all
            updatePickerInput(session, "phase_list", choices = phase_list(), selected = sel)
        })

        phase_list_sbs1 = reactive({
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs1(), .data$game_id %in% selected_game_id_sbs1(), .data$player_name %in% input$player_list_sbs1, .data$team %in% input$team_list_sbs1, .data$skill %in% input$skill_list_sbs1)
                sort(unique(tmp$phase))
            }
        })
        observe({
            isolate(sel <- intersect(phase_list_sbs1(), input$phase_list_sbs1))
            if (length(sel) < 1) sel <- phase_list_sbs1() ## select all
            updatePickerInput(session, "phase_list_sbs1", choices = phase_list_sbs1(), selected = sel)
        })
        
        phase_list_sbs2 = reactive({
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs2(), .data$game_id %in% selected_game_id_sbs2(), .data$player_name %in% input$player_list_sbs2, .data$team %in% input$team_list_sbs2, .data$skill %in% input$skill_list_sbs2)
                sort(unique(tmp$phase))
            }
        })
        observe({
            isolate(sel <- intersect(phase_list_sbs2(), input$phase_list_sbs2))
            if (length(sel) < 1) sel <- phase_list_sbs2() ## select all
            updatePickerInput(session, "phase_list_sbs2", choices = phase_list_sbs2(), selected = sel)
        })
        
        ## Advanced filter
        adFilter_list = reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                temp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
                avail <- colnames(temp)
                avail <- avail[vapply(avail, function(z) !all(is.na(temp[[z]])), FUN.VALUE = TRUE)] ## exclude all-NA cols
                avail <- adfilter_cols_to_show[adfilter_cols_to_show %in% avail] ## only those in our pre-defined list of adfilter_cols_to_show
                avail <- avail[order(names(avail))]
                c(list("No filter" = ""), avail) ## add a "no filter" option
            }
        })
        observe({
            isolate(sel <- intersect(adFilter_list(), input$adFilter_list))
            if (length(sel) < 1) sel <- character() ## select none
            updateSelectInput(session, "adFilter_list", choices = adFilter_list(), selected = sel)
        })

        adFilter_list_sbs1 = reactive({
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1) {
                character()
            } else {
                temp <- dplyr::filter(pbp_augment_sbs1(), .data$game_id %in% selected_game_id_sbs1(), .data$player_name %in% input$player_list_sbs1,
                                      .data$team %in% input$team_list_sbs1, .data$skill %in% input$skill_list_sbs1)
                avail <- colnames(temp)
                avail <- avail[vapply(avail, function(z) !all(is.na(temp[[z]])), FUN.VALUE = TRUE)] ## exclude all-NA cols
                avail <- adfilter_cols_to_show[adfilter_cols_to_show %in% avail] ## only those in our pre-defined list of adfilter_cols_to_show
                avail <- avail[order(names(avail))]
                c(list("No filter" = ""), avail) ## add a "no filter" option
            }
        })
        observe({
            isolate(sel <- intersect(adFilter_list_sbs1(), input$adFilter_list_sbs1))
            if (length(sel) < 1) sel <- character() ## select none
            updateSelectInput(session, "adFilter_list_sbs1", choices = adFilter_list_sbs1(), selected = sel)
        })
        
        adFilter_list_sbs2 = reactive({
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1) {
                character()
            } else {
                temp <- dplyr::filter(pbp_augment_sbs2(), .data$game_id %in% selected_game_id_sbs2(), .data$player_name %in% input$player_list_sbs2,
                                      .data$team %in% input$team_list_sbs2, .data$skill %in% input$skill_list_sbs2)
                avail <- colnames(temp)
                avail <- avail[vapply(avail, function(z) !all(is.na(temp[[z]])), FUN.VALUE = TRUE)] ## exclude all-NA cols
                avail <- adfilter_cols_to_show[adfilter_cols_to_show %in% avail] ## only those in our pre-defined list of adfilter_cols_to_show
                avail <- avail[order(names(avail))]
                c(list("No filter" = ""), avail) ## add a "no filter" option
            }
        })
        observe({
            isolate(sel <- intersect(adFilter_list_sbs2(), input$adFilter_list_sbs2))
            if (length(sel) < 1) sel <- character() ## select none
            updateSelectInput(session, "adFilter_list_sbs2", choices = adFilter_list_sbs2(), selected = sel)
        })
        
        ## Advanced filter value
        adFilterValue_list = reactive({
            col_to_select <- input$adFilter_list
            if (is.null(col_to_select) || !nzchar(col_to_select)) return(list())
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || length(col_to_select) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
                sort(unique(tmp[[col_to_select]]))
            }
        })
        observe({
            isolate(sel <- intersect(adFilterValue_list(), input$adFilterValue_list))
            if (length(sel) < 1) sel <- adFilterValue_list() ## select all
            updatePickerInput(session, "adFilterValue_list", choices = adFilterValue_list(), selected = sel)
        })

        adFilterValue_list_sbs1 = reactive({
            col_to_select <- input$adFilter_list_sbs1
            if (is.null(col_to_select) || !nzchar(col_to_select)) return(list())
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1 || length(col_to_select) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs1(), .data$game_id %in% selected_game_id_sbs1(), .data$player_name %in% input$player_list_sbs1,
                                     .data$team %in% input$team_list_sbs1, .data$skill %in% input$skill_list_sbs1)
                sort(unique(tmp[[col_to_select]]))
            }
        })
        observe({
            isolate(sel <- intersect(adFilterValue_list_sbs1(), input$adFilterValue_list_sbs1))
            if (length(sel) < 1) sel <- adFilterValue_list_sbs1() ## select all
            updatePickerInput(session, "adFilterValue_list_sbs1", choices = adFilterValue_list_sbs1(), selected = sel)
        })
        
        adFilterValue_list_sbs2 = reactive({
            col_to_select <- input$adFilter_list_sbs2
            if (is.null(col_to_select) || !nzchar(col_to_select)) return(list())
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1 || length(col_to_select) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs2(), .data$game_id %in% selected_game_id_sbs2(), .data$player_name %in% input$player_list_sbs2,
                                     .data$team %in% input$team_list_sbs2, .data$skill %in% input$skill_list_sbs2)
                sort(unique(tmp[[col_to_select]]))
            }
        })
        observe({
            isolate(sel <- intersect(adFilterValue_list_sbs2(), input$adFilterValue_list_sbs2))
            if (length(sel) < 1) sel <- adFilterValue_list_sbs2() ## select all
            updatePickerInput(session, "adFilterValue_list_sbs2", choices = adFilterValue_list_sbs2(), selected = sel)
        })
        ## Advanced filter 2
        observe({
            isolate(sel <- intersect(adFilter_list_sbs1(), input$adFilterB_list_sbs1))
            if (length(sel) < 1) sel <- character() ## select none
            updateSelectInput(session, "adFilterB_list_sbs1", choices = adFilter_list_sbs1(), selected = sel)
        })
        observe({
            isolate(sel <- intersect(adFilter_list_sbs2(), input$adFilterB_list_sbs2))
            if (length(sel) < 1) sel <- character() ## select none
            updateSelectInput(session, "adFilterB_list_sbs2", choices = adFilter_list_sbs2(), selected = sel)
        })
        observe({
            isolate(sel <- intersect(adFilter_list(), input$adFilterB_list))
            if (length(sel) < 1) sel <- character() ## select none
            updateSelectInput(session, "adFilterB_list", choices = adFilter_list(), selected = sel)
        })
        ## Advanced filter 2 value
        adFilterBValue_list = reactive({
            col_to_select <- input$adFilterB_list
            if (is.null(col_to_select) || !nzchar(col_to_select)) return(list())
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || length(col_to_select) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
                sort(unique(tmp[[col_to_select]]))
            }
        })
        observe({
            isolate(sel <- intersect(adFilterBValue_list(), input$adFilterBValue_list))
            if (length(sel) < 1) sel <- adFilterBValue_list() ## select all
            updatePickerInput(session, "adFilterBValue_list", choices = adFilterBValue_list(), selected = sel)
        })

        adFilterBValue_list_sbs1 = reactive({
            col_to_select <- input$adFilterB_list_sbs1
            if (is.null(col_to_select) || !nzchar(col_to_select)) return(list())
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1 || length(col_to_select) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs1(), .data$game_id %in% selected_game_id_sbs1(), .data$player_name %in% input$player_list_sbs1,
                                     .data$team %in% input$team_list_sbs1, .data$skill %in% input$skill_list_sbs1)
                sort(unique(tmp[[col_to_select]]))
            }
        })
        observe({
            isolate(sel <- intersect(adFilterBValue_list_sbs1(), input$adFilterBValue_list_sbs1))
            if (length(sel) < 1) sel <- adFilterBValue_list_sbs1() ## select all
            updatePickerInput(session, "adFilterBValue_list_sbs1", choices = adFilterBValue_list_sbs1(), selected = sel)
        })
        
        adFilterBValue_list_sbs2 = reactive({
            col_to_select <- input$adFilterB_list_sbs2
            if (is.null(col_to_select) || !nzchar(col_to_select)) return(list())
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1 || length(col_to_select) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment_sbs2(), .data$game_id %in% selected_game_id_sbs2(), .data$player_name %in% input$player_list_sbs2,
                                     .data$team %in% input$team_list_sbs2, .data$skill %in% input$skill_list_sbs2)
                sort(unique(tmp[[col_to_select]]))
            }
        })
        observe({
            isolate(sel <- intersect(adFilterBValue_list_sbs2(), input$adFilterBValue_list_sbs2))
            if (length(sel) < 1) sel <- adFilterBValue_list_sbs2() ## select all
            updatePickerInput(session, "adFilterBValue_list_sbs2", choices = adFilterBValue_list_sbs2(), selected = sel)
        })
        
        ## Help
        observeEvent(input$help, rintrojs::introjs(session, options = list("nextLabel" = "Next", "prevLabel" = "Previous", "skipLabel" = "Skip")))

        game_table_dropdown <- reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                output$no_game_data <- renderUI(
                    if (is.null(input$season)) {
                        tags$div(class = "alert alert-info", "No competition data sets. Log in?")
                    } else if (!is.null(meta()) && is.null(pbp())) {
                        tags$div(class = "alert alert-danger", "No matches with video could be found.")
                    } else if (is.null(meta()) && have_done_startup()) {
                        if (isTRUE(isolate(got_no_video()))) {
                            tags$div(class = "alert alert-danger", "No matches with video could be found.")
                        } else {
                            tags$div(class = "alert alert-danger", "Sorry, something went wrong processing this data set.")
                        }
                    } else {
                        NULL
                    })
                ## hide the game selector if we have no games with video (or haven't selected a data set)
                js_hide("game_table_dropdown")
                character()
            } else {
                output$no_game_data <- renderUI(NULL)
                js_show("game_table_dropdown")
                ## Customize pbp
                datatble <- dplyr::select(distinct(pbp_augment(), .data$game_id, .data$game_date, .data$visiting_team, .data$home_team), "game_id", "game_date", "visiting_team", "home_team")
                datatble <- dplyr::arrange(dplyr::mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team)), .data$game_date)
                dplyr::pull(dplyr::select(datatble, .data$display_ID))
            }
        })

        observe({
            isolate(sel <- intersect(game_table_dropdown(), input$game_table_dropdown))
            updatePickerInput(session, "game_table_dropdown", choices = game_table_dropdown(), selected = sel)
        })

        game_table_dropdown_sbs1 <- reactive({
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1) {
                output$no_game_data_sbs1 <- renderUI(
                    if (is.null(input$season_sbs1)) {
                        tags$div(class = "alert alert-info", "No competition data sets. Log in?")
                    } else if (!is.null(meta_sbs1()) && is.null(pbp_sbs1())) {
                        tags$div(class = "alert alert-danger", "No matches with video could be found.")
                    } else if (is.null(meta_sbs1()) && have_done_startup_sbs1()) {
                        if (isTRUE(isolate(got_no_video_sbs1()))) {
                            tags$div(class = "alert alert-danger", "No matches with video could be found.")
                        } else {
                            tags$div(class = "alert alert-danger", "Sorry, something went wrong processing this data set.")
                        }
                    } else {
                        NULL
                    })
                ## hide the game selector if we have no games with video (or haven't selected a data set)
                js_hide("game_table_dropdown_sbs1")
                character()
            } else {
                output$no_game_data_sbs1 <- renderUI(NULL)
                js_show("game_table_dropdown_sbs1")
                ## Customize pbp
                datatble <- dplyr::select(distinct(pbp_augment_sbs1(), .data$game_id, .data$game_date, .data$visiting_team, .data$home_team), "game_id", "game_date", "visiting_team", "home_team")
                datatble <- dplyr::arrange(dplyr::mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team)), .data$game_date)
                dplyr::pull(dplyr::select(datatble, .data$display_ID))
            }
        })
        
        observe({
            isolate(sel_sbs1 <- intersect(game_table_dropdown_sbs1(), input$game_table_dropdown_sbs1))
            updatePickerInput(session, "game_table_dropdown_sbs1", choices = game_table_dropdown_sbs1(), selected = sel_sbs1)
        })
        
        game_table_dropdown_sbs2 <- reactive({
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1) {
                output$no_game_data_sbs2 <- renderUI(
                    if (is.null(input$season_sbs2)) {
                        tags$div(class = "alert alert-info", "No competition data sets. Log in?")
                    } else if (!is.null(meta_sbs2()) && is.null(pbp_sbs2())) {
                        tags$div(class = "alert alert-danger", "No matches with video could be found.")
                    } else if (is.null(meta_sbs2()) && have_done_startup_sbs2()) {
                        if (isTRUE(isolate(got_no_video_sbs2()))) {
                            tags$div(class = "alert alert-danger", "No matches with video could be found.")
                        } else {
                            tags$div(class = "alert alert-danger", "Sorry, something went wrong processing this data set.")
                        }
                    } else {
                        NULL
                    })
                ## hide the game selector if we have no games with video (or haven't selected a data set)
                js_hide("game_table_dropdown_sbs2")
                character()
            } else {
                output$no_game_data_sbs2 <- renderUI(NULL)
                js_show("game_table_dropdown_sbs2")
                ## Customize pbp
                datatble <- dplyr::select(distinct(pbp_augment_sbs2(), .data$game_id, .data$game_date, .data$visiting_team, .data$home_team), "game_id", "game_date", "visiting_team", "home_team")
                datatble <- dplyr::arrange(dplyr::mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team)), .data$game_date)
                dplyr::pull(dplyr::select(datatble, .data$display_ID))
            }
        })
        
        observe({
            isolate(sel_sbs2 <- intersect(game_table_dropdown_sbs2(), input$game_table_dropdown_sbs2))
            updatePickerInput(session, "game_table_dropdown_sbs2", choices = game_table_dropdown_sbs2(), selected = sel_sbs2)
        })
        
        ## Table of all actions as per selected_game_id() and player_id() and evaluation()
        playstable_to_delete <- NULL
        playstable_data_raw <- debounce(reactive({
            ## Customize pbp
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(selected_game_id()) || is.null(meta())) {
                playstable_to_delete <<- NULL
                NULL
            } else {
                if (trace_execution) message("recalculating playstable_data")
                pbp <- pbp_augment()
                meta <- meta()
                game_select <- selected_game_id()
                team_select <- input$team_list
                player_select <- input$player_list
                skill_select <- input$skill_list
                skilltype_select <- input$skilltype_list
                phase_select <- input$phase_list
                filter_var <- input$adFilter_list
                filterB_var <- input$adFilterB_list
                playlist_select <- input$playlist_list
                highlight_select <- input$highlight_list
                if (!is.null(playlist_select) && !is.null(skill_select) && !is.null(game_select) && !is.null(player_select) && !is.null(team_select)) {
                    myfuns <- funs_from_playlist(playlist_select)
                    if (length(game_select) == 1) {
                        ## apply each of myfuns in turn and rbind the results
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) myfun(x = dplyr::filter(pbp, .data$game_id %in% game_select), team = team_select, player = player_select)))
                    } else{
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
                    pbp_tmp <- dplyr::filter(pbp, .data$player_name %in% input$player_list &.data$skill %in% input$skill_list & .data$team %in% input$team_list & .data$game_id %in% selected_game_id())
                    if (!is.null(input$skilltype_list)) pbp_tmp <- dplyr::filter(pbp_tmp, .data$skilltype %in% input$skilltype_list)
                    if (!is.null(input$phase_list)) pbp_tmp <- dplyr::filter(pbp_tmp, .data$phase %in% input$phase_list)
                }
                ## advanced filters apply to all
                if (!is.null(filter_var) && nzchar(filter_var)) pbp_tmp <- dplyr::filter(pbp_tmp, .data[[filter_var]] %in% input$adFilterValue_list)
                if (!is.null(filterB_var) && nzchar(filterB_var)) pbp_tmp <- dplyr::filter(pbp_tmp, .data[[filterB_var]] %in% input$adFilterBValue_list)
                playstable_to_delete <<- rep(FALSE, nrow(pbp_tmp))
                pbp_tmp$ROWID <- seq_len(nrow(pbp_tmp)) ## keep track of original row numbers for deletion
                master_playstable_selected_row <<- 1 ## fresh table/playlist, start from row 1
                is_fresh_playlist <<- TRUE
                pbp_tmp
            }
        }), 250)

        playstable_data_raw_sbs1 <- reactive({
            ## Customize pbp
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1 || is.null(selected_game_id_sbs1()) || is.null(meta_sbs1())) {
                NULL
            } else {
                if (trace_execution) cat("recalculating playstable_data\n")
                pbp <- pbp_augment_sbs1()
                meta <- meta_sbs1()
                game_select <- selected_game_id_sbs1()
                team_select <- input$team_list_sbs1
                player_select <- input$player_list_sbs1
                skill_select <- input$skill_list_sbs1
                skilltype_select <- input$skilltype_list_sbs1
                phase_select <- input$phase_list_sbs1
                filter_var <- input$adFilter_list_sbs1
                filterB_var <- input$adFilterB_list_sbs1
                playlist_select <- input$playlist_list_sbs1
                highlight_select <- input$highlight_list_sbs1
                if (!is.null(playlist_select) && !is.null(skill_select) && !is.null(game_select) && !is.null(player_select) && !is.null(team_select)) {
                    myfuns <- funs_from_playlist(playlist_select)
                    if (length(game_select) == 1) {
                        ## apply each of myfuns in turn and rbind the results
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) myfun(x = dplyr::filter(pbp, .data$game_id %in% game_select), team = team_select, player = player_select)))
                    } else{
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
                    pbp_tmp <- dplyr::filter(pbp, .data$player_name %in% input$player_list_sbs1 &.data$skill %in% input$skill_list_sbs1 & .data$team %in% input$team_list_sbs1 & 
                                                 .data$game_id %in% selected_game_id_sbs1())
                    if (!is.null(input$skilltype_list_sbs1)) pbp_tmp <- dplyr::filter(pbp_tmp, .data$skilltype %in% input$skilltype_list_sbs1)
                    if (!is.null(input$phase_list_sbs1)) pbp_tmp <- dplyr::filter(pbp_tmp, .data$phase %in% input$phase_list_sbs1)
                }
                ## advanced filters apply to all
                if (!is.null(filter_var) && nzchar(filter_var)) pbp_tmp <- dplyr::filter(pbp_tmp, .data[[filter_var]] %in% input$adFilterValue_list_sbs1)
                if (!is.null(filterB_var) && nzchar(filterB_var)) pbp_tmp <- dplyr::filter(pbp_tmp, .data[[filterB_var]] %in% input$adFilterBValue_list_sbs1)
                master_playstable_selected_row_sbs1 <<- 1
                pbp_tmp
            }
        })
        
        playstable_data_raw_sbs2 <- reactive({
            ## Customize pbp
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1 || is.null(selected_game_id_sbs2()) || is.null(meta_sbs2())) {
                NULL
            } else {
                if (trace_execution) cat("recalculating playstable_data\n")
                pbp <- pbp_augment_sbs2()
                meta <- meta_sbs2()
                game_select <- selected_game_id_sbs2()
                team_select <- input$team_list_sbs2
                player_select <- input$player_list_sbs2
                skill_select <- input$skill_list_sbs2
                skilltype_select <- input$skilltype_list_sbs2
                phase_select <- input$phase_list_sbs2
                filter_var <- input$adFilter_list_sbs2
                filterB_var <- input$adFilterB_list_sbs2
                playlist_select <- input$playlist_list_sbs2
                highlight_select <- input$highlight_list_sbs2
                if (!is.null(playlist_select) && !is.null(skill_select) && !is.null(game_select) && !is.null(player_select) && !is.null(team_select)) {
                    myfuns <- funs_from_playlist(playlist_select)
                    if (length(game_select) == 1) {
                        ## apply each of myfuns in turn and rbind the results
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) myfun(x = dplyr::filter(pbp, .data$game_id %in% game_select), team = team_select, player = player_select)))
                    } else{
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
                    pbp_tmp <- dplyr::filter(pbp, .data$player_name %in% input$player_list_sbs2 &.data$skill %in% input$skill_list_sbs2 & .data$team %in% input$team_list_sbs2 & 
                                                 .data$game_id %in% selected_game_id_sbs2())
                    if (!is.null(input$skilltype_list_sbs2)) pbp_tmp <- dplyr::filter(pbp_tmp, .data$skilltype %in% input$skilltype_list_sbs2)
                    if (!is.null(input$phase_list_sbs2)) pbp_tmp <- dplyr::filter(pbp_tmp, .data$phase %in% input$phase_list_sbs2)
                }
                ## advanced filters apply to all
                if (!is.null(filter_var) && nzchar(filter_var)) pbp_tmp <- dplyr::filter(pbp_tmp, .data[[filter_var]] %in% input$adFilterValue_list_sbs2)
                if (!is.null(filterB_var) && nzchar(filterB_var)) pbp_tmp <- dplyr::filter(pbp_tmp, .data[[filterB_var]] %in% input$adFilterBValue_list_sbs2)
                master_playstable_selected_row_sbs2 <<- 1
                pbp_tmp
            }
        })
        
        ## the actual playstable_data is playstable_data_raw but with user-deleted rows removed
        deltrigger <- reactiveVal(0)
        playstable_data <- debounce(reactive({
            blah <- deltrigger() ## react to this
            ptdel <- playstable_to_delete
            if (allow_item_deletion && !is.null(ptdel) && length(ptdel) == nrow(playstable_data_raw())) {
                playstable_data_raw()[!ptdel, ]
            } else {
                playstable_data_raw()
            }
        }), 500)

        playstable_data_sbs1 <- debounce(reactive({
            playstable_data_raw_sbs1()
        }), 500)
        
        playstable_data_sbs2 <- debounce(reactive({
            playstable_data_raw_sbs2()
        }), 500)
        
        observeEvent(input$del_plitem, {
            ## when the user clicks a delete icon, mark that row for removal
            if (!is.null(input$del_plitem) && nzchar(input$del_plitem) && grepl("@", input$del_plitem) && !is.null(playstable_to_delete)) {
                plchk <- playstable_to_delete
                temp <- strsplit(substr(input$del_plitem, 4, nchar(input$del_plitem)), "@")[[1]]
                plchk[as.numeric(temp[1])] <- TRUE
                playstable_to_delete <<- plchk
                is_fresh_playlist <<- FALSE
                deltrigger(deltrigger() + 1L)
            }
        })

        output$playstable <- DT::renderDataTable({
            mydat <- playstable_data()
            if (!is.null(mydat)) {
                if (allow_item_deletion) {
                    mydat$`Delete` <- as.list(paste0('<i class="fa fa-trash-alt" id="pl_', mydat$ROWID, '" onclick="delete_pl_item(this);" />'))
                    mydat <- mydat[, c("Delete", plays_cols_to_show), drop = FALSE]
                    cnames <- var2fc(names(mydat))
                    cnames[1] <- ""
                } else {
                    mydat <- mydat[, plays_cols_to_show, drop = FALSE]
                    cnames <- var2fc(names(mydat))
                }
                if (trace_execution) message("redrawing playstable, master_selected is: ", master_playstable_selected_row)
                ## when the table is redrawn but the selected row is not in the first few rows, need to scroll the table - use initComplete callback
                DT::datatable(mydat, rownames = FALSE, colnames = cnames, escape = FALSE,
                              extensions = "Scroller", selection = list(mode = "single", selected = max(master_playstable_selected_row, 1L), target = "row"),
                              options = list(sDom = '<"top">t<"bottom">rlp', deferRender = TRUE, scrollY = 200, scroller = TRUE, ordering = FALSE,
                                             initComplete = DT::JS('function(setting, json) { Shiny.setInputValue("scroll_trigger", new Date().getTime()); }')))
            } else {
                NULL
            }
        })

        output$playstable_sbs1 <- DT::renderDataTable({
            mydat <- playstable_data_sbs1()
            if (!is.null(mydat)) {
                mydat <- mydat[, plays_cols_to_show, drop = FALSE]
                cnames <- var2fc(names(mydat))
                DT::datatable(mydat, rownames = FALSE, colnames = cnames, escape = FALSE,
                              extensions = "Scroller", selection = list(mode = "single", selected = 1, target = "row"),
                              options = list(sDom = '<"top">t<"bottom">rlp', deferRender = TRUE, scrollY = 200, scroller = TRUE, ordering = FALSE,
                                             initComplete = DT::JS('function(setting, json) { Shiny.setInputValue("scroll_trigger_sbs1", new Date().getTime()); }')))
            } else {
                NULL
            }
        })
        
        output$playstable_sbs2 <- DT::renderDataTable({
            mydat <- playstable_data_sbs2()
            if (!is.null(mydat)) {
                mydat <- mydat[, plays_cols_to_show, drop = FALSE]
                cnames <- var2fc(names(mydat))
                DT::datatable(mydat, rownames = FALSE, colnames = cnames, escape = FALSE,
                              extensions = "Scroller", selection = list(mode = "single", selected = 1, target = "row"),
                              options = list(sDom = '<"top">t<"bottom">rlp', deferRender = TRUE, scrollY = 200, scroller = TRUE, ordering = FALSE,
                                             initComplete = DT::JS('function(setting, json) { Shiny.setInputValue("scroll_trigger_sbs2", new Date().getTime()); }')))
            } else {
                NULL
            }
        })
        
        playstable_proxy <- DT::dataTableProxy("playstable", deferUntilFlush = TRUE)
        playstable_proxy_sbs1 <- DT::dataTableProxy("playstable_sbs1", deferUntilFlush = TRUE)
        playstable_proxy_sbs2 <- DT::dataTableProxy("playstable_sbs2", deferUntilFlush = TRUE)

        playstable_select_row <- function(rw) {
            if (!is.null(rw) && !is.na(rw) && (rw != master_playstable_selected_row)) {
                master_playstable_selected_row <<- rw
                DT::selectRows(playstable_proxy, rw)
                scroll_playstable(rw)
            }
        }

        playstable_select_row_sbs1 <- function(rw) {
            if (!is.null(rw) && !is.na(rw) && (rw != master_playstable_selected_row_sbs1)) {
                master_playstable_selected_row_sbs1 <<- rw
                DT::selectRows(playstable_proxy_sbs1, rw)
                scroll_playstable_sbs1(rw)
            }
        }
        playstable_select_row_sbs2 <- function(rw) {
            if (!is.null(rw) && !is.na(rw) && (rw != master_playstable_selected_row_sbs2)) {
                master_playstable_selected_row_sbs2 <<- rw
                DT::selectRows(playstable_proxy_sbs2, rw)
                scroll_playstable_sbs2(rw)
            }
        }
        

        observeEvent(input$scroll_trigger, scroll_playstable())
        scroll_playstable <- function(rw = NULL) {
            selr <- if (!is.null(rw)) rw else input$playstable_rows_selected
            if (!is.null(selr)) {
                ## scrolling works on the VISIBLE row index, so it depends on any column filters that might have been applied
                visible_rowidx <- which(input$playstable_rows_all == selr)
                scrollto <- max(visible_rowidx-1-2, 0) ## -1 for zero indexing, -2 to keep the selected row 2 from the top
                evaljs(paste0("$('#playstable').find('.dataTable').DataTable().scroller.toPosition(", scrollto, ", false);"))
            }
        }
        observeEvent(input$scroll_trigger_sbs1, scroll_playstable_sbs1())
        scroll_playstable_sbs1 <- function(rw = NULL) {
            selr <- if (!is.null(rw)) rw else input$playstable_sbs1_rows_selected
            if (!is.null(selr)) {
                ## scrolling works on the VISIBLE row index, so it depends on any column filters that might have been applied
                visible_rowidx <- which(input$playstable_sbs1_rows_all == selr)
                scrollto <- max(visible_rowidx-1-2, 0) ## -1 for zero indexing, -2 to keep the selected row 2 from the top
                evaljs(paste0("$('#playstable_sbs1').find('.dataTable').DataTable().scroller.toPosition(", scrollto, ", false);"))
            }
        }
        observeEvent(input$scroll_trigger_sbs2, scroll_playstable_sbs2())
        scroll_playstable_sbs2 <- function(rw = NULL) {
            selr <- if (!is.null(rw)) rw else input$playstable_sbs2_rows_selected
            if (!is.null(selr)) {
                ## scrolling works on the VISIBLE row index, so it depends on any column filters that might have been applied
                visible_rowidx <- which(input$playstable_sbs2_rows_all == selr)
                scrollto <- max(visible_rowidx-1-2, 0) ## -1 for zero indexing, -2 to keep the selected row 2 from the top
                evaljs(paste0("$('#playstable_sbs2').find('.dataTable').DataTable().scroller.toPosition(", scrollto, ", false);"))
            }
        }
        ## when player changes item, it triggers input$playstable_current_item via the video_onstart() function. Update the selected row in the playstable
        observeEvent(input$playstable_current_item, {
            if (!is.null(input$playstable_current_item) && !is.null(playlist())) {
                try({
                    ## input$playstable_current_item is 0-based
                    isolate(np <- nrow(playlist()))
                    if (np < 1) {
                        ## empty table
                        master_playstable_selected_row <<- -99L
                    } else if (input$playstable_current_item < np) {
                        playstable_select_row(input$playstable_current_item+1)
                    } else {
                        ## reached the end of the playlist
                        master_playstable_selected_row <<- -99L
                    }
                })
            }
        })
        observeEvent(input$playstable_sbs1_current_item, {
            if (!is.null(input$playstable_sbs1_current_item) && !is.null(playlist_sbs1())) {
                try({
                    ## input$playstable_current_item is 0-based
                    isolate(np <- nrow(playlist_sbs1()))
                    if (np < 1) {
                        ## empty table
                        master_playstable_selected_row_sbs1 <<- -99L
                    } else if (input$playstable_sbs1_current_item < np) {
                        playstable_select_row_sbs1(input$playstable_sbs1_current_item+1)
                    } else {
                        ## reached the end of the playlist
                        master_playstable_selected_row_sbs1 <<- -99L
                    }
                })
            }
        })
        observeEvent(input$playstable_sbs2_current_item, {
            if (!is.null(input$playstable_sbs2_current_item) && !is.null(playlist_sbs2())) {
                try({
                    ## input$playstable_current_item is 0-based
                    isolate(np <- nrow(playlist_sbs2()))
                    if (np < 1) {
                        ## empty table
                        master_playstable_selected_row_sbs2 <<- -99L
                    } else if (input$playstable_sbs2_current_item < np) {
                        playstable_select_row_sbs2(input$playstable_sbs2_current_item+1)
                    } else {
                        ## reached the end of the playlist
                        master_playstable_selected_row_sbs2 <<- -99L
                    }
                })
            }
        })
        ## when the user chooses a row in the playstable, it will be selected by that click action, so we just need to play it
        ## use input$playstable_cell_clicked rather than input$playstable_rows_selected to detect user input, because the latter is also triggered by the player incrementing rows
        observeEvent(input$playstable_cell_clicked, { ## note, can't click the same row twice in a row ...
            clicked_row <- input$playstable_cell_clicked$row ## 1-based
            if (!is.null(clicked_row) && !is.na(clicked_row)) {
                if (!allow_item_deletion || isTRUE(input$playstable_cell_clicked$col > 0)) {
                    master_playstable_selected_row <<- clicked_row
                    evaljs(paste0("dvpl.video_controller.current=", clicked_row-1, "; dvpl.video_play();"))
                } else {
                    ## deleted a row
                    ## if it was on or before the current selected row, then subtract one off the master_playstable_selected_row to keep it in sync
                    if (clicked_row <= master_playstable_selected_row) {
                        master_playstable_selected_row <<- master_playstable_selected_row-1
                    }
                    evaljs(paste0("dvpl.video_controller.current=", master_playstable_selected_row-1, ";"))

                }
            }
        })
        observeEvent(input$playstable_sbs1_cell_clicked, { ## note, can't click the same row twice in a row ...
            clicked_row <- input$playstable_sbs1_cell_clicked$row ## 1-based
            if (!is.null(clicked_row) && !is.na(clicked_row) && clicked_row != master_playstable_selected_row_sbs1) { ## TODO take this last condition out?
                master_playstable_selected_row_sbs1 <<- clicked_row
                evaljs(paste0("dvpl_sbs1.video_controller.current=", clicked_row-1, "; dvpl_sbs1.video_play();"))
            }
        })
        observeEvent(input$playstable_sbs2_cell_clicked, { ## note, can't click the same row twice in a row ...
            clicked_row <- input$playstable_sbs2_cell_clicked$row ## 1-based
            if (!is.null(clicked_row) && !is.na(clicked_row) && clicked_row != master_playstable_selected_row_sbs2) { ## TODO take this last condition out?
                master_playstable_selected_row_sbs2 <<- clicked_row
                evaljs(paste0("dvpl_sbs2.video_controller.current=", clicked_row-1, "; dvpl_sbs2.video_play();"))
            }
        })
        
        selected_matches <- reactive({
            if (trace_execution) message("recalculating selected matches")
            if (is.null(input$game_table_dropdown) || is.null(pbp()) || nrow(pbp()) < 1) {
                NULL
            } else {
                datatble <- dplyr::select(distinct(pbp(), .data$match_id, .data$game_date, .data$visiting_team, .data$home_team), "match_id", "game_date", "visiting_team", "home_team")
                datatble <- mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team))
                unique(na.omit(datatble$match_id[datatble$display_ID %in% input$game_table_dropdown]))
            }
        })

        selected_matches_sbs1 <- reactive({
            if (trace_execution) cat("recalculating selected matches\n")
            if (is.null(input$game_table_dropdown_sbs1) || is.null(pbp_sbs1()) || nrow(pbp_sbs1()) < 1) {
                NULL
            } else {
                datatble <- dplyr::select(distinct(pbp_sbs1(), .data$match_id, .data$game_date, .data$visiting_team, .data$home_team), "match_id", "game_date", "visiting_team", "home_team")
                datatble <- mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team))
                unique(na.omit(datatble$match_id[datatble$display_ID %in% input$game_table_dropdown_sbs1]))
            }
        })
        
        selected_matches_sbs2 <- reactive({
            if (trace_execution) cat("recalculating selected matches\n")
            if (is.null(input$game_table_dropdown_sbs2) || is.null(pbp_sbs2()) || nrow(pbp_sbs2()) < 1) {
                NULL
            } else {
                datatble <- dplyr::select(distinct(pbp_sbs2(), .data$match_id, .data$game_date, .data$visiting_team, .data$home_team), "match_id", "game_date", "visiting_team", "home_team")
                datatble <- mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team))
                unique(na.omit(datatble$match_id[datatble$display_ID %in% input$game_table_dropdown_sbs2]))
            }
        })
        
        
        video_meta <- reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(meta()) || is.null(selected_game_id()) || is.null(selected_matches())) {
                NULL
            } else {
                if (trace_execution) message("recalculating video_meta")
                meta_video <- bind_rows(lapply(meta(), function(z) if (!is.null(z$video)) mutate(z$video, match_id = z$match_id, dvw_filename = z$filename)))
                meta_video <- dplyr::filter(meta_video, .data$match_id %in% selected_matches())
                if (nrow(meta_video) < 1) return(NULL)
                if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("lighttpd", "servr")) {
                    ## we are serving the video through the lighttpd/servr server, so need to make symlinks in its document root directory pointing to the actual video files
                    vf <- tryCatch(fs::path_real(meta_video$file), error = function(e) NULL)
                    if (is.null(vf) || length(vf) < 1) return(NULL)
                    ## may have multiple video files at this point
                    for (thisf in vf) {
                        if (fs::file_exists(thisf)) {
                            symlink_abspath <- fs::path_abs(file.path(app_data$video_server_dir, basename(thisf)))
                            suppressWarnings(try(unlink(symlink_abspath), silent = TRUE))
                            fs::link_create(thisf, symlink_abspath)
                            onStop(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                            onSessionEnded(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                        } else if (is_youtube_id(vf) || grepl("https?://", vf, ignore.case = TRUE)) {
                            ## youtube ID or link to video, don't do anything
                        } else {
                            ## video file does not exist!
                            stop("video file ", thisf, " does not exist, not handled yet")
                        }
                    }
                    meta_video$video_src <- file.path(app_data$video_server_url, basename(meta_video$file))
                    nidx <- is_youtube_id(meta_video$file) | grepl("https?://", meta_video$file, ignore.case = TRUE)
                    ## replace these with verbatim copy of original info
                    meta_video$video_src[nidx] <- meta_video$file[nidx]
                ##} else if (app_data$video_serve_method == "standalone") {
                    ##    meta_video$video_src <- meta_video$file ## full (local) path
                } else if (is.function(app_data$video_serve_method)) {
                    if (nrow(meta_video) > 0) {
                        ## block user interaction while this happens
                        showModal(modalDialog(title = "Please wait", size = "l", footer = NULL))
                        meta_video$video_src <- tryCatch({
                            shiny::withProgress(message = "Processing video files", {
                                vapply(seq_len(nrow(meta_video)), function(z) {
                                    shiny::setProgress(value = z/nrow(meta_video))
                                    app_data$video_serve_method(video_filename = meta_video$file[z], dvw_filename = meta_video$dvw_filename[z])
                                }, FUN.VALUE = "", USE.NAMES = FALSE)
                            })
                        }, error = function(e) character())
                        removeModal()
                    } else {
                        meta_video$video_src <- character()
                    }
                } else if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("none")) {
                    ## do nothing except pass the video file info into video_src
                    meta_video$video_src <- meta_video$file
                } else {
                    stop("unrecognized video_serve_method: ", app_data$video_serve_method)
                }
                output$video_dialog <- renderUI({
                    if (any(is.na(meta_video$video_src) | !nzchar(meta_video$video_src))) {
                        tags$div(class = "alert alert-danger", "No video files for these match(es) could be found.")
                    } else if (any(is.na(meta_video$video_src) | !nzchar(meta_video$video_src))) {
                        tags$div(class = "alert alert-danger", "At least one video could not be found.")
                    } else {
                        NULL
                    }
                })
                meta_video[which(!is.na(meta_video$video_src) & nzchar(meta_video$video_src)), ]
            }
        })

        video_meta_sbs1 <- reactive({
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1 || is.null(meta_sbs1()) || is.null(selected_game_id_sbs1()) || is.null(selected_matches_sbs1())) {
                NULL
            } else {
                if (trace_execution) cat("recalculating video_meta\n")
                meta_video <- bind_rows(lapply(meta_sbs1(), function(z) if (!is.null(z$video)) mutate(z$video, match_id = z$match_id, dvw_filename = z$filename)))
                meta_video <- dplyr::filter(meta_video, .data$match_id %in% selected_matches_sbs1())
                if (nrow(meta_video) < 1) return(NULL)
                if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("lighttpd", "servr")) {
                    ## we are serving the video through the lighttpd server, so need to make symlinks in its document root directory pointing to the actual video files
                    vf <- tryCatch(fs::path_real(meta_video$file), error = function(e) NULL)
                    if (is.null(vf) || length(vf) < 1) return(NULL)
                    ## may have multiple video files at this point
                    for (thisf in vf) {
                        if (fs::file_exists(thisf)) {
                            symlink_abspath <- fs::path_abs(file.path(app_data$video_server_dir, basename(thisf)))
                            suppressWarnings(try(unlink(symlink_abspath), silent = TRUE))
                            fs::link_create(thisf, symlink_abspath)
                            onStop(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                            onSessionEnded(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                        } else if (is_youtube_id(vf) || grepl("https?://", vf, ignore.case = TRUE)) {
                            ## youtube ID or link to video, don't do anything
                        } else {
                            ## video file does not exist!
                            stop("video file ", thisf, " does not exist, not handled yet")
                        }
                    }
                    meta_video$video_src <- file.path(app_data$video_server_url, basename(meta_video$file))
                    nidx <- is_youtube_id(meta_video$file) | grepl("https?://", meta_video$file, ignore.case = TRUE)
                    ## replace these with verbatim copy of original info
                    meta_video$video_src[nidx] <- meta_video$file[nidx]
                    ##} else if (app_data$video_serve_method == "standalone") {
                    ##    meta_video$video_src <- meta_video$file ## full (local) path
                } else if (is.function(app_data$video_serve_method)) {
                    if (nrow(meta_video) > 0) {
                        ## block user interaction while this happens
                        showModal(modalDialog(title = "Please wait", size = "l", footer = NULL))
                        meta_video$video_src <- tryCatch({
                            shiny::withProgress(message = "Processing video files", {
                                vapply(seq_len(nrow(meta_video)), function(z) {
                                    shiny::setProgress(value = z/nrow(meta_video))
                                    app_data$video_serve_method(video_filename = meta_video$file[z], dvw_filename = meta_video$dvw_filename[z])
                                }, FUN.VALUE = "", USE.NAMES = FALSE)
                            })
                        }, error = function(e) character())
                        removeModal()
                    } else {
                        meta_video$video_src <- character()
                    }
                } else if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("none")) {
                    ## do nothing except pass the video file info into video_src
                    meta_video$video_src <- meta_video$file
                } else {
                    stop("unrecognized video_serve_method: ", app_data$video_serve_method)
                }
                output$video_dialog <- renderUI({
                    if (any(is.na(meta_video$video_src) | !nzchar(meta_video$video_src))) {
                        tags$div(class = "alert alert-danger", "No video files for these match(es) could be found.")
                    } else if (any(is.na(meta_video$video_src) | !nzchar(meta_video$video_src))) {
                        tags$div(class = "alert alert-danger", "At least one video could not be found.")
                    } else {
                        NULL
                    }
                })
                meta_video[which(!is.na(meta_video$video_src) & nzchar(meta_video$video_src)), ]
            }
        })
        
        video_meta_sbs2 <- reactive({
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1 || is.null(meta_sbs2()) || is.null(selected_game_id_sbs2()) || is.null(selected_matches_sbs2())) {
                NULL
            } else {
                if (trace_execution) cat("recalculating video_meta\n")
                meta_video <- bind_rows(lapply(meta_sbs2(), function(z) if (!is.null(z$video)) mutate(z$video, match_id = z$match_id, dvw_filename = z$filename)))
                meta_video <- dplyr::filter(meta_video, .data$match_id %in% selected_matches_sbs2())
                if (nrow(meta_video) < 1) return(NULL)
                if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("lighttpd", "servr")) {
                    ## we are serving the video through the lighttpd server, so need to make symlinks in its document root directory pointing to the actual video files
                    vf <- tryCatch(fs::path_real(meta_video$file), error = function(e) NULL)
                    if (is.null(vf) || length(vf) < 1) return(NULL)
                    ## may have multiple video files at this point
                    for (thisf in vf) {
                        if (fs::file_exists(thisf)) {
                            symlink_abspath <- fs::path_abs(file.path(app_data$video_server_dir, basename(thisf)))
                            suppressWarnings(try(unlink(symlink_abspath), silent = TRUE))
                            fs::link_create(thisf, symlink_abspath)
                            onStop(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                            onSessionEnded(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                        } else if (is_youtube_id(vf) || grepl("https?://", vf, ignore.case = TRUE)) {
                            ## youtube ID or link to video, don't do anything
                        } else {
                            ## video file does not exist!
                            stop("video file ", thisf, " does not exist, not handled yet")
                        }
                    }
                    meta_video$video_src <- file.path(app_data$video_server_url, basename(meta_video$file))
                    nidx <- is_youtube_id(meta_video$file) | grepl("https?://", meta_video$file, ignore.case = TRUE)
                    ## replace these with verbatim copy of original info
                    meta_video$video_src[nidx] <- meta_video$file[nidx]
                    ##} else if (app_data$video_serve_method == "standalone") {
                    ##    meta_video$video_src <- meta_video$file ## full (local) path
                } else if (is.function(app_data$video_serve_method)) {
                    if (nrow(meta_video) > 0) {
                        ## block user interaction while this happens
                        showModal(modalDialog(title = "Please wait", size = "l", footer = NULL))
                        meta_video$video_src <- tryCatch({
                            shiny::withProgress(message = "Processing video files", {
                                vapply(seq_len(nrow(meta_video)), function(z) {
                                    shiny::setProgress(value = z/nrow(meta_video))
                                    app_data$video_serve_method(video_filename = meta_video$file[z], dvw_filename = meta_video$dvw_filename[z])
                                }, FUN.VALUE = "", USE.NAMES = FALSE)
                            })
                        }, error = function(e) character())
                        removeModal()
                    } else {
                        meta_video$video_src <- character()
                    }
                } else if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("none")) {
                    ## do nothing except pass the video file info into video_src
                    meta_video$video_src <- meta_video$file
                } else {
                    stop("unrecognized video_serve_method: ", app_data$video_serve_method)
                }
                output$video_dialog <- renderUI({
                    if (any(is.na(meta_video$video_src) | !nzchar(meta_video$video_src))) {
                        tags$div(class = "alert alert-danger", "No video files for these match(es) could be found.")
                    } else if (any(is.na(meta_video$video_src) | !nzchar(meta_video$video_src))) {
                        tags$div(class = "alert alert-danger", "At least one video could not be found.")
                    } else {
                        NULL
                    }
                })
                meta_video[which(!is.na(meta_video$video_src) & nzchar(meta_video$video_src)), ]
            }
        })
        
        
        playlist <- reactive({
            if (trace_execution) message("recalculating playlist")
            ## Customize pbp
            meta_video <- video_meta()
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(meta()) || is.null(selected_game_id()) || is.null(meta_video) || nrow(meta_video) < 1 || is.null(playstable_data()) || nrow(playstable_data()) < 1) {
                NULL
            } else {
                event_list <- mutate(playstable_data(), skill = case_when(.data$skill %in% c("Freeball dig", "Freeball over") ~ "Freeball", TRUE ~ .data$skill), ## ov_video needs just "Freeball"
                                     skilltype = case_when(.data$skill %in% c("Serve", "Reception", "Dig", "Freeball", "Block", "Set") ~ .data$skill_type,
                                                           .data$skill == "Attack" ~ ifelse(is.na(.data$attack_description), .data$skill_type, .data$attack_description)),
                                     subtitle = js_str_nospecials(paste("Set", .data$set_number, "-", .data$home_team, .data$home_team_score, "-", .data$visiting_team_score, .data$visiting_team)),
                                     subtitleskill = js_str_nospecials(paste(.data$player_name, "-", .data$skilltype, ":", .data$evaluation_code)))
                event_list <- dplyr::filter(event_list, !is.na(.data$video_time)) ## can't have missing video time entries
                ## TODO: if we filter items out here because of missing video times (but not filter from the playstable), doesn't the playstable selected row get out of whack with the actual item being played?
                vpt <- if (all(is_youtube_id(meta_video$video_src) | grepl("https?://.*youtube", meta_video$video_src, ignore.case = TRUE) | grepl("https?://youtu\\.be", meta_video$video_src, ignore.case = TRUE))) {
                           "youtube"
                       } else {
                           "local"
                       }
                ## TODO also check for mixed sources, which we can't handle yet
                video_player_type(vpt)
                if (!is.null(input$highlight_list)) {
                    pl <- ovideo::ov_video_playlist_pid(x = event_list, meta = meta_video, type = vpt, extra_cols = c("subtitle", plays_cols_to_show))
                } else {
                    pl <- ovideo::ov_video_playlist(x = event_list, meta = meta_video, type = vpt, timing = clip_timing(), extra_cols = c("subtitle", "subtitleskill", plays_cols_to_show))
                }
                pl <- pl[!is.na(pl$start_time) & !is.na(pl$duration), ]
                ## also keep track of actual file paths
                left_join(pl, meta_video[, c("file", "video_src")], by = "video_src")
            }
        })

        playlist_sbs1 <- reactive({
            ## Customize pbp
            meta_video <- video_meta_sbs1()
            if (is.null(pbp_augment_sbs1()) || nrow(pbp_augment_sbs1()) < 1 || is.null(meta_sbs1()) || is.null(selected_game_id_sbs1()) || is.null(meta_video) || nrow(meta_video) < 1 || is.null(playstable_data_sbs1()) || nrow(playstable_data_sbs1()) < 1) {
                NULL
            } else {
                if (trace_execution) cat("recalculating playlist\n")
                event_list <- mutate(playstable_data_sbs1(), skill = case_when(.data$skill %in% c("Freeball dig", "Freeball over") ~ "Freeball", TRUE ~ .data$skill), ## ov_video needs just "Freeball"
                                     skilltype = case_when(.data$skill %in% c("Serve", "Reception", "Dig", "Freeball", "Block", "Set") ~ .data$skill_type,
                                                           .data$skill == "Attack" ~ .data$attack_description),
                                     subtitle = js_str_nospecials(paste("Set", .data$set_number, "-", .data$home_team, .data$home_team_score, "-", .data$visiting_team_score, .data$visiting_team)),
                                     subtitleskill = js_str_nospecials(paste(.data$player_name, "-", .data$skilltype, ":", .data$evaluation_code)))
                event_list <- dplyr::filter(event_list, !is.na(.data$video_time)) ## can't have missing video time entries
                ## TODO: if we filter items out here because of missing video times (but not filter from the playstable), doesn't the playstable selected row get out of whack with the actual item being played?
                vpt <- if (all(is_youtube_id(meta_video$video_src) | grepl("https?://.*youtube", meta_video$video_src, ignore.case = TRUE) | grepl("https?://youtu\\.be", meta_video$video_src, ignore.case = TRUE))) {
                    "youtube"
                } else {
                    "local"
                }
                ## TODO also check for mixed sources, which we can't handle yet
                video_player_type_sbs1(vpt)
                if (!is.null(input$highlight_list_sbs1)) {
                    pl <- ovideo::ov_video_playlist_pid(x = event_list, meta = meta_video, type = vpt, extra_cols = c("subtitle", plays_cols_to_show))
                } else {
                    pl <- ovideo::ov_video_playlist(x = event_list, meta = meta_video, type = vpt, timing = clip_timing_sbs1(), extra_cols = c("subtitle", "subtitleskill", plays_cols_to_show))
                }
                pl <- pl[!is.na(pl$start_time) & !is.na(pl$duration), ]
                ## also keep track of actual file paths
                left_join(pl, meta_video[, c("file", "video_src")], by = "video_src")
            }
        })
        
        playlist_sbs2 <- reactive({
            ## Customize pbp
            meta_video <- video_meta_sbs2()
            if (is.null(pbp_augment_sbs2()) || nrow(pbp_augment_sbs2()) < 1 || is.null(meta_sbs2()) || is.null(selected_game_id_sbs2()) || is.null(meta_video) || nrow(meta_video) < 1 || is.null(playstable_data_sbs2()) || nrow(playstable_data_sbs2()) < 1) {
                NULL
            } else {
                if (trace_execution) cat("recalculating playlist\n")
                event_list <- mutate(playstable_data_sbs2(), skill = case_when(.data$skill %in% c("Freeball dig", "Freeball over") ~ "Freeball", TRUE ~ .data$skill), ## ov_video needs just "Freeball"
                                     skilltype = case_when(.data$skill %in% c("Serve", "Reception", "Dig", "Freeball", "Block", "Set") ~ .data$skill_type,
                                                           .data$skill == "Attack" ~ .data$attack_description),
                                     subtitle = js_str_nospecials(paste("Set", .data$set_number, "-", .data$home_team, .data$home_team_score, "-", .data$visiting_team_score, .data$visiting_team)),
                                     subtitleskill = js_str_nospecials(paste(.data$player_name, "-", .data$skilltype, ":", .data$evaluation_code)))
                event_list <- dplyr::filter(event_list, !is.na(.data$video_time)) ## can't have missing video time entries
                ## TODO: if we filter items out here because of missing video times (but not filter from the playstable), doesn't the playstable selected row get out of whack with the actual item being played?
                vpt <- if (all(is_youtube_id(meta_video$video_src) | grepl("https?://.*youtube", meta_video$video_src, ignore.case = TRUE) | grepl("https?://youtu\\.be", meta_video$video_src, ignore.case = TRUE))) {
                    "youtube"
                } else {
                    "local"
                }
                ## TODO also check for mixed sources, which we can't handle yet
                video_player_type_sbs2(vpt)
                if (!is.null(input$highlight_list_sbs2)) {
                    pl <- ovideo::ov_video_playlist_pid(x = event_list, meta = meta_video, type = vpt, extra_cols = c("subtitle", plays_cols_to_show))
                } else {
                    pl <- ovideo::ov_video_playlist(x = event_list, meta = meta_video, type = vpt, timing = clip_timing_sbs2(), extra_cols = c("subtitle", "subtitleskill", plays_cols_to_show))
                }
                pl <- pl[!is.na(pl$start_time) & !is.na(pl$duration), ]
                ## also keep track of actual file paths
                left_join(pl, meta_video[, c("file", "video_src")], by = "video_src")
            }
        })
        
        
        clip_timing <- reactive({
            ## parse timing from inputs, with fallback to ov_video_timing_df() if it fails
            tryCatch({
                ## defaults
                def <- ovideo::ov_video_timing_df()
                ## need to explicitly list all the inputs to get reactivity, ergh
                blah <- list(input$timing_serve_serve_start_offset, input$timing_serve_serve_duration,
                             input$timing_reception_reception_start_offset, input$timing_reception_reception_duration,
                             input$timing_set_reception_start_offset, input$timing_set_reception_duration,
                             input$timing_set_transition_start_offset, input$timing_set_transition_duration,
                             input$timing_attack_reception_start_offset, input$timing_attack_reception_duration,
                             input$timing_attack_transition_start_offset, input$timing_attack_transition_duration,
                             input$timing_block_reception_start_offset, input$timing_block_reception_duration,
                             input$timing_block_transition_start_offset, input$timing_block_transition_duration,
                             input$timing_dig_transition_start_offset, input$timing_dig_transition_duration,
                             input$timing_freeball_reception_start_offset, input$timing_freeball_reception_duration,
                             input$timing_freeball_transition_start_offset, input$timing_freeball_transition_duration)
                for (ri in seq_len(nrow(def))) {
                    skill <- def$skill[ri]
                    phase <- def$phase[ri]
                    def$start_offset[ri] <- input[[paste0("timing_", tolower(skill), "_", tolower(phase), "_start_offset")]]
                    def$duration[ri] <- input[[paste0("timing_", tolower(skill), "_", tolower(phase), "_duration")]]
                }
                def
            }, error = function(e) ovideo::ov_video_timing_df())
        })
        tweak_all_timings <- function(whch, by) {
            if (whch %in% c("start_offset", "duration") && is.numeric(by)) {
                def <- data.frame(skill = c("serve", "reception", "set", "set", "attack", "attack", "block", "block", "dig", "freeball", "freeball"),
                                  phase = c("serve", "reception", "reception", "transition", "reception", "transition", "reception", "transition", "transition", "reception", "transition"),
                                  stringsAsFactors = FALSE)
                for (ri in seq_len(nrow(def))) {
                    thisid <- paste0("timing_", def$skill[ri], "_", def$phase[ri], "_", whch)
                    updateNumericInput(session, inputId = thisid, value = input[[thisid]] + by)
                }
            }
        }
        observeEvent(input$timing_all_start_minus, tweak_all_timings("start_offset", -1))
        observeEvent(input$timing_all_start_plus, tweak_all_timings("start_offset", 1))
        observeEvent(input$timing_all_duration_minus, tweak_all_timings("duration", -1))
        observeEvent(input$timing_all_duration_plus, tweak_all_timings("duration", 1))

        
        clip_timing_sbs1 <- reactive({
            ## parse timing from inputs, with fallback to ov_video_timing_df() if it fails
            tryCatch({
                ## defaults
                def <- ovideo::ov_video_timing_df()
                ## need to explicitly list all the inputs to get reactivity, ergh
                blah <- list(input$timing_serve_serve_start_offset, input$timing_serve_serve_duration,
                             input$timing_reception_reception_start_offset, input$timing_reception_reception_duration,
                             input$timing_set_reception_start_offset, input$timing_set_reception_duration,
                             input$timing_set_transition_start_offset, input$timing_set_transition_duration,
                             input$timing_attack_reception_start_offset, input$timing_attack_reception_duration,
                             input$timing_attack_transition_start_offset, input$timing_attack_transition_duration,
                             input$timing_block_reception_start_offset, input$timing_block_reception_duration,
                             input$timing_block_transition_start_offset, input$timing_block_transition_duration,
                             input$timing_dig_transition_start_offset, input$timing_dig_transition_duration,
                             input$timing_freeball_reception_start_offset, input$timing_freeball_reception_duration,
                             input$timing_freeball_transition_start_offset, input$timing_freeball_transition_duration)
                for (ri in seq_len(nrow(def))) {
                    skill <- def$skill[ri]
                    phase <- def$phase[ri]
                    def$start_offset[ri] <- input[[paste0("timing_", tolower(skill), "_", tolower(phase), "_start_offset")]]
                    def$duration[ri] <- input[[paste0("timing_", tolower(skill), "_", tolower(phase), "_duration")]]
                }
                def
            }, error = function(e) ovideo::ov_video_timing_df())
        })
        
        clip_timing_sbs2 <- reactive({
            ## parse timing from inputs, with fallback to ov_video_timing_df() if it fails
            tryCatch({
                ## defaults
                def <- ovideo::ov_video_timing_df()
                ## need to explicitly list all the inputs to get reactivity, ergh
                blah <- list(input$timing_serve_serve_start_offset, input$timing_serve_serve_duration,
                             input$timing_reception_reception_start_offset, input$timing_reception_reception_duration,
                             input$timing_set_reception_start_offset, input$timing_set_reception_duration,
                             input$timing_set_transition_start_offset, input$timing_set_transition_duration,
                             input$timing_attack_reception_start_offset, input$timing_attack_reception_duration,
                             input$timing_attack_transition_start_offset, input$timing_attack_transition_duration,
                             input$timing_block_reception_start_offset, input$timing_block_reception_duration,
                             input$timing_block_transition_start_offset, input$timing_block_transition_duration,
                             input$timing_dig_transition_start_offset, input$timing_dig_transition_duration,
                             input$timing_freeball_reception_start_offset, input$timing_freeball_reception_duration,
                             input$timing_freeball_transition_start_offset, input$timing_freeball_transition_duration)
                for (ri in seq_len(nrow(def))) {
                    skill <- def$skill[ri]
                    phase <- def$phase[ri]
                    def$start_offset[ri] <- input[[paste0("timing_", tolower(skill), "_", tolower(phase), "_start_offset")]]
                    def$duration[ri] <- input[[paste0("timing_", tolower(skill), "_", tolower(phase), "_duration")]]
                }
                def
            }, error = function(e) ovideo::ov_video_timing_df())
        })
        
        ## video stuff
        video_player_type <- reactiveVal("local") ## the current player type, either "local" or "youtube"
        video_player_type_sbs1 <- reactiveVal("local") ## the current player type, either "local" or "youtube"
        video_player_type_sbs2 <- reactiveVal("local") ## the current player type, either "local" or "youtube"
        
        observe({
            if (!is.null(playlist()) && nrow(playlist()) > 0) {
                if (trace_execution) message("reinitializing video player")
                ## when playlist() changes, push it through to the javascript playlist
                isolate(waspaused <- isTRUE(input$player_pause_state))
                if (video_player_type() == "local") {
                    js_hide("dvyt_player")
                    js_show("dv_player")
                } else {
                    js_hide("dv_player")
                    js_show("dvyt_player")
                }
                ov_video_control("stop", controller_var = "dvpl")
                if (is_fresh_playlist) {
                    evaljs(ovideo::ov_playlist_as_onclick(playlist(), video_id = if (video_player_type() == "local") "dv_player" else "dvyt_player", dvjs_fun = "dvjs_set_playlist_and_play", seamless = TRUE, controller_var = "dvpl"))
                } else {
                    ## should only be here if the playlist was modified but playstable was NOT (i.e. we deleted an item from the playlist)
                    ## so if we are mid-playlist already, do some other shenanigans so as not to restart from the first playlist item
                    evaljs(ovideo::ov_playlist_as_onclick(playlist(), video_id = if (video_player_type() == "local") "dv_player" else "dvyt_player", dvjs_fun = "dvjs_set_playlist", seamless = TRUE, controller_var = "dvpl")) ## set the playlist but don't auto-start playing (which would start from item 1)
                    evaljs(paste0("dvpl.video_controller.current=", master_playstable_selected_row - 1, ";")) ## set the current item
                    ## if we were paused, don't restart but set the player state to paused since it got reset when the new playlist was loaded
                    if (!waspaused) evaljs("dvpl.video_play();") else evaljs("dvpl.video_controller.paused=true;")
                }
            } else {
                ## empty playlist, so stop the video, and clean things up
                evaljs("dvpl.clear_playlist();")
                ## evaljs("remove_vspinner();") ## doesn't have an effect?
                evaljs("document.getElementById(\"subtitle\").textContent=\"Score\"; document.getElementById(\"subtitleskill\").textContent=\"Skill\";")
            }
        })
        observe({
            if (!is.null(playlist_sbs1()) && nrow(playlist_sbs1()) > 0) {
                ## when playlist() changes, push it through to the javascript playlist
                if (video_player_type_sbs1() == "local") {
                    js_hide("dvyt_player_sbs1")
                    js_show("dv_player_sbs1")
                } else {
                    js_hide("dv_player_sbs1")
                    js_show("dvyt_player_sbs1")
                }
                ov_video_control("stop", controller_var = "dvpl_sbs1")
                evaljs(ovideo::ov_playlist_as_onclick(playlist_sbs1(), video_id = if (video_player_type_sbs1() == "local") "dv_player_sbs1" else "dvyt_player_sbs1", dvjs_fun = "dvjs_set_playlist", seamless = TRUE, controller_var = "dvpl_sbs1"))
            } else {
                ## empty playlist, so stop the video, and clean things up
                evaljs("dvpl_sbs1.clear_playlist();")
                ## evaljs("remove_vspinner();") ## doesn't have an effect?
                evaljs("document.getElementById(\"subtitle_sbs1\").textContent=\"Score\"; document.getElementById(\"subtitleskill_sbs1\").textContent=\"Skill\";")
            }
        })
        observe({
            if (!is.null(playlist_sbs2()) && nrow(playlist_sbs2()) > 0) {
                ## when playlist() changes, push it through to the javascript playlist
                if (video_player_type_sbs2() == "local") {
                    js_hide("dvyt_player_sbs2")
                    js_show("dv_player_sbs2")
                } else {
                    js_hide("dv_player_sbs2")
                    js_show("dvyt_player_sbs2")
                }
                ov_video_control("stop")
                evaljs(ovideo::ov_playlist_as_onclick(playlist_sbs2(), video_id = if (video_player_type_sbs2() == "local") "dv_player_sbs2" else "dvyt_player_sbs2", dvjs_fun = "dvjs_set_playlist", seamless = TRUE, controller_var = "dvpl_sbs2"))
            } else {
                ## empty playlist, so stop the video, and clean things up
                evaljs("dvpl_sbs2.clear_playlist();")
                ## evaljs("remove_vspinner();") ## doesn't have an effect?
                evaljs("document.getElementById(\"subtitle_sbs2\").textContent=\"Score\"; document.getElementById(\"subtitleskill_sbs2\").textContent=\"Skill\";")
            }
        })
        
        output$player_controls_ui <- renderUI({
            tags$div(tags$div(tags$button("Start", onclick = "dvpl.video_play();"),
                              tags$button("Prev", onclick = "dvpl.video_prev();"),
                              tags$button("Next", onclick = "dvpl.video_next(false);"),
                              tags$button("Play/Pause", onclick = "dvpl.video_pause();"),
                              tags$button("Back 1s", onclick = "dvpl.jog(-1);")),
                     tags$div(style="margin-top:10px;", tags$span(id = "subtitle", "Score"), tags$span(id = "subtitleskill", "Skill"),
                              uiOutput("create_clip_button_ui", inline = TRUE)))
        })

        
        output$player_controls_ui_sbs1 <- renderUI({
            tags$div(tags$div(
                tags$span(icon("angle-double-left")),
                tags$button("- 1s", onclick = "dvpl_sbs1.jog(-1);"),
                tags$button("- 0.1s", onclick = "dvpl_sbs1.jog(-0.1);"),#,
                tags$button("+ 0.1s", onclick = "dvpl_sbs1.jog(+0.1);"),
                tags$button("+ 1s", onclick = "dvpl_sbs1.jog(+1);"))
            )
        })
        
        output$player_controls_ui_sbs2 <- renderUI({
            tags$div(tags$div(
                tags$button("- 1s", onclick = "dvpl_sbs2.jog(-1);"),
                tags$button("- 0.1s", onclick = "dvpl_sbs2.jog(-0.1);"),#,
                tags$button("+ 0.1s", onclick = "dvpl_sbs2.jog(+0.1);"),
                tags$button("+ 1s", onclick = "dvpl_sbs2.jog(+1);"),
                tags$span(icon("angle-double-right")))
            )
        })
        
        output$player_controls_ui_sbs12 <- renderUI({
            tags$div(tags$button("Start", onclick = "dvpl_sbs1.video_play();dvpl_sbs2.video_play();"),
                     tags$button("Play/Pause", onclick = "dvpl_sbs1.video_pause();dvpl_sbs2.video_pause();"),
                     tags$button("- 1s", onclick = "dvpl_sbs1.jog(-1);dvpl_sbs2.jog(-1);"),
                     tags$button("+ 1s", onclick = "dvpl_sbs1.jog(+1);dvpl_sbs2.jog(+1);")

                     )
        })
        

        clip_filename <- reactiveVal("")
        clip_status <- reactiveVal(NULL)
        output$create_clip_button_ui <- renderUI({
            ok <- !is.null(playlist()) && nrow(playlist()) > 0 && video_player_type() != "youtube"
            ## also check that videos are not remote
            ok <- ok && !any(grepl("^https?://", playlist()$video_src, ignore.case = TRUE))
            if (ok) {
                actionButton("create_clip_button", "Download clip")
            } else {
                NULL
            }
        })
        observeEvent(input$create_clip_button, {
            ov_video_control("stop", controller_var = "dvpl")
            showModal(modalDialog(title = "Create and download video clip", size = "l", "Please wait, creating clip. This could take some time.", uiOutput("clip_status_ui")))
            ## TODO - add progress indicator to that somehow? may not be possible with parallel
            ## do the video crunching
            clip_status(NULL)
            filename <- tempfile(fileext = ".mp4")
            tryCatch({
                chk <- sys::exec_internal("ffmpeg", "-version")
                future::plan("multisession")
                pll <- lapply(seq_len(nrow(playlist())), function(z) as.list(playlist()[z, ])) ## need a non-reactive list-formatted copy of this to use with future_lapply
                tempfiles <- future.apply::future_lapply(pll, function(plitem) {
                    ##tempfiles <- lapply(pll), function(plitem) { ## for testing, no parallel
                    outfile <- tempfile(fileext = paste0(".", fs::path_ext(plitem$file)))
                    if (file.exists(outfile)) unlink(outfile)
                    infile <- plitem$file
                    res <- sys::exec_internal("ffmpeg", c("-ss", plitem$start_time, "-i", infile, "-strict", "-2", "-t", plitem$duration, outfile))
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
                clip_filename(filename)
                removeModal()
                showModal(modalDialog(title = "Create and download video clip", size = "l", shiny::downloadButton("download_clip")))
            }, error = function(e) {
                clip_status(conditionMessage(e))
            })
        })
        output$clip_status_ui <- renderUI({
            if (is.null(clip_status())) {
                NULL
            } else {
                tags$div(class = "alert alert-danger", "Sorry, something went wrong. The error message was: ", clip_status())
            }
        })
        output$download_clip <- shiny::downloadHandler(
            filename = function() {
                filename <- paste0("Highlights", selected_game_id(), ".mp4")
            },
            content = function(file) {
                removeModal()
                file.copy(clip_filename(), file)
            },
            contentType = "video/mp4"
        )

        observeEvent(input$playback_rate, {
            if (!is.null(input$playback_rate)) ov_video_control("set_playback_rate", input$playback_rate, controller_var = "dvpl")
        })

        observeEvent(input$playback_rate_sbs12, {
            if (!is.null(input$playback_rate_sbs12)) {
                ov_video_control("set_playback_rate", input$playback_rate_sbs12, controller_var = "dvpl_sbs1")
                ov_video_control("set_playback_rate", input$playback_rate_sbs12, controller_var = "dvpl_sbs2")
            }
        })
        

        output$chart_ui <- renderUI(app_data$chart_renderer)

        ## height of the video player element
        vo_height <- reactiveVal("auto")
        observe({
            if (video_player_type() %eq% "youtube") {
                if (!is.null(input$dvyt_height) && as.numeric(input$dvyt_height) > 0) {
                    vo_height(as.numeric(input$dvyt_height))
                    evaljs(paste0("document.getElementById('video_overlay').style.height = '", vo_height(), "px';"))
                } else {
                    vo_height("auto")
                    evaljs(paste0("document.getElementById('video_overlay').style.height = '400px';"))
                }
            } else {
                if (!is.null(input$dv_height) && as.numeric(input$dv_height) > 0) {
                    vo_height(as.numeric(input$dv_height))
                    evaljs(paste0("document.getElementById('video_overlay').style.height = '", vo_height(), "px';"))
                } else {
                    vo_height("auto")
                    evaljs(paste0("document.getElementById('video_overlay').style.height = '400px';"))
                }
            }
        })
        ## width of the video player element
        vo_width <- reactiveVal("auto")
        observe({
            if (video_player_type() %eq% "youtube") {
                if (!is.null(input$dvyt_width) && as.numeric(input$dvyt_width) > 0) {
                    vo_width(as.numeric(input$dvyt_width))
                } else {
                    vo_width("auto")
                }
            } else {
                if (!is.null(input$dv_width) && as.numeric(input$dv_width) > 0) {
                    vo_width(as.numeric(input$dv_width))
                } else {
                    vo_width("auto")
                }
            }
        })
        ## height of the video player container, use as negative vertical offset on the overlay element
        observe({
            if (!is.null(input$vo_voffset) && as.numeric(input$vo_voffset) > 0) {
                evaljs(paste0("document.getElementById('video_overlay').style.marginTop = '-", input$vo_voffset, "px';"))
            } else {
                evaljs("document.getElementById('video_overlay').style.marginTop = '0px';")
            }
        })

        ## panel show/hide
        panel_visible <- reactiveValues(filter2 = FALSE)
        observeEvent(input$collapse_filter2, {
            if (panel_visible$filter2) js_hide("filter2_panel") else js_show("filter2_panel")
            panel_visible$filter2 <- !panel_visible$filter2
        })
        observe({
            if (panel_visible$filter2) updateActionButton(session, "collapse_filter2", label = "Hide") else updateActionButton(session, "collapse_filter2", label = "Show")
        })
        
        panel_visible_sbs1 <- reactiveValues(filter2_sbs1 = FALSE)
        observeEvent(input$collapse_filter2_sbs1, {
            if (panel_visible_sbs1$filter2_sbs1) js_hide("filter2_panel_sbs1") else js_show("filter2_panel_sbs1")
            panel_visible_sbs1$filter2_sbs1 <- !panel_visible_sbs1$filter2_sbs1
        })
        observe({
            if (panel_visible_sbs1$filter2_sbs1) updateActionButton(session, "collapse_filter2_sbs1", label = "Hide") else updateActionButton(session, "collapse_filter2_sbs1", label = "Show")
        })
        
        panel_visible_sbs2 <- reactiveValues(filter2_sbs2 = FALSE)
        observeEvent(input$collapse_filter2_sbs2, {
            if (panel_visible_sbs2$filter2_sbs2) js_hide("filter2_panel_sbs2") else js_show("filter2_panel_sbs2")
            panel_visible_sbs2$filter2_sbs2 <- !panel_visible_sbs2$filter2_sbs2
        })
        observe({
            if (panel_visible_sbs2$filter2_sbs2) updateActionButton(session, "collapse_filter2_sbs2", label = "Hide") else updateActionButton(session, "collapse_filter2_sbs2", label = "Show")
        })

    }
}
