globalVariables("playstable_add_mp4_col") ## suppress check warning about this

ovva_shiny_server <- function(app_data) {
    function(input, output, session) {
        trace_execution <- FALSE ## for debugging
        debug_mp4 <- FALSE
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
            "Opposition team rotation (setter position)" = "opposition_setter_position",
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
            "Serve zone" = "pt_serve_zone", "Pass/dig zone" = "ts_pass_zone",
            "Setter on court" = "setter_on_court", "Opposition setter on court" = "opposition_setter_on_court", "Opposition team" = "opposition_team")

        ## some inits
        master_playstable_selected_row <- -99L ## non-reactive
        is_fresh_playlist <- FALSE

        ## helper function: get the right function from the playlist handler for a given skill and specific
        funs_from_playlist <- function(specific) {
            ## return a list of functions
            app_data$playlist_handler$fun[which(app_data$playlist_handler$specific %in% specific)]
        }
        funs_from_highlight <- function(specific) {
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

        ## highlight the season selector if it's empty
        observe({
            if (isTRUE(app_data$no_initial_season_selection)) {
                if (identical(input$season, "")) evaljs("$('#season_highlight').addClass('reminder');") else evaljs("$('#season_highlight').removeClass('reminder');")
            }
        })

        ## update the season choices
        season_choices <- reactive(names(get_data_paths()))
        observe({
            chc <- season_choices()
            isolate(sel <- input$season)
            if (isTRUE(app_data$no_initial_season_selection)) chc <- c("Choose" = "", chc) ## no initial selection in this case
            if (is.null(sel) || !sel %in% chc) sel <- chc[1]
            updateSelectInput(session, "season", choices = chc, selected = sel)
        })
        ## play-by-play data for selected season
        meta_unfiltered <- reactiveVal(NULL)
        pbp <- reactiveVal(NULL)
        pbp_augment <- reactiveVal(NULL)
        got_no_video <- reactiveVal(0L) ## only used to show message about no matches with video: 0 = ok, 1 = all files missing video, 2 = all files missing video times and/or videos
        season_data_type <- reactiveVal("indoor")
        empty_video_list <- dplyr::tibble(match_id = character(), filename = character(), video_source = character())
        video_list <- reactiveVal(empty_video_list)
        ## process metadata for selected season matches and update pbp reactiveVal accordingly
        meta <- reactive({
            if (!is.null(input$season) && input$season %in% season_choices()) {
                isolate({
                    sdigest <- digest::digest(input$season)
                    if (trace_execution) cat("recalculating meta\n")
                    showModal(modalDialog(title = "Processing match metadata ...", footer = NULL, "Please wait"))
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
                    ## augment the match_id values with input$season, in case there are the same matches in different data sets (seasons)
                    out <- lapply(out, function(z) { z$match_id <- paste0(sdigest, "|", z$match_id); z })
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
                    meta_unfiltered(out)
                    ## if the plays component doesn't have clock times, then we need to get the date from the metadata
                    game_dates_meta <- bind_rows(lapply(out, function(z) list(match_id = z$match_id, game_date = z$match$date))) %>%
                        dplyr::filter(!is.na(.data$game_date))
                    game_dates_meta <- game_dates_meta[!game_dates_meta$match_id %in% game_dates_meta$match_id[duplicated(game_dates_meta$match_id)], ]
                    ## out is a list of metadata objects
                    ## prune out any that don't have video
                    if (!is.null(out)) out <- Filter(function(z) !is.null(z$video) && nrow(z$video) > 0, out)
                    if (length(out) < 1) {
                        got_no_video(1L)
                        video_list(empty_video_list)
                        pbp(NULL)
                        pbp_augment(NULL)
                        out <- NULL
                        season_data_type("indoor") ## default
                    } else {
                        season_data_type(tryCatch(if (grepl("beach", out[[1]]$match$regulation)) "beach" else "indoor", error = function(e) "indoor"))
                        ## for each video file, check if it exists and try and find it if not
                        for (z in seq_along(out)) {
                            if (is_youtube_id(out[[z]]$video$file)) {
                                ## do nothing
                            } else if (grepl("^https?://", out[[z]]$video$file, ignore.case = TRUE)) {
                                ## check that the URL is valid, otherwise do nothing
                                ## skip for now, it will be slow with many files
##                                chk <- tryCatch(httr::status_code(httr::HEAD(out[[z]]$video$file)), error = function(e) 500)
##                                if (chk >= 300) out[[z]]$video$file <- NA_character_
                            } else {
                                try({
                                    if (isTRUE(app_data$video_subtree_only)) {
                                        out[[z]]$video$file <- find_video_in_subtree(dvw_filename = out[[z]]$filename, video_filename = fs::fs_path(out[[z]]$video$file), subtree_only = TRUE, alt_path = app_data$alt_video_path)
                                    } else {
                                        temp <- find_video_in_subtree(dvw_filename = out[[z]]$filename, video_filename = fs::fs_path(out[[z]]$video$file), subtree_only = FALSE, alt_path = app_data$alt_video_path)
                                        out[[z]]$video$file <- ifelse(!fs::file_exists(as.character(out[[z]]$video$file)) && !is.na(temp), temp, out[[z]]$video$file)
                                    }
                                })
                            }
                        }
                        ## keep track of videos
                        video_list(bind_rows(lapply(out, function(z) list(match_id = z$match_id, filename = z$filename, video_source = if (nrow(z$video) == 1 && !is.na(z$video$file) && nzchar(z$video$file)) z$video$file else NA_character_))))
                        ## remove any files with no associated video
                        out <- Filter(Negate(is.null), lapply(out, function(z) if (nrow(z$video) == 1 && !is.na(z$video$file) && nzchar(z$video$file)) z))
                        if (length(out) < 1) {
                            ## no files with video
                            got_no_video(1L)
                            pbp(NULL)
                            pbp_augment(NULL)
                            out <- NULL
                        } else {
                            ## will also need to check actual plays data to remove any files with all-missing video times
                            showModal(modalDialog(title = "Processing match data ...", footer = NULL, "Please wait"))
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
                            ## augment the match_id values with input$season to match what we did to the match_ids in the metadata above
                            mydat$match_id <- paste0(sdigest, "|", mydat$match_id)
                            ## check for all-missing video times now
                            no_video_times_mids <- mydat %>% group_by(.data$match_id) %>% dplyr::summarize(nv = all(is.na(.data$video_time))) %>% dplyr::filter(.data$nv) %>% dplyr::pull(.data$match_id)
                            ## remove those from out
                            out <- Filter(Negate(is.null), lapply(out, function(z) if (z$match_id %in% no_video_times_mids) NULL else z))
                            if (length(out) < 1) {
                                ## no files with video/video times
                                got_no_video(2L)
                                pbp(NULL)
                                pbp_augment(NULL)
                                out <- NULL
                            } else {
                                my_match_ids <- as.character(lapply(out, function(z) z$match_id))
                                mydat <- dplyr::filter(mydat, .data$match_id %in% my_match_ids)
                                got_no_video(0L)
                                ## now process pbp()
                                mydat <- mydat[mydat$match_id %in% my_match_ids, ]
                                mydat <- ungroup(mutate(group_by(mydat, .data$match_id), game_date = if (all(is.na(.data$time))) as.Date(NA) else min(as.Date(.data$time), na.rm = TRUE)))
                                ## replace missing game dates with those from meta, if we can
                                mydat <- left_join(mydat, game_dates_meta %>% dplyr::rename(game_date2 = "game_date"), by = "match_id") %>%
                                    mutate(game_date = if_else(is.na(.data$game_date) | is.infinite(.data$game_date), .data$game_date2, .data$game_date)) %>%
                                    dplyr::select(-"game_date2")
##                        mydat <- mutate(mydat, game_id = paste0(gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1', .data$home_team, perl = TRUE),
##                                                                "_", gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',.data$visiting_team, perl = TRUE)))
##                        mydat <- mutate(mydat, game_id = case_when(!is.na(.data$game_date) & !is.infinite(.data$game_date) ~ paste0(.data$game_date, "_", .data$game_id),
##                                                                   TRUE ~ .data$game_id))
##                        ## de-duplicate game_ids
##                        dedup <- mutate(distinct(dplyr::select(mydat, .data$match_id, .data$game_id)), game_id = make.unique(.data$game_id, sep = "_"))
##                        mydat <- left_join(dplyr::select(mydat, -"game_id"), dedup, by = "match_id")
                                pbp(mydat)
                                ## Augment pbp with additional covariates
                                pbp_augment(preprocess_data(mydat, data_type = season_data_type()))
                            }
                        }
                    }
                    removeModal()
                    have_done_startup(TRUE)
                })
                out
            } else {
                isolate({
                    got_no_video(0L)
                    meta_unfiltered(NULL)
                    video_list(empty_video_list)
                    pbp(NULL)
                    pbp_augment(NULL)
                    season_data_type("indoor") ## default
                })
                NULL
            }
        })

        ## Games
        last_game_table_hash <- ""
        game_table_dropdown <- reactiveVal(NULL)
        observe({
            if (trace_execution) cat("updating game_table_dropdown()\n")
            out <- if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                       isolate({
                           ## don't be reactive to these, just use them to tailor the message
                           output$no_game_data <- renderUI(
                               if (is.null(input$season)) {
                                   tags$div(class = "alert alert-info", "No competition data sets. Log in?")
                               } else if (!is.null(meta()) && is.null(pbp())) {
                                   tags$div(class = "alert alert-danger", "All matches are missing their video files.")
                               } else if (is.null(meta()) && have_done_startup()) {
                                   if (isolate(got_no_video()) > 1L) {
                                       tags$div(class = "alert alert-danger", "All matches are missing their video files and/or have not been synchronized with video.")
                                   } else if (isolate(got_no_video()) > 0L) {
                                       tags$div(class = "alert alert-danger", "All matches are missing their video files.")
                                   } else {
                                       ## some other failure
                                       tags$div(class = "alert alert-danger", "Sorry, something went wrong processing this data set.")
                                   }
                               } else {
                                   NULL
                               })
                       })
                       ## hide the game selector if we have no games with video (or haven't selected a data set)
                       js_hide("game_table_dropdown")
                       character()
                   } else {
                       output$no_game_data <- renderUI(NULL)
                       js_show("game_table_dropdown")
                       ## Customize pbp
                       datatble <- distinct(pbp_augment(), .data$match_id, .data$game_date, .data$visiting_team, .data$home_team, .keep_all = FALSE)
                       if (all(is.na(datatble$game_date))) {
                           datatble <- dplyr::mutate(datatble, display_ID = paste0("Unknown date: ",.data$home_team," - ",.data$visiting_team))
                       } else {
                           datatble <- dplyr::mutate(datatble, display_ID = paste0(ifelse(is.na(.data$game_date), "Unknown date", format(.data$game_date, "%d %b %Y")),": ",.data$home_team," - ",.data$visiting_team))
                       }
                       datatble <- dplyr::arrange(datatble, .data$game_date)
                       ## named list, so that display_ID gets shown but the code can operate directly on match_id as the selected value
                       setNames(as.list(datatble$match_id), datatble$display_ID)
                   }
            gt_hash <- digest::digest(out)
            if (last_game_table_hash != gt_hash) {
                last_game_table_hash <<- gt_hash
                game_table_dropdown(out)
                if (trace_execution) cat("updating input$game_table_dropdown\n")
                sel <- intersect(out, input$game_table_dropdown)
                updatePickerInput(session, "game_table_dropdown", choices = out, selected = sel)
            }
        })

        selected_match_id <- reactive({
            if (trace_execution) cat("updating selected_match_id\n")
            unique(na.omit(input$game_table_dropdown))
        })

        ## Team
        team_list <- reactiveVal(NULL)
        observe({
            if (trace_execution) cat("recalculating team_list\n")
            blah <- selected_match_id() ## reactive to this
            pbp <- pbp_augment() ## no reason to isolate()?
            tl <- if (is.null(pbp) || nrow(pbp) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp, .data$match_id %in% selected_match_id())
                sort(unique(na.omit(tmp$team)))
            }
            team_list(tl)
            isolate(sel <- intersect(tl, input$team_list))
            if (length(sel) < 1) sel <- character()## ## select none, which will be treated as "no filter" ## previously tl ## select all
            updatePickerInput(session, "team_list", choices = tl, selected = sel)
        })

        ## Player ID
        player_list <- reactiveVal(NULL)
        last_player_list_hash <- ""
        observe({
            if (trace_execution) cat("recalculating player_list\n")
            blah <- list(selected_match_id(), input$team_list) ## reactive to these and pbp_augment
            pl <- if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), all_or_filter(.data$team, input$team_list))
                sort(unique(na.omit(tmp$player_name)))
            }
            pl_hash <- digest::digest(pl)
            if (last_player_list_hash != pl_hash) {
                if (trace_execution) cat("  updating player selector\n")
                last_player_list_hash <<- pl_hash
                player_list(pl)
                isolate(sel <- intersect(pl, input$player_list))
                if (length(sel) < 1) sel <- character() ## select none, which will be treated as "no filter" ## previously pl ## select all
                updatePickerInput(session, "player_list", choices = pl, selected = sel)
            } else {
                if (trace_execution) cat("  player list unchanged\n")
            }
        })##, priority = -100)

        ## Skill
        skill_list <- reactiveVal(NULL)
        last_skill_list_hash <- ""
        observe({
            if (trace_execution) cat("recalculating skill_list\n")
            sl <- if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), all_or_filter(.data$player_name, input$player_list), all_or_filter(.data$team, input$team_list))
                sort(unique(na.omit(tmp$skill)))
            }
            sl_hash <- digest::digest(sl)
            if (last_skill_list_hash != sl_hash) {
                if (trace_execution) cat("  updating skill selector\n")
                last_skill_list_hash <<- sl_hash
                skill_list(sl)
##            isolate(cat("+SS:\n", str(input$skill_list), "\n-SS\n"))
                isolate(sel <- intersect(sl, input$skill_list))
                if (length(sel) < 1) sel <- character() ## select none, which will be treated as "no filter" ## previously sl ## select all
                updatePickerInput(session, "skill_list", choices = sl, selected = sel)
            } else {
                if (trace_execution) cat("  skill list unchanged\n")
            }
        })##, priority = -101)

        ## Pre-defined playlist
        playlist_list <- reactive({
            tryCatch(
                app_data$playlist_handler$specific[all_or_filter(app_data$playlist_handler$skill, input$skill_list)],
                error = function(e) character())
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
        highlight_list <- reactive({
            c("None", app_data$highlight_handler$specific[app_data$highlight$skill %in% "Highlights"])
        })
        output$highlight_based_ui <- renderUI({
                if (length(selected_match_id()) < 1) {
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
                            multiple = FALSE)
            }
        })

        ## Skilltype
        skilltype_list <- reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), all_or_filter(.data$player_name, input$player_list), all_or_filter(.data$skill, input$skill_list), all_or_filter(.data$team, input$team_list))
                sort(unique(tmp$skilltype))
            }
        })
        observe({
            ## if the skilltype list changes, then we need to select all, otherwise we may have changes from e.g. just attacks to all skills, but we'll be restricted to just the previously-selected attack skill types
            updatePickerInput(session, "skilltype_list", choices = skilltype_list(), selected = skilltype_list())
        })

        ## Phase
        phase_list <- reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), all_or_filter(.data$player_name, input$player_list), all_or_filter(.data$team, input$team_list), all_or_filter(.data$skill, input$skill_list))
                sort(unique(tmp$phase))
            }
        })
        observe({
            isolate(sel <- intersect(phase_list(), input$phase_list))
            if (length(sel) < 1) sel <- phase_list() ## select all
            updatePickerInput(session, "phase_list", choices = phase_list(), selected = sel)
        })

        ## Advanced filter
        adFilter_list <- reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                temp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), all_or_filter(.data$player_name, input$player_list), all_or_filter(.data$team, input$team_list), all_or_filter(.data$skill, input$skill_list))
                avail <- colnames(temp)
                avail <- avail[vapply(avail, function(z) !all(is.na(temp[[z]])), FUN.VALUE = TRUE)] ## exclude all-NA cols
                avail <- adfilter_cols_to_show[adfilter_cols_to_show %in% avail] ## only those in our pre-defined list of adfilter_cols_to_show
                ## also refine by data_type
                if (grepl("beach", season_data_type())) {
                    avail <- avail[!avail %in% c("set_code", "set_description", "home_setter_position", "visiting_setter_position", "opposition_setter_position", "receiving_setter_position", "serving_setter_position", "setter_position", "setter_on_court", "opposition_setter_on_court")]
                }
                avail <- avail[order(names(avail))]
                c(list("No filter" = ""), avail) ## add a "no filter" option
            }
        })
        observe({
            isolate(sel <- intersect(adFilter_list(), input$adFilter_list))
            if (length(sel) < 1) sel <- character() ## select none
            updateSelectInput(session, "adFilter_list", choices = adFilter_list(), selected = sel)
        })

        ## Advanced filter value
        adFilterValue_list <- reactive({
            col_to_select <- input$adFilter_list
            if (is.null(col_to_select) || !nzchar(col_to_select)) return(list())
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || length(col_to_select) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), all_or_filter(.data$player_name, input$player_list), all_or_filter(.data$team, input$team_list), all_or_filter(.data$skill, input$skill_list))
                sort(unique(tmp[[col_to_select]]))
            }
        })
        observe({
            isolate(sel <- intersect(adFilterValue_list(), input$adFilterValue_list))
            if (length(sel) < 1) sel <- adFilterValue_list() ## select all
            updatePickerInput(session, "adFilterValue_list", choices = adFilterValue_list(), selected = sel)
        })

        ## Advanced filter 2
        observe({
            isolate(sel <- intersect(adFilter_list(), input$adFilterB_list))
            if (length(sel) < 1) sel <- character() ## select none
            updateSelectInput(session, "adFilterB_list", choices = adFilter_list(), selected = sel)
        })

        ## Advanced filter 2 value
        adFilterBValue_list <- reactive({
            col_to_select <- input$adFilterB_list
            if (is.null(col_to_select) || !nzchar(col_to_select)) return(list())
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || length(col_to_select) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), all_or_filter(.data$player_name, input$player_list), all_or_filter(.data$team, input$team_list), all_or_filter(.data$skill, input$skill_list))
                sort(unique(tmp[[col_to_select]]))
            }
        })
        observe({
            isolate(sel <- intersect(adFilterBValue_list(), input$adFilterBValue_list))
            if (length(sel) < 1) sel <- adFilterBValue_list() ## select all
            updatePickerInput(session, "adFilterBValue_list", choices = adFilterBValue_list(), selected = sel)
        })

        ## Help
        observeEvent(input$help, rintrojs::introjs(session, options = list("nextLabel" = "Next", "prevLabel" = "Previous", "skipLabel" = "Skip")))

        ## which vars are available for custom sort?
        observe({
            ##sortchc <- c()
            can_sort_on <- function(z) !is.null(z) && (length(z) < 1 || length(z) > 1)
            ## some inputs can now be NULL (none selected) meaning no filter applied
            ## so assume that the fixed ones (match, team, player, skill, skill type, phase) can always be sorted on?
            sortchc <- c(Game = "match_id", Team = "team", Player = "player_name", Skill = "skill", `Skill type` = "skilltype", `Phase` = "phase")
            ## if (can_sort_on(selected_match_id())) sortchc <- c(sortchc, Game = "match_id")
            ## if (can_sort_on(input$team_list)) sortchc <- c(sortchc, Team = "team")
            ## if (can_sort_on(input$player_list)) sortchc <- c(sortchc, Player = "player_name")
            ## if (can_sort_on(input$skill_list)) sortchc <- c(sortchc, Skill = "skill")
            ## if (can_sort_on(input$skilltype_list)) sortchc <- c(sortchc, `Skill type` = "skilltype")
            ## if (can_sort_on(input$phase_list)) sortchc <- c(sortchc, `Phase` = "phase")
            if (can_sort_on(input$adFilter_list)) {
                adfvar <- input$adFilter_list
                adfvarname <- names(intersect(adFilter_list(), adfvar))
                if (length(adfvarname) != 1) adfvarname <- adfvar
                sortchc <- c(sortchc, setNames(adfvar, adfvarname))
            }
            if (can_sort_on(input$adFilterB_list)) {
                adfvar <- input$adFilterB_list
                adfvarname <- names(intersect(adFilter_list(), adfvar))
                if (length(adfvarname) != 1) adfvarname <- adfvar
                sortchc <- c(sortchc, setNames(adfvar, adfvarname))
            }
            isolate(sel <- intersect(sortchc, input$playlist_sort))
            updateSelectInput(session, "playlist_sort", choices = sortchc, selected = sel)
        })

        playstable_to_delete <- NULL ## vector of logical
        playstable_ticked <- NULL ## vector of logical
        playstable_display_order <- NULL ## vector of integers
        ## these three variables are vectors with length equal to number of rows in playstable_data_raw(), and indexed according to playstable_data_raw() row ordering. The actual playstable_data() is playstable_data_raw() but ordered according to playstable_display_order and with user-deleted rows removed

        ## the raw playstable data (i.e. before reordering or deleting any rows)
        playstable_data_raw <- debounce(reactive({
            ## Customize pbp
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(selected_match_id()) || is.null(meta())) {
                playstable_to_delete <<- NULL
                playstable_ticked <<- NULL
                playstable_display_order <<- NULL
                NULL
            } else {
                if (trace_execution) cat("recalculating playstable_data\n")
                pbp <- pbp_augment()
                meta <- meta()
                filter_var <- input$adFilter_list
                filterB_var <- input$adFilterB_list
                ## skill, player, team inputs can be NULL (treated as "no filter applied")
                was_playlist <- !is.null(input$playlist_list) && !is.null(selected_match_id()) ##&& !is.null(input$skill_list) && !is.null(input$player_list) && !is.null(input$team_list)
                was_highlight <- !is.null(input$highlight_list) && !input$highlight_list %eq% "None" && !is.null(selected_match_id())
                if (was_playlist || was_highlight) {
                    myfuns <- if (was_playlist) funs_from_playlist(input$playlist_list) else funs_from_highlight(input$highlight_list)
                    if (length(selected_match_id()) == 1) {
                        ## apply each of myfuns in turn and rbind the results
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) myfun(x = dplyr::filter(pbp, .data$match_id %in% selected_match_id()), team = input$team_list, player = input$player_list)))
                    } else{
                        pbp_tmp <- dplyr::filter(pbp, .data$match_id %in% selected_match_id())
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) bind_rows(lapply(split(pbp_tmp, pbp_tmp$match_id), myfun, team = input$team_list, player = input$player_list))))
                    }
                } else {
                    pbp_tmp <- dplyr::filter(pbp, all_or_filter(.data$player_name, input$player_list), all_or_filter(.data$skill, input$skill_list), all_or_filter(.data$team, input$team_list), .data$match_id %in% selected_match_id())
                    if (!is.null(input$skilltype_list)) pbp_tmp <- dplyr::filter(pbp_tmp, .data$skilltype %in% input$skilltype_list)
                    if (!is.null(input$phase_list)) pbp_tmp <- dplyr::filter(pbp_tmp, .data$phase %in% input$phase_list)
                }
                ## advanced filters apply to all
                if (!is.null(filter_var) && nzchar(filter_var)) pbp_tmp <- dplyr::filter(pbp_tmp, .data[[filter_var]] %in% input$adFilterValue_list)
                if (!is.null(filterB_var) && nzchar(filterB_var)) pbp_tmp <- dplyr::filter(pbp_tmp, .data[[filterB_var]] %in% input$adFilterBValue_list)
                playstable_to_delete <<- rep(FALSE, nrow(pbp_tmp))
                playstable_ticked <<- rep(FALSE, nrow(pbp_tmp))
                playstable_display_order <<- seq_nrows(pbp_tmp)
                pbp_tmp$ROWID <- seq_nrows(pbp_tmp) ## keep track of original row numbers for deletion
                master_playstable_selected_row <<- 1 ## fresh table/playlist, start from row 1
                is_fresh_playlist <<- TRUE
                ## set playstable_display_order according to chosen vars but leave pbp_tmp with its default ordering and leave this block unreactive to input$playlist_sort
                playstable_display_order <<- calc_playstable_order(pbp_tmp)
                pbp_tmp
            }
        }), 250)

        ## helper function to generate the ordering of the playstable, using input$playlist_sort if it has been set
        calc_playstable_order <- function(pl) {
            if (missing(pl)) pl <- isolate(playstable_data_raw())
            if (is.null(pl)) return(NULL)
            pls <- isolate(input$playlist_sort)
            if (length(pls) < 1) return(seq_nrows(pl)) ## default ordering
            ## otherwise according to input$playlist_sort
            temp <- intersect(pls, names(pl))
            pl %>% ungroup %>% mutate(rownum = seq_len(n())) %>% dplyr::arrange(across(all_of(temp))) %>% dplyr::pull(.data$rownum)
        }

        ## adjust playstable_display_order if input$playlist_sort changes
        observe({
            blah <- list(input$playlist_sort) ## react to this
            temp <- calc_playstable_order()
            if (!identical(temp, playstable_display_order)) {
                playstable_display_order <<- temp
                pl_check_trigger(pl_check_trigger() + 1L)
            }
        })

        ## the actual playstable_data is playstable_data_raw but ordered and with user-deleted rows removed
        pl_check_trigger <- reactiveVal(0)
        playstable_data_bouncy <- reactiveVal(NULL)
        playstable_data <- debounce(playstable_data_bouncy, 500)
        last_pt_hash <- ""
        observe({
            if (trace_execution) cat("checking playstable_data\n")
            if (!is.null(playstable_data_raw())) {
                blah <- pl_check_trigger() ## react to this
                ptdel <- playstable_to_delete
                ord <- if (length(playstable_display_order) == nrow(playstable_data_raw())) playstable_display_order else seq_nrows(playstable_data_raw())
                ord <- ord[!ord %in% which(ptdel)]
                out <- playstable_data_raw()[ord, ]
                pt_hash <- digest::digest(out)
                if (last_pt_hash != pt_hash) {
                    if (trace_execution) cat("  updating playstable_data\n")
                    last_pt_hash <<- pt_hash
                    playstable_data_bouncy(out)
                } else {
                    if (trace_execution) cat("  playstable_data has not changed\n")
                }
            }
        })

        observeEvent(input$randomize_playlist, {
            if (length(playstable_display_order) > 0) {
                playstable_display_order <<- sample.int(length(playstable_display_order), size = length(playstable_display_order), replace = FALSE)
                is_fresh_playlist <<- TRUE ## this forces the playlist to re-start playing from the first item
                pl_check_trigger(pl_check_trigger() + 1L)
            }
        })
        observeEvent(input$delete_ticked, {
            if (isTRUE(any(playstable_ticked[!playstable_to_delete]))) {
                plchk <- playstable_to_delete
                plchk[which(playstable_ticked)] <- TRUE
                playstable_to_delete <<- plchk
                is_fresh_playlist <<- FALSE
                pl_check_trigger(pl_check_trigger() + 1L)
            }
        })

        observeEvent(input$keep_ticked, {
            if (isTRUE(any(playstable_ticked[!playstable_to_delete]))) {
                plchk <- playstable_to_delete
                ## anything already deleted remains deleted
                ## anything not ticked gets added to the delete list
                plchk[which(playstable_to_delete | !playstable_ticked)] <- TRUE
                playstable_to_delete <<- plchk
                is_fresh_playlist <<- FALSE
                pl_check_trigger(pl_check_trigger() + 1L)
            }
        })

        observeEvent(input$reset_ticked, {
            ## replace any deleted items and revert to the original ordering
            playstable_ticked <<- rep(FALSE, length(playstable_ticked))
            playstable_to_delete <<- rep(FALSE, length(playstable_to_delete))
            playstable_display_order <<- calc_playstable_order()
            is_fresh_playlist <<- FALSE
            pl_check_trigger(pl_check_trigger() + 1L)
        })

        observeEvent(input$toggle_plitem, {
            ## toggle selection of the playlist item
            if (!is.null(input$toggle_plitem) && nzchar(input$toggle_plitem) && grepl("@", input$toggle_plitem) && !is.null(playstable_ticked)) {
                plchk <- playstable_ticked
                thisid <- strsplit(input$toggle_plitem, "@")[[1]][1]
                totoggle <- as.numeric(sub("^pl_", "", thisid))
                ## totoggle will be the row number in the original data, i.e. playstable_data()$ROWID
                ##cat("toggling:\n "); print(totoggle)
                plchk[totoggle] <- !plchk[totoggle]
                playstable_ticked <<- plchk
                ##cat("ticked:\n  "); print(which(playstable_ticked))
            }
        })

        output$playstable <- DT::renderDataTable({
            mydat <- playstable_data()
            scrolly <- if (is.numeric(vo_height())) max(200, vo_height() - 80) else 200 ## 80px for table header row
            if (!is.null(mydat)) {
                ## we are potentially showing a subset of rows of playstable_data_raw() according to playstable_to_delete
                tbc <- ifelse(playstable_ticked[mydat$ROWID], "checked", "") ## or subset by playstable_to_delete
                mydat$tickboxes <- as.list(paste0('<input type="checkbox" id="pl_', mydat$ROWID, '" onmousedown="event.stopPropagation();" onclick="toggle_pl_item(this);" ', tbc, '/>'))
                show_mp4_col <- isTRUE(app_data$mp4_clip_convert) || tryCatch(isTRUE(exists("playstable_add_mp4_col") && is.function(playstable_add_mp4_col) && isTRUE(playstable_add_mp4_col())), error = function(e) FALSE)
                if (debug_mp4) cat("show_mp4_col is: ", capture.output(str(show_mp4_col)), "\n")
                if (show_mp4_col) {
                    ## don't show mp4 icon on youtube/twitch sources
                    ## video type is not in the playlist yet, this happens when the playlist is built, so do a workaround
                    isolate(vsrc <- if (!is.null(video_meta()) && nrow(video_meta()) > 0) tryCatch(dplyr::pull(left_join(dplyr::select(mydat, "match_id"), distinct(dplyr::select(video_meta(), "match_id", "video_src")), by = "match_id"), .data$video_src), error = function(e) NA_character_))
                    if (debug_mp4) { cat("vsrc (0):\n"); print(table(vsrc, useNA = "always")) }
                    if (length(vsrc) != nrow(mydat)) vsrc <- rep(NA_character_, nrow(mydat))
                    if (debug_mp4) { cat("vsrc (1):\n"); print(table(vsrc, useNA = "always")) }
                    vsrc <- case_when(is_youtube_id(vsrc) | grepl("https?://.*youtube", vsrc, ignore.case = TRUE) | grepl("https?://youtu\\.be", vsrc, ignore.case = TRUE) ~ "youtube",
                                      is_twitch_video(vsrc) ~ "twitch",
                                      is.na(vsrc) ~ "unknown",
                                      TRUE ~ "local")
                    if (debug_mp4) { cat("vsrc (2):\n"); print(table(vsrc, useNA = "always")) }
                    mydat$mp4 <- as.list(ifelse(vsrc %in% c("local"), paste0('<i class="fa fa-file" id="plmp4_', mydat$ROWID, '" onclick="mp4_pl_item(this);" />'), ""))
                }
                mydat <- mydat[, c("tickboxes", if (show_mp4_col) "mp4", plays_cols_to_show), drop = FALSE]
                cnames <- var2fc(names(mydat))
                cnames[1] <- "" ## no name on the delete column
                if (show_mp4_col) cnames[2] <- "" ## ditto mp4 if present
                js_show("dk_buts")
                if (trace_execution) cat("redrawing playstable, master_selected is: ", master_playstable_selected_row, "\n")
                ## when the table is redrawn but the selected row is not in the first few rows, need to scroll the table - use initComplete callback
                DT::datatable(mydat, rownames = FALSE, colnames = cnames, escape = FALSE,
                              extensions = "Scroller", selection = list(mode = "single", selected = max(master_playstable_selected_row, 1L), target = "row"),
                              options = list(sDom = '<"top">t<"bottom">rlp', deferRender = TRUE, scrollX = "100%", scrollY = scrolly, scroller = TRUE, ordering = FALSE,
                                             initComplete = DT::JS('function(setting, json) { Shiny.setInputValue("scroll_trigger", new Date().getTime()); }')))
            } else {
                js_hide("dk_buts")
                NULL
            }
        })

        playstable_proxy <- DT::dataTableProxy("playstable", deferUntilFlush = TRUE)
        playstable_select_row <- function(rw) {
            if (!is.null(rw) && !is.na(rw) && (rw != master_playstable_selected_row)) {
                master_playstable_selected_row <<- rw
                DT::selectRows(playstable_proxy, rw)
                scroll_playstable(rw)
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
        ## when the user chooses a row in the playstable, it will be selected by that click action, so we just need to play it
        ## use input$playstable_cell_clicked rather than input$playstable_rows_selected to detect user input, because the latter is also triggered by the player incrementing rows
        observeEvent(input$playstable_cell_clicked, { ## note, can't click the same row twice in a row ...
            clicked_row <- input$playstable_cell_clicked$row ## 1-based
            if (!is.null(clicked_row) && !is.na(clicked_row)) {
                if (isTRUE(input$playstable_cell_clicked$col > 0)) {
                    master_playstable_selected_row <<- clicked_row
                    evaljs(paste0("dvpl.video_controller.current=", clicked_row-1, "; dvpl.video_play();"))
                } else {
                    ## deleted a row
                    ## if it was on or before the current selected row, then subtract one off the master_playstable_selected_row to keep it in sync
                    if (clicked_row <= master_playstable_selected_row) {
                        master_playstable_selected_row <<- max(master_playstable_selected_row - 1, 1) ## R 1-based indexing
                    }
                    evaljs(paste0("dvpl.video_controller.current=", master_playstable_selected_row-1, ";")) ## -1 for js 0-based indexing
                }
            }
        })

        video_meta <- reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(meta()) || is.null(selected_match_id())) {
                NULL
            } else {
                if (trace_execution) cat("recalculating video_meta\n")
                meta_video <- bind_rows(lapply(meta(), function(z) if (!is.null(z$video)) mutate(z$video, camera = as.character(.data$camera), file = as.character(.data$file), match_id = z$match_id, dvw_filename = z$filename)))
                meta_video <- dplyr::filter(meta_video, .data$match_id %in% selected_match_id())
                if (nrow(meta_video) < 1) return(NULL)
                if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("lighttpd", "servr")) {
                    ## we are serving the video through the lighttpd/servr server, so need to make symlinks in its document root directory pointing to the actual video files
                    is_url <- is_youtube_id(meta_video$file) | grepl("^https?://", meta_video$file, ignore.case = TRUE)
                    vf <- NULL
                    if (any(!is_url)) vf <- tryCatch(fs::path_real(meta_video$file[!is_url]), error = function(e) NULL)
                    if (length(vf) < 1 && !any(is_url)) return(NULL)
                    ## may have multiple video files at this point
                    for (thisf in unique(vf)) {
                        if (fs::file_exists(as.character(thisf))) {
                            ## link_create doesn't allow files to be symlinked on windows, see https://github.com/r-lib/fs/issues/79
                            ## we can only symlink directories
                            ## so the symlink created in the servr root is a link to the directory containing the video file
                            symlink_abspath <- fs::path_abs(file.path(app_data$video_server_dir, digest::digest(fs::path_dir(thisf), algo = "sha1")))
                            do_create_this_symlink <- !(fs::link_exists(symlink_abspath) && fs::link_path(symlink_abspath) == fs::path_dir(thisf))
                            if (isTRUE(do_create_this_symlink)) {
                                suppressWarnings(try(unlink(symlink_abspath), silent = TRUE))
                                fs::link_create(fs::path_dir(thisf), symlink_abspath)
                                onStop(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                                onSessionEnded(function() try({ unlink(symlink_abspath) }, silent = TRUE))
                            }
                        } else if (is_youtube_id(thisf) || grepl("https?://", thisf, ignore.case = TRUE)) {
                            ## youtube ID or link to video, don't do anything
                            ## should never get here, but just in case
                        } else {
                            ## video file does not exist!
                            stop("video file ", thisf, " does not exist, not handled yet")
                        }
                    }
                    ## windows causes us headaches here, because we can only symlink directories (not files), see above
                    ## so we have to symlink the directory containing each video file
                    ## that symlink will be given the hashed name of the video directory
                    path_hashes <- rep("", length(meta_video$file))
                    path_hashes[!is_url] <- vapply(fs::path_dir(fs::path_real(meta_video$file[!is_url])), digest::digest, algo = "sha1", FUN.VALUE = "")

                    ## so the video_src is the symlink (i.e. hashed name, the directory) then the file name itself
                    meta_video$video_src <- paste_url(app_data$video_server_url, path_hashes, basename(meta_video$file))
                    ## replace URLs with verbatim copy of original info
                    meta_video$video_src[is_url] <- meta_video$file[is_url]
                } else if (is.function(app_data$video_serve_method)) {
                    if (nrow(meta_video) > 0) {
                        ## block user interaction while this happens
                        showModal(modalDialog(title = "Please wait", size = "l", footer = NULL))
                        shiny::withProgress(message = "Processing video files", {
                            meta_video$video_src <- vapply(seq_nrows(meta_video), function(z) {
                                shiny::setProgress(value = z/nrow(meta_video))
                                tryCatch(app_data$video_serve_method(video_filename = meta_video$file[z], dvw_filename = meta_video$dvw_filename[z]), error = function(e) NA_character_)
                            }, FUN.VALUE = "", USE.NAMES = FALSE)
                        })
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

        build_playlist <- function(dat, meta_video) {
            if ("home_score_start_of_point" %in% names(dat)) {
                hsc <- "home_score_start_of_point"
                vsc <- "visiting_score_start_of_point"
            } else {
                hsc <- "home_team_score"
                vsc <- "visiting_team_score"
            }
            event_list <- mutate(dat, skill = case_when(.data$skill %in% c("Freeball dig", "Freeball over") ~ "Freeball", TRUE ~ .data$skill), ## ov_video needs just "Freeball"
                                 skilltype = case_when(.data$skill %in% c("Serve", "Reception", "Dig", "Freeball", "Block", "Set") ~ .data$skill_type,
                                                       .data$skill == "Attack" ~ ifelse(is.na(.data$attack_description), .data$skill_type, .data$attack_description)),
                                 subtitle = js_str_nospecials(paste("Set", .data$set_number, "-", .data$home_team, .data[[hsc]], "-", .data[[vsc]], .data$visiting_team)),
                                 subtitleskill = js_str_nospecials(paste(.data$player_name, "-", .data$skilltype, ":", .data$evaluation_code)))
            event_list <- dplyr::filter(event_list, !is.na(.data$video_time)) ## can't have missing video time entries
            ## note that the event_list can contain match_ids that do not appear in meta_video, if meta gets updated and the corresponding pbp_augment update is pending
            if (!all(na.omit(event_list$match_id) %in% meta_video$match_id)) return(NULL) ## return NULL and await retrigger
            ## TODO: if we filter items out here because of missing video times (but not filter from the playstable), doesn't the playstable selected row get out of whack with the actual item being played?
            vpt <- if (all(is_youtube_id(meta_video$video_src) | grepl("https?://.*youtube", meta_video$video_src, ignore.case = TRUE) | grepl("https?://youtu\\.be", meta_video$video_src, ignore.case = TRUE))) {
                       "youtube"
                   } else if (all(is_twitch_video(meta_video$video_src))) {
                       "twitch"
                   } else {
                       "local"
                   }
            ## TODO also check for mixed sources, which we can't handle yet
            video_player_type(vpt)
            if (!is.null(input$highlight_list)) {
                pl <- ovideo::ov_video_playlist(x = event_list, meta = meta_video, type = vpt, timing = clip_timing(), extra_cols = c("match_id", "subtitle", "subtitleskill", plays_cols_to_show))
            } else {
                pl <- ovideo::ov_video_playlist(x = event_list, meta = meta_video, type = vpt, timing = clip_timing(), extra_cols = c("match_id", "subtitle", "subtitleskill", plays_cols_to_show))
            }
            pl <- pl[!is.na(pl$start_time) & !is.na(pl$duration), ]
            ## also keep track of actual file paths
            left_join(pl, meta_video[, c("file", "video_src")], by = "video_src")
        }

        playlist <- reactiveVal(NULL)
        last_playlist_hash <- ""
        observe({
            if (trace_execution) cat("recalculating playlist\n")
            ## Customize pbp
            meta_video <- video_meta()
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(meta()) || is.null(selected_match_id()) || is.null(meta_video) || nrow(meta_video) < 1 || is.null(playstable_data()) || nrow(playstable_data()) < 1) {
                ## TODO does that need to be reactive to all of those things? Or just playstable_data() and video_meta() ?
                playlist(NULL)
            } else {
                pl <- build_playlist(playstable_data(), meta_video = meta_video)
                pl_hash <- digest::digest(pl)
                if (last_playlist_hash != pl_hash) {
                    if (trace_execution) cat("  updating playlist\n")
                    last_playlist_hash <<- pl_hash
                    playlist(pl)
                } else {
                    if (trace_execution) cat("  playlist has not changed\n")
                }
            }
        })

        clip_timing <- reactive({
            ## parse timing from inputs, with fallback to defaults if it fails
            def0 <- def <- if (check_timing_df(app_data$video_timing_df)) app_data$video_timing_df else ovideo::ov_video_timing_df()
            tryCatch({
                ## defaults
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
                for (ri in seq_nrows(def)) {
                    skill <- def$skill[ri]
                    phase <- def$phase[ri]
                    def$start_offset[ri] <- input[[paste0("timing_", tolower(skill), "_", tolower(phase), "_start_offset")]]
                    def$duration[ri] <- input[[paste0("timing_", tolower(skill), "_", tolower(phase), "_duration")]]
                }
                def
            }, error = function(e) def0)
        })
        tweak_all_timings <- function(whch, by) {
            if (whch %in% c("start_offset", "duration") && is.numeric(by)) {
                def <- data.frame(skill = c("serve", "reception", "set", "set", "attack", "attack", "block", "block", "dig", "freeball", "freeball"),
                                  phase = c("serve", "reception", "reception", "transition", "reception", "transition", "reception", "transition", "transition", "reception", "transition"),
                                  stringsAsFactors = FALSE)
                for (ri in seq_nrows(def)) {
                    thisid <- paste0("timing_", def$skill[ri], "_", def$phase[ri], "_", whch)
                    updateNumericInput(session, inputId = thisid, value = input[[thisid]] + by)
                }
            }
        }
        observeEvent(input$timing_all_start_minus, tweak_all_timings("start_offset", -1))
        observeEvent(input$timing_all_start_plus, tweak_all_timings("start_offset", 1))
        observeEvent(input$timing_all_duration_minus, tweak_all_timings("duration", -1))
        observeEvent(input$timing_all_duration_plus, tweak_all_timings("duration", 1))

        ## video stuff
        video_player_type <- reactiveVal("local") ## the current player type, either "local" or "youtube" or "twitch"
        observe({
            if (!is.null(playlist()) && nrow(playlist()) > 0) {
                js_show("playstable");
                if (trace_execution) cat("reinitializing video player\n")
                ## when playlist() changes, push it through to the javascript playlist
                isolate({
                    was_paused <- isTRUE(input$player_pause_state)
                    suspended_state <- input$player_suspend_state
                })
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
                    if (!isTRUE(input$player_suspend_state > 0)) { ##suspended_state < 1) {
                        ## not suspended
                        ## if we were paused, don't restart but set the player state to paused since it got reset when the new playlist was loaded
                        if (!was_paused) evaljs("dvpl.video_play();") else evaljs("dvpl.video_controller.paused=true;")
                    } else {
                        ## the player was suspended, so set this one to the same state but otherwise do nothing, the unsuspend handler will take care of it
                        evaljs(paste0("dvpl.video_controller.suspended=", suspended_state, ";"))
                    }
                }
            } else {
                js_hide("playstable");
                ## empty playlist, so stop the video, and clean things up
                evaljs("dvpl.clear_playlist();")
                ## evaljs("remove_vspinner();") ## doesn't have an effect?
                evaljs("if (document.getElementById(\"subtitle\")) { document.getElementById(\"subtitle\").textContent=\"Score\"; }; if (document.getElementById(\"subtitleskill\")) { document.getElementById(\"subtitleskill\").textContent=\"Skill\"; }")
            }
        })
        output$player_controls_ui <- renderUI({
            tags$div(tags$div(class = "player_controls", tags$button(tags$span(icon("play-circle", style = "vertical-align:middle;")), onclick = "dvpl.video_play();", title = "Play"),
                              tags$button(tags$span(icon("step-backward", style = "vertical-align:middle;")), onclick = "dvpl.video_prev();", title = "Previous"),
                              tags$button(tags$span(icon("step-forward", style = "vertical-align:middle;")), onclick = "dvpl.video_next(false);", title = "Next"),
                              tags$button(tags$span(icon("pause-circle", style = "vertical-align:middle;")), onclick = "dvpl.video_pause();", title = "Pause"),
                              tags$button(tags$span(icon("backward", style = "vertical-align:middle;"), " 1s"), onclick = "dvpl.jog(-1);", title = "Back 1 second"),
                              tags$button(tags$span(icon("expand", style = "vertical-align:middle;")), onclick = "dvpl.fullscreen();", title = "Full screen"),
                              tags$button(tags$span(icon("volume-mute", style = "vertical-align:middle;")), onclick = "dvpl.toggle_mute()", title = "Toggle mute")
                              ),
                     tags$div(style="margin-top:10px;", tags$span(id = "subtitle", "Score"), tags$span(id = "subtitleskill", "Skill")))
        })

        clip_filename <- reactiveVal("")
        clip_status <- reactiveVal(NULL)
        output$create_clip_button_ui <- renderUI({
            ok <- !is.null(playlist()) && nrow(playlist()) > 0 && !video_player_type() %in% c("youtube", "twitch")
            ## also check that videos are not remote - exclude videos served by http[s], but not if they are being served by the local (ovva-initiated) server
            temp_src <- playlist()$video_src
            if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("lighttpd", "servr")) {
                is_local_url <- substr(temp_src, 1, nchar(app_data$video_server_url)) == app_data$video_server_url
                temp_src[which(is_local_url)] <- "" ## these are ok
            }
            ok <- ok && !any(grepl("^https?://", temp_src, ignore.case = TRUE))
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
                ovideo::ov_ffmpeg_ok(do_error = TRUE)
                future::plan("multisession")
                pll <- lapply(seq_nrows(playlist()), function(z) as.list(playlist()[z, ])) ## need a non-reactive list-formatted copy of this to use with future_lapply
                tempfiles <- future.apply::future_lapply(pll, function(plitem) {
                ##tempfiles <- lapply(pll), function(plitem) { ## for testing, no parallel
                    outfile <- tempfile(fileext = paste0(".", fs::path_ext(plitem$file)))
                    if (file.exists(outfile)) unlink(outfile)
                    infile <- tryCatch(fs::path_real(plitem$file), error = function(e) {
                        stop("file '", plitem$file, "' could not be resolved to a real file")
                    })
                    res <- sys::exec_internal(unname(ovideo::ov_ffmpeg_exe()), c("-ss", plitem$start_time, "-i", infile, "-strict", "-2", "-t", plitem$duration, outfile))
                    if (res$status != 0) stop("failed to get video clip, ", rawToChar(res$stderr))
                    outfile
                })
                tempfiles <- unlist(tempfiles)
                cfile <- tempfile(fileext = ".txt")
                on.exit(unlink(c(cfile, tempfiles)))
                cat(paste0("file ", tempfiles), file = cfile, sep = "\n")
                if (file.exists(filename)) unlink(filename)
                res <- sys::exec_internal(unname(ovideo::ov_ffmpeg_exe()), c("-safe", 0, "-f", "concat", "-i", cfile, "-c", "copy", filename))
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
                filename <- paste0("ovva_highlights.mp4")
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

        pl_can_be_saved <- reactive({
            !is.null(playlist()) && (nrow(playlist()) > 0)
        })
        output$chart_ui <- renderUI({
            out <- list(tags$div(style = "height:24px;"),
                        if (pl_can_be_saved()) downloadButton("download_playlist", label = "Download playlist CSV"),
                        uiOutput("create_clip_button_ui", inline = TRUE),
                        uiOutput("chart2_ui"))
            do.call(tagList, Filter(Negate(is.null), out))
        })
        output$chart2_ui <- renderUI(app_data$chart_renderer)

        ## download playlist
        output$download_playlist <- downloadHandler(filename = "playlist.csv",
                                                    content = function(file) write.csv(playlist(), file, row.names = FALSE, fileEncoding = "UTF-8"))

        ## height of the video player element
        vo_height <- reactiveVal("auto")
        observe({
            my_height <- if (video_player_type() %in% c("youtube", "twitch")) input$dvyt_height else input$dv_height
            if (!is.null(my_height) && as.numeric(my_height) > 0) {
                vo_height(as.numeric(my_height))
                ## +1 because of 1px border on video element
                evaljs(paste0("document.getElementById('video_overlay').style.height = '", vo_height()+1, "px';"))
                evaljs(paste0("document.getElementById('video_overlay_img').style.height = '", vo_height()+1, "px';"))
                evaljs(paste0("document.getElementById('dv_h_overlay').style.height = '", vo_height()+1, "px';"))
            } else {
                vo_height("auto")
                evaljs(paste0("document.getElementById('video_overlay').style.height = '400px';"))
                evaljs(paste0("document.getElementById('video_overlay_img').style.height = '400px';"))
                evaljs(paste0("document.getElementById('dv_h_overlay').style.height = '400px';"))
            }
        })
        ## width of the video player element
        vo_width <- reactiveVal("auto")
        observe({
            my_width <- if (video_player_type() %in% c("youtube", "twitch")) input$dvyt_width else input$dv_width
            if (!is.null(my_width) && as.numeric(my_width) > 0) {
                vo_width(as.numeric(my_width))
                evaljs(paste0("document.getElementById('video_overlay_img').style.width = '", vo_width()+1, "px';"))
                evaljs(paste0("document.getElementById('dv_h_overlay').style.width = '", vo_width()+1, "px';"))
            } else {
                vo_width("auto")
                evaljs(paste0("document.getElementById('video_overlay_img').style.width = '600px';"))
                evaljs(paste0("document.getElementById('dv_h_overlay').style.width = '600px';"))
            }
        })
        ## height of the video player container, use as negative vertical offset on the overlay element
        observe({
            if (!is.null(input$vo_voffset) && as.numeric(input$vo_voffset) > 0) {
                evaljs(paste0("document.getElementById('video_overlay').style.marginTop = '-", input$vo_voffset, "px';"))
                evaljs(paste0("document.getElementById('video_overlay_img').style.marginTop = '-", input$vo_voffset, "px';"))
                evaljs(paste0("document.getElementById('dv_h_overlay').style.marginTop = '-", input$vo_voffset, "px';"))
            } else {
                evaljs("document.getElementById('video_overlay').style.marginTop = '0px';")
                evaljs("document.getElementById('video_overlay_img').style.marginTop = '0px';")
                evaljs("document.getElementById('dv_h_overlay').style.marginTop = '0px';")
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

        observeEvent(input$video_error, {
            vid_err_msgs <- c("video playback was aborted", "a network error caused the video download to fail", "an error occurred while trying to decode the video", "the video could not be loaded or the format is not supported")
            temp <- if (!is.null(input$video_error) && nzchar(input$video_error) && grepl("@", input$video_error)) {
                        tryCatch(strsplit(input$video_error, "@")[[1]], error = function(e) NULL)
                    } else {
                        NULL
                    }
            if (length(temp) > 2) {
                errmsg <- if (as.numeric(temp[3]) %in% 1:4) vid_err_msgs[as.numeric(temp[3])] else "unknown error"
                this_src <- tryCatch(rawToChar(base64enc::base64decode(temp[2])), error = function(e) "unknown")
                if (length(this_src) < 1 || !is.character(this_src) || !nzchar(this_src)) this_src <- "unknown"
            } else {
                errmsg <- "unknown error"
                this_src <- "unknown"
            }
            ##status_msg <- NULL
            if (length(this_src == 1) && is.character(this_src) && grepl("^https?://", this_src, ignore.case = TRUE)) {
                ## ## can we get the http status?
                ## status_msg <- tryCatch(httr::http_status(httr::HEAD(this_src))$message, error = function(e) if (grepl("Connection refused", conditionMessage(e), ignore.case = TRUE)) "Connection refused" else NULL)
                this_src <- tags$a(href = this_src, this_src, target = "_blank")
            }
            output$video_dialog <- renderUI(tags$div(class = "alert alert-danger", tags$div("Video error ", paste0("(", errmsg, "). Is the video URL correct?"), tags$br(), "Video source: ", this_src)))
        })
    }
}
