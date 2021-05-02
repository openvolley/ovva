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
            Setter = "setter", "Opposition setter" = "opposition_setter", "Opposition team" = "opposition_team")

        ## some inits
        master_playstable_selected_row <- -99L ## non-reactive
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
        ## play-by-play data for selected season
        meta_unfiltered <- reactiveVal(NULL)
        pbp <- reactiveVal(NULL)
        pbp_augment <- reactiveVal(NULL)
        got_no_video <- reactiveVal(FALSE)
        season_data_type <- reactiveVal("indoor")
        video_list <- reactiveVal(dplyr::tibble(match_id = character(), video = character()))
        ## process metadata for selected season matches and update pbp reactiveVal accordingly
        meta <- reactive({
            if (!is.null(input$season) && input$season %in% season_choices()) {
                isolate({
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
                    meta_unfiltered(out)
                    ## out is a list of metadata objects
                    ## prune out any that don't have video
                    if (!is.null(out)) out <- Filter(function(z) !is.null(z$video) && nrow(z$video) > 0, out)
                    if (length(out) < 1) {
                        got_no_video(TRUE)
                        pbp(NULL)
                        pbp_augment(NULL)
                        out <- NULL
                        season_data_type("indoor") ## default
                    } else {
                        season_data_type(tryCatch(if (grepl("beach", out[[1]]$match$regulation)) "beach" else "indoor", error = function(e) "indoor"))
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
                        ## keep track of videos
                        video_list(bind_rows(lapply(out, function(z) list(match_id = z$match_id, filename = z$filename, video_source = if (nrow(z$video) == 1 && !is.na(z$video$file) && nzchar(z$video$file)) z$video$file else NA_character_))))
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
                    removeModal()
                    have_done_startup(TRUE)
                })
                out
            } else {
                isolate({
                    got_no_video(FALSE)
                    meta_unfiltered(NULL)
                    pbp(NULL)
                    pbp_augment(NULL)
                    season_data_type("indoor") ## default
                })
                NULL
            }
        })

        ## Game ID
        selected_match_id <- reactive({
            if (trace_execution) message("recalculating game_table")
            if (is.null(input$game_table_dropdown)) {
                NULL
            } else {
                datatble <- distinct(pbp_augment(), .data$match_id, .data$game_date, .data$visiting_team, .data$home_team, .keep_all = FALSE)
                datatble <- mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team))
                datatble$match_id[datatble$display_ID %in% input$game_table_dropdown]
            }
        })

        ## Team
        team_list = reactive({
            if (trace_execution) message("recalculating team_list")
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id())
                sort(unique(na.omit(tmp$team)))
            }
        })
        observe({
            isolate(sel <- intersect(team_list(), input$team_list))
            if (length(sel) < 1) sel <- team_list() ## select all
            updatePickerInput(session, "team_list", choices = team_list(), selected = sel)
        })

        ## Player ID
        player_list = reactive({
            if (trace_execution) message("recalculating player_list")
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), .data$team %in% input$team_list)
                sort(unique(na.omit(tmp$player_name)))
            }
        })
        observe({
            isolate(sel <- intersect(player_list(), input$player_list))
            if (length(sel) < 1) sel <- player_list() ## select all
            updatePickerInput(session, "player_list", choices = player_list(), selected = sel)
        })

        ## Skill
        skill_list = reactive({
            if (trace_execution) message("recalculating skill_list")
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list)
                sort(unique(na.omit(tmp$skill)))
            }
        })
        observe({
            isolate(sel <- intersect(skill_list(), input$skill_list))
            if (length(sel) < 1) sel <- skill_list() ## select all
            updatePickerInput(session, "skill_list", choices = skill_list(), selected = sel)
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
                            multiple = TRUE)
            }
        })

        ## Skilltype
        skilltype_list = reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), .data$player_name %in% input$player_list, .data$skill %in% input$skill_list, .data$team %in% input$team_list)
                sort(unique(tmp$skilltype))
            }
        })
        observe({
            ## if the skilltype list changes, then we need to select all, otherwise we may have changes from e.g. just attacks to all skills, but we'll be restricted to just the previously-selected attack skill types
            updatePickerInput(session, "skilltype_list", choices = skilltype_list(), selected = skilltype_list())
        })

        ## Phase
        phase_list = reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
                sort(unique(tmp$phase))
            }
        })
        observe({
            isolate(sel <- intersect(phase_list(), input$phase_list))
            if (length(sel) < 1) sel <- phase_list() ## select all
            updatePickerInput(session, "phase_list", choices = phase_list(), selected = sel)
        })

        ## Advanced filter
        adFilter_list = reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1) {
                character()
            } else {
                temp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
                avail <- colnames(temp)
                avail <- avail[vapply(avail, function(z) !all(is.na(temp[[z]])), FUN.VALUE = TRUE)] ## exclude all-NA cols
                avail <- adfilter_cols_to_show[adfilter_cols_to_show %in% avail] ## only those in our pre-defined list of adfilter_cols_to_show
                ## also refine by data_type
                if (grepl("beach", season_data_type())) {
                    avail <- avail[!avail %in% c("set_code", "set_description", "home_setter_position", "visiting_setter_position", "opposition_setter_position", "receiving_setter_position", "serving_setter_position", "setter_position", "setter", "opposition_setter")]
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
        adFilterValue_list = reactive({
            col_to_select <- input$adFilter_list
            if (is.null(col_to_select) || !nzchar(col_to_select)) return(list())
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || length(col_to_select) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
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
        adFilterBValue_list = reactive({
            col_to_select <- input$adFilterB_list
            if (is.null(col_to_select) || !nzchar(col_to_select)) return(list())
            col_to_select <- col_to_select[nzchar(col_to_select)]
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || length(col_to_select) < 1) {
                character()
            } else {
                tmp <- dplyr::filter(pbp_augment(), .data$match_id %in% selected_match_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
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
                datatble <- distinct(pbp_augment(), .data$match_id, .data$game_date, .data$visiting_team, .data$home_team, .keep_all = FALSE)
                datatble <- dplyr::arrange(dplyr::mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team)), .data$game_date)
                dplyr::pull(dplyr::select(datatble, .data$display_ID))
            }
        })

        observe({
            isolate(sel <- intersect(game_table_dropdown(), input$game_table_dropdown))
            updatePickerInput(session, "game_table_dropdown", choices = game_table_dropdown(), selected = sel)
        })

        ## Table of all actions as per selected_match_id() and player_id() and evaluation()
        playstable_to_delete <- NULL
        playstable_data_raw <- debounce(reactive({
            ## Customize pbp
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(selected_match_id()) || is.null(meta())) {
                playstable_to_delete <<- NULL
                NULL
            } else {
                if (trace_execution) message("recalculating playstable_data")
                pbp <- pbp_augment()
                meta <- meta()
                game_select <- selected_match_id()
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
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) myfun(x = dplyr::filter(pbp, .data$match_id %in% game_select), team = team_select, player = player_select)))
                    } else{
                        pbp_tmp <- dplyr::filter(pbp, .data$match_id %in% game_select)
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) bind_rows(lapply(split(pbp_tmp, pbp_tmp$match_id), myfun, team = team_select, player = player_select))))
                    }
                } else if (!is.null(highlight_select) & !is.null(game_select)) {
                    myfuns <- funs_from_highlight(highlight_select)
                    if (length(game_select) == 1) {
                        ## apply each of myfuns in turn and rbind the results
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) myfun(x = dplyr::filter(pbp, .data$match_id %in% game_select), team = team_select, player = player_select)))
                    } else{
                        pbp_tmp <- dplyr::filter(pbp, .data$match_id %in% game_select)
                        pbp_tmp <- bind_rows(lapply(myfuns, function(myfun) bind_rows(lapply(split(pbp_tmp, pbp_tmp$match_id), myfun, team = team_select, player = player_select))))
                    }
                } else {
                    pbp_tmp <- dplyr::filter(pbp, .data$player_name %in% input$player_list &.data$skill %in% input$skill_list & .data$team %in% input$team_list & .data$match_id %in% selected_match_id())
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

        selected_matches <- reactive({
            if (trace_execution) message("recalculating selected matches")
            if (is.null(input$game_table_dropdown) || is.null(pbp()) || nrow(pbp()) < 1) {
                NULL
            } else {
                datatble <- distinct(pbp(), .data$match_id, .data$game_date, .data$visiting_team, .data$home_team, .keep_all = FALSE)
                datatble <- mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team))
                unique(na.omit(datatble$match_id[datatble$display_ID %in% input$game_table_dropdown]))
            }
        })

        video_meta <- reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(meta()) || is.null(selected_match_id()) || is.null(selected_matches())) {
                NULL
            } else {
                if (trace_execution) message("recalculating video_meta")
                meta_video <- bind_rows(lapply(meta(), function(z) if (!is.null(z$video)) mutate(z$video, match_id = z$match_id, dvw_filename = z$filename)))
                meta_video <- dplyr::filter(meta_video, .data$match_id %in% selected_matches())
                if (nrow(meta_video) < 1) return(NULL)
                if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("lighttpd", "servr")) {
                    ## we are serving the video through the lighttpd/servr server, so need to make symlinks in its document root directory pointing to the actual video files
                    is_url <- is_youtube_id(meta_video$file) | grepl("^https?://", meta_video$file, ignore.case = TRUE)
                    vf <- NULL
                    if (any(!is_url)) vf <- tryCatch(fs::path_real(meta_video$file[!is_url]), error = function(e) NULL)
                    if (length(vf) < 1 && !any(is_url)) return(NULL)
                    ## may have multiple video files at this point
                    for (thisf in unique(vf)) {
                        if (fs::file_exists(thisf)) {
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
                    path_hashes <- vapply(fs::path_dir(meta_video$file), digest::digest, algo = "sha1", FUN.VALUE = "")
                    ## so the video_src is the symlink (i.e. hashed name, the directory) then the file name itself
                    meta_video$video_src <- paste_url(app_data$video_server_url, path_hashes, basename(meta_video$file))
                    ## replace URLs with verbatim copy of original info
                    meta_video$video_src[is_url] <- meta_video$file[is_url]
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
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(meta()) || is.null(selected_match_id()) || is.null(meta_video) || nrow(meta_video) < 1 || is.null(playstable_data()) || nrow(playstable_data()) < 1) {
                NULL
            } else {
                event_list <- mutate(playstable_data(), skill = case_when(.data$skill %in% c("Freeball dig", "Freeball over") ~ "Freeball", TRUE ~ .data$skill), ## ov_video needs just "Freeball"
                                     skilltype = case_when(.data$skill %in% c("Serve", "Reception", "Dig", "Freeball", "Block", "Set") ~ .data$skill_type,
                                                           .data$skill == "Attack" ~ ifelse(is.na(.data$attack_description), .data$skill_type, .data$attack_description)),
                                     subtitle = js_str_nospecials(paste("Set", .data$set_number, "-", .data$home_team, .data$home_team_score, "-", .data$visiting_team_score, .data$visiting_team)),
                                     subtitleskill = js_str_nospecials(paste(.data$player_name, "-", .data$skilltype, ":", .data$evaluation_code)))
                event_list <- dplyr::filter(event_list, !is.na(.data$video_time)) ## can't have missing video time entries
                ## note that the event_list can contain match_ids that do not appear in meta_video, if meta gets updated and the corresponding pbp_augment update is pending
                if (!all(na.omit(event_list$match_id) %in% meta_video$match_id)) return(NULL) ## return NULL and await retrigger
                ## TODO: if we filter items out here because of missing video times (but not filter from the playstable), doesn't the playstable selected row get out of whack with the actual item being played?
                vpt <- if (all(is_youtube_id(meta_video$video_src) | grepl("https?://.*youtube", meta_video$video_src, ignore.case = TRUE) | grepl("https?://youtu\\.be", meta_video$video_src, ignore.case = TRUE))) {
                           "youtube"
                       } else {
                           "local"
                       }
                ## TODO also check for mixed sources, which we can't handle yet
                video_player_type(vpt)
                if (!is.null(input$highlight_list)) {
                    pl <- ovideo::ov_video_playlist_pid(x = event_list, meta = meta_video, type = vpt, extra_cols = c("match_id", "subtitle", plays_cols_to_show))
                } else {
                    pl <- ovideo::ov_video_playlist(x = event_list, meta = meta_video, type = vpt, timing = clip_timing(), extra_cols = c("match_id", "subtitle", "subtitleskill", plays_cols_to_show))
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

        ## video stuff
        video_player_type <- reactiveVal("local") ## the current player type, either "local" or "youtube"
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
                evaljs("if (document.getElementById(\"subtitle\")) { document.getElementById(\"subtitle\").textContent=\"Score\"; }; if (document.getElementById(\"subtitleskill\")) { document.getElementById(\"subtitleskill\").textContent=\"Skill\"; }")
            }
        })
        output$player_controls_ui <- renderUI({
            tags$div(tags$div(tags$button("Play", onclick = "dvpl.video_play();"),
                              tags$button("Prev", onclick = "dvpl.video_prev();"),
                              tags$button("Next", onclick = "dvpl.video_next(false);"),
                              tags$button("Pause", onclick = "dvpl.video_pause();"),
                              tags$button("Back 1s", onclick = "dvpl.jog(-1);")),
                     tags$div(style="margin-top:10px;", tags$span(id = "subtitle", "Score"), tags$span(id = "subtitleskill", "Skill"),
                              uiOutput("create_clip_button_ui", inline = TRUE)))
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

        output$chart_ui <- renderUI(app_data$chart_renderer)

        ## height of the video player element
        vo_height <- reactiveVal("auto")
        observe({
            my_height <- if (video_player_type() %eq% "youtube") input$dvyt_height else input$dv_height
            if (!is.null(my_height) && as.numeric(my_height) > 0) {
                vo_height(as.numeric(my_height))
                ## +1 because of 1px border on video element
                evaljs(paste0("document.getElementById('video_overlay').style.height = '", vo_height()+1, "px';"))
                evaljs(paste0("document.getElementById('video_overlay_img').style.height = '", vo_height()+1, "px';"))
            } else {
                vo_height("auto")
                evaljs(paste0("document.getElementById('video_overlay').style.height = '400px';"))
                evaljs(paste0("document.getElementById('video_overlay_img').style.height = '400px';"))
            }
        })
        ## width of the video player element
        vo_width <- reactiveVal("auto")
        observe({
            my_width <- if (video_player_type() %eq% "youtube") input$dvyt_width else input$dv_width
            if (!is.null(my_width) && as.numeric(my_width) > 0) {
                vo_width(as.numeric(my_width))
                evaljs(paste0("document.getElementById('video_overlay_img').style.width = '", vo_width()+1, "px';"))
            } else {
                vo_width("auto")
                evaljs(paste0("document.getElementById('video_overlay_img').style.width = '600px';"))
            }
        })
        ## height of the video player container, use as negative vertical offset on the overlay element
        observe({
            if (!is.null(input$vo_voffset) && as.numeric(input$vo_voffset) > 0) {
                evaljs(paste0("document.getElementById('video_overlay').style.marginTop = '-", input$vo_voffset, "px';"))
                evaljs(paste0("document.getElementById('video_overlay_img').style.marginTop = '-", input$vo_voffset, "px';"))
            } else {
                evaljs("document.getElementById('video_overlay').style.marginTop = '0px';")
                evaljs("document.getElementById('video_overlay_img').style.marginTop = '0px';")
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

    }
}
