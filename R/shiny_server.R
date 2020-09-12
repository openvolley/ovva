ovva_shiny_server <- function(app_data) {
    function(input, output, session) {
        trace_execution <- FALSE ## for debugging
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
            "Receiving player" = "receiving_player", "Reception grade" = "reception_grade")
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
        season_choices <- reactive(names(get_data_paths()))
        observe({
            chc <- season_choices()
            isolate(sel <- input$season)
            if (is.null(sel) || !sel %in% chc) sel <- chc[1]
            updateSelectInput(session, "season", choices = chc, selected = sel)
        })
        ## play-by-play data for selected season
        pbp <- reactiveVal(NULL)
        pbp_augment <- reactiveVal(NULL)
        got_no_video <- reactiveVal(FALSE)
        ## process metadata for selected season matches and update pbp reactiveVal accordingly
        meta <- reactive({
            if (!is.null(input$season) && input$season %in% season_choices()) {
                if (trace_execution) cat("recalculating meta\n")
                showModal(modalDialog(title = "Processing data ...", footer = NULL, "Please wait"))
                if (file.exists(file.path(get_data_paths()[[input$season]], "allmeta.rds"))) {
                    ## use allmeta.rds if available
                    tmp <- readRDS(file.path(get_data_paths()[[input$season]], "allmeta.rds"))
                    out <- lapply(tmp, function(z) z$meta)
                } else {
                    myfiles <- dir(get_data_paths()[[input$season]], pattern = "\\.(dvw|psvb)$", ignore.case = TRUE, full.names = TRUE)
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
                            mydat <- bind_rows(lapply(myfiles, function(z) if (grepl("psvb$", z)) pv_read(z)$plays else read_dv(z, skill_evaluation_decode = "guess")$plays)) ## other args to read_dv?
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

        ## Game ID
        selected_game_id <- reactive({
            if (is.null(input$game_table_dropdown)) {
                NULL
            } else {
                datatble <- dplyr::select(distinct(pbp_augment(), .data$game_id, .data$game_date, .data$visiting_team, .data$home_team), "game_id", "game_date", "visiting_team", "home_team")
                datatble <- mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team))
                datatble$game_id[datatble$display_ID %in% input$game_table_dropdown]
            }
        })

        ## Team
        team_list = reactive({
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

        ## Player ID
        player_list = reactive({
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

        ## Skill
        skill_list = reactive({
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
                tmp <- dplyr::filter(pbp_augment(), .data$game_id %in% selected_game_id(), .data$player_name %in% input$player_list, .data$team %in% input$team_list, .data$skill %in% input$skill_list)
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
                datatble <- dplyr::select(distinct(pbp_augment(), .data$game_id, .data$game_date, .data$visiting_team, .data$home_team), "game_id", "game_date", "visiting_team", "home_team")
                datatble <- dplyr::arrange(dplyr::mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team)), .data$game_date)
                dplyr::pull(dplyr::select(datatble, .data$display_ID))
            }
        })
        
        observe({
            isolate(sel <- intersect(game_table_dropdown(), input$game_table_dropdown))
            updatePickerInput(session, "game_table_dropdown", choices = game_table_dropdown(), selected = sel)
        })
        
        ## Table of all actions as per selected_game_id() and player_id() and evaluation()
        playstable_data <- reactive({
            ## Customize pbp
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(selected_game_id()) || is.null(meta())) {
                NULL
            } else {
                if (trace_execution) cat("recalculating playstable_data\n")
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
                if (!is.null(playlist_select) & !is.null(skill_select) & !is.null(game_select) & !is.null(player_select) & !is.null(team_select)) {
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
                    pbp_tmp <- dplyr::filter(pbp, .data$player_name %in% input$player_list,
                                             .data$skill %in% input$skill_list,
                                             .data$team %in% input$team_list,
                                             .data$game_id %in% selected_game_id())
                    if (!is.null(input$skilltype_list)) pbp_tmp <- dplyr::filter(pbp_tmp, .data$skilltype %in% input$skilltype_list)
                    if (!is.null(input$phase_list)) pbp_tmp <- dplyr::filter(pbp_tmp, .data$phase %in% input$phase_list)
                }
                ## advanced filters apply to all
                if (!is.null(filter_var) && nzchar(filter_var)) pbp_tmp <- dplyr::filter(pbp_tmp, .data[[filter_var]] %in% input$adFilterValue_list)
                if (!is.null(filterB_var) && nzchar(filterB_var)) pbp_tmp <- dplyr::filter(pbp_tmp, .data[[filterB_var]] %in% input$adFilterBValue_list)
                pbp_tmp
            }
        })

        output$playstable <- DT::renderDataTable({
            mydat <- playstable_data()
            if (!is.null(mydat)) {
                DT::datatable(names_first_to_capital(mydat[, plays_cols_to_show, drop = FALSE]), rownames = FALSE,
                              extensions = "Scroller", selection = list(mode = "single", selected = 1, target = "row"),
                              options = list(sDom = '<"top">t<"bottom">rlp', deferRender = TRUE, scrollY = 200, scroller = TRUE, ordering = FALSE)) ## no column sorting
            } else {
                NULL
            }
        })
        playstable_proxy <- DT::dataTableProxy("playstable", deferUntilFlush = TRUE)
        master_playstable_selected_row <- -99L ## non-reactive
        playstable_select_row <- function(rw) {
            if (!is.null(rw) && !is.na(rw) && (rw != master_playstable_selected_row)) {
                master_playstable_selected_row <<- rw
                DT::selectRows(playstable_proxy, rw)
                scroll_playstable(rw)
            }
        }
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
            if (!is.null(clicked_row) && !is.na(clicked_row) && clicked_row != master_playstable_selected_row) { ## TODO take this last condition out?
                master_playstable_selected_row <<- clicked_row
                evaljs(paste0("dvjs_video_controller.current=", clicked_row-1, "; dvjs_video_play();"))
            }
        })

        selected_matches <- reactive({
            if (trace_execution) cat("recalculating selected matches\n")
            if (is.null(input$game_table_dropdown) || is.null(pbp()) || nrow(pbp()) < 1) {
                NULL
            } else {
                datatble <- dplyr::select(distinct(pbp(), .data$match_id, .data$game_date, .data$visiting_team, .data$home_team), "match_id", "game_date", "visiting_team", "home_team")
                datatble <- mutate(datatble, display_ID = paste0(format(.data$game_date, "%d %b %Y"),": ",.data$home_team," - ",.data$visiting_team))
                unique(na.omit(datatble$match_id[datatble$display_ID %in% input$game_table_dropdown]))
            }
        })

        video_meta <- reactive({
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(meta()) || is.null(selected_game_id()) || is.null(selected_matches())) {
                NULL
            } else {
                if (trace_execution) cat("recalculating video_meta\n")
                meta_video <- bind_rows(lapply(meta(), function(z) if (!is.null(z$video)) mutate(z$video, match_id = z$match_id, dvw_filename = z$filename)))
                meta_video <- dplyr::filter(meta_video, .data$match_id %in% selected_matches())
                if (nrow(meta_video) < 1) return(NULL)
                if (is.string(app_data$video_serve_method) && app_data$video_serve_method %in% c("lighttpd", "servr")) {
                    ## we are serving the video through the lighttpd server, so need to make symlinks in its document root directory pointing to the actual video files
                    vf <- fs::path_norm(meta_video$file)
                    if (is.null(vf) || length(vf) < 1) return(NULL)
                    ## may have multiple video files at this point
                    for (thisf in vf) {
                        if (fs::file_exists(thisf)) {
                            symlink_abspath <- fs::path_abs(file.path(app_data$video_server_dir, basename(thisf)))
                            suppressWarnings(try(unlink(symlink_abspath), silent = TRUE))
                            thisf <- gsub(" ", "\\\\ " , thisf) ## this may not work on Windows
                            fs::link_create(thisf, symlink_abspath) ##system2("ln", c("-s", thisf, symlink_abspath))
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
            ## Customize pbp
            meta_video <- video_meta()
            if (is.null(pbp_augment()) || nrow(pbp_augment()) < 1 || is.null(meta()) || is.null(selected_game_id()) || is.null(meta_video) || nrow(meta_video) < 1 || is.null(playstable_data()) || nrow(playstable_data()) < 1) {
                NULL
            } else {
                if (trace_execution) cat("recalculating playlist\n")
                event_list <- mutate(playstable_data(), skill = case_when(.data$skill %in% c("Freeball dig", "Freeball over") ~ "Freeball", TRUE ~ .data$skill), ## ov_video needs just "Freeball"
                                     skilltype = case_when(.data$skill %in% c("Serve", "Reception", "Dig", "Freeball", "Block", "Set") ~ .data$skill_type,
                                                           .data$skill == "Attack" ~ .data$attack_description),
                                     subtitle = js_str_nospecials(paste("Set", .data$set_number, "-", .data$home_team, .data$home_team_score, "-", .data$visiting_team_score, .data$visiting_team)),
                                     subtitleskill = js_str_nospecials(paste(.data$player_name, "-", .data$skilltype, ":", .data$evaluation_code)))
                event_list <- dplyr::filter(event_list, !is.na(.data$video_time)) ## can't have missing video time entries
                vpt <- if (all(is_youtube_id(meta_video$video_src) | grepl("https?://.*youtube", meta_video$video_src, ignore.case = TRUE))) {
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
                ## when playlist() changes, push it through to the javascript playlist
                if (video_player_type() == "local") {
                    js_hide("dvyt_player")
                    js_show("dv_player")
                } else {
                    js_hide("dv_player")
                    js_show("dvyt_player")
                }
                ov_video_control("stop")
                evaljs(ovideo::ov_playlist_as_onclick(playlist(), video_id = if (video_player_type() == "local") "dv_player" else "dvyt_player", dvjs_fun = "dvjs_set_playlist_and_play", seamless = TRUE))
            } else {
                ## empty playlist, so stop the video, and clean things up
                evaljs("dvjs_clear_playlist();")
                ## evaljs("remove_vspinner();") ## doesn't have an effect?
                evaljs("document.getElementById(\"subtitle\").textContent=\"Score\"; document.getElementById(\"subtitleskill\").textContent=\"Skill\";")
            }
        })
        output$player_controls_ui <- renderUI({
            ##if (is.null(playlist()) || app_data$video_serve_method == "standalone") {
            ##    NULL
            ##} else {
                tags$div(tags$button("Play", onclick = "dvjs_video_play();"),
                         tags$button("Prev", onclick = "dvjs_video_prev();"),
                         tags$button("Next", onclick = "dvjs_video_next(false);"),
                         tags$button("Pause", onclick = "dvjs_video_pause();"),
                         tags$button("Back 1s", onclick = "dvjs_jog(-1);"),
                         tags$span(id = "subtitle", "Score"),
                         tags$span(id = "subtitleskill", "Skill"),
                         uiOutput("create_clip_button_ui", inline = TRUE))
            ##}
        })

        clip_filename <- reactiveVal("")
        clip_status <- reactiveVal(NULL)
        output$create_clip_button_ui <- renderUI({
            if (is.null(playlist()) && nrow(playlist()) > 0) {
                NULL
            } else {
                actionButton("create_clip_button", "Download clip")
            }
        })
        observeEvent(input$create_clip_button, {
            ov_video_control("stop")
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
            if (!is.null(input$playback_rate)) ov_video_control("set_playback_rate", input$playback_rate)
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

    }
}
