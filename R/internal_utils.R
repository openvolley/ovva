`%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)

single_value_or_na <- function(x) if (length(x) == 1) x else NA

single_value_or_na_char <- function(x) if (length(x) == 1) x else NA_character_

single_value_or_na_int <- function(x) if (length(x) == 1) x else NA_integer_

single_unique_or <- function(z, or = "[multiple values]") {
    if (is.na(or)) or <- as(NA, class(z))
    tmp <- unique(na.omit(z))
    if (length(tmp) == 1) tmp else if (length(tmp) > 1) or else as(NA, class(z))
}

names_first_to_capital <- function(x, fun) {
    setNames(x, var2fc(if (missing(fun)) names(x) else vapply(names(x), fun, FUN.VALUE = "", USE.NAMES = FALSE)))
}

var2fc <- function(x) {
    vapply(x, function(z) gsub("_", " ", paste0(toupper(substr(z, 1, 1)), substr(z, 2, nchar(z)))), FUN.VALUE = "", USE.NAMES = FALSE)
}

preprocess_data <- function(x) {
    if (!"end_cone" %in% names(x)) x$end_cone <- NA_integer_
    if (!"receiving_team" %in% names(x)) {
        x <- mutate(x, receiving_team = case_when(.data$serving_team %eq% .data$home_team ~ .data$visiting_team,
                                                  .data$serving_team %eq% .data$visiting_team ~ .data$home_team))
    }
    if (!all(c("receiving_player", "reception_grade") %in% names(x))) {
        x <- x[, setdiff(names(x), c("receiving_player", "reception_grade"))]
        rpx <- dplyr::select(dplyr::filter(x, .data$skill == "Reception"), "match_id", "point_id", receiving_player = "player_name", reception_grade = "evaluation")
        rpx <- distinct(rpx, .data$match_id, .data$point_id, .keep_all = TRUE)
        x <- left_join(x, rpx, by = c("match_id", "point_id"))
    }
    if (!all(c("setter", "opposition_setter") %in% names(x))) {
        x <- x[, setdiff(names(x), c("setter", "opposition_setter"))]
        x <- mutate(x, home_setter_id = case_when(.data$home_setter_position == 1 ~ .data$home_player_id1,
                                                  .data$home_setter_position == 2 ~ .data$home_player_id2,
                                                  .data$home_setter_position == 3 ~ .data$home_player_id3,
                                                  .data$home_setter_position == 4 ~ .data$home_player_id4,
                                                  .data$home_setter_position == 5 ~ .data$home_player_id5,
                                                  .data$home_setter_position == 6 ~ .data$home_player_id6),
                    visiting_setter_id = case_when(.data$visiting_setter_position == 1 ~ .data$visiting_player_id1,
                                                   .data$visiting_setter_position == 2 ~ .data$visiting_player_id2,
                                                   .data$visiting_setter_position == 3 ~ .data$visiting_player_id3,
                                                   .data$visiting_setter_position == 4 ~ .data$visiting_player_id4,
                                                   .data$visiting_setter_position == 5 ~ .data$visiting_player_id5,
                                                   .data$visiting_setter_position == 6 ~ .data$visiting_player_id6))
        x <- mutate(x, setter_id = case_when(.data$team %eq% .data$home_team ~ .data$home_setter_id,
                                             .data$team %eq% .data$visiting_team ~ .data$visiting_setter_id),
                    opposition_setter_id = case_when(.data$team %eq% .data$visiting_team ~ .data$home_setter_id,
                                                     .data$team %eq% .data$home_team ~ .data$visiting_setter_id))
        px <- na.omit(distinct(x[, c("player_id", "player_name")]))
        px <- px[!px$player_id %in% px$player_id[duplicated(px$player_id)], ]
        x <- left_join(x, dplyr::rename(px, setter = "player_name"), by = c(setter_id = "player_id"))
        x <- left_join(x, dplyr::rename(px, opposition_setter = "player_name"), by = c(opposition_setter_id = "player_id"))
    }
    if (!"receiving_setter_position" %in% names(x)) {
        x <- mutate(x, receiving_setter_position = case_when(.data$receiving_team %eq% .data$home_team ~ .data$home_setter_position,
                                                             .data$receiving_team %eq% .data$visiting_team ~ .data$visiting_setter_position))
    }
    if (!"serving_setter_position" %in% names(x)) {
        x <- mutate(x, serving_setter_position = case_when(.data$serving_team %eq% .data$home_team ~ .data$home_setter_position,
                                                           .data$serving_team %eq% .data$visiting_team ~ .data$visiting_setter_position))
    }
    if (!"breakpoint/sideout" %in% names(x)) {
        x <- mutate(x, `breakpoint/sideout` = case_when(.data$team %eq% .data$receiving_team ~ "Sideout",
                                                        .data$team %eq% .data$serving_team ~ "Breakpoint"))
    }
    if (!"setter_position" %in% names(x)) {
        x <- mutate(x, setter_position = case_when(.data$team %eq% .data$home_team ~ .data$home_setter_position,
                                                   .data$team %eq% .data$visiting_team ~ .data$visiting_setter_position))
    }
    if (!"opposing_team" %in% names(x)) {
        x <- mutate(x, opposing_team = case_when(.data$team %eq% .data$home_team ~ .data$visiting_team,
                                                 .data$team %eq% .data$visiting_team ~ .data$home_team))
    }
    if (!"freeball_over" %in% names(x)) {
        ## "Freeball" skill can be used both for sending a freeball to the opposition as well as receiving one, so disambiguate these usages
        x <- mutate(x, freeball_over = .data$skill %eq% "Freeball" & lead(.data$match_id) %eq% .data$match_id & lead(.data$set_number) %eq% .data$set_number & !lead(.data$team) %eq% .data$team)
    }
    ## separate freeballs into "Freeball dig" and "Freeball attack" (comment these lines out to disable this)
    x <- mutate(x, skill = case_when(.data$skill %eq% "Freeball" & .data$freeball_over ~ "Freeball over",
                                     .data$skill %eq% "Freeball" ~ "Freeball dig",
                                     TRUE ~ .data$skill))
    x <- mutate(x, skilltype = case_when(.data$skill %in% c("Serve", "Reception") ~ .data$skill_type,
                                         .data$skill == "Attack" ~ ifelse(is.na(.data$attack_description), .data$skill_type, .data$attack_description),
                                         .data$skill == "Set" ~ ifelse(is.na(.data$set_code), .data$skill_type, .data$set_code)))
    x <- mutate(x, skill_type = case_when(.data$skill == "Set" & !is.na(.data$set_code) ~ .data$set_code,
                                          TRUE ~ .data$skill_type))
  x
}

## identify whether a given string looks like a youtube video ID
is_youtube_id <- function(z) nchar(z) == 11 & grepl("^[[:alnum:]_\\-]+$", z)
## is_youtube_id(c("7DnQWfTJiP4", "qwSIgTaWK5s", "a", "qwSIgTaW-5s", "_qwSIgTaWK5"))


## internal function to try and locate a video file, when the path embedded in the dvw file is for another computer
## dvw_filename should be full path to file
find_video_in_subtree <- function(dvw_filename, video_filename = NULL, alt_path = NULL, subtree_only = FALSE, ignore_case = TRUE, file_extensions = video_file_extensions) {
    stopifnot(length(dvw_filename) == 1) ## single dvw file, but can handle multiple video files
    if (is.null(video_filename)) {
        video_filename <- datavolley::dv_read(dvw_filename, metadata_only = TRUE)$meta$video
        if (!is.null(video_filename) && nrow(video_filename) > 0) {
            return(find_video_in_subtree(dvw_filename = dvw_filename, video_filename = fs::fs_path(video_filename$file), alt_path = alt_path, subtree_only = subtree_only, ignore_case = ignore_case))
        } else {
            video_filename <- NA_character_
        }
    }
    if (length(video_filename) > 1) {
        return(vapply(seq_along(video_filename), function(z) find_video_in_subtree(dvw_filename = dvw_filename, video_filename = video_filename[z], alt_path = alt_path, subtree_only = subtree_only, ignore_case = ignore_case), FUN.VALUE = "", USE.NAMES = FALSE))
    }
    if (length(video_filename) == 1 && !is.na(video_filename) && nzchar(video_filename)) {
        if (fs::file_exists(video_filename) && !subtree_only) return(as.character(video_filename)) ## ok, the path in the dvw file is actually correct, and we are allowing non-subtree paths
        ## otherwise let's go looking for it
        this_dir <- dirname(dvw_filename) ## actual file has to be under the same path
        out <- NA_character_
        look_for_it <- function(vfilename, top_dir, ignore_case) {
            if (inherits(vfilename, "regex")) {
                ff <- fs::dir_ls(top_dir, recurse = TRUE, regexp = vfilename, ignore.case = ignore_case)
            } else {
                if (ignore_case) {
                    ## yikes, this could be slow on a big directory
                    ff <- fs::dir_ls(top_dir, recurse = TRUE, regexp = paste0("\\.(", file_extensions, ")$"), ignore.case = TRUE)
                    ff <- ff[tolower(basename(ff)) == tolower(basename(vfilename))]
                } else {
                    possible_paths <- c(top_dir, fs::dir_ls(top_dir, type = "dir", recurse = TRUE))
                    ff <- fs::path(possible_paths, basename(vfilename))
                    ff <- ff[fs::file_exists(ff)]
                }
            }
            ff
        }
        if (!fs::dir_exists(this_dir) && !fs::link_exists(this_dir)) {
            ## do nothing yet, maybe try the alt path below
        } else {
            ff <- look_for_it(video_filename, top_dir = this_dir, ignore_case = ignore_case)
            if (length(ff) ==1) out <- ff
        }
        if (is.na(out) && !is.null(alt_path) && (fs::dir_exists(alt_path) || fs::link_exists(alt_path))) {
            ## didn't find it under the subtree, try the alt path
            ff <- look_for_it(video_filename, top_dir = alt_path, ignore_case = ignore_case)
            if (length(ff) == 1) out <- ff
        }
        as.character(out)
    } else {
        NA_character_
    }
}

js_str_nospecials <- function(z) gsub("['\"\\]+", "", z)

evaljs <- function(expr) {
    shiny::getDefaultReactiveDomain()$sendCustomMessage("evaljs", expr)
}

js_show <- function(id) evaljs(paste0("var el=$('#", id, "'); if (el.hasClass('shiny-bound-input')) { el.closest('.shiny-input-container').show(); } else { el.show(); }"))
js_hide <- function(id) evaljs(paste0("var el=$('#", id, "'); if (el.hasClass('shiny-bound-input')) { el.closest('.shiny-input-container').hide(); } else { el.hide(); }"))


introbox_or_div <- function(...) {
    if (requireNamespace("rintrojs", quietly = TRUE)) rintrojs::introBox(...) else tags$div(...)
}

timing_tstart <- function(skill, phase, start_offset = -5) {
    tags$td(numericInput(paste0("timing_", tolower(skill), "_", tolower(phase), "_start_offset"), label = NULL, value = start_offset, width = "8ex"))
}
timing_tdur <- function(skill, phase, duration = 8) {
    tags$td(numericInput(paste0("timing_", tolower(skill), "_", tolower(phase), "_duration"), label = NULL, value = duration, width = "8ex"))
}
