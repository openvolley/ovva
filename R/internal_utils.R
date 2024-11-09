paste_url <- function(...) {
    ## build URL from parts with / as separator, taking care to strip those before we start
    parts <- lapply(list(...), function(z) sub("^/", "", sub("/$", "", z)))
    do.call(paste, c(parts, list(sep = "/")))
}

`%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)

seq_nrows <- function(x) {
    if (length(dim(x)) < 1 || nrow(x) < 1) integer() else seq_len(nrow(x))
}

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

preprocess_data <- function(x, data_type = "indoor") {
    if (!"end_cone" %in% names(x)) x$end_cone <- NA_integer_
    if (!"receiving_team" %in% names(x)) {
        x <- mutate(x, receiving_team = case_when(.data$serving_team == .data$home_team ~ .data$visiting_team,
                                                  .data$serving_team == .data$visiting_team ~ .data$home_team))
    }
    if (!all(c("receiving_player", "reception_grade") %in% names(x))) {
        x <- x[, setdiff(names(x), c("receiving_player", "reception_grade"))]
        rpx <- dplyr::select(dplyr::filter(x, .data$skill == "Reception"), "match_id", "point_id", receiving_player = "player_name", reception_grade = "evaluation")
        rpx <- distinct(rpx, .data$match_id, .data$point_id, .keep_all = TRUE)
        x <- left_join(x, rpx, by = c("match_id", "point_id"))
    }
    if (grepl("indoor", data_type)) {
        if (!all(c("setter_on_court", "opposition_setter_on_court") %in% names(x))) {
            x <- x[, setdiff(names(x), c("setter_on_court", "opposition_setter_on_court"))]
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
            x <- mutate(x, setter_id = case_when(.data$team == .data$home_team ~ .data$home_setter_id,
                                                 .data$team == .data$visiting_team ~ .data$visiting_setter_id),
                        opposition_setter_id = case_when(.data$team == .data$visiting_team ~ .data$home_setter_id,
                                                         .data$team == .data$home_team ~ .data$visiting_setter_id))
            px <- na.omit(distinct(x[, c("player_id", "player_name")]))
            px <- px[!px$player_id %in% px$player_id[duplicated(px$player_id)], ]
            x <- left_join(x, dplyr::rename(px, setter_on_court = "player_name"), by = c(setter_id = "player_id"))
            x <- left_join(x, dplyr::rename(px, opposition_setter_on_court = "player_name"), by = c(opposition_setter_id = "player_id"))
        }
        if (!"receiving_setter_position" %in% names(x)) {
            x <- mutate(x, receiving_setter_position = case_when(.data$receiving_team == .data$home_team ~ .data$home_setter_position,
                                                                 .data$receiving_team == .data$visiting_team ~ .data$visiting_setter_position))
        }
        if (!"serving_setter_position" %in% names(x)) {
            x <- mutate(x, serving_setter_position = case_when(.data$serving_team == .data$home_team ~ .data$home_setter_position,
                                                               .data$serving_team == .data$visiting_team ~ .data$visiting_setter_position))
        }
        if (!"opposition_setter_position" %in% names(x)) {
            x <- mutate(x, opposition_setter_position = case_when(.data$team == .data$home_team ~ .data$visiting_setter_position,
                                                                  .data$team == .data$visiting_team ~ .data$home_setter_position))
        }
        if (!"setter_position" %in% names(x)) {
            x <- mutate(x, setter_position = case_when(.data$team == .data$home_team ~ .data$home_setter_position,
                                                       .data$team == .data$visiting_team ~ .data$visiting_setter_position))
        }
    }
    if (!"breakpoint/sideout" %in% names(x)) {
        x <- mutate(x, `breakpoint/sideout` = case_when(.data$team == .data$receiving_team ~ "Sideout",
                                                        .data$team == .data$serving_team ~ "Breakpoint"))
    }
    if (!"opposition_team" %in% names(x)) {
        x <- mutate(x, opposition_team = case_when(.data$team == .data$home_team ~ .data$visiting_team,
                                                 .data$team == .data$visiting_team ~ .data$home_team))
    }
    if (!"freeball_over" %in% names(x)) {
        ## "Freeball" skill can be used both for sending a freeball to the opposition as well as receiving one, so disambiguate these usages
        x <- mutate(x, freeball_over = .data$skill %eq% "Freeball" & lag(.data$match_id) %eq% .data$match_id & lag(.data$point_id) %eq% .data$point_id &
                           ((!is.na(lead(.data$team)) & lead(.data$team) != .data$team) | lag(.data$team) %eq% .data$team))
    }

    if (!all(c("pt_serve_zone", "ts_pass_zone") %in% names(x))) {
        x <- x[, setdiff(names(x), c("pt_serve_zone", "ts_pass_zone"))]
        tempsrv <- dplyr::rename(dplyr::filter(ungroup(x), .data$skill == "Serve"), pt_serve_zone = "start_zone")
        tempsrv <- dplyr::filter(ungroup(mutate(group_by(tempsrv, .data$match_id, .data$point_id), ok = n() == 1), .data$ok))
        x <- left_join(x, dplyr::select(tempsrv, "match_id", "point_id", "pt_serve_zone"), by = c("match_id", "point_id"))
        touchsum <- group_by(dplyr::filter(ungroup(x), !is.na(.data$team)), .data$match_id, .data$team, .data$team_touch_id)
        touchsum <- ungroup(dplyr::summarize(touchsum, ts_pass_zone = single_value_or_na(.data$end_zone[.data$skill %in% c("Dig", "Reception") | (.data$skill %eq% "Freeball" & !.data$freeball_over)])))
        x <- left_join(x, dplyr::select(touchsum, -"team"), by = c("match_id", "team_touch_id"))
    }
    ## separate freeballs into "Freeball dig" and "Freeball attack" (comment these lines out to disable this)
    x <- mutate(x, skill = case_when(.data$skill == "Freeball" & .data$freeball_over ~ "Freeball over",
                                     .data$skill == "Freeball" ~ "Freeball dig",
                                     TRUE ~ .data$skill))
    x <- mutate(x, skilltype = case_when(.data$skill %in% c("Serve", "Reception") ~ .data$skill_type,
                                         .data$skill == "Attack" ~ ifelse(is.na(.data$attack_description), .data$skill_type, .data$attack_description),
                                         .data$skill == "Set" ~ ifelse(is.na(.data$set_code), .data$skill_type, .data$set_code)))
    x <- mutate(x, skill_type = case_when(.data$skill == "Set" & !is.na(.data$set_code) ~ .data$set_code,
                                          TRUE ~ .data$skill_type))
    ## NA skilltype causes problems, if we have multiple skills selected then it will filter out the missing skilltype (e.g. all blocks, usually)
    x$skilltype[is.na(x$skilltype) & !is.na(x$skill) & !(x$skill %in% c("Timeout", "Technical timeout", "Substitution"))] <- "no value"
    ## use scores at start of point
    if (!isTRUE(check_scores_start_of_point(x))) {
        ## regenerate those columns
        x <- x %>% group_by(.data$match_id) %>%
            mutate(home_score_start_of_point = pmax(ifelse(.data$point_won_by %eq% .data$home_team, as.integer(.data$home_team_score - 1L), as.integer(.data$home_team_score)), 0L),
                   visiting_score_start_of_point = pmax(ifelse(.data$point_won_by %eq% .data$visiting_team, as.integer(.data$visiting_team_score - 1L), as.integer(.data$visiting_team_score)), 0L)) %>% ungroup
    }
    x$home_team_score <- x$home_score_start_of_point
    x$visiting_team_score <- x$visiting_score_start_of_point
    x
}

## check whether we have scores at the start of the point - for historical reasons, these columns might be absent from some files
check_scores_start_of_point <- function(x) {
    if (!all(c("home_score_start_of_point", "visiting_score_start_of_point") %in% names(x))) return(FALSE)
    ## the columns exist, but there is still the possibility that some matches have these values populated and some do not
    idxh <- which(!is.na(x$home_team_score))
    idxv <- which(!is.na(x$home_visiting_score))
    !any(is.na(x$home_score_start_of_point[idxh])) && !any(is.na(x$visiting_score_start_of_point[idxv]))
}

## identify whether a given string looks like a youtube video ID
is_youtube_id <- function(z) nchar(z) == 11 & grepl("^[[:alnum:]_\\-]+$", z)
## is_youtube_id(c("7DnQWfTJiP4", "qwSIgTaWK5s", "a", "qwSIgTaW-5s", "_qwSIgTaWK5"))

is_twitch_video <- function(z) {
    if (is.null(z)) {
        FALSE
    } else if (!is.character(z)) {
        rep(FALSE, length(z))
    } else {
        grepl("twitch\\.tv", z)
    }
}

## internal function to try and locate a video file, when the path embedded in the dvw file is for another computer
## dvw_filename should be full path to file
## alt_path can be a character vector of one or more alternative paths to search
find_video_in_subtree <- function(dvw_filename, video_filename = NULL, alt_path = NULL, subtree_only = FALSE, ignore_case = TRUE, file_extensions = video_file_extensions, dir_ls_fun = fs::dir_ls) {
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
        if (fs::file_exists(as.character(video_filename))) {
            ## the path in the dvw file is actually correct
            if (!subtree_only || any(fs::path_has_parent(video_filename, c(alt_path, dirname(dvw_filename))))) {
                ## we are allowing non-subtree paths or the video file is in the subtree or an alt path
                return(as.character(video_filename))
            }
        }
        ## otherwise let's go looking for it
        this_dir <- dirname(dvw_filename) ## actual file has to be under the same path
        out <- NA_character_
        look_for_it <- function(vfilename, top_dir, ignore_case) {
            if (inherits(vfilename, c("regex", "stringr_regex", "stringr_pattern"))) {
                ff <- dir_ls_fun(top_dir, recurse = TRUE, regexp = vfilename, ignore.case = ignore_case)
            } else {
                if (ignore_case) {
                    ## yikes, this could be slow on a big directory
                    ff <- dir_ls_fun(top_dir, recurse = TRUE, regexp = paste0("\\.(", file_extensions, ")$"), ignore.case = TRUE)
                    ff <- ff[tolower(basename(ff)) == tolower(basename(vfilename))]
                } else {
                    possible_paths <- c(top_dir, dir_ls_fun(top_dir, type = "dir", recurse = TRUE))
                    ff <- fs::path(possible_paths, basename(vfilename))
                    ff <- ff[fs::file_exists(as.character(ff))]
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
        if (is.na(out) && length(alt_path) > 0) {
            ## didn't find it under the subtree, try the alt path(s)
            for (altp in alt_path) {
                if (fs::dir_exists(altp) || fs::link_exists(altp)) {
                    ff <- look_for_it(video_filename, top_dir = altp, ignore_case = ignore_case)
                    if (length(ff) == 1) {
                        out <- ff
                        break
                    } else if (length(ff) > 1) {
                        ## can we match on file plus parent (first) directory?
                        f2 <- function(z) {
                            z <- fs::path_split(z)[[1]]
                            if (length(z) < 2) {
                                NA_character_
                            } else {
                                fs::path_join(utils::tail(z, 2))
                            }
                        }
                        vf2 <- f2(video_filename)
                        ff2 <- vapply(ff, f2, FUN.VALUE = "", USE.NAMES = FALSE)
                        temp <- which(ff2 == vf2)
                        if (length(temp) == 1) {
                            out <- ff[temp]
                            break
                        }
                    }
                }
            }
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

check_timing_df <- function(x) {
    isTRUE(is.data.frame(x) && all(c("skill", "phase", "start_offset", "duration") %in% names(x)) && nrow(x) > 0)
}

timing_tstart <- function(skill, phase, start_offset = -5, timing_df) {
    if (missing(start_offset) && !missing(timing_df) && check_timing_df(timing_df)) {
        try({
            chk <- timing_df$start_offset[timing_df$skill %eq% skill & timing_df$phase %eq% phase]
            if (length(chk) == 1 && !is.na(chk)) start_offset <- chk
        })
    }
    tags$td(numericInput(paste0("timing_", tolower(skill), "_", tolower(phase), "_start_offset"), label = NULL, value = start_offset, width = "8ex"))
}
timing_tdur <- function(skill, phase, duration = 8, timing_df) {
    if (missing(duration) && !missing(timing_df) && check_timing_df(timing_df)) {
        try({
            chk <- timing_df$duration[timing_df$skill %eq% skill & timing_df$phase %eq% phase]
            if (length(chk) == 1 && !is.na(chk)) duration <- chk
        })
    }
    tags$td(numericInput(paste0("timing_", tolower(skill), "_", tolower(phase), "_duration"), label = NULL, value = duration, width = "8ex"))
}

## adapted from http://conjugateprior.org/2015/06/identifying-the-os-from-r/
get_os <- function() {
    if (.Platform$OS.type == "windows") return("windows")
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf["sysname"]
        if (tolower(os) == "darwin")
            os <- "osx"
    } else {
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os, ignore.case = TRUE))
            os <- "osx"
        if (grepl("linux-gnu", R.version$os, ignore.case = TRUE))
            os <- "linux"
    }
    os <- tolower(os)
    if (!os %in% c("windows", "linux", "unix", "osx"))
        stop("unknown operating system: ", os)
    os
}

## convenience function to silence the fa warnings about icons
icon <- function(...) shiny::icon(..., verify_fa = FALSE)

## helper function to filter according to a selection
##  if the selection is empty, return all TRUE, otherwise the elements of x that are in the selection
all_or_filter <- function(x, selection) {
    if (length(selection) < 1) rep(TRUE, length(x)) else x %in% selection
}
