`%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)

single_value_or_na <- function(x) if (length(x) == 1) x else NA

single_value_or_na_char <- function(x) if (length(x) == 1) x else NA_character_

single_value_or_na_int <- function(x) if (length(x) == 1) x else NA_integer_

single_unique_or <- function(z, or = "[multiple values]") {
    if (is.na(or)) or <- as(NA, class(z))
    tmp <- unique(na.omit(z))
    if (length(tmp) == 1) tmp else if (length(tmp) > 1) or else as(NA, class(z))
}

preprocess_data <- function(x) {
    if (!"end_cone" %in% names(x)) x$end_cone <- NA_integer_
    if (!"receiving_team" %in% names(x)) {
        x <- mutate(x, receiving_team = case_when(.data$serving_team %eq% .data$home_team ~ .data$visiting_team,
                                                  .data$serving_team %eq% .data$visiting_team ~ .data$home_team))
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
    x <- mutate(x, skilltype = case_when(.data$skill %in% c("Serve", "Reception") ~ .data$skill_type,
                                         .data$skill == "Attack" ~ .data$attack_description,
                                         .data$skill == "Set" ~ .data$set_code))
    x <- mutate(x, skill_type = case_when(.data$skill == "Set" ~ .data$set_code,
                                          TRUE ~ .data$skill_type))
  x
}

## identify whether a given string looks like a youtube video ID
is_youtube_id <- function(z) nchar(z) == 11 & grepl("^[[:alnum:]_\\-]+$", z)
## is_youtube_id(c("7DnQWfTJiP4", "qwSIgTaWK5s", "a", "qwSIgTaW-5s", "_qwSIgTaWK5"))


## internal function to try and locate a video file, when the path embedded in the dvw file is for another computer
## dvw_filename should be full path to file
find_video_in_subtree <- function(dvw_filename, video_filename = NULL) {
    stopifnot(length(dvw_filename) == 1) ## single dvw file, but can handle multiple video files
    if (is.null(video_filename)) {
        video_filename <- datavolley::dv_read(dvw_filename, metadata_only = TRUE)$meta$video
        if (nrow(video_filename) > 0) {
            return(find_video_in_subtree(dvw_filename = dvw_filename, video_filename = video_filename$file))
        } else {
            video_filename <- NA_character_
        }
    }
    if (length(video_filename) > 1) {
        return(vapply(seq_len(nrow(video_filename)), function(z) find_video_in_subtree(dvw_filename = dvw_filename, video_filename = video_filename$file[z]), FUN.VALUE = "", USE.NAMES = FALSE))
    }
    if (length(video_filename) == 1 && !is.na(video_filename) && nzchar(video_filename)) {
        if (fs::file_exists(video_filename)) return(video_filename) ## ok, the path in the dvw file is actually correct
        ## otherwise let's go looking for it
        this_dir <- dirname(dvw_filename) ## actual file has to be under the same path
        if (!fs::dir_exists(this_dir) && !fs::link_exists(this_dir)) return(NA_character_)
        possible_paths <- c(this_dir, fs::dir_ls(this_dir, type = "dir", recurse = TRUE))
        ff <- fs::path(possible_paths, basename(video_filename))
        ff <- ff[fs::file_exists(ff)]
        if (length(ff) ==1) ff else NA_character_
    } else {
        NA_character_
    }
}
