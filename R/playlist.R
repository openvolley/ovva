#' The default handler for playlists within the shiny app
#'
#' You can cook up your own version of this function to suit your liking, and pass it as the `playlist_handler` parameter to \code{\link{ovva_shiny}}.
#' 
#' @return A tibble, with columns 'skill', 'specific', and 'fun'. Each row defines a playlist entry that corresponds to specific match conditions. This playlist entry is referred to by the name given in `specific`, and applies to the skill in column `skill`. The corresponding entry in `fun` should be a function that takes three arguments (`x`, the play-by-play data; `team`, the team names to look for, and `player`, the player names to look for) and returns the subset of rows of `x` that correspond to the playlist conditions.
#'
#' @seealso \code{\link{ovva_shiny}}
#'
#' @export
ovva_playlist_handler <- function() {
    ## start by defining the table of skills and specifics
    out <- dplyr::tribble(~skill, ~specific,
                   "Attack", "Hitting off the block",
                   "Attack", "Attacks against 0 or 1 blocker",
                   "Attack", "OH, OPP: Hitting back court hits",
                   "Reception", "Reception after a bad pass",
                   "Dig", "Digs on hard driven balls"
                   )
    ## now add the corresponding code for each one
    out$fun <- list(rep(NULL, nrow(out)))
    out$fun[[which(out$skill == "Attack" & out$specific == "Hitting off the block")]] <- function(x, team, player) {
        x[x$team %in% team & x$player_name %in% player & x$skill %eq% "Attack" & lead(x$skill %eq% "Block") & lead(x$evaluation) %eq% "Error", ]
    }
    out$fun[[which(out$skill == "Attack" & out$specific == "Attacks against 0 or 1 blocker")]] <- function(x, team, player) {
        x[x$team %in% team & x$player_name %in% player & x$skill %eq% "Attack" & x$num_players %in% c("No block", "1 player block"), ]
    }
    out$fun[[which(out$skill == "Attack" & out$specific == "OH, OPP: Hitting back court hits")]] <- function(x, team, player) {
        x[x$team %in% team & x$player_name %in% player & x$skill %eq% "Attack" & x$start_zone %in% c(8,9), ]
    }
    out$fun[[which(out$skill == "Reception" & out$specific == "Reception after a bad pass")]] <- function(x, team, player) {
        ## all receps for this player
        mpid_reception <- x[x$team %in% team & x$player_name %in% player & x$skill %eq% "Reception", ]
        ## match and point ids of poor receps
        mpid_poor <- distinct(x[x$team %in% team & x$skill %eq% "Reception" & grepl("Error|Poor|Negative", x$evaluation), c("match_id", "point_id")])
        ## receps following poor receps
        dplyr::semi_join(mpid_reception, mutate(mpid_poor, point_id = .data$point_id+1), by = c("match_id", "point_id"))
    }
    out$fun[[which(out$skill == "Dig" & out$specific == "Digs on hard driven balls")]] <- function(x, team, player) {
        x[x$team %in% team & x$player_name %in% player & x$skill %eq% "Dig" & (lag(x$skill) %eq% "Attack" & lag(x$skill_subtype %in% c("Hard spike")) & !lag(x$team) %eq% x$team), ]
    }
    out
}

#' The default handler for highlights within the shiny app
#'
#' You can cook up your own version of this function to suit your liking, and pass it as the `highlight_handler` parameter to \code{\link{ovva_shiny}}.
#'
#' @return A tibble, with columns 'skill', 'specific', and 'fun'. Each row defines a playlist entry that corresponds to specific match conditions. This playlist entry is referred to by the name given in `specific`, and applies to the skill in column `skill`. The corresponding entry in `fun` should be a function that takes three arguments (`x`, the play-by-play data; `team`, the team names to look for, and `player`, the player names to look for) and returns the subset of rows of `x` that correspond to the playlist conditions.
#'
#' @seealso \code{\link{ovva_shiny}}
#'
#' @export
ovva_highlight_handler <- function() {
    ## start by defining the table of skills and specifics

    # whaouh factor points matrix

    wfpm <- dplyr::tribble(~skill, ~evaluation_code, ~whaouhfactorpoints,
                           "Attack", "#", 8,
                           "Attack", "+", .1,
                           "Attack", "-", .1,
                           "Attack", "=", -2,
                           "Serve",  "#", 8,
                           "Block", "#", 8,
                           "Block", "!", 1,
                           "Set", "+", 1,
                           "Set", "=", -2,
                           "Freeball", "=", -2,
                           "Freeball", "#", -2,
                           "Freeball", "+", -2,
                           "Freeball", "-", -2,
                           "Freeball", "!", -2
                           )

    out <- dplyr::tribble(~skill, ~specific,
                          "Highlights", "Game",
                          "Highlights", "Team",
                          "Highlights", "Player"
    )
    ## now add the corresponding code for each one
    out$fun <- list(rep(NULL, nrow(out)))
    out$fun[[which(out$skill == "Highlights" & out$specific == "Game")]] <- function(x, team, player) {

        x_tmp <- dplyr::left_join(x, wfpm, by = c("skill", "evaluation_code"))
        x_tmp <- tidyr::drop_na(x_tmp, .data$video_time)

        x_tmp <- dplyr::summarize(dplyr::group_by(x_tmp, .data$point_id), WFP = sum(.data$whaouhfactorpoints, na.rm = TRUE), duration = diff(range(.data$video_time, na.rm = TRUE)))

        x_tmp <- dplyr::arrange(x_tmp, -.data$WFP, -.data$duration)

        x_tmp$cumsumDur <- cumsum(x_tmp$duration)

        pid_highlights <- sort(x_tmp$point_id[x_tmp$cumsumDur < 180 & x_tmp$cumsumDur > 0])

        x[x$point_id %in% pid_highlights & !is.na(x$video_time), ]
    }
    out$fun[[which(out$skill == "Highlights" & out$specific == "Team")]] <- function(x, team, player) {

        x_tmp <- dplyr::left_join(x, wfpm, by = c("skill", "evaluation_code"))
        x_tmp$whaouhfactorpoints[x_tmp$team %eq% team] <- x_tmp$whaouhfactorpoints[x_tmp$team %eq% team]*2

        x_tmp <- tidyr::drop_na(x_tmp, .data$video_time)

        x_tmp <- dplyr::summarize(dplyr::group_by(x_tmp, .data$point_id), WFP = sum(.data$whaouhfactorpoints, na.rm = TRUE), duration = diff(range(.data$video_time, na.rm = TRUE)))

        x_tmp <- dplyr::arrange(x_tmp, -.data$WFP, -.data$duration)

        x_tmp$cumsumDur <- cumsum(x_tmp$duration)

        pid_highlights <- sort(x_tmp$point_id[x_tmp$cumsumDur < 180 & x_tmp$cumsumDur > 0])

        x[x$point_id %in% pid_highlights & !is.na(x$video_time),]
    }
    out$fun[[which(out$skill == "Highlights" & out$specific == "Player")]] <- function(x, team,  player) {

        x_tmp <- dplyr::left_join(x, wfpm, by = c("skill", "evaluation_code"))
        x_tmp$whaouhfactorpoints[x_tmp$player_name %eq% player] <- x_tmp$whaouhfactorpoints[x_tmp$player_name %eq% player]*2

        x_tmp <- tidyr::drop_na(x_tmp, .data$video_time)

        x_tmp <- dplyr::summarize(dplyr::group_by(x_tmp, .data$point_id), WFP = sum(.data$whaouhfactorpoints, na.rm = TRUE), duration = diff(range(.data$video_time, na.rm = TRUE)))

        x_tmp <- dplyr::arrange(x_tmp, -.data$WFP, -.data$duration)

        x_tmp$cumsumDur <- cumsum(x_tmp$duration)

        pid_highlights <- sort(x_tmp$point_id[x_tmp$cumsumDur < 180 & x_tmp$cumsumDur > 0])

        x[x$point_id %in% pid_highlights & !is.na(x$video_time),]
    }
    out
}
