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
                   "Attack", "Attacks against 0 or 1 blocker",
                   "Attack", "Hitting angles",
                   "Attack", "Hitting lines",
                   "Attack", "Hitting off the block",
                   "Attack", "OH, OPP: Hitting back court hits",
                   "Attack", "Recycle",
                   "Block", "Triple block",
                   "Reception", "Reception after a bad pass",
                   "Dig", "Digs on hard driven balls"
                   )
    ## now add the corresponding code for each one
    out$fun <- list(rep(NULL, nrow(out)))
    out$fun[[which(out$skill == "Attack" & out$specific == "Hitting off the block")]] <- function(x, team = NULL, player = NULL) {
        dplyr::filter(x, all_or_filter(.data$team, .env$team), all_or_filter(.data$player_name, .env$player), .data$skill == "Attack", lead(.data$skill) == "Block", lead(.data$evaluation) %in% c("Error", "Positive, block touch", "Poor, opposition to replay"))
    }
    out$fun[[which(out$skill == "Attack" & out$specific == "Attacks against 0 or 1 blocker")]] <- function(x, team = NULL, player = NULL) {
        dplyr::filter(x, all_or_filter(.data$team, .env$team), all_or_filter(.data$player_name, .env$player), .data$skill == "Attack", .data$num_players %in% c("No block", "1 player block"))
    }
    out$fun[[which(out$skill == "Attack" & out$specific == "OH, OPP: Hitting back court hits")]] <- function(x, team = NULL, player = NULL) {
        dplyr::filter(x, all_or_filter(.data$team, .env$team), all_or_filter(.data$player_name, .env$player), .data$skill == "Attack", .data$start_zone %in% c(8, 9, 6, 1))
    }
    out$fun[[which(out$skill == "Attack" & out$specific == "Hitting angles")]] <- function(x, team = NULL, player = NULL) {
        aidx <- all_or_filter(x$team, team) & all_or_filter(x$player_name, player) & x$skill %eq% "Attack"
        if (all(is.na(x$end_cone))) {
            ## using zones
            aidx <- aidx & ((x$start_zone %in% c(4, 7, 5) & x$end_zone %in% c(4, 7, 5)) | (x$start_zone %in% c(3, 8, 6) & x$end_zone %in% c(2, 9, 1, 4, 7, 5)) | (x$start_zone %in% c(2, 9, 1) & x$end_zone %in% c(2, 9, 1)))
        } else {
            aidx <- aidx & ((x$start_zone %in% c(4, 7, 5, 2, 9, 1) & x$end_cone %in% c(5, 6, 7)) | (x$start_zone %in% c(3, 8, 6) & x$end_cone %in% c(1, 2, 6, 7)))
        }
        x[which(aidx), ]
    }
    out$fun[[which(out$skill == "Attack" & out$specific == "Hitting lines")]] <- function(x, team = NULL, player = NULL) {
        aidx <- all_or_filter(x$team, team) & all_or_filter(x$player_name, player) & x$skill %eq% "Attack"
        if (all(is.na(x$end_cone))) {
            ## using zones
            aidx <- aidx & ((x$start_zone %in% c(4, 7, 5) & x$end_zone %in% c(2, 9, 1)) | (x$start_zone %in% c(2, 9, 1) & x$end_zone %in% c(4, 7, 5)))
        } else {
            aidx <- aidx & x$start_zone %in% c(4, 7, 5, 2, 9, 1) & x$end_cone %in% c(1, 2)
        }
        x[which(aidx), ]
    }
    out$fun[[which(out$skill == "Attack" & out$specific == "Recycle")]] <- function(x, team = NULL, player = NULL) {
        dplyr::filter(x, all_or_filter(.data$team, .env$team), all_or_filter(.data$player_name, .env$player), .data$skill == "Attack", (.data$evaluation %eq% "Blocked for reattack" | .data$special_code %eq% "Block control"))
    }
    out$fun[[which(out$skill == "Block" & out$specific == "Triple block")]] <- function(x, team = NULL, player = NULL) {
        if (!"opposition_team" %in% names(x)) x <- mutate(x, opposition_team = case_when(.data$team == .data$home_team ~ .data$visiting_team,
                                                                                         .data$team == .data$visiting_team ~ .data$home_team))
        ## want this referenced to the blocking team, not the attacking team. But we return attacks, because there might not be a corresponding block scouted and the block does not incorporate the number of blockers anyway
        ## `player` is ignored here
        dplyr::filter(x, all_or_filter(.data$opposition_team, .env$team), .data$skill == "Attack", .data$num_players == "3 player block")
    }
    out$fun[[which(out$skill == "Reception" & out$specific == "Reception after a bad pass")]] <- function(x, team = NULL, player = NULL) {
        ## all receps for this player
        mpid_reception <- dplyr::filter(x, all_or_filter(.data$team, .env$team), all_or_filter(.data$player_name, .env$player), .data$skill == "Reception")
        ## match and point ids of poor receps
        mpid_poor <- dplyr::filter(x, all_or_filter(.data$team, .env$team), .data$skill == "Reception", grepl("Error|Poor|Negative", .data$evaluation)) %>% dplyr::distinct(.data$match_id, .data$point_id)
        ## receps following poor receps - TODO this will miss some because the point_ids might not be consecutive (e.g. timeout in between)
        dplyr::semi_join(mpid_reception, mutate(mpid_poor, point_id = .data$point_id + 1), by = c("match_id", "point_id"))
    }
    out$fun[[which(out$skill == "Dig" & out$specific == "Digs on hard driven balls")]] <- function(x, team = NULL, player = NULL) {
        ## only non-error digs, and allow block touch
        dplyr::filter(x, all_or_filter(.data$team, .env$team), all_or_filter(.data$player_name, .env$player), .data$skill == "Dig", .data$evaluation != "Error",
          (lag(.data$skill) %eq% "Attack" & lag(.data$skill_subtype) %eq% "Hard spike" & !lag(.data$team) %eq% .data$team) |
          (lag(.data$skill, 2) %eq% "Attack" & lag(.data$skill_subtype, 2) %eq% "Hard spike" & !lag(.data$team, 2) %eq% .data$team & lag(.data$skill) %eq% "Block" & lag(.data$team) %eq% .data$team))
    }
    out
}

#' The default handler for highlights within the shiny app
#'
#' `ovva_highlight_handler` provides functions that create highlight playlists. Highlights are judged according to the weighting scheme defined by `ovva_highlight_weighting`.
#'
#' @param clip_duration numeric: the maximum highlight clip length in seconds
#' @param weights data.frame: a data.frame or tibble with columns `skill`, `evaluation_code`, and `highlight_weighting`. Any values provided here will override the corresponding defaults
#'
#' You can cook up your own version of this highlight handler function to suit your liking, and pass it as the `highlight_handler` parameter to \code{\link{ovva_shiny}}.
#'
#' @return A tibble, with columns 'skill', 'specific', and 'fun'. Each row defines a playlist entry that corresponds to specific match conditions. This playlist entry is referred to by the name given in `specific`, and applies to the skill in column `skill`. The corresponding entry in `fun` should be a function that takes three arguments (`x`, the play-by-play data; `team`, the team names to look for, and `player`, the player names to look for) and returns the subset of rows of `x` that correspond to the playlist conditions.
#'
#' @seealso \code{\link{ovva_shiny}}
#'
#' @export
ovva_highlight_handler <- function(clip_duration = 180) {
    ## start by defining the table of skills and specifics

    # whaouh factor points matrix
    wfpm <- ovva_highlight_weighting()

    out <- dplyr::tribble(~skill, ~specific,
                          "Highlights", "Game",
                          "Highlights", "Team",
                          "Highlights", "Player"
    )
    ## now add the corresponding code for each one
    out$fun <- list(rep(NULL, nrow(out)))
    out$fun[[which(out$skill == "Highlights" & out$specific == "Game")]] <- function(x, team = NULL, player = NULL) {
        x_tmp <- dplyr::filter(dplyr::left_join(x, wfpm, by = c("skill", "evaluation_code")), !is.na(.data$video_time) , .data$skill %in% c("Attack", "Set", "Serve", "Reception", "Dig", "Block"))
        # Special character @ gets bonus points (100)
        x_tmp$highlight_weighting[x_tmp$custom_code %eq% '@'] <- 100
        x_tmp <- dplyr::summarize(dplyr::group_by(x_tmp, .data$match_id, .data$point_id), WFP = sum(.data$highlight_weighting, na.rm = TRUE), duration = diff(range(.data$video_time, na.rm = TRUE)))
        x_tmp <- dplyr::arrange(x_tmp, -.data$WFP, -.data$duration)
        x_tmp$cumsumDur <- cumsum(x_tmp$duration)
        x_tmp <- dplyr::filter(x_tmp, .data$cumsumDur > 0 & .data$cumsumDur <= clip_duration)
        ## so x_tmp holds the match_id and point_id of the rallies that we want
        dplyr::semi_join(dplyr::filter(x, !is.na(.data$video_time) & !is.na(.data$skill)), x_tmp, by = c("match_id", "point_id"))
    }
    out$fun[[which(out$skill == "Highlights" & out$specific == "Team")]] <- function(x, team = NULL, player = NULL) {
        x_tmp <- dplyr::filter(dplyr::left_join(x, wfpm, by = c("skill", "evaluation_code")), !is.na(.data$video_time), .data$skill %in% c("Attack", "Set", "Serve", "Reception", "Dig", "Block"))
        if (length(team) > 0) x_tmp$highlight_weighting[x_tmp$team %in% team] <- x_tmp$highlight_weighting[x_tmp$team %in% team] * 2
        x_tmp <- dplyr::summarize(dplyr::group_by(x_tmp, .data$match_id, .data$point_id), WFP = sum(.data$highlight_weighting, na.rm = TRUE), duration = diff(range(.data$video_time, na.rm = TRUE)))
        x_tmp <- dplyr::arrange(x_tmp, -.data$WFP, -.data$duration)
        x_tmp$cumsumDur <- cumsum(x_tmp$duration)
        x_tmp <- dplyr::filter(x_tmp, .data$cumsumDur > 0 & .data$cumsumDur <= clip_duration)
        dplyr::semi_join(dplyr::filter(x, !is.na(.data$video_time) & !is.na(.data$skill)), x_tmp, by = c("match_id", "point_id"))

    }
    out$fun[[which(out$skill == "Highlights" & out$specific == "Player")]] <- function(x, team = NULL, player = NULL) {
        x_tmp <- dplyr::filter(dplyr::left_join(x, wfpm, by = c("skill", "evaluation_code")), !is.na(.data$video_time), .data$skill %in% c("Attack", "Set", "Serve", "Reception", "Dig", "Block"))
        if (length(player) > 0) x_tmp$highlight_weighting[x_tmp$player_name %in% player] <- x_tmp$highlight_weighting[x_tmp$player_name %in% player] * 2
        x_tmp <- dplyr::summarize(dplyr::group_by(x_tmp, .data$match_id, .data$point_id), WFP = sum(.data$highlight_weighting, na.rm = TRUE), duration = diff(range(.data$video_time, na.rm = TRUE)))
        x_tmp <- dplyr::arrange(x_tmp, -.data$WFP, -.data$duration)
        x_tmp$cumsumDur <- cumsum(x_tmp$duration)
        x_tmp <- dplyr::filter(x_tmp, .data$cumsumDur > 0 & .data$cumsumDur <= clip_duration)
        dplyr::semi_join(dplyr::filter(x, !is.na(.data$video_time) & !is.na(.data$skill)), x_tmp, by = c("match_id", "point_id"))
    }
    out
}


#' @export
#' @rdname ovva_highlight_handler
ovva_highlight_weighting <- function(weights) {
    out <- dplyr::tribble(~skill, ~evaluation_code, ~highlight_weighting,
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
                          "Freeball", "!", -2,
                          "Freeball dig", "=", -2,
                          "Freeball dig", "#", -2,
                          "Freeball dig", "+", -2,
                          "Freeball dig", "-", -2,
                          "Freeball dig", "!", -2,
                          "Freeball over", "=", -2,
                          "Freeball over", "#", -2,
                          "Freeball over", "+", -2,
                          "Freeball over", "-", -2,
                          "Freeball over", "!", -2
                          )
    if (!missing(weights) && !is.null(weights)) {
        assert_that(is.data.frame(weights), all(c("skill", "evaluation_code", "highlight_weighting") %in% names(weights)))
        weights$skill <- as.character(weights$skill)
        weights$evaluation_code <- as.character(weights$evaluation_code)
        out <- bind_rows(dplyr::anti_join(out, weights, by = c("skill", "evaluation_code")), weights)
    }
    out
}
