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
