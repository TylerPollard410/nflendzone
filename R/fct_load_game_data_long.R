# R/fct_load_game_data_long.R

#' Compute Long-Format Team-Game Data from Game Schedule
#'
#' Takes a game-level data frame and generates a long-format team-game data set
#' with per-team rolling records and game stats.
#'
#' @param game_df Data frame or tibble of game schedule data. Must contain columns:
#'   `season`, `game_id`, `team`, `opponent`, `result`, `spread_line`,
#'   `team_score`, `opponent_score`, `winner`, and `location`.
#'
#' @return A tibble in long format with one row per team-game, including:
#'   - Per-team cumulative statistics (`team_GP`, `team_W`, `team_L`, `team_T`, `team_PF`, `team_PFG`, `team_PA`, `team_PAG`)
#'   - `winner` flag (TRUE if the team won, FALSE if lost, NA if tie or missing)
#'   - `locationID` (1 or 2, indicating home vs. away after reshaping)
#'
#' @details
#' This function reshapes the input `game_df` to long format (one row per team per game)
#' using `clean_homeaway()` to handle home/away labeling. It then groups by `season` and `team`
#' to compute running tallies for games played, wins, losses, ties, and points for/against,
#' as well as per-game averages. Afterward, it assigns a `locationID` within each `game_id`
#' to distinguish the two sides.
#'
#' @seealso \code{\link{clean_homeaway}}
#'
#' @importFrom nflreadr clean_homeaway
#' @importFrom dplyr group_by mutate row_number lag ungroup
#' @export
#' @noRd
load_game_data_long <- function(game_df) {
  gameDataLong <- game_df |>
    clean_homeaway(invert = c("result", "spread_line")) |>
    dplyr::group_by(season, team) |>
    dplyr::mutate(
      team_GP   = dplyr::row_number(),
      winner    = ifelse(team == winner, TRUE,
                         ifelse(opponent == winner, FALSE, NA)),
      team_W    = cumsum(result > 0),
      team_L    = cumsum(result < 0),
      team_T    = team_GP - team_W - team_L,
      team_PF   = cumsum(team_score),
      team_PFG  = team_PF / team_GP,
      team_PA   = cumsum(opponent_score),
      team_PAG  = team_PA / team_GP
    ) |>
    dplyr::mutate(
      team_W = ifelse(is.na(dplyr::lag(team_W)), 0, dplyr::lag(team_W)),
      team_L = ifelse(is.na(dplyr::lag(team_L)), 0, dplyr::lag(team_L)),
      team_T = ifelse(is.na(dplyr::lag(team_T)), 0, dplyr::lag(team_T))
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(game_id) |>
    dplyr::mutate(
      locationID = dplyr::row_number(),
      .after     = location
    ) |>
    dplyr::ungroup()

  return(gameDataLong)
}
