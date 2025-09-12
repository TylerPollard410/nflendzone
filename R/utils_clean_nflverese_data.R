# R/utils_clean_teamopponent.R

#' Add a Consecutive Week Sequence Across Seasons
#'
#' Given a data frame that contains `season` and `week` columns (e.g., `game_data` or
#' `game_data_long`), this function computes a sequential integer index (`week_seq`)
#' that increases by one for each distinct (season, week) pair in ascending order.
#' All rows sharing the same season/week receive the same `week_seq`.
#'
#' @param df A data frame or tibble containing at least two columns:
#'   - `season`: numeric or integer season identifier
#'   - `week`: numeric or integer week identifier within that season
#' @return The input `df`, with a new column `week_seq` appended.  `week_seq` is an
#'   integer that starts at 1 for the earliest season/week and increments by 1 for each
#'   subsequent distinct (season, week) pair sorted by `season` then `week`.
#'
#' @examples
#' library(dplyr)
#' # Example with game_data (one row per game)
#' game_data <- tibble(
#'   season = c(2020, 2020, 2020, 2021, 2021),
#'   week   = c(1,    2,    2,    1,    2),
#'   home_team = c("NE","BUF","MIA","DAL","PHI"),
#'   away_team = c("MIA","NE","BUF","PHI","DAL")
#' )
#' game_data_with_seq <- add_week_seq(game_data)
#' # Result: distinct (2020,1)->week_seq=1; (2020,2)->week_seq=2; (2021,1)->3; (2021,2)->4
#'
#' # Example with game_data_long (one row per team-game)
#' game_data_long <- tibble(
#'   season = c(2020,2020,2020,2020,2020,2021,2021,2021,2021),
#'   week   = c(1,    1,    2,    2,    2,    1,    1,    2,    2),
#'   team   = c("NE","MIA","BUF","MIA","NE","DAL","PHI","DAL","PHI")
#' )
#' game_data_long_with_seq <- add_week_seq(game_data_long)
#'
#' @importFrom dplyr distinct arrange mutate left_join row_number
#' @export
#' @noRd
add_week_seq <- function(df) {
  # 1) Extract distinct season/week combinations, sorted
  weeks_tbl <- df |>
    dplyr::distinct(season, week) |>
    dplyr::arrange(season, week) |>
    dplyr::mutate(week_seq = dplyr::row_number())

  # 2) Join back onto the original data frame
  df |>
    dplyr::left_join(weeks_tbl, by = c("season", "week")) |>
    dplyr::relocate(week_seq, .after = week)
}
