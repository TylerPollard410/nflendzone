#' Compute processed game schedule data with betting probabilities and related features
#'
#' @description
#' `load_game_data()` loads raw NFL game schedules via `nflreadr::load_schedules()`, filters to the requested seasons,
#' cleans team abbreviations, and computes a variety of betting-related probabilities and cover flags. It also determines
#' the game winner and classifies the time-of-day (Day, Evening, Night) based on the game time.
#'
#' @param seasons Integer vector of seasons to include (default: 2006 through the most recent available season).
#' @return A tibble of game schedules with columns:
#' * `home_team`, `away_team` (cleaned abbreviations)
#' * `season_type` ("REG" or "POST")
#' * betting probabilities: `home_spread_prob`, `away_spread_prob`, `under_prob`, `over_prob`, `home_moneyline_prob`, `away_moneyline_prob`
#' * cover flags: `spreadCover`, `totalCover`
#' * `winner` (team abbreviation)
#' * `time_of_day` ("Day", "Evening", or "Night")
#'
#' @importFrom nflreadr load_schedules most_recent_season clean_team_abbrs
#' @importFrom dplyr filter mutate select relocate between case_when
#' @importFrom stringr str_extract
#'
#' @export
#' @noRd
load_game_data <- function(seasons = 2006:most_recent_season()) {
  games <- nflreadr::load_schedules(seasons = TRUE)      # load schedule for specified seasons
  games |>
    dplyr::filter(season >= min(seasons)) |>       # ensure only seasons at or after the minimum
    dplyr::mutate(
      home_team = clean_team_abbrs(home_team),
      away_team = clean_team_abbrs(away_team),
      # season type: REG vs POST
      season_type = ifelse(game_type == "REG", "REG", "POST"),
      # betting probabilities from American odds
      home_spread_prob = ifelse(home_spread_odds < 0,
                                abs(home_spread_odds)/(abs(home_spread_odds) + 100),
                                100/(home_spread_odds + 100)),
      away_spread_prob = ifelse(away_spread_odds < 0,
                                abs(away_spread_odds)/(abs(away_spread_odds) + 100),
                                100/(away_spread_odds + 100)),
      under_prob = ifelse(under_odds < 0,
                          abs(under_odds)/(abs(under_odds) + 100),
                          100/(under_odds + 100)),
      over_prob = ifelse(over_odds < 0,
                         abs(over_odds)/(abs(over_odds) + 100),
                         100/(over_odds + 100)),
      home_moneyline_prob = ifelse(home_moneyline < 0,
                                   abs(home_moneyline)/(abs(home_moneyline) + 100),
                                   100/(home_moneyline + 100)),
      away_moneyline_prob = ifelse(away_moneyline < 0,
                                   abs(away_moneyline)/(abs(away_moneyline) + 100),
                                   100/(away_moneyline + 100)),
      # cover flags and winner
      spreadCover = case_when(
        result > spread_line  ~ TRUE,
        result < spread_line  ~ FALSE,
        TRUE                  ~ NA
      ),
      totalCover = case_when(
        total  > total_line  ~ TRUE,
        total  < total_line  ~ FALSE,
        TRUE                 ~ NA
      ),
      winner = case_when(
        result >  0 ~ home_team,
        result <  0 ~ away_team,
        TRUE        ~ NA_character_
      ),
      # time of day based on gametime ("HH:MM:SS" or similar)
      gamehour = as.numeric(str_extract(gametime, "[:digit:]+(?=:)")),
      time_of_day = case_when(
        gamehour < 15               ~ "Day",
        between(gamehour, 15, 18)   ~ "Evening",
        gamehour > 18               ~ "Night",
        TRUE                        ~ NA_character_
      )
    ) |>                                # remove helper and order columns
    select(-gamehour) |>                # drop intermediate field
    relocate(season_type,       .after = game_type) |>
    relocate(home_spread_prob,  .after = home_spread_odds) |>
    relocate(away_spread_prob,  .after = away_spread_odds) |>
    relocate(under_prob,        .after = under_odds) |>
    relocate(over_prob,         .after = over_odds) |>
    relocate(home_moneyline_prob, .after = home_moneyline) |>
    relocate(away_moneyline_prob, .after = away_moneyline) |>
    relocate(spreadCover,       .after = spread_line) |>
    relocate(totalCover,        .after = total_line) |>
    relocate(winner,            .after = result) |>
    relocate(time_of_day,       .after = gametime) |>
    add_week_seq() |>
    select(
      -old_game_id,
      -gsis,
      -nfl_detail_id,
      -pfr,
      -pff,
      -espn,
      -ftn,
      -away_qb_id,
      -home_qb_id,
      -stadium_id
    )
}
