# —– STARTUP (runs once per app process) —–
# Team Picker Options
teams_picker_choices <- nflreadr::load_teams(current = TRUE) |>
  dplyr::select(team_abbr, team_name, team_conf, team_division) |>
  dplyr::arrange(team_division, team_name) |>
  as.data.frame()
all_seasons     <- 2006:nflreadr::most_recent_season()
teams_data       <- nflreadr::load_teams(current = FALSE)
game_data       <- load_game_data(seasons = all_seasons)
game_data_long  <- load_game_data_long(game_df = game_data)
season_weeks_df <- game_data |> dplyr::distinct(season, week, week_seq)

base_repo_url <- "https://github.com/TylerPollard410/nflendzoneData/releases/download/"
# season_standings_data <- readRDS(url(paste0(base_repo_url, "season_standings/season_standings.rds")))
# feature_long_data <- readRDS(url(paste0(base_repo_url, "feature_long/feature_long.rds")))
season_standings_data <- nflreadr::rds_from_url(paste0(base_repo_url, "season_standings/season_standings.rds"))
feature_long_data <- nflreadr::rds_from_url(paste0(base_repo_url, "feature_long/feature_long.rds"))
# —– END STARTUP —–
