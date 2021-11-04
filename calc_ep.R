data <- tibble::tibble(
  "season" = 2020,
  "home_team" = "DET",
  "posteam" = "GBP",
  "roof" = "outdoors",
  "half_seconds_remaining" = 590,
  "yardline_100" = 72,
  "down" = 1,
  "ydstogo" = 10,
  "posteam_timeouts_remaining" = 3,
  "defteam_timeouts_remaining" = 3
)


nflfastR::calculate_expected_points(data) %>%
  dplyr::select(season, yardline_100, td_prob, ep)