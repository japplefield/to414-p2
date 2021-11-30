
my_func <- function(home_team, posteam, oppteam, yardline_100, seconds, prob){

  
  
  
data <- tibble::tibble(
  "season" = 2020,
  "home_team" = home_team,
  "posteam" = posteam,
  "roof" = "outdoors",
  "half_seconds_remaining" = seconds,
  "yardline_100" = yardline_100,
  "down" = 1,
  "ydstogo" = 10,
  "posteam_timeouts_remaining" = 3,
  "defteam_timeouts_remaining" = 3
)


success <- nflfastR::calculate_expected_points(data) %>%
  dplyr::select(season, yardline_100, td_prob, ep)

data <- tibble::tibble(
  "season" = 2020,
  "home_team" = home_team,
  "posteam" = oppteam,
  "roof" = "outdoors",
  "half_seconds_remaining" = seconds,
  "yardline_100" = 100 - yardline_100,
  "down" = 1,
  "ydstogo" = 10,
  "posteam_timeouts_remaining" = 3,
  "defteam_timeouts_remaining" = 3
)


failure <- nflfastR::calculate_expected_points(data) %>%
  dplyr::select(season, yardline_100, td_prob, ep)

return (prob * success$ep + (1-prob) * failure$ep * -1)

}

my_func("DET", "GBP", "DET", 72, 590, 0.8)
