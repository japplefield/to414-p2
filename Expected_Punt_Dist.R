library(nflfastR)
future::plan("multisession")
pbp <- load_pbp(2019:2020, file_type = "qs")

punts <- pbp[pbp$play_type == "punt",]
punts$end_spot <- 100 - punts$yardline_100 + punts$kick_distance - punts$return_yards
blocked_punts <- punts[punts$punt_blocked == 1,]