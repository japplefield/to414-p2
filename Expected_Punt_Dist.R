library(nflfastR)
future::plan("multisession")
pbp <- load_pbp(2010:2020, file_type = "qs")
pbp <- pbp[!is.na(pbp$play_type), ]
pbp$play_type <- as.factor(pbp$play_type)
summary(pbp$play_type)

punts <- pbp[pbp$play_type == "punt",]
punts$end_spot <- 100 - punts$yardline_100 + punts$kick_distance - punts$return_yards
blocked_punts <- punts[punts$punt_blocked == 1,]