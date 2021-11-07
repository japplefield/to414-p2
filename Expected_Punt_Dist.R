library(nflfastR)
future::plan("multisession")
pbp <- load_pbp(2010:2020, file_type = "qs")
pbp <- pbp[!is.na(pbp$play_type), ]
pbp$play_type <- as.factor(pbp$play_type)
summary(pbp$play_type)

# Simplifying assumption: Ignore all punts that end in a score, which represent less than 1% of observations
punts <- pbp[pbp$play_type == "punt" & pbp$series_result == "Punt",]
punts$series_result <- as.factor(punts$series_result)
punts$end_spot <- ifelse(punts$touchback, 75, 100 - punts$yardline_100 + punts$kick_distance - punts$return_yards)

punts_relevant <- punts[,c("season_type", "posteam_type", "yardline_100", "week", "game_seconds_remaining", "game_half", "home_timeouts_remaining", "away_timeouts_remaining", "score_differential", "roof", "surface", "temp", "wind", "end_spot")]
punts_relevant[sapply(punts_relevant, is.character)] <- lapply(punts_relevant[sapply(punts_relevant, is.character)], 
                                       as.factor)
punts_relevant$temp <- ifelse(is.na(punts_relevant$temp), 72, punts_relevant$temp)
punts_relevant$wind <- ifelse(is.na(punts_relevant$wind), 0, punts_relevant$wind)

str(punts_relevant)
summary(punts_relevant)


model1 <- lm(formula=end_spot ~ .-game_seconds_remaining -game_half -home_timeouts_remaining, data=punts_relevant)
summary(model1)
