library(nflfastR)
future::plan("multisession")
pbp <- load_pbp(2010:2020, file_type = "qs")

# Data Cleaning
field_goal_result <- as.factor(field_goal_result)
weather <- as.factor(weather)
season_type <- as.factor(season_type)

# Filter data to only analyze plays where the field goal was successful
field_goal_made <- subset(pbp, field_goal_attempt == 1 & field_goal_result == 1)

# Run Logistic Regression Modelon successful field goal attempts
field_goal_glm1 <- glm(field_goal_made ~ weather + yardline_100 + game_seconds_remaining + 
                         season_type + score_differential, data = pbp, family = binomial)
summary(field_goal_glm1)

# Run Ann Model

# Run Knn Model

