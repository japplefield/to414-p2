library(nflfastR)
future::plan("multisession")
pbp <- load_pbp(2010:2020, file_type = "qs")

# Data Cleaning
pbp$field_goal_result <- as.factor(pbp$field_goal_result)
pbp$weather <- as.factor(pbp$weather)
pbp$season_type <- as.factor(pbp$season_type)
pbp$temp <- ifelse(is.na(pbp$temp), 72, pbp$temp)
pbp$wind <- ifelse(is.na(pbp$wind), 0, pbp$wind)

# Filter data to only analyze plays where the field goal was successful
field_goals <- pbp[!is.na(pbp$field_goal_attempt) & pbp$field_goal_attempt == 1,]
non_blocked_field_goals <- field_goals[field_goals$field_goal_result != "blocked",]
non_blocked_field_goals$success <- ifelse(non_blocked_field_goals$field_goal_result == "made", 1, 0)


## Run Logistic Regression Model for successful field goal attempts
field_goal_glm1 <- glm(success ~ temp + wind  + yardline_100 + game_seconds_remaining + 
                         season_type + score_differential, data = non_blocked_field_goals, family = binomial)

#note: fix the NA values for wind and temp for this to run smoothly
summary(field_goal_glm1)

## Ann Model
# Normalize data
pbpmm <- as.data.frame(model.matrix(~.-1,pbp))

set.seed(12345)

pbp_random <- pbpmm[sample(nrow(pbpmm)), ]
normalize <- function(x) {
  rerturn ((x - min(x)) / (max(x) - min(x)))
}
pbp_norm <- as.data.frame(lapply(pbp_random, normalize))

# Test and traning data
test_set <- sample(1:nrow(pbp_norm), 100000) #not sure about what value would be good to use
pbp_train <- pbp_norm[-test_set, -match("field_goal_made", names(pbp_norm))]
pbp_test <- pbp_norm[test_set, -match("field_goal_made", names(pbp_norm))]

pbp_train_labels <- pbp_norm[-test_set, "field_goal_made"]
pbp_test_labels <- pbp_norm[test_set, "field_goal_made"]

pbp_train_ANN <- pbp_norm[-test_set, ] 
pbp_test_ANN <- pbp_norm[test_set, ]

# Creating Ann Prediction
library(neuralnet)
pbp_model <- neuralnet(formula = pbp$field_goal_made ~ . , data = pbp_train_ANN)

model_results1 <- compute(pbp_model, pbp_test_ANN[-53]) #not sure what row number to eliminate
predicted_field_goal_made <- model_results1$net.result

prediction1 <- ifelse(predicted_field_goal_made >= 0.1, 1, 0) #not what this line should do

# Ann Confusion Matrix
library(gmodels)
CrossTable(x = pbp_test_labels, y = prediction1, prop.chisq = FALSE)

## Run Knn Model
library(class)
library(caret)

knum <- sqrt(nrow(pbp_train))

pbp_test_pred <- knn(train = pbp_train, test = pbp_test, cl = pbp_train_labels, k= knum)

library(gmodels)
CrossTable(x = pbp_test_labels, y= pbp_test_pred, prop.chisq=FALSE)




