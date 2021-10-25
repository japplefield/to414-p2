# Project 2
### TO 414: Advanced Analytics
### Hip Hip Array

# Business Problem

The purpose of this analysis is to provide football coaches, players, and fans with information and insight into the appropriate decision making that goes into a 4th down. We want to provide a model that will tell coaches whether or not it is worth it to either attempt a 4th down conversion, punt the football, or attempt a field goal. This problem is significant and important because 4th downs are a crucial part of any football game. Correctly deciding what approach to do can change the course of the game, and this is especially important when every point matters. By maximizing the expected points and win probability that comes with each decision, our audience can better understand the game of football and make these otherwise difficult judgement calls that have historically been made on instinct.

# Dataset Information

**Data Summary:**
We are using the detailed NFL Play-by-Play Data from 2010-2020 (this may be expanded into seasons pre-2010 if we either want more data, or want to test our model on additional seasons) . We initially found this dataset on Kaggle. The data was scraped from NFL data by Carnegie Melon researchers from NFL databases. The source dataset is too large to keep in the git repository so we had to use the R package “nflfastR” (built from the same Kaggle dataset) to extract data.

**Data Structure:**

The data has 531,515 rows and 372 variables. Variable descriptions and dataset information can be found here. Many of the variables in the dataset include information about the play itself, after the decision to go for it was made, and thus we will not include these variables in our prediction. There are many variables that we will consider using in our prediction including, yards to go, position on the field, weather, the location of the game, etc. Time permitting, we will explore using recurrent neural networks (RNNs) to also take into account a team’s past offensive performance and the opponent’s past defensive performance to make this prediction.

**Response Value:**

We will evaluate three choices for a given team: attempt to convert a fourth down, attempt to punt on a fourth down, and attempt to kick a field goal. We will examine what is the likely net effect this decision will have on the score of the game as follows:

*Fourth Down Conversion:* The probability that the fourth down conversion will be successful, multiplied by the expected outcome of the remainder of the drive, plus the probability that the fourth down conversion will not be successful times the expected outcome of the opponent’s drive starting from the spot of the turnover.

*Punt:* The expected outcome of the opponent’s drive starting from the expected destination of the punt.

*Field Goal:* The probability that the kick is good multiplied by the resulting outcome (+3 points), plus the probability that the kick is no good multiplied by the expected outcome of the opponent’s drive starting from the spot of the kick.

# Challenges with Dataset

With a dataset of this magnitude and complexity there are many technical issues that we have run into. To start, we had trouble finding the adequate dataset as the NFL does not provide play-by-play data explicitley. Instead, third-parties have used web-scraping to extract the data and provide it to us. In doing so we had to be careful in what dataset we selected. 

After choosing a dataset we also have to understand the dataset and decide what is relevant and what variables can actually be used in our prediction. Many variables are already predictions from the variables within the same dataset and we need to ensure we are not using any of these variables in predictions.

In addition, one challenge we may face is the time period we choose to use. We currently plan on analyzing NFL statistics from 2010-2020, but we could also consider all data from 2000-2020. Our current understanding is that NFL gameplay strategy has evolved since the early 2000s, and looking at data that is earlier than the most recent decade may incorporate gameplay statistics that are irrelevant and outdated to current gameplay. After initial analysis, we may take a broader look at these statistics, but our team generally may face challenges in determining at what point data becomes relevant. 


# Why This is Important

This analysis is important to coaches, players, and fans for many reasons. Money and resources should be spent on this project because it can help football decision makers appropriately determine the best course of action which could then lead to winning the game. The decision to punt, go for a field goal, or attempt a 4th down conversion can be the determining factor in if a team is going to win a game. Winning the game for a team is clearly the goal because the more games they win, the better of a chance they have of going to the playoffs and then making it to the Super Bowl. If a team wins a game during the playoff, they make more money. Fans, coaches and players will all be pleased, so it is an obviously good spot to be in. Anything that could potentially help teams get to this  spot should be something that you would want to invest in. In addition to this, sports analytics is a huge industry. By being able to appropriately make decisions this model would be of great use to any team and sports analytics companies.

# Data Retrieval
```{r}
#install.packages("devtools")
#install.packages("nflfastR")
#install.packages("gsisdecoder")
#install.packages("cli")
library(cli)
library(nflfastR)
future::plan("multisession")
pbp <- load_pbp(2010:2020, file_type = "qs")
```