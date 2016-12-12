# ETM
## Basketball Metric Development

This README file describes the data and the code used to develop the end-of-game tactics metric (ETM). This project started in MATLAB for a class project, where much of the data cleansing and manipulation took place. Since then, I have converted the project to R and have only been working with the cleaner data. 

### Win probability model

`NBA_WPM.R` builds a logistic regression model that gives the win probability of a team in the last three minutes of an NBA team given possession, time remaining, score differential, and the Las Vegas point spread. This code imports play-by-play data files `situationxxyy.mat`, which contains the matrix `newest.situation` and is a cleansed version of the raw play-by-play text data. The `xx` and `yy` part of the `.mat` file name represent the last two digits of the year for the NBA season (i.e. 0506 is the play-by-play data from the 2005-2006 season). This win probability serves as the foundation of ETM. 

### ETM Development

`simulate_season.R`
