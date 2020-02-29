library(ggplot2)
library(mice)
library(VIM)
library(tidyverse)
library(cowplot)
library(car)
library(knitr)
library(caTools)


### Load Data
moneyball_df <- read.csv("https://raw.githubusercontent.com/soumya2g/CUNYDataMiningHomeWork/master/HomeWork_1/DataFiles/moneyball-training-data.csv", 
                         header = TRUE,
                         row.names = 1)

## Counts of missing data per feature
apply(moneyball_df, 2, function(x) length(which(is.na(x)))) %>% kable()

## Per centage of missing data per feature
pMiss <- function(x) {sum(is.na(x)) / length(x) * 100}
apply(moneyball_df, 2, pMiss) %>% kable()


## Box plots:
gb1 <- ggplot(data = moneyball_df, aes(y = TARGET_WINS)) + geom_boxplot()
gb2 <- ggplot(data = moneyball_df, aes(y = TEAM_BATTING_H)) + geom_boxplot()
gb3 <- ggplot(data = moneyball_df, aes(y = TEAM_BATTING_2B)) + geom_boxplot()
gb4 <- ggplot(data = moneyball_df, aes(y = TEAM_BATTING_3B)) + geom_boxplot()
gb5 <- ggplot(data = moneyball_df, aes(y = TEAM_BATTING_HR)) + geom_boxplot()
gb6 <- ggplot(data = moneyball_df, aes(y = TEAM_BATTING_BB)) + geom_boxplot()
gb7 <- ggplot(data = moneyball_df, aes(y = TEAM_BATTING_HBP)) + geom_boxplot()
gb8 <- ggplot(data = moneyball_df, aes(y = TEAM_BATTING_SO)) + geom_boxplot()
gb9 <- ggplot(data = moneyball_df, aes(y = TEAM_BASERUN_SB)) + geom_boxplot()
gb10 <- ggplot(data = moneyball_df, aes(y = TEAM_BASERUN_CS)) + geom_boxplot()
gb11 <- ggplot(data = moneyball_df, aes(y = TEAM_FIELDING_E)) + geom_boxplot()
gb12 <- ggplot(data = moneyball_df, aes(y = TEAM_FIELDING_DP)) + geom_boxplot()
gb13 <- ggplot(data = moneyball_df, aes(y = TEAM_PITCHING_BB)) + geom_boxplot()
gb14 <- ggplot(data = moneyball_df, aes(y = TEAM_PITCHING_H)) + geom_boxplot()
gb15 <- ggplot(data = moneyball_df, aes(y = TEAM_PITCHING_HR)) + geom_boxplot()
gb16 <- ggplot(data = moneyball_df, aes(y = TEAM_PITCHING_SO)) + geom_boxplot()

plot_grid(gb1, gb2, gb3, gb4, gb5, gb6, gb7, gb8, gb9, gb10,
          gb11, gb12, gb13, gb14, gb15, gb16, labels = "AUTO, scale = 10")

## Remove the feature: TEAM_BATTING_HBP
moneyball_df_model  <- moneyball_df %>% select(-TEAM_BATTING_HBP)

## Plotting the missing data pattern after removing the max. missing feature already

aggr_plot <- aggr(moneyball_df_model, col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

## Imputing the missing values

### Actual Imputation

moneyball_df_model_imputed <- mice(data = moneyball_df_model, m = 1,
                                   method = "pmm", maxit = 5, seed = 500)

moneyball_df_model_imputed_df1 <- mice::complete(moneyball_df_model_imputed, 1)

#### Test train approach
set.seed(123)
split <- sample.split(moneyball_df_model_imputed_df1$TARGET_WINS, SplitRatio = 0.8)
training_set <- subset(moneyball_df_model_imputed_df1, split == TRUE)
test_set <- subset(moneyball_df_model_imputed_df1, split == FALSE)


#### Start preparing models
g1 <- lm(TARGET_WINS ~TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B +
           TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + 
           TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_H +
           TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_PITCHING_SO +
           TEAM_FIELDING_E + TEAM_FIELDING_DP, data = training_set)

vif(g1)

summary(g1)

rmse_calc <- function(actual, predicted) {
  rmse_val <- sqrt(sum((actual - predicted)^2) / length(actual))
  return(rmse_val)
}

### RMSE of first model - training dataset
rmse_calc(training_set$TARGET_WINS, predict(g1, newdata = training_set))
### RMSE of first model - test dataset
rmse_calc(test_set$TARGET_WINS, predict(g1, newdata = test_set))

# Removing : TEAM_BATTING_3B, TEAM_BASERUN_CS, TEAM_PITCHING_HR - based on p-values

g2 <- lm(TARGET_WINS ~TEAM_BATTING_H + TEAM_BATTING_2B +
           TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + 
           TEAM_BASERUN_SB + TEAM_PITCHING_H + TEAM_PITCHING_BB + TEAM_PITCHING_SO +
           TEAM_FIELDING_E + TEAM_FIELDING_DP, data = training_set)


summary(g2)

### RMSE of first model - training dataset
rmse_calc(training_set$TARGET_WINS, predict(g2, newdata = training_set))
### RMSE of first model - test dataset
rmse_calc(test_set$TARGET_WINS, predict(g2, newdata = test_set))


# Removing : TEAM_PITCHING_BB - based on p-value
g3 <- lm(TARGET_WINS ~TEAM_BATTING_H + TEAM_BATTING_2B +
           TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + 
           TEAM_BASERUN_SB + TEAM_PITCHING_H + TEAM_PITCHING_SO +
           TEAM_FIELDING_E + TEAM_FIELDING_DP, data = training_set)


summary(g3)

### RMSE of first model - training dataset
rmse_calc(training_set$TARGET_WINS, predict(g3, newdata = training_set))
### RMSE of first model - test dataset
rmse_calc(test_set$TARGET_WINS, predict(g3, newdata = test_set))
