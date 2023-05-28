library(ggplot2)
library(dplyr)
library(tidytext)
library(highcharter)
library(xts)
library(viridis)
library(prophet)
library(dygraphs)

Year12_16 <- cc[cc$Year2 %in% c("2012", "2013", "2014", "2015", "2016"),]

df2 <- Year12_16 %>% group_by(Date2) %>% summarise(y = n()) %>% mutate(y = log(y))
names(df2) <- c("ds", "y")
df2$ds <- factor(df2$ds)

ind <- sample(2, nrow(df2), replace = TRUE, prob = c(0.7,0.3))

train_size <- nrow(df2) *0.7

trainData <- df2[1:train_size,]
testData <- df2[(train_size+1):nrow(df2),]

m2 <- prophet(trainData)

p2 <- predict(m2, testData)

plot(m2, p2)


df.cv <- cross_validation(m2, initial = 540, period = 90, horizon = 180, units = 'days')
head(df.cv)
df.p <- performance_metrics(df.cv)
head(df.p)
plot_cross_validation_metric(df.cv, metric = "mse")
plot_cross_validation_metric(df.cv, metric = "rmse")
plot_cross_validation_metric(df.cv, metric = "mae")
plot_cross_validation_metric(df.cv, metric = "mape")
