#### POL298 Assignment 4 ####

setwd("/Users/richardkornrumpf/Documents/GitHub/directed-dataviz/Richard")

library(ggplot2)
library(wesanderson)
library(devtools)
library(fivethirtyeight)
library(AppliedPredictiveModeling)
library(dplyr)
library(tidyr)
library(GGally)

set.seed(1986)

#### Scatterplot #### 
data(abalone)
subsetabalone <- abalone[abalone$Type %in% c('M', 'F'), ]

Scatterplot <- ggplot(subsetabalone, aes(x = WholeWeight, y = LongestShell)) +
  geom_point(aes(color = Type), size = 3, alpha = 0.5) + 
  labs(x = "Total Weight", y = "Shell Length", color = "Sex", title = "Relationship Between Length and Weight of the Abalone Mollusk") +
  theme_minimal()
ggsave("Scatterplot.pdf", plot = Scatterplot)

#### Correlogram #### 
Correlogram <- ggcorr(subsetabalone, method = c("everything", "pearson"), label = TRUE) +
  ggtitle("Correlation between Traits of the Abalone Mollusk") + theme(
  legend.position = "top", legend.direction = "horizontal")
ggsave("Correlogram.pdf", plot = Correlogram)
  
#### Time Series #### 
data(congress_age)

subsetcongressage <- congress_age %>%
  filter(party %in% c('D', 'I', 'R')) %>%
  group_by(party, chamber) %>%
  filter(n() > 1)

congressavgage <- subsetcongressage %>%
  group_by(congress) %>%
  summarize(averageage = mean(age, na.rm = TRUE))

TimeSeries <- ggplot(congressavgage, aes(x = congress, y = averageage)) +
  geom_line(color = "red") + theme_minimal() + labs(x = "Congressional Session", y = "Average Age",
  title = "Average Age of Congress Over Time")
ggsave("TimeSeries.pdf", plot = TimeSeries)
  
#### Multiple Time Series ####
congressavgageparty <- subsetcongressage %>%
  filter(party %in% c("D", "R")) %>%
  group_by(congress, party) %>%
  summarize(averageage = mean(age, na.rm = TRUE)) %>%
  spread(key = party, value = averageage, fill = NA) %>%
  rename(averageagedem = D, averageagerep = R) %>%
  select(congress, averageagedem, averageagerep)

MultipleTimeSeries <- ggplot(congressavgageparty) + geom_line(aes(x = congress, y = averageagedem, 
  color = "Democrats"), size = 1) + geom_line(aes(x = congress, y = averageagerep, color = "Republicans"),
  size = 1) + scale_color_manual(values = c("Democrats" = "blue", "Republicans" = "red")) +
  geom_text(data = subset(congressavgageparty, congress == max(congress, na.rm = TRUE)), 
  aes(x = congress, y = averageagedem, label = "Democrats"), nudge_y = 1, color = "blue") +
  geom_text(data = subset(congressavgageparty, congress == max(congress, na.rm = TRUE)), 
  aes(x = congress, y = averageagerep, label = "Republicans"), nudge_y = -1, color = "red") +
  theme_minimal() + labs(x = "Congressional Session", y = "Average Age",
  title = "Average Age in Congress by Party", color = NULL) + theme(legend.position = "none")
ggsave("MultipleTimeSeries.pdf", plot = MultipleTimeSeries)


