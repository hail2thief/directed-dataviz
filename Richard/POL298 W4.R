#### POL298 Assignment 3 - Week 4: Distributions ####

setwd("/Users/richardkornrumpf/Documents/GitHub/directed-dataviz/Richard")

library(ggplot2)
library(wesanderson)
library(devtools)
install_github("jorvlan/raincloudplots")
library(ggrain)
library(fivethirtyeight)
library(dplyr)

set.seed(1986)

### Make a histogram, density graph, grouped version of each (e.g, using fill), boxplot, violin plot

data(congress_age)

filtered_data <- congress_age %>%
  filter(party %in% c('D', 'I', 'R')) %>%
  group_by(party, chamber) %>%
  filter(n() > 1)

## Histogram
Histogram <- ggplot(filtered_data, aes(x=age)) + 
  geom_histogram(binwidth = 2, fill = "#B3A369", color = "#002855", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Age Distribution of Members of Congress",
       y = "Frequency",
       x = "Age")
ggsave("Histogram.pdf", plot = Histogram)

## Density Graph
DensityGraph <- filtered_data %>%
  ggplot(aes(x=age)) +
  geom_density(fill="#B3A369", color="#002855", alpha=0.8) +
  theme_minimal() +
  labs(title = "Age Distribution of Members of Congress",
       y = "Density",
       x = "Age")
ggsave("DensityGraph.pdf", plot = DensityGraph)

## Grouped Versions
# Grouped Histogram
GroupedHistogram <- filtered_data %>%
  ggplot(aes(x=age, fill=party)) +
  geom_histogram(position="identity", alpha=0.6, binwidth=2) +
  scale_fill_manual(values = c("D" = "blue", "R" = "red", "I" = "yellow"),
                    name = "Party",
                    breaks = c("D", "I", "R"),
                    labels = c("Democrat", "Independent", "Republican")) +
  theme_minimal() +
  labs(title = "Grouped Age Distribution of Members of Congress",
       y = "Frequency",
       x = "Age")
ggsave("GroupedHistogram.pdf", plot = GroupedHistogram)

# Grouped Density Graph
GroupedDensityGraph <- filtered_data %>%
  ggplot(aes(x=age, fill=party)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values = c("D" = "blue", "R" = "red", "I" = "yellow"),
                    name = "Party",
                    breaks = c("D", "I", "R"),
                    labels = c("Democrat", "Independent", "Republican")) +
  theme_minimal() +
  labs(title = "Grouped Age Density of Members of Congress",
       y = "Density",
       x = "Age")
ggsave("GroupedDensityGraph.pdf", plot = GroupedDensityGraph)

## Boxplot
Boxplot <- filtered_data %>%
  ggplot(aes(x=party, y=age, fill=party)) +
  geom_boxplot(alpha=0.6) +
  scale_fill_manual(values = c("D" = "blue", "R" = "red", "I" = "gray"),
                    name = "Party", 
                    breaks = c("D", "I", "R"),
                    labels = c("Democrat", "Independent", "Republican")) +
  scale_x_discrete(labels = c(D = "Democrat", I = "Independent", R = "Republican")) +
  theme_minimal() +
  labs(title = "Age Distribution of Members of Congress by Party",
       y = "Age",
       x = "Party")
ggsave("Boxplot.pdf", plot = Boxplot)

## Violin Plot
ViolinPlot <- filtered_data %>%
  ggplot(aes(x=party, y=age, fill=party)) +
  geom_violin(alpha=0.6) +
  scale_fill_manual(values = c("D" = "blue", "R" = "red", "I" = "gray"),
                    name = "Party",
                    breaks = c("D", "I", "R"),
                    labels = c("Democrat", "Independent", "Republican")) +
  scale_x_discrete(labels = c(D = "Democrat", I = "Independent", R = "Republican")) +
  theme_minimal() +
  labs(title = "Age Distribution of Members of Congress by Party",
       y = "Age",
       x = "Party")
ggsave("ViolinPlot.pdf", plot = ViolinPlot)

## Raincloud Plot
RaincloudPlot <- ggplot(filtered_data, aes(x = party, y = age, fill = chamber)) + 
  geom_rain() +
  theme_minimal() +
  labs(title = "Age Distribution of Members of Congress by Party and Chamber",
       y = "Age",
       x = "Party",
       fill = "Chamber") +
  scale_x_discrete(labels = c(D = "Democrat", I = "Independent", R = "Republican")) +
  scale_fill_discrete(labels = c(house = "House", senate = "Senate"))
ggsave("RaincloudPlot.pdf", plot = RaincloudPlot)



