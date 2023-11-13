#### POL298 Assignment 6 - Week 7: Annotations ####

setwd("/Users/richardkornrumpf/Documents/GitHub/directed-dataviz/Richard")

library(ggplot2)
library(wesanderson)
library(devtools)
library(dplyr)
library(tidyr)
library(remotes)
remotes::install_github('hail2thief/juanr')
library(juanr)
library(broom)

set.seed(1986)

data("elections")
head(elections)

### make a jitter plot, scatterplot with labeled points, barplot with text 
### over the bars, use facet_wrap in a plot, make all plots with title, 
### subtitle, caption, and every aesthetic has a title

#### Jitter Plot w/ title, subtitle, caption, and aesthetic titles ####
JitterPlot <- ggplot(elections, aes(x = census_region, y = per_dem_2020)) +
  geom_jitter(alpha=.6, width = 0.2, height = 0.2, aes(color = census_region), size = 2.5, shape = 16, alpha = 0.7) +
  labs(title = "Jitter Plot of Democratic Vote Share by Census Region",
       subtitle = "Data distributed with jitter for clarity",
       caption = "Source: 'elections' dataset",
       x = "Census Region",
       y = "Democratic Vote Share in 2020",
       color = "Census Region") +
  theme_minimal()
ggsave("JitterPlot.pdf", plot = JitterPlot)

#### Scatterplot using facet_wrap w/ labeled points, title, subtitle, caption, and aesthetic titles ####
ScatterplotFaceted <- ggplot(elections %>% filter(!is.na(per_dem_2020) & !is.na(black)), aes(x = black, y = per_dem_2020, color = census_region)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess") +
  facet_wrap(~ census_region, scales = "free") +
  labs(title = "Relationship between Black Population and Democratic Vote Share",
       subtitle = "Faceted by Census Region, excluding NAs",
       caption = "Source: 'elections' dataset",
       x = "Percentage of Black Population",
       y = "Democratic Vote Share in 2020",
       color = "Census Region") +
  theme_minimal()
ggsave("ScatterplotFaceted.pdf", plot = ScatterplotFaceted)

#### Barplot w/ text over the bars, title, subtitle, caption, and aesthetic titles ####
# avg dem vote share by region
avgvoteshare <- elections %>%
  group_by(census_region) %>%
  summarize(avgvoteshare = mean(per_dem_2020, na.rm = TRUE))

BarplotwithText <- ggplot(avgvoteshare, aes(x = census_region, y = avgvoteshare, fill = census_region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avgvoteshare, 2)), vjust = -0.5) + 
  labs(title = "Average Democratic Vote Share by Census Region",
       subtitle = "Based on 2020 election data",
       caption = "Source: 'elections' dataset",
       x = "Census Region",
       y = "Average Democratic Vote Share in 2020") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")
ggsave("BarplotwithText.pdf", plot = BarplotwithText)


