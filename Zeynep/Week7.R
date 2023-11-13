#Week 7

# Tasks: make a jitter plot, scatterplot with labeled points, barplot with text over the bars, use facet_wrap in a plot, make all plots with title, subtitle, caption, and every aesthetic has a title

library(ggplot2)
library(tidyverse)
library(gapminder)
library(dplyr)

gap_07 = 
  gapminder |> 
  filter(year == 2007)

# Jitter Plot

ggplot(gapminder, aes(x = continent, y = lifeExp)) +
  geom_jitter() + labs(title = "Jitter Plot of Life Expectancy by Continent",
       subtitle = "Data from 1952 to 2007 ", caption = "Source: Gapminder",
       x = "Continent", y = "Life Expectancy")

ggsave(filename = "figures/Zeynep Plots/Week_7/JitterPlot.pdf")

# Scatterplot with labeled points

ggplot(gap_07, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  geom_text(aes(label = country), check_overlap = TRUE, vjust = -1) +
  labs(title = "GDP per Capita vs Life Expectancy", subtitle = "Data from 2007",
   caption = "Source: Gapminder", x = "GDP per Capita", y = "Life Expectancy")

ggsave(filename = "figures/Zeynep Plots/Week_7/ScatterPlot.pdf")

# Barplot

gap_07 %>%
  group_by(continent) %>% summarize(mean_lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x = continent, y = mean_lifeExp, label = round(mean_lifeExp, 1))) +
  geom_bar(stat = "identity") + geom_text(vjust = -0.3) +
  labs(title = "Average Life Expectancy by Continent", subtitle = "Data from 2007",
       caption = "Source: Gapminder", x = "Continent", y = "Average Life Expectancy")

ggsave(filename = "figures/Zeynep Plots/Week_7/BarPlot.pdf")

# Facet_wrap

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + facet_wrap(~continent) +
  labs(title = "Life Expectancy by Continent", subtitle = "Data from 1952 to 2007",
       caption = "Source: Gapminder", x = "GDP Per Capita", y = "Life Expectancy")


ggsave(filename = "figures/Zeynep Plots/Week_7/Facet_Wrap.pdf")
