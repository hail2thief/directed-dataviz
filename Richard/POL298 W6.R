#### POL298 Assignment 5 ####

setwd("/Users/richardkornrumpf/Documents/GitHub/directed-dataviz/Richard")

library(ggplot2)
library(wesanderson)
library(devtools)
library(dplyr)
library(tidyr)
library(remotes)
remotes::install_github('hail2thief/juanr')
library(juanr)
library(modelsummary)
library(broom)

set.seed(1986)

data("elections")
head(elections)

#### Scatterplot with Smoothing Line ####
ScatterplotLoess <- ggplot(elections, aes(x = black, y = per_dem_2020, color = census_region)) +
  geom_point() + geom_smooth(method = "loess") +
  labs(title = "Relationship between Black Population and Democratic Vote Share by Region",
  x = "Percentage of Black Population",
  y = "Democratic Vote Share in 2020",
  color = "Census Region") +
  theme_minimal()
ggsave("ScatterplotLoess.pdf", plot = ScatterplotLoess)

#### Coefficient Plot (pointrange) ####
Mod1 <- lm(per_dem_2020 ~ black, data = elections)
TidyMod1 <- tidy(Mod1, conf.int = TRUE)

CoefficientPlot <- ggplot(TidyMod1, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() + labs(title = "Coefficient Plot", x = "Coefficients", 
  y = "Estimate") + theme_minimal()
ggsave("CoefficientPlot.pdf", plot = CoefficientPlot)

#### Marginal Effect Plots ####
Mod2 <- lm(per_dem_2020 ~ black + pop + land_area + hh_income, data = elections)

black_range <- data.frame(black = seq(min(elections$black, na.rm = TRUE), 
               max(elections$black, na.rm = TRUE), length.out = 100))
black_range$pop <- mean(elections$pop, na.rm = TRUE)
black_range$land_area <- mean(elections$land_area, na.rm = TRUE)
black_range$hh_income <- mean(elections$hh_income, na.rm = TRUE)

black_range$predicted <- predict(Mod2, newdata = black_range)

MarginalEffectsPlot <- ggplot(black_range, aes(x = black, y = predicted)) + geom_line() +
  labs(title = "Marginal Effects of Black Population on Democratic Vote Share",
  x = "Percentage of Black Population", y = "Predicted Democratic Vote Share in 2020") +
  theme_minimal()
ggsave("MarginalEffectsPlot.pdf", plot = MarginalEffectsPlot)

#### Regression Table with 3 Models using 'modelsummary' package ####
Mod1 <- lm(per_dem_2020 ~ black, data = elections)

Mod3 <- lm(per_dem_2020 ~ black + white, data = elections)

Mod4 <- lm(per_dem_2020 ~ black + white + female, data = elections)

Mods <- list(Mod1, Mod3, Mod4)

modelsummary(Mods)

