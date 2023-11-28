#### POL298 Assignment 8 - Week 9: Themes and Color ####

setwd("/Users/richardkornrumpf/Documents/GitHub/directed-dataviz/Richard")

library(ggplot2)
library(ggthemes)
library(wesanderson)
library(devtools)
library(dplyr)
library(tidyr)
library(remotes)
remotes::install_github('hail2thief/juanr')
library(juanr)
library(rnaturalearth)
library(rnaturalearthdata)
library(socviz)
library(sp)
library(sf)
library(RColorBrewer)
library(viridis)
library(paletteer)

set.seed(1986)

data("elections")

### make a plot using a non-default color or fill scale, 
### make one using a sequential scale, one using a discrete scale, 
### a plot where you use the fact that geometries can have local aesthetics 
### to highlight specific points or bars

#### Plot sequential scale using non-default color or fill scale ####
SequentialPlot <- ggplot(elections %>% 
         filter(!is.na(per_dem_2020) & !is.na(hh_income) & !is.na(white)), 
       aes(x = white, y = per_dem_2020, color = hh_income)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  scale_color_distiller(palette = "YlGnBu", direction = 1) +  
  labs(title = "Relationship between White Population,  Household Income, and Democratic Vote Share",
       caption = "Source: 'elections' dataset",
       x = "Percentage of White Population",
       y = "Democratic Vote Share in 2020",
       color = "Household Income") +
  theme_minimal()
ggsave("SequentialPlot.pdf", plot = SequentialPlot)

#### Plot discrete scale using non-default color or fill scale ####
DiscretePlot <- ggplot(elections %>% 
         filter(!is.na(per_dem_2020) & !is.na(hh_income)), 
       aes(x = hh_income, y = per_dem_2020, color = census_region)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1") +  
  labs(title = "Relationship between Household Income and Democratic Vote Share by Census Region",
       caption = "Source: 'elections' dataset",
       x = "Household Income",
       y = "Democratic Vote Share in 2020",
       color = "Census Region") +  
  theme_minimal()
ggsave("DiscretePlot.pdf", plot = DiscretePlot)

#### Plot using local geometry to highlight specific points ####
highlight_threshold <- .60

LocalGeomPlot <- ggplot(elections %>% 
         filter(!is.na(per_dem_2020) & !is.na(hh_income)), 
       aes(x = hh_income, y = per_dem_2020, color = census_region)) +
  geom_point(alpha = 0.1) +
  geom_point(data = filter(elections, per_dem_2020 > highlight_threshold), 
             aes(x = hh_income, y = per_dem_2020), 
             shape = 17, alpha = 0.3, size = 2) +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Relationship between Household Income and Democratic Vote Share by Census Region",
       subtitle = paste("Areas with more than 60% Democratic vote share displayed as triangles"),
       caption = "Source: 'elections' dataset",
       x = "Household Income",
       y = "Democratic Vote Share in 2020",
       color = "Census Region") +  
  theme_minimal()
ggsave("LocalGeomPlot.pdf", plot = LocalGeomPlot)
