#### POL298 Assignment 7 - Week 8: Maps ####

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

set.seed(1986)

data("elections")

### make a country level world map or region map where countries filled 
### by some variable using rnaturalearth, make a county-level chloropleth 
### map using county_data from socviz

#### Country-Level World Map ####
WorldMapunfilled <- ne_countries(scale = "medium", returnclass = "sf")

WorldData <- (WorldMapunfilled$gdp_md_est)

WorldMap <- ggplot(data = WorldMapunfilled) +
  geom_sf(aes(fill = WorldData)) +
  scale_fill_viridis_c() + 
  theme_minimal() +
  labs(fill = "GDP (Million USD)", title = "Countries of the World by GDP")
ggsave("WorldMap.pdf", plot = WorldMap)

#### Country-Level Chloropleth Map ####
data("county_data")

countydat <- left_join(county_map, county_data, by = "id")

ChloroplethMap <- ggplot(data = countydat, mapping = aes(x = long, y = lat, fill = pct_black, group = group)) +
  geom_polygon(color = "gray90", size = 0.05) +
  coord_equal() +
  scale_fill_brewer(palette="Blues") +
  labs(title = "County-level Percentage of Black Population in US", 
       fill = "Black Population") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_map() +
  theme(legend.position = "bottom")
ggsave("ChloroplethMap.pdf", plot = ChloroplethMap)
