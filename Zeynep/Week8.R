### Week 8 ###
# Tasks: make a country level world map or region map where countries filled by some variable using rnaturalearth, make a county-level chloropleth map using county_data from socviz

install.packages("rnaturalearth")

library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(gapminder)

# Extract map data
world_map <- ne_countries(type= "countries", scale = "medium", returnclass = "sf")

#Filter gapminder
gap_2007 <- gapminder %>% filter(year == 2007)

# Merge datasets
merged_data <- world_map %>% left_join(gap_2007, by = c("name" = "country"))

#Plot life expectancy on map
ggplot(data = merged_data) +
  geom_sf(aes(fill = lifeExp)) +
  theme_minimal() +
  labs(fill = "Life Expextancy", title = "Worldwide Life Expectancy on Map",
       subtitle = "Data from 2007 ", caption = "Source: Gapminder")

ggsave(filename = "figures/Zeynep Plots/Week_8/WorldMap.pdf")

################# make county level map #############

install.packages("socviz")
library(socviz)

#Load the datasets

county_map <- socviz::county_map
county_data <- socviz::county_data 


####
county_data <- na.omit(county_data)

county_full <- left_join(county_map, county_data, by = "id")
county_full <- na.omit(county_full)

ggplot(data = county_full,
           mapping = aes(x = long, y = lat, fill = pct_black,
                         group = group)) +
  geom_polygon(color = "gray90", size = 0.05) + coord_equal() +
  scale_fill_brewer(palette="Yellows") + labs(title = "County Level Black Population Density in the US", subtitle = "By Percentage", fill = "Percentage")

ggsave(filename = "figures/Zeynep Plots/Week_8/CountyMap.pdf")
