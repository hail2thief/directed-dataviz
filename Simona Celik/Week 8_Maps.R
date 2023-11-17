# Load necessary libraries
library(ggplot2)
library(maps)
library(here)
library(dplyr)

# Sample data from the 'election' dataset
election_sample <- election %>%
  select(state, total_vote, r_points, pct_trump, party, census) %>%
  sample_n(5)

# Section 1: State-Level Map
# Create a ggplot object (p0) for the state-level map
p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat,
                           group = group, fill = party))

# Add polygons with gray border and specify projection
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

# Customize fill colors and remove legend title
p2 <- p1 + scale_fill_manual(values = party_colors) +
  labs(title = "Election Results 2016", fill = NULL)

# Display the map with a clean theme
p2 + theme_void()

# Save the plot in the "Simona Celik" directory
ggsave(filename = here("Simona Celik", "state_level_map.png"), p2 + theme_void(), width = 8, height = 6, dpi = 300)

# Section 2: County-Level Choropleth Map
# Create a ggplot object (p) for the county-level choropleth map
p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat,
                          fill = pop_dens, 
                          group = group))

# Add polygons with gray border and equal coordinate scales
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

# Customize fill colors using Brewer palette and set legend labels
p2 <- p1 + scale_fill_brewer(palette = "Blues",
                             labels = c("0-10", "10-50", "50-100", "100-500",
                                        "500-1,000", "1,000-5,000", ">5,000"))

# Add labels, adjust theme, and customize legend position
p3 <- p2 + labs(fill = "Population per\nsquare mile") +
  theme_void() +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")

# Save the plot as an image file in the "Simona Celik" directory
ggsave(filename = here("Simona Celik", "county_level_choropleth.png"), p3, width = 10, height = 8, dpi = 300)

# Section 3: USA Map
# Get map data for the USA and states
usa <- map_data("usa")
states <- map_data("state")

# Display dimensions of the data frames
dim(usa)    # Dimensions of the USA map data
dim(states) # Dimensions of the states map data

# Create a ggplot object for the state-level map of the USA
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +  # Adjust aspect ratio
  guides(fill = FALSE) +  # Remove the color legend to leave off the color legend
  ggtitle("USA Map")  # Add the title

# Save the plot as an image file in the "Simona Celik" directory
ggsave(filename = here("Simona Celik", "usa_map.png"), width = 8, height = 6, dpi = 300)


# Section 4: World Map Visualization
# Install and load packages
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("ggplot2")
library("cowplot")
library("googleway")
library("ggrepel")
library("ggspatial")
library("libwgeom")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create a world map 
world_map <- ggplot(data = world) + geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))

ggsave(filename = here("Simona Celik", "world_map.png"), plot = world_map, width = 8, height = 6, dpi = 300)

# Choropleth Map with Population Estimate and save it
choropleth_map <- ggplot(data = world) + geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggsave(filename = here("Simona Celik", "choropleth_map.png"), plot = choropleth_map, width = 8, height = 6, dpi = 300)

# Get map data for Slovenia
map_data_svn <- map_data('world')[map_data('world')$region == "Slovenia", ]

# Create a map of Slovenia
slovenia_map <- ggplot() +
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               color = '#9c9c9c', fill = '#f3f3f3') +
  geom_polygon(data = map_data_svn,
               aes(x = long, y = lat, group = group),
               color = 'red', fill = 'blue') +
  coord_map() +
  coord_fixed(1.3,
              xlim = c(13, 17),
              ylim = c(45.5, 47.5)) +
  ggtitle("Zemljevid Slovenije") +
  theme(plot.background = element_rect(fill = 'white'))

ggsave(filename = here("Simona Celik", "slovenia_map.png"), plot = slovenia_map, width = 8, height = 6, dpi = 300)

# Create a map of Bosnia 
bosnia_map <- ggplot() +
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               color = '#9c9c9c', fill = '#f3f3f3') +
  geom_polygon(data = map_data_bosnia,
               aes(x = long, y = lat, group = group),
               color = 'red', fill = 'blue') +
  coord_map() +
  coord_fixed(1.3,
              xlim = c(16, 20),
              ylim = c(42, 45.5)) +
  ggtitle("Karta Bosne i Hercegovine") +
  theme(plot.background = element_rect(fill = 'white'))

ggsave(filename = here("Simona Celik", "bosnia_map.png"), plot = bosnia_map, width = 8, height = 6, dpi = 300)

# Create a map of Bosnia with custom colors and save it
bosnia_custom_colors_map <- ggplot() +
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               color = '#9c9c9c', fill = '#f3f3f3') +
  geom_polygon(data = map_data_bosnia,
               aes(x = long, y = lat, group = group),
               color = 'white', fill = 'blue', size = 0.5) +
  coord_map() +
  coord_fixed(1.3,
              xlim = c(16, 20),
              ylim = c(42, 45.5)) +
  ggtitle("Karta Bosne i Hercegovine") +
  theme(plot.background = element_rect(fill = 'yellow'))

ggsave(filename = here("Simona Celik", "bosnia_custom_colors_map.png"), plot = bosnia_custom_colors_map, width = 8, height = 6, dpi = 300)

# Create a map of Denmark 
denmark_map <- ggplot() +
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               color = '#9c9c9c', fill = '#f3f3f3') +
  geom_polygon(data = map_data_denmark,
               aes(x = long, y = lat, group = group),
               color = 'red', fill = 'white') +
  coord_map() +
  coord_fixed(1.3,
              xlim = c(8, 15),
              ylim = c(54.5, 58)) +
  ggtitle("Danske Kort") +
  theme(plot.background = element_rect(fill = 'white'))

ggsave(filename = here("Simona Celik", "denmark_map.png"), plot = denmark_map, width = 8, height = 6, dpi = 300)

