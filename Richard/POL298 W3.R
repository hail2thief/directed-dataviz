#### POL298 Assignment 2 ####
### Make a barplot, stacked bar plot, heat map, and nested area plot.
library(ggplot2)
library(AppliedPredictiveModeling)
library(wesanderson)

set.seed(1986)
data(abalone)

# making new categorical variable for color
n_observations <- nrow(abalone)
color_values <- sample(c('grey', 'brown'), size = n_observations, replace = TRUE)
abalone$color <- color_values

head(abalone)

### Bar Plot
BarPlot <- ggplot(abalone, aes(y = Type, x = WholeWeight, fill = Type)) + 
  geom_col() +
  scale_fill_manual(values = wes_palette("Zissou1", n = 3, type = "discrete"),
                    breaks = c("F", "I", "M"), 
                    labels = c("Female", "Unidentified", "Male")) +  
  labs(title = "Total Weight by Type of Abalone Mollusk", x = "Total Weight with Shell") +
  theme_classic()
ggsave("BarPlot.pdf", plot = BarPlot)

### Stacked Bar Plot
StackedBarPlot <- ggplot(abalone, aes(x = Type, y = WholeWeight, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = wes_palette("Zissou1", n = 2, type = "discrete"),
                    name = "Color",  
                    labels = c("Brown", "Grey")) +  
  labs(title = "Comparison of Total Weight to Type and Color",
       x = "Type",
       y = "Total Weight") +
  theme_classic()
ggsave("StackedBarPlot.pdf", plot = StackedBarPlot)

### Heat Maps
HeatMap <- ggplot(abalone, aes(x = WholeWeight, y = Type, fill = ShuckedWeight)) +
  geom_tile() +
  scale_fill_gradientn(colors = wes_palette("Zissou1", n = 5, type = "continuous"),name = "Shucked Weight") +
  labs(title = "Heat Map Comparing Shucked and Total Weight by Type", 
       x = "Total Weight", 
       y = "Type") +
  theme_classic()
ggsave("HeatMap.pdf", plot = HeatMap)

FacetedHeatMap <- ggplot(abalone, aes(x = WholeWeight, y = Type, fill = ShuckedWeight)) +
  geom_tile() +
  scale_fill_gradientn(colors = wes_palette("Zissou1", n = 5, type = "continuous"),
                       name = "Shucked Weight") +  
  labs(title = "Faceted Heat Map Comparing Shucked and Total Weight by Type and Color",
       x = "Total Weight",
       y = "Type") +
  theme_classic() +
  facet_wrap(~ color, labeller = labeller(color = c("brown" = "Brown", "grey" = "Grey")))
ggsave("FacetedHeatMap.pdf", plot = FacetedHeatMap)

### Stacked Area Plot
StackedAreaPlot <- ggplot(abalone, aes(x = Rings, y = ShuckedWeight, fill = Type, group = Type)) +
  geom_area(alpha = 0.6, position = 'stack') +
  scale_fill_manual(values = wes_palette("Zissou1", n = nlevels(abalone$Type)),
                    labels = c("Female", "Unidentified", "Male")) +  
  labs(title = "Shucked Weight of the Abalone Mollusk by Age and Type",
       x = "Age",
       y = "Shucked Weight") +
  theme_classic()
ggsave("StackedAreaPlot.pdf", plot = StackedAreaPlot)

