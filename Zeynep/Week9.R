### Week 9 Assignment ###

## Tasks: make a plot using a non-default color or fill scale, make one using a sequential scale, one using a discrete scale, a plot where you use the fact that geometries can have local aesthetics to highlight specific points or bars


# Load libraries
library(ggplot2)
library(gapminder)
library(dplyr)


#Filter gapminder by year
gap_2007 <- gapminder %>% filter(year == 2007)


## Make a plot with a non-default fill scale
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(color = "#E69F00") +
  labs(title = "Life Expectancy vs Gdp Per Capita",
       x = "GDP Per Capita", y = "Life Expectancy", caption = "Source: Gapminder") + theme_minimal()

ggsave(filename = "figures/Zeynep Plots/Week_9/Non-DefaultColor.pdf")


## Make a plot with sequential scale

ggplot(gapminder, aes(x = log(gdpPercap), y = lifeExp, color = year, shape = continent)) +
  geom_point() +
  scale_color_gradient(low = "green", high = "red") +
  labs(title = "Life Expectancy vs GDP per Capita Over Years",
       x = "Logged GDP per Capita", y = "Life Expectancy", caption = "Source: Gapminder") + theme_minimal()

ggsave(filename = "figures/Zeynep Plots/Week_9/SequentialScale.pdf")


## Make a plot with a discrete color scale

# Calculate average life expectancy per continent
avg_lifeExp_2007 <- aggregate(lifeExp ~ continent, gap_2007, mean)

ggplot(avg_lifeExp_2007, aes(x = continent, y = lifeExp, fill = continent)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Average Life Expectancy per Continent in 2007",
       x = "Continent", y = "Average Life Expectancy", caption = "Source: Gapminder") + theme_minimal()

ggsave(filename = "figures/Zeynep Plots/Week_9/DiscreteColor.pdf")


## Make a plot with different Local Aesthetics
ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent)) +
  geom_label(data = subset(gap_2007, gdpPercap > 40000), aes(label = country), vjust = -1) +
  labs(title = "Worldwide GDP Per Capita and Life Expectancy in 2007", subtitle = "Labeled countries with GDP per Capita higher than $40,000",
       x = "GDP per Capita", y = "Life Expectancy") + theme_minimal()

ggsave(filename = "figures/Zeynep Plots/Week_9/Dif_Aesthetics.pdf")

