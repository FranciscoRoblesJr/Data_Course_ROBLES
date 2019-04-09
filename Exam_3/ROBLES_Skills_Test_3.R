#### load packages ####
library(tidyverse)
library(gapminder)


#### setting variables ####
pal = c("#6b5456","#ec8d1b","#6abf2a","#8b53b7","#70acbe","#01c95b","#c00014","#31332f","#f7d000","#abba00")
plot1.data = gapminder


#### plot 1 ####
ggplot(plot1.data, aes(year, lifeExp, color = continent)) +
  theme_light() +
  geom_point(alpha = 0.25) +
  geom_smooth(se = F) +
  scale_color_manual(values = pal) +
  theme(panel.border = element_blank()) +
  labs(x = 'Year', y = 'Life Expectancy', color = 'Continent',
       title = 'Life Expectancy Over Time',
       subtitle = 'Colored by Continent')
ggsave('ROBLES_plot1.png', dpi = 300, width = 5, height = 5, units = 'in')


#### plot 2 ####
set.seed(123)
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
plot2.data <- rbind(a,b,c)

ggplot(plot2.data, aes(x, y)) +
  geom_bin2d() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
ggsave('ROBLES_plot2.png', dpi = 300, width = 5, height = 5, units = 'in')

