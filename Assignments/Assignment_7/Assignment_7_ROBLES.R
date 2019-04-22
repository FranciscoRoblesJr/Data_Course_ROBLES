df = read.csv('../../../Data_Course/Data/mushroom_growth.csv')
library(tidyverse)

glimpse(df)
ggplot(df, aes(x = Temperature, y = GrowthRate)) + geom_point()
ggplot(df, aes(x = Nitrogen, y = GrowthRate)) + geom_point()
ggplot(df, aes(x = Light, y = GrowthRate)) + geom_point()
ggplot(df, aes(x = Species, y = GrowthRate)) + geom_boxplot()

mod1 = aov(GrowthRate ~ Species + Humidity, data = df)
summary(mod1)
mean(mod1$residuals^2)

mod2 = glm(GrowthRate ~ Light + Nitrogen + Temperature, data = df)
summary(mod2)
mean(mod2$residuals^2)

# Mod1 has a lower MSE so I'd say that it is the better model

df = modelr::add_predictions(df, mod1, 'pred1')
df = modelr::add_predictions(df, mod2, 'pred2')
ggplot(df, aes(x = Nitrogen, y = GrowthRate)) + facet_wrap(~Species) +
  geom_point() +
  geom_point(aes(y = pred1, color = 'red')) +
  geom_point(aes(y = pred2, color = 'blue'))
