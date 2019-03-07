sal.df = read.csv('salaries.csv')
library(tidyverse)

#### Part 1 ####
sal.df %>%
  gather('Faculty_Rank', 'Salary', 5:7) -> sal.df

sal.df %>%
  ggplot(aes(x = Tier, y = Salary, color = Faculty_Rank)) +
  geom_boxplot(aes(fill = Faculty_Rank)) +
  labs(title = 'Faculty Salaries - 1995')
ggsave("ROBLES_exam2_plot1.jpeg")

#### Part 2 ####
df = read.csv('atmosphere.csv')
names(df)
str(df)

ggplot(df, aes(y = Diversity)) +
  geom_point(aes(x=Aerosol_Density))

ggplot(df, aes(y = Diversity)) +
  geom_point(aes(x=CO2_Concentration))

ggplot(df, aes(y = Diversity)) +
  geom_point(aes(x=Precip))

### PART 2 ###
# Choosing Precip as my variable because from the plot, it looks like the only one with a relatively strong
# linear relationship
mod1 = glm(Diversity ~ Precip, data = df)

# Using a complete model for this one, which includes the other two independent variables
mod2 = glm(Diversity ~ Precip + Aerosol_Density + CO2_Concentration, data = df)

summary(mod1)
summary(mod2)
# All variables seem to be significant, which is weird since it didn't look like that from the graphs

MASS::stepAIC(mod2)
# Says I shouldn't take anything out


### PART 3 ###
sqrt(mean(mod1$residuals^2))
sqrt(mean(mod2$residuals^2))
# RMSE is smaller for mod2 therefore, I would say it is the better one. Reconfirming what was shown in
# summary above


### PART 4 ###
modelr::add_predictions(df, mod1, 'pred1') -> df
modelr::add_predictions(df, mod2, 'pred2') -> df



### PART 5 ###
ggplot(df, aes(x = Aerosol_Density, y = Diversity)) +
  geom_point() +
  geom_point(aes(y = pred1, color = 'blue'), alpha = 0.5) +
  geom_point(aes(y = pred2, color = 'red'), alpha = 0.5) +
  scale_color_manual(labels = c('Mod1 Pred', 'Mod2 Pred'), values = c('red', 'blue'))

ggplot(df, aes(x = CO2_Concentration, y = Diversity)) +
  geom_point() +
  geom_point(aes(y = pred1, color = 'blue'), alpha = 0.5) +
  geom_point(aes(y = pred2, color = 'red'), alpha = 0.5) +
  scale_color_manual(labels = c('Mod1 Pred', 'Mod2 Pred'), values = c('red', 'blue'))

ggplot(df, aes(x = Precip, y = Diversity)) +
  geom_point() +
  geom_point(aes(y = pred1, color = 'blue'), alpha = 0.5) +
  geom_point(aes(y = pred2, color = 'red'), alpha = 0.5) +
  scale_color_manual(labels = c('Mod1 Pred', 'Mod2 Pred'), values = c('red', 'blue'))



#### PART 6 ####
test.df = read.csv('hyp_data.csv')
modelr::add_predictions(test.df, mod1, 'pred1') -> test.df
modelr::add_predictions(test.df, mod2, 'pred2') -> test.df
test.df




#### PART 7 ####
sink("model_summaries.txt")
print(summary(mod1))
print(summary(mod2))
sink()



#### PART 8 ####
ggplot(df, aes(x = Precip, y = Diversity)) +
  geom_point() +
  geom_point(data = test.df, aes(y = pred1, color = 'blue'), size = 3) +
  geom_point(data = test.df, aes(y = pred2, color = 'red'), size = 3) +
  scale_color_manual(labels = c("Mod1 Pred","Mod2 Pred"), values = c('red', 'blue')) +
  labs(x = 'Precipitation on Sampling Day in mm',
       y = 'Fungal Species found in Air',
       title = 'Diversity vs. Precipiation with Hypothetical Estimates')

                       