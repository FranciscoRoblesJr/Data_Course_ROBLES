library(tidyverse)
data(mtcars)
df = mtcars
rm(mtcars)

## Subset mtcars to only auto cars and saves
df.auto = df %>% filter(am == 0)
write_csv(df.auto, 'automatic_mtcars.csv')

## HP vs. MPG and saves
ggplot(df.auto, aes(x = hp, y = mpg)) + 
  geom_point() +
  labs(x = 'Horsepower', y = 'Miles Per Gallon', title = 'Horsepower vs. Miles per Gallon')
ggsave('mpg_vs_hp_auto.png')

## Weight vs MPG
ggplot(df.auto, aes(wt, mpg)) + 
  geom_point() +
  labs(x = 'Weight', y = 'Miles Per Gallon', title = 'Weight vs. Miles per Gallon')
ggsave('mpg_vs_wt_auto.tiff')

## Displacements <= 200
df.disp = df %>% filter(disp <= 200)
write_csv(df.disp, 'mtcars_max200_disp.csv')

## Max HP for the three
max(df$hp) # Original
max(df.auto$hp) # Auto
max(df.disp$hp) # Max200
df.max = rbind.data.frame(max(df$hp), max(df.auto$hp), max(df.disp$hp))
rownames(df.max) = c('Original', 'Automatic', 'Displacement') ; colnames(df.max) = 'Max_Horsepower'
write.table(df.max, 'hp_maximums.txt')
