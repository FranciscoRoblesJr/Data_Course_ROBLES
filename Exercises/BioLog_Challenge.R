library(tidyverse)
df = read.csv("./Data/BioLog_Plate_Data.csv")

glimpse(df)
library(plyr)
?plyr::mapvalues()

# write a command the subsets the BioLog data to Clear_Creek samples, 
# with dilution of 0.01, and only "Glycogen"
df.Clear = df %>% filter(Sample.ID == 'Clear_Creek' & Dilution == 0.01 & Substrate == 'Glycogen')

# Now plot those three replicates over time
df.long = df.Clear %>% gather(key ='Hours', value='Absorbents', c('Hr_24', 'Hr_48', 'Hr_144'))
df.long$Hours = as.numeric(mapvalues(df.long$Hours, from = unique(df.long$Hours), to = c(24,48,144))) # Data, possible factors, what to replace those factors
df.long$Rep = factor(df.long$Rep)
df.long %>% ggplot(aes(x=Hours, y=Absorbents, col=Rep)) +
  geom_point() + geom_smooth()


# Make a plot of Tween 80 utilization over time for ALL the samples, colored by Sample.ID
fulldf.long = df %>% gather(key ='Hours', value='Absorbents', c('Hr_24', 'Hr_48', 'Hr_144'))
fulldf.long$Hours = as.numeric(mapvalues(fulldf.long$Hours, from = unique(fulldf.long$Hours), to = c(24,48,144)))
tween.80 = fulldf.long %>% filter(Substrate == 'Tween 80 ')
tween.80 %>% ggplot(aes(x=Hours, y=Absorbents, col=Sample.ID, fill = Sample.ID)) +
  geom_point() + geom_smooth() + facet_wrap(~Sample.ID)

# Now, same plot, but combine both soils and both waters into soil and water groups and color by soil vs water
tween.80$SoilOrWater = mapvalues(tween.80$Sample.ID, levels(tween.80$Sample.ID), c('Water', 'Soil', 'Soil', 'Water'))
tween.80 %>% ggplot(aes(x=Hours, y=Absorbents, col=SoilOrWater)) +
  geom_jitter() + facet_wrap(~SoilOrWater) + geom_smooth()

# Make a table of summary statistics: for each combination of Sample.ID and Substrate, give:
# -- Number of observations
# -- Mean absorbance value
summ.df = fulldf.long %>% 
  group_by(Sample.ID, Substrate) %>% 
  summarise(N = n(), Mean = mean(Absorbents)) %>%
  as.data.frame()

# Example output ....

# Sample.ID     Substrate                       N Mean
# <fct>         <fct>                       <int> <dbl>
# 1 Clear_Creek 2-Hydroxy Benzoic Acid         27 0.0562
# 2 Clear_Creek 4-Hydroxy Benzoic Acid         27 0.247 
# 3 Clear_Creek D-Cellobiose                   27 0.403 
# 4 Clear_Creek D-Galactonic Acid γ-Lactone    27 0.314 
# 5 Clear_Creek D-Galacturonic Acid            27 0.385 
# 6 Clear_Creek D-Glucosaminic Acid            27 0.154 
# 7 Clear_Creek D.L -α-Glycerol Phosphate      27 0.0335
# 8 Clear_Creek D-Mallic Acid                  27 0.170 
# 9 Clear_Creek D-Mannitol                     27 0.346 
# 10 Clear_Creek D-Xylose                       27 0.0323
# 

