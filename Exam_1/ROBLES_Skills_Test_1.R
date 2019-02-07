df = read.csv('DNA_Conc_by_Extraction_Date.csv', sep='\t')
df$Year_Collected = factor(df$Year_Collected)

#### Part 1 ####
hist(df$DNA_Concentration_Ben, xlab = "DNA Concentration", main="Histogram of Ben's DNA Concentration")
hist(df$DNA_Concentration_Katy, xlab = "DNA Concentration", main="Histogram of Katy's DNA Concentration")




#### Part 2 ####
plot(df$Year_Collected, df$DNA_Concentration_Ben, xlab = 'YEAR', ylab = 'DNA Concentration', main = "Ben's Extractions")
plot(df$Year_Collected, df$DNA_Concentration_Katy, xlab = 'YEAR', ylab = 'DNA Concentration', main = "Katy's Extractions")




#### Part 3 ####
# Creating my first plot of Katy
jpeg('ROBLES_Plot1.jpeg')
plot(df$Year_Collected, df$DNA_Concentration_Katy, xlab = 'YEAR', ylab = 'DNA Concentration', main = "Katy's Extractions")
dev.off()

# Creating my second plot of Ben
jpeg('ROBLES_Plot2.jpeg')
plot(df$Year_Collected, df$DNA_Concentration_Ben, xlab = 'YEAR', ylab = 'DNA Concentration', main = "Ben's Extractions")
dev.off()



#### Part 4 ####
t.test(df$DNA_Concentration_Katy, df$DNA_Concentration_Ben)
# It was most certainly different

mean.year.ben = NULL
mean.year.katy = NULL
for(i in levels(df$Year_Collected)){
  mean.year.ben[i] = mean(df[df$Year_Collected == i, 'DNA_Concentration_Ben'])
  mean.year.katy[i] = mean(df[df$Year_Collected == i, 'DNA_Concentration_Katy'])
}

mean.year.ben = round(mean.year.ben, 2) ; mean.year.katy = round(mean.year.katy, 2) ; mean.diff = (mean.year.ben - mean.year.katy)
# mean.diff is just (mean.year.ben - mean.year.katy)
# The lowest diff is what I'm saying is the lowest relative difference

mean.year.ben ; mean.year.katy ; mean.diff
# First line is Bens, second is Katys, third is the difference
mean.diff[mean.diff == min(mean.year.ben - mean.year.katy)]
# Least difference in means is the year 2000

plot(df$Year_Collected, (df$DNA_Concentration_Ben - df$DNA_Concentration_Katy), 
     main='Difference between Ben and Katy by Year', xlab = 'YEAR', ylab = 'Difference (Ben - Katy)')
# The plot seems to confirm that
# Argument could be mad for 2003 as well




#### Part 5 ####
# Wrote the for-loop in Part 4 already
Ben_df = cbind.data.frame(YEAR = levels(df$Year_Collected), Ben_Average_Conc = mean.year.ben)
Ben_df[Ben_df$Ben_Average_Conc == max(Ben_df$Ben_Average_Conc),]
# Highest Average concentration is 2007
write.csv(Ben_df, file = 'Ben_average_by_year.csv', row.names = F)
