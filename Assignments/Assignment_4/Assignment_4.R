df = read.csv('../../../Data_Course/Data/ITS_mapping.csv', sep = '\t', header = T)
# I think this is reading in right but i'm not 100% sure, what is up with the data
str(df)
# This is a terrible summarization, but you said do it for every column
table(df$SampleID)
summary(df$BarcodeSequence)
# just a bunch of NA's, not sure what you want me to summarize with that
summary(df$LinkerPrimerSequence)
# just a bunch of NA's, not sure what you want me to summarize with that
table(df$Run)
table(df$Ecosystem)
table(df$Island)
# 320 rows were left blank
summary(df$Lat)
# 735 rows left blank
summary(df$Lon)
# 735 rows left blank, and the 157.7 seems to be a very strong outlier
table(df$Collection_Date)
table(df$F_Primer)
table(df$R_Primer)
# Seems like F_Primer and R_Primer is giving the same information
table(df$Ecosys_Type)
summary(df$Host_Type)
# just a bunch of NA's, not sure what you want me to summarize with that
table(df$Host)
table(df$InputFileName)
table(df$Description)

# A lot of these 'factors' have so many variables that they end up having no real significance to our data

# further exploration
hist(df$Lon)
plot(df$Lat, df$Lon) # Only one point that is way off on the variable Lon
plot(df$Island, df$Lat)
# this data is whack



png(filename = './silly_boxplot.png')
plot(x = df$Ecosystem, y = df$Lat)
dev.off()
