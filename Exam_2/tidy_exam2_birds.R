library(tidyverse)

## read the data
bird = read.csv('Bird_Measurements.csv')
names(bird)


#### PART 1: Two Duplicated Rows ####

length(unique(levels(bird$Species_name)))
### NOTE TWO SPECIES NAME SHOW UP TWICE
### They are Ardea Picata and Eudyptes Schlegeli



# find which rows are the same for ardea picata
ardea.picata.ind = which(bird$Species_name == 'Ardea picata')
# rows have some values in some but none in the other, am going to combine the rows
# taking first 28 from second row, and then the rest from the other row. This will get rid of NA's
ardea.picata = c(bird[ardea.picata.ind[2],][1:28], bird[ardea.picata.ind[1],][29:37])
# getting rid of one of the extra rows and resaving the combined rows back in
bird = bird[-ardea.picata.ind[2],]
bird[ardea.picata.ind[1],] = ardea.picata



# Doing the same process as above
eudyptes.schlegeli.ind = which(bird$Species_name == 'Eudyptes schlegeli')
# same as above, except second row has info up to 7
eudyptes.schlegeli = c(bird[eudyptes.schlegeli.ind[2],][1:7], bird[eudyptes.schlegeli.ind[1],][8:37])
bird = bird[-eudyptes.schlegeli.ind[2],]
bird[eudyptes.schlegeli.ind[1],] = eudyptes.schlegeli

length(unique(bird$Species_name))
# This matches the amount of rows we have so each row is now a different species
rm(ardea.picata) ; rm(ardea.picata.ind) ; rm(eudyptes.schlegeli) ; rm(eudyptes.schlegeli.ind)






#### PART 2: Initial Tidy Process ####

# reorganize columns
bird = cbind.data.frame(bird[,1:4], bird[,35:37], bird[,5:34])


# Creating main data frame that won't change
main = rbind.data.frame(bird[,1:7], bird[,1:7], bird[,1:7])
# Once I do all my gathers, there will be 11,301 rows (3767 * 3)
# Giving each row a unique ID so I can full_join off of that later
id.ind = 1:11301
main = cbind.data.frame(ID = id.ind, main[,1:7])



# Gathering data based off of each variable type.
# Gather mass variable for regular value and one for N value
mass.ind = grep('_mass', names(bird))[-1]
massN.ind = mass.ind[seq(2, 6, 2)]
mass.ind = mass.ind[seq(1, 6, 2)]

# Create small data frame with gathered mass data and id #
mass.df = bird %>%
  gather(G.Mass, Mass, mass.ind) %>%
  select(G.Mass, Mass) %>%
  cbind.data.frame(id.ind)

# Create small data frame with gathered mass N data and id #
massN.df = bird %>%
  gather(G.Mass.N, Mass.N, massN.ind) %>%
  select(G.Mass.N, Mass.N) %>%
  cbind.data.frame(id.ind)

# full join these to into one data frame
mass.df = full_join(mass.df, massN.df, by = 'id.ind')




# TEST TO SEE IF ORDERED CORRECTLY
# taking off the _N and seeing if it matches the the other variable
mass.df$G.Mass.N = map(str_split(mass.df$G.Mass.N, '_N'),1)
sum(mass.df$G.Mass == mass.df$G.Mass.N)
# they all match which means that they ordered the rows correctly

# PROOF
table(mass.df$G.Mass)
# Each appear 3767 times
min(which(mass.df$G.Mass == 'M_mass'))
max(which(mass.df$G.Mass == 'M_mass'))
# M_mass goes from 1:3767
min(which(mass.df$G.Mass == 'F_mass'))
max(which(mass.df$G.Mass == 'F_mass'))
# F_mass goes from 3768:7534
min(which(mass.df$G.Mass == 'unsexed_mass'))
max(which(mass.df$G.Mass == 'unsexed_mass'))
# unsexed_mass goes from 7535:11301
mass.df$G.Mass[3767] ; mass.df$G.Mass[3768]
mass.df$G.Mass[7534] ; mass.df$G.Mass[7535]

# CONCLUSION
# gather will put all males variable first, then females, then unsexed
# Will create a vector of 3767 M's, 3767 F's, and 3767 U's later to denote gender 
# since that is how it will be organized
mass.df = cbind.data.frame(mass.df$id.ind, mass.df$Mass, mass.df$Mass.N)
colnames(mass.df) = c('ID', 'Mass', 'Mass_N')

# Add in mass to main dataframe
main = full_join(main, mass.df, by = 'ID')



#### PART 3: Tidying up rest of Data ####
# This part will repeat the process of what I did for the mass variable, but do it in a quicker way

# Tidying for wing
wing.ind = grep('_wing', names(bird))
wingN.ind = wing.ind[seq(2, 6, 2)]
wing.ind = wing.ind[seq(1, 6, 2)]

wing.df = bird %>%
  gather(G.wing, Wing, wing.ind) %>%
  select(Wing) %>%
  cbind.data.frame(id.ind)

wingN.df = bird %>%
  gather(G.wing.N, Wing.N, wingN.ind) %>%
  select(Wing.N) %>%
  cbind.data.frame(id.ind)
  
wing.df = full_join(wing.df, wingN.df, by = 'id.ind')
wing.df = cbind.data.frame(wing.df[,2], wing.df[,1], wing.df[,3])
names(wing.df) = c('ID', 'Wing', 'Wing_N')
main = full_join(main, wing.df, by ='ID')

# Tidying for tarsus
tarsus.ind = grep('_tarsus', names(bird))
tarsusN.ind = tarsus.ind[seq(2, 6, 2)]
tarsus.ind = tarsus.ind[seq(1, 6, 2)]

tarsus.df = bird %>%
  gather(G.tarsus, tarsus, tarsus.ind) %>%
  select(tarsus) %>%
  cbind.data.frame(id.ind)

tarsusN.df = bird %>%
  gather(G.tarsus.N, tarsus.N, tarsusN.ind) %>%
  select(tarsus.N) %>%
  cbind.data.frame(id.ind)

tarsus.df = full_join(tarsus.df, tarsusN.df, by = 'id.ind')
tarsus.df = cbind.data.frame(tarsus.df[,2], tarsus.df[,1], tarsus.df[,3])
names(tarsus.df) = c('ID', 'Tarsus', 'Tarsus_N')
main = full_join(main, tarsus.df, by ='ID')

# Tidying for bill
bill.ind = grep('_bill', names(bird))
billN.ind = bill.ind[seq(2, 6, 2)]
bill.ind = bill.ind[seq(1, 6, 2)]

bill.df = bird %>%
  gather(G.bill, bill, bill.ind) %>%
  select(bill) %>%
  cbind.data.frame(id.ind)

billN.df = bird %>%
  gather(G.bill.N, bill.N, billN.ind) %>%
  select(bill.N) %>%
  cbind.data.frame(id.ind)

bill.df = full_join(bill.df, billN.df, by = 'id.ind')
bill.df = cbind.data.frame(bill.df[,2], bill.df[,1], bill.df[,3])
names(bill.df) = c('ID', 'Bill', 'Bill_N')
main = full_join(main, bill.df, by='ID')

# Tidying for tail
tail.ind = grep('_tail', names(bird))
tailN.ind = tail.ind[seq(2, 6, 2)]
tail.ind = tail.ind[seq(1, 6, 2)]

tail.df = bird %>%
  gather(G.tail, tail, tail.ind) %>%
  select(tail) %>%
  cbind.data.frame(id.ind)

tailN.df = bird %>%
  gather(G.tail.N, tail.N, tailN.ind) %>%
  select(tail.N) %>%
  cbind.data.frame(id.ind)

tail.df = full_join(tail.df, tailN.df, by ='id.ind')
tail.df = cbind.data.frame(tail.df[,2], tail.df[,1], tail.df[,3])
names(tail.df) = c('ID', 'Tail', 'Tail_N')
main = full_join(main, tail.df, by = 'ID')

# Get rid of all unwanted data frame
rm(mass.df) ; rm(massN.df) ; rm(mass.ind) ; rm(massN.ind)
rm(wing.df) ; rm(wingN.df) ; rm(wing.ind) ; rm(wingN.ind)
rm(tarsus.df) ; rm(tarsusN.df) ; rm(tarsus.ind) ; rm(tarsusN.ind)
rm(bill.df) ; rm(billN.df) ; rm(bill.ind) ; rm(billN.ind)
rm(tail.df) ; rm(tailN.df) ; rm(tail.ind) ; rm(tailN.ind)
rm(id.ind)


#### PART 4: Finally... ####
# Get rid of ID number and add in gender
main = main[,-1]
Gender = c(rep('M', 3767), rep('F', 3767), rep('U', 3767))
main = cbind.data.frame(main[,1:4], Gender, main[5:17])

# main is now as tidy as I could get it to be
