library(tidyverse)
library(teamcolors)
library(plotly)
library(zoo)
library(xts)
library(forecast)

#TeamsFranchises = read_csv('../../Documents/College/baseballdatabank-2019.2/core/TeamsFranchises.csv')
#TeamsFranchises %>%
#  filter(active == 'Y') -> active.teams


#### Best Predictor ####
Teams = read_csv('../../Documents/College/baseballdatabank-2019.2/core/Teams.csv')
Teams %>%
  mutate(actual_win = W / (W + L),
         pyt_win = R^2 / (R^2 + RA^2),
         pyt_resid = actual_win - pyt_win,
         opt_win = R^1.83 / (R^1.83 + RA^1.83),
         opt_resid = actual_win - opt_win,
         port_x = 1.5 * log10((R + RA)/G) + 0.45,
         port_win = R^port_x / (R^port_x + RA^port_x),
         port_resid = actual_win - port_win,
         pat_x = ((R + RA)/G)^.287,
         pat_win = R^pat_x / (R^pat_x + RA^pat_x),
         pat_resid = actual_win - pat_win,
         lin_win = 0.000683 * (R - RA) + 0.5,
         lin_resid = actual_win - lin_win) -> Teams
Teams %>%
  pull(yearID) %>%
  paste0('-01-01') %>%
  as.Date() -> Teams$yearID
Teams %>%
  filter(yearID >= as.Date('1998-01-01')) -> Teams
Teams.Split = Teams %>%
  group_by(franchID) %>%
  nest()
# unnest used to get access to data


Teams %>%
  summarize(pyt_rmse = sqrt(sum((pyt_resid)^2)/629),
            opt_rmse = sqrt(sum((opt_resid)^2)/629),
            port_rmse = sqrt(sum((port_resid)^2)/629),
            pat_rmse = sqrt(sum((pat_resid)^2)/629),
            lin_rmse = sqrt(sum((lin_resid)^2)/629)) -> rmse
rmse
rmse*162
# opt_win, port_win, and pat_win are essentially the same
# port_win is very slightly better than the others so that is what I will use

#### Team Plot ####
teamcolors %>%
  filter(league == 'mlb') -> mlb.colors
rbind.data.frame(mlb.colors[13,], mlb.colors[-13,]) -> mlb.colors
rbind.data.frame(mlb.colors[1:11,], mlb.colors[15,], mlb.colors[12:14,], mlb.colors[16:30,]) -> mlb.colors
rbind.data.frame(mlb.colors[1:23,], mlb.colors[25,], mlb.colors[24,], mlb.colors[26:30,]) -> mlb.colors
c(mlb.colors$primary[1:5], mlb.colors$secondary[6], mlb.colors$primary[7:8], mlb.colors$secondary[9],
  mlb.colors$primary[10:11], mlb.colors$secondary[12], '#F4911E', mlb.colors$secondary[14],
  mlb.colors$primary[15], mlb.colors$secondary[16:17], mlb.colors$primary[18:20], mlb.colors$secondary[21],
  mlb.colors$primary[22], mlb.colors$tertiary[23], mlb.colors$secondary[24], mlb.colors$primary[25:26], 
  mlb.colors$secondary[27:28], mlb.colors$primary[29:30]) -> mlb.palette
names(mlb.palette) = sort(unique(Teams.Split$franchID))

ggplot(Teams, aes(yearID, actual_win, color = franchID)) +
  geom_line() +
  scale_color_manual(values = mlb.palette)

plot.list = list()
for(i in unique(Teams$franchID)){
  Teams.Split %>%
    filter(franchID == i) %>%
    unnest() -> df
  ggplot(df,aes(yearID, actual_win, color = franchID)) +
    geom_line() +
    scale_color_manual(values = mlb.palette[i]) +
    labs(x = 'Year', y = 'Win %', title = paste(i, 'Win Percentage 1998-2018'),
         color = '') -> plot.list[[i]]
}
plot.list$ANA



#### acf analysis ####
acf.list = list()
for(i in unique(Teams.Split$franchID)){
  Teams.Split %>%
    filter(franchID == i) %>%
    unnest() %>%
    select(actual_win) %>%
    acf() -> test
  acf.list[[i]] = test
}
map(acf.list, 1)
lapply(map(acf.list, 1), length) > 14


# summarizing acf
unlist(map(acf.list, 1)) -> acf.means
matrix(acf.means, nrow = 30, ncol = 14, byrow = T) -> acf.means
acf.means = as.data.frame(acf.means)
acf.means %>%
  colMeans() -> acf.means
acf.means = data.frame(index = 1:14, means = acf.means)
ggplot(acf.means, aes(index, means)) +
  geom_point() +
  geom_hline(color = 'red', yintercept = 0) +
  geom_hline(color = 'blue', yintercept = 0.42) +
  geom_hline(color = 'blue', yintercept = -0.42)
# seems to show AR1





#### pacf analysi2 ####
pacf.list = list()
for(i in unique(Teams.Split$franchID)){
  Teams.Split %>%
    filter(franchID == i) %>%
    unnest() %>%
    select(actual_win) %>%
    pacf() -> test
  pacf.list[[i]] = test
}

# summarizing pacf
unlist(map(pacf.list, 1)) -> pacf.means
matrix(pacf.means, nrow = 30, ncol = 13, byrow = T) -> pacf.means
pacf.means = as.data.frame(pacf.means)
pacf.means %>%
  colMeans() -> pacf.means
pacf.means = data.frame(index = 1:13, means = pacf.means)
ggplot(pacf.means, aes(index, means)) +
  geom_point() +
  geom_hline(color = 'red', yintercept = 0) +
  geom_hline(color = 'blue', yintercept = 0.42) +
  geom_hline(color = 'blue', yintercept = -0.42)

# everything seems to show AR1


#### ARIMA analysis ####
arima.list = list()
for(i in unique(Teams.Split$franchID)){
  Teams.Split %>%
    filter(franchID == i) %>%
    unnest() %>%
    select(opt_win) %>%
    auto.arima() -> test
  arima.list[[i]] = test
}


map(arima.list, 1)
# AR = 1, MA = 1 seems somewhat likely

#### Back-testing 2017 ####
arima.list.2017 = list()
for(i in unique(Teams.Split$franchID)){
  Teams.Split %>%
    filter(franchID == i) %>%
    unnest() %>%
    slice(1:20) %>%
    select(port_win) %>%
    arima(order = c(1, 0, 1)) -> test
  arima.list.2017[[i]] = test
}
map(arima.list.2017, 1)
map(arima.list.2017, 1) %>%
  unlist() %>%
  matrix(nrow = 30, ncol = 3)  %>%
  colMeans() -> ts.coef.2017



pred.list.2018 = list()
for(i in unique(Teams.Split$franchID)){
  Teams.Split %>%
    filter(franchID == i) %>%
    unnest() %>%
    filter(yearID > as.Date('1997-01-01')) %>%
    pull(port_win) -> win.pct
  win.pct[19:20] -> win.pct
  win.pct[1] * ts.coef.2017[1] -> pred1
  win.pct[2] - pred1 -> resid1
  win.pct[2] * ts.coef.2017[1] + resid1 * ts.coef.2017[2] -> new.pred
  new.pred + ts.coef.2017[3] -> new.pred
  pred.list.2018[[i]] = new.pred
}
pred.list.2018


#### Getting talents ####
pred.list.2018 %>%
  unlist() -> pred.win.2018
talent.2018 = scale(pred.win.2018) / 5
talent.2018 = as.vector(talent.2018)
names(talent.2018) = unique(Teams.Split$franchID)
# Bradley-Terry Model says N(0, .2) produces good results


# uploading schedule
schedule_2018 = read.table('Project/schedule_2018.TXT', sep = ',')
schedule_2018 %>%
  select(V4, V7) -> schedule_2018
schedule_2018$V4 = as.character(schedule_2018$V4)
schedule_2018$V7 = as.character(schedule_2018$V7)

# changing team ID's to ID's I've been using
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'CHN', replacement = 'CHC')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'CHA', replacement = 'CHW')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'KCA', replacement = 'KCR')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'LAN', replacement = 'LAD')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'MIA', replacement = 'FLA')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'NYN', replacement = 'NYM')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'NYA', replacement = 'NYY')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'SDN', replacement = 'SDP')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'SFN', replacement = 'SFG')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'SLN', replacement = 'STL')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'TBA', replacement = 'TBD')
schedule_2018 = mutate_if(schedule_2018, is.character, str_replace_all, pattern = 'WAS', replacement = 'WSN')

# Combining talent scores with schedule
talent.2018 = data.frame(names(talent.2018), talent.2018)
colnames(talent.2018) = c('Team', 'Talent')
talent.2018$Team = as.character(talent.2018$Team)
rownames(talent.2018) = NULL
colnames(schedule_2018) = c('Away_Team', 'Home_Team')
talent.matrix.2018 = left_join(schedule_2018, talent.2018, by = c('Away_Team' = 'Team'))
talent.matrix.2018 = left_join(talent.matrix.2018, talent.2018, by = c('Home_Team' = 'Team'))
colnames(talent.matrix.2018) = c('Away_Team', 'Home_Team', 'Away_Talent', 'Home_Talent')
talent.matrix.2018 %>%
  mutate(Prob_Away = exp(Away_Talent) / (exp(Away_Talent) + exp(Home_Talent))) -> talent.matrix.2018



#### simulation time ####

