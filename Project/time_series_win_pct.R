library(tidyverse)
library(teamcolors)
library(plotly)
library(zoo)
library(xts)
library(forecast)
library(magrittr)

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
plot.list$LAD



#### acf analysis ####
acf.list = list()
for(i in unique(Teams.Split$franchID)){
  Teams.Split %>%
    filter(franchID == i) %>%
    unnest() %>%
    select(port_win) %>%
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
as.data.frame(pred.list.2018) %>%
  gather('Team', 'Pred_Win%')


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
talent.2018 %>%
  arrange(desc(Talent))


baseball.sim = function(data){
  # creating division indices
  al.east = c('BAL', 'BOS', 'NYY', 'TBD', 'TOR')
  al.central = c('CHW', 'CLE', 'DET', 'KCR', 'MIN')
  al.west = c('ANA', 'HOU', 'OAK', 'SEA', 'TEX')
  al = c(al.east, al.central, al.west)
  nl.east = c('ATL', 'FLA', 'NYM', 'PHI', 'WSN')
  nl.central = c('CHC', 'CIN', 'MIL', 'PIT', 'STL')
  nl.west = c('ARI', 'COL', 'LAD', 'SDP', 'SFG')
  nl = c(nl.east, nl.central, nl.west)
  
  # simulation
  data %>%
    mutate(Outcome = rbinom(nrow(.), 1, Prob_Away),
           Winner = ifelse(Outcome, Away_Team, Home_Team)) -> data
  
  # summary
  data %>%
    group_by(Winner) %>%
    summarise(Wins = n()) %>%
    arrange(desc(Wins)) -> summ
  colnames(summ)[1] = 'Team'
  
  # division winners
  al.east.winner = summ[first(which(summ$Team %in% al.east)), 1]
  al.central.winner = summ[first(which(summ$Team %in% al.central)), 1]
  al.west.winner = summ[first(which(summ$Team %in% al.west)), 1]
  nl.east.winner = summ[first(which(summ$Team %in% nl.east)), 1]
  nl.central.winner = summ[first(which(summ$Team %in% nl.central)), 1]
  nl.west.winner = summ[first(which(summ$Team %in% nl.west)), 1]
  al.winners = c(al.east.winner, al.central.winner, al.west.winner)
  nl.winners = c(nl.east.winner, nl.central.winner, nl.west.winner)
  
  # wild card winners
  al.wildcard = summ[((summ$Team %in% al) & !(summ$Team %in% al.winners)), 1] %>% slice(1:2)
  nl.wildcard = summ[((summ$Team %in% nl) & !(summ$Team %in% nl.winners)), 1] %>% slice(1:2)
  al.wildcard %>% left_join(talent.2018, by = 'Team') -> al.wildcard
  nl.wildcard %>% left_join(talent.2018, by = 'Team') -> nl.wildcard
  al.wild.prob = exp(al.wildcard$Talent[2]) / (exp(al.wildcard$Talent[1]) + exp(al.wildcard$Talent[2]))
  nl.wild.prob = exp(nl.wildcard$Talent[2]) / (exp(nl.wildcard$Talent[1]) + exp(nl.wildcard$Talent[2]))
  al.wild.win.num = rbinom(1, 1, al.wild.prob)
  nl.wild.win.num = rbinom(1, 1, nl.wild.prob)
  al.wild.winner = al.wildcard[(al.wild.win.num + 1), 1]
  nl.wild.winner = nl.wildcard[(nl.wild.win.num + 1), 1]
  
  
  # Division Series
  al.playoff = unlist(c(summ$Team[summ$Team %in% al.winners], al.wild.winner))
  names(al.playoff) = NULL
  nl.playoff = unlist(c(summ$Team[summ$Team %in% nl.winners], nl.wild.winner))
  names(nl.playoff) = NULL
  alds.1 = data.frame(Team = c(al.playoff[1], al.playoff[4]), stringsAsFactors = F)
  alds.2 = data.frame(Team = c(al.playoff[2], al.playoff[3]), stringsAsFactors = F)
  nlds.1 = data.frame(Team = c(nl.playoff[1], nl.playoff[4]), stringsAsFactors = F)
  nlds.2 = data.frame(Team = c(nl.playoff[2], nl.playoff[3]), stringsAsFactors = F)
  alds.1 %>% left_join(talent.2018, by = 'Team') -> alds.1
  alds.2 %>% left_join(talent.2018, by = 'Team') -> alds.2
  nlds.1 %>% left_join(talent.2018, by = 'Team') -> nlds.1
  nlds.2 %>% left_join(talent.2018, by = 'Team') -> nlds.2
  
  
  # series simulation
  series.sim = function(series, length){
    first.to = (length/2)
    team1.win = NULL
    team2.win = NULL
    i = 1
    while(sum(team1.win) <= first.to & sum(team2.win) <= first.to){
      team1.win.prob = exp(series[1,2]) / (exp(series[1,2]) + exp(series[2,2]))
      winner = rbinom(1, 1, team1.win.prob)
      team1.win[i] = winner
      team2.win[i] = ifelse(winner == 1, 0, 1)
      i = i + 1
    }
    series.list = list()
    series.list[[series[1,1]]] = team1.win
    series.list[[series[2,1]]] = team2.win
    return(series.list)
  }
  
  # Divison Series
  alds.1.result = series.sim(alds.1, 5)
  alds.2.result = series.sim(alds.2, 5)
  nlds.1.result = series.sim(nlds.1, 5)
  nlds.2.result = series.sim(nlds.2, 5)
  
  # make series look nice function
  series.clean = function(series, result){
    result = rbind.data.frame(c(series[1,], result[[1]], sum(result[[1]])), 
                              c(series[2,], result[[2]], sum(result[[2]])))
    for(i in 3:(ncol(result) - 1)){
      names(result)[i] = paste('Game', (i-2))
    }
    names(result)[ncol(result)] = 'Series'
    return(result)
  }
  alds.1.result = series.clean(alds.1, alds.1.result)
  alds.2.result = series.clean(alds.2, alds.2.result)
  nlds.1.result = series.clean(nlds.1, nlds.1.result)
  nlds.2.result = series.clean(nlds.2, nlds.2.result)
  alds.1.winner = alds.1.result[which(alds.1.result$Series == 3), c(1,2)]
  alds.2.winner = alds.2.result[which(alds.2.result$Series == 3), c(1,2)]
  nlds.1.winner = nlds.1.result[which(nlds.1.result$Series == 3), c(1,2)]
  nlds.2.winner = nlds.2.result[which(nlds.2.result$Series == 3), c(1,2)]
  
  
  # Championship Series
  alcs = rbind.data.frame(alds.1.winner, alds.2.winner)
  nlcs = rbind.data.frame(nlds.1.winner, nlds.2.winner)
  alcs$Team = as.character(alcs$Team)
  nlcs$Team = as.character(nlcs$Team)
  alcs.result = series.sim(alcs, 7)
  nlcs.result = series.sim(nlcs, 7)
  alcs.result = series.clean(alcs, alcs.result)
  nlcs.result = series.clean(nlcs, nlcs.result)
  alcs.winner = alcs.result[which(alcs.result$Series == 4), c(1,2)]
  nlcs.winner = nlcs.result[which(nlcs.result$Series == 4), c(1,2)]
  
  
  # World Series
  ws = rbind.data.frame(alcs.winner, nlcs.winner)
  ws$Team = as.character(ws$Team)
  ws.result = series.sim(ws, 7)
  ws.result = series.clean(ws, ws.result)
  ws.result$Team = as.character(ws.result$Team)
  ws.winner = ws.result[which(ws.result$Series == 4), c(1,2)]
  
  
  ### Return Lists
  season.list = list()
  summ %>% left_join(talent.2018, by = 'Team') -> summ
  summ = cbind.data.frame(summ[,1], round(summ[,3], 4), summ[,2])
  season.list[['Total_Wins']] = as.data.frame(summ)
  division.list = list()
  division.list[['AL_East']] = summ %>%
    filter(Team %in% al.east) %>%
    mutate(Losses = 162 - Wins, Win_Pct = Wins / (Wins + Losses))
  division.list[['AL_Central']] = summ %>%
    filter(Team %in% al.central) %>%
    mutate(Losses = 162 - Wins, Win_Pct = Wins / (Wins + Losses))
  division.list[['AL_West']] = summ %>%
    filter(Team %in% al.west) %>%
    mutate(Losses = 162 - Wins, Win_Pct = Wins / (Wins + Losses))
  division.list[['NL_East']] = summ %>%
    filter(Team %in% nl.east) %>%
    mutate(Losses = 162 - Wins, Win_Pct = Wins / (Wins + Losses))
  division.list[['NL_Central']] = summ %>%
    filter(Team %in% nl.central) %>%
    mutate(Losses = 162 - Wins, Win_Pct = Wins / (Wins + Losses))
  division.list[['NL_West']] = summ %>%
    filter(Team %in% nl.west) %>%
    mutate(Losses = 162 - Wins, Win_Pct = Wins / (Wins + Losses))
  season.list[['Standings']] = division.list
  
  
  wildcard.list = list()
  al.wildcard.list = list()
  al.wildcard.list[['Teams']] = al.wildcard
  al.wildcard.list[['Winner']] = al.wild.winner
  nl.wildcard.list = list()
  nl.wildcard.list[['Teams']] = nl.wildcard
  nl.wildcard.list[['Winner']] = nl.wild.winner
  wildcard.list[['AL']] = al.wildcard.list
  wildcard.list[['NL']] = nl.wildcard.list
  
  
  ds.list = list()
  ds.list[['ALDS_1']] = list(Result = alds.1.result, Winner = alds.1.winner)
  ds.list[['ALDS_2']] = list(Result = alds.2.result, Winner = alds.2.winner)
  ds.list[['NLDS_1']] = list(Result = nlds.1.result, Winner = nlds.1.winner)
  ds.list[['NLDS_2']] = list(Result = nlds.2.result, Winner = nlds.2.winner)
  
  
  cs.list = list()
  cs.list[['ALCS']] = list(Result = alcs.result, Winner = alcs.winner)
  cs.list[['NLCS']] = list(Result = nlcs.result, Winner = nlcs.winner)
  
  ws.list = list(Result = ws.result, Winner = ws.winner)
  
  
  complete.list = list()
  complete.list[['Season']] = season.list
  complete.list[['Wildcard']] = wildcard.list
  complete.list[['Division Series']] = ds.list
  complete.list[['Championship Series']] = cs.list
  complete.list[['World Series']] = ws.list
  
  return(complete.list)
}



#### lots of simulation time ####
results.list = list()
for(j in 1:10000){
  results.list[[paste0('year', j)]] = baseball.sim(talent.matrix.2018)
}

# Games won
Teams %>%
  filter(yearID >= as.Date('2018-01-01')) %>%
  select(franchID, actual_win) -> wins.df
colnames(wins.df) = c('Team', 'Win%')
for(i in 1:length(results.list)){
  results.list %>%
    map(1) %>%
    map(1) %>%
    extract2(i) %>%
    select(Team, Wins) %>%
    mutate(Wins = Wins/162) -> pred.wins
  wins.df = left_join(wins.df, pred.wins, by = 'Team')
  colnames(wins.df)[(i + 2)] = paste0('Year', i, '_Win%')
}
wins.df %>%
  select(c(1, 3:ncol(wins.df))) %>%
  gather(key = 'Year', value = 'Win_Pct', -c('Team')) %>%
  group_by(Team) %>%
  summarize(Avg.Win = mean(Win_Pct)) -> pred.wins
colnames(wins.df)[2] = 'Actual_Pct'
left_join(wins.df[,1:2], pred.wins, by = 'Team') %>%
  select(Actual_Pct,Avg.Win) -> resid.df
sqrt(sum((resid.df$Actual_Pct - resid.df$Avg.Win)^2)/30) * 162
  



# world series winners
results.list %>%
  map(5) %>%
  map(2) %>%
  map(1) %>%
  unlist() %>%
  data.frame() -> ws.winners
colnames(ws.winners) = 'Winners'
ws.winners %>%
  group_by(Winners) %>%
  summarize(Titles = n()) %>%
  arrange(desc(Titles))

Teams.Split[!(Teams.Split$franchID %in% ws.winners$Winners),1]
