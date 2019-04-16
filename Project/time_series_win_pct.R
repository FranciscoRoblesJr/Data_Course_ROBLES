library(tidyverse)
library(teamcolors)
library(plotly)


#TeamsFranchises = read_csv('../../Documents/College/baseballdatabank-2019.2/core/TeamsFranchises.csv')
#TeamsFranchises %>%
#  filter(active == 'Y') -> active.teams


# Find predictors
Teams = read_csv('../../Documents/College/baseballdatabank-2019.2/core/Teams.csv')
Teams %>%
  filter(yearID >= 1998) -> Teams
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

ggplot(Teams, aes(yearID, actual_win, color = franchID)) +
  geom_line() +
  scale_color_manual(values = mlb.palette)