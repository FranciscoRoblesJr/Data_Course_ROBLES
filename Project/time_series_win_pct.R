library(tidyverse)



TeamsFranchises = read_csv('../../Documents/College/baseballdatabank-2019.2/core/TeamsFranchises.csv')
TeamsFranchises %>%
  filter(active == 'Y') -> active.teams

Teams = read_csv('../../Documents/College/baseballdatabank-2019.2/core/Teams.csv')
Teams %>%
  filter(yearID >= 1998) -> Teams
Teams %>%
  mutate(actual_win = W / (W + L),
         pyt_win = R^2 / (R^2 + RA^2),
         opt_win = R^1.83 / (R^1.83 + RA^1.83),
         port_x = 1.5 * log10((R + RA)/G) + 0.45,
         port_win = R^port_x / (R^port_x + RA^port_x),
         pat_x = ((R + RA)/G)^.287,
         pat_win = R^pat_x / (R^pat_x + RA^pat_x),
         lin_win = 0.000683 * (R - RA) + 0.5) -> Teams

Teams.Split = Teams %>%
  group_by(franchID) %>%
  nest()
# unnest used to get access to data


