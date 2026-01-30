stadiums <- read.csv(file.choose()) |>
  filter(League == "NBA")

stadiums <- stadiums |>
  mutate(
    Timezone = case_when(
      tz_lookup_coords(Lat, Long, method = "accurate") %in% c("America/New_York", "America/Toronto") ~ "EST",
      tz_lookup_coords(Lat, Long, method = "accurate") %in% c("America/Chicago", "America/Indiana/Indianapolis", "America/Detroit") ~ "CST",
      tz_lookup_coords(Lat, Long, method = "accurate") == "America/Denver"  ~ "MST",
      tz_lookup_coords(Lat, Long, method = "accurate") == "America/Phoenix" ~ "MST",
      tz_lookup_coords(Lat, Long, method = "accurate") == "America/Los_Angeles" ~ "PST"))
  
stadiums

games <- read.csv(file.choose())

library(dplyr)
library(tidyverse)

# combine column to rename
games <- games %>%
  mutate(HomeTeam = paste(hometeamCity, hometeamName, sep = " "))

games <- games %>%
  mutate(AwayTeam = paste(awayteamCity, awayteamName, sep = " "))


games <- games %>%
  rename(winner_num = winner) %>%
  mutate(
    winningTeam = ifelse(winner_num == 1, "Home", "Away"),
    winner = ifelse(winningTeam == "Home", HomeTeam, AwayTeam)
  ) %>%
  select(-winner_num)

#delete unneeded columns
games <- games %>%
  select(-gameId, -hometeamId, awayteamId, -gameType, -arenaId, -gameLabel, -gameSubLabel, -seriesGameNumber)


