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

stadiums <- stadiums |>
  mutate(Team = recode(Team,
                       "Sacremento Kings" = "Sacramento Kings",
                       "Los Angeles Clippers" = "LA Clippers"))
  
stadiums

games <- read.csv(file.choose())
library(dplyr)
library(tidyverse)

# Combine columns to rename
games <- games |>
  mutate(HomeTeam = paste(hometeamCity, hometeamName, sep = " ")) |>
  mutate(AwayTeam = paste(awayteamCity, awayteamName, sep = " "))

games <- games |>
  rename(winner_num = winner) |>
  mutate(
    winningTeam = ifelse(winner_num == 1, "Home", "Away"),
    winner = ifelse(winningTeam == "Home", HomeTeam, AwayTeam)
  ) |>
  select(-winner_num)

# Delete unneeded columns
games <- games |>
  select(-hometeamId, -awayteamId, -gameType, -arenaId, -gameLabel, -gameSubLabel, -seriesGameNumber, -hometeamCity, -hometeamName, -awayteamCity, -awayteamName)

#Reorder
games <- games |>
  select(gameId, gameDateTimeEst, HomeTeam, AwayTeam, homeScore, awayScore, winningTeam, winner, attendance)

#seperate date and time and get rid of original column
library(lubridate)

games <- games |>
  mutate(
    gameDateTimeEst = ymd_hms(gameDateTimeEst),
    game_date = as.Date(gameDateTimeEst),
    game_time = format(gameDateTimeEst, "%H:%M:%S")
  ) |>
  select(-gameDateTimeEst)

# flag back-to-back games
  # Home team back-to-back
home_b2b <- games |>
  select(gameId, game_date, team = HomeTeam) |>
  arrange(team, game_date) |>
  group_by(team) |>
  mutate(home_b2b = ifelse(game_date - lag(game_date) == 1, "Yes", "No")) |>
  ungroup() |>
  select(gameId, home_b2b)

  # Away team back-to-back
away_b2b <- games |>
  select(gameId, game_date, team = AwayTeam) |>
  arrange(team, game_date) |>
  group_by(team) |>
  mutate(away_b2b = ifelse(game_date - lag(game_date) == 1, "Yes", "No")) |>
  ungroup() |>
  select(gameId, away_b2b)

  # Merge back into main data set
games <- games |>
  left_join(home_b2b, by = "gameId") |>
  left_join(away_b2b, by = "gameId")

#fixing winningTeam column
games <- games |>
  mutate(
    winningTeam = case_when(
      homeScore > awayScore ~ "Home",
      awayScore > homeScore ~ "Away",
      TRUE ~ NA_character_
    )
  )

#Reorder
games <- games |>
  select(gameId, game_date, game_time, HomeTeam, AwayTeam, homeScore, awayScore, winningTeam, winner, home_b2b, away_b2b, attendance)

#Rename
games <- games |>
  rename(GameId = gameId,
         Game_Date = game_date,
         Game_Time = game_time,
         HomeTeam = HomeTeam,
         AwayTeam = AwayTeam,
         HomeScore = homeScore,
         AwayScore = awayScore,
         WinningTeam = winningTeam,
         Winner = winner,
         Home_B2B = home_b2b,
         Away_B2B = away_b2b,
         Attendance = attendance)

#merge stadiums.csv and games.csv
HomeCourt <- games_updated |>
  left_join(
    stadiums_updated |>
      rename(
        HomeTeam = Team,
        HomeTeamLat = Lat,
        HomeTeamLong = Long,
        HomeTeamTimeZone = Timezone
      ),
    by = "HomeTeam"
  ) |>
  left_join(
    stadiums |>
      rename(
        AwayTeam = Team,
        AwayTeamLat = Lat,
        AwayTeamLong = Long,
        AwayTeamTimeZone = Timezone
      ),
    by = "AwayTeam"
  )

# reorder
HomeCourt <- HomeCourt |>
  select(GameId, Game_Date, Game_Time, HomeTeam, HomeTeamLat, HomeTeamLong, HomeTeamTimeZone, AwayTeam, AwayTeamLat, AwayTeamLong, AwayTeamTimeZone, HomeScore, AwayScore, WinningTeam, Winner, Home_B2B, Away_B2B, Attendance, League.y, Division.y)

# to get rid of postponed or cancelled games
HomeCourt <- HomeCourt |>
  filter(WinningTeam != "NA")

install.packages("geosphere")
library(geosphere)
# calculate away teams distance traveled 
HomeCourt <- HomeCourt |>
  mutate(
    Distance_Traveled =
      round(
        distHaversine(
        cbind(AwayTeamLong, AwayTeamLat),
        cbind(HomeTeamLong, HomeTeamLat)
      ) / 1609.34 # converting meters to miles
  ))

# reorder
HomeCourt <- HomeCourt |>
  select(GameId, Game_Date, Game_Time, HomeTeam, HomeTeamLat, HomeTeamLong, HomeTeamTimeZone, AwayTeam, AwayTeamLat, AwayTeamLong, AwayTeamTimeZone, HomeScore, AwayScore, WinningTeam, Winner, Home_B2B, Away_B2B, Distance_Traveled, Attendance, League.y, Division.y)

# creating a column calculating the difference in time zones
Home_court <- Home_court %>%
  mutate(
    HomeTeamTimeZoneNum = case_when(
      HomeTeamTimeZone == "EST" ~ 0,
      HomeTeamTimeZone == "CST" ~ -1,
      HomeTeamTimeZone == "MST" ~ -2,
      HomeTeamTimeZone == "PST" ~ -3
      ),
    AwayTeamTimeZoneNum = case_when(
      AwayTeamTimeZone == "EST" ~ 0,
      AwayTeamTimeZone == "CST" ~ -1,
      AwayTeamTimeZone == "MST" ~ -2,
      AwayTeamTimeZone == "PST" ~ -3
      ),
    TimeZoneDiffHours = HomeTeamTimeZoneNum - AwayTeamTimeZoneNum
    )



























