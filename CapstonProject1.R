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
