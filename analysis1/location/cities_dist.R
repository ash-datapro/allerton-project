library(dplyr)
library(tidyr)
library(tidygeocoder)
library(geosphere)

monticello_lat = 40.0275
monticello_lon = -88.5734

cities_dist = cities %>%
  separate(
    city_state,
    into = c("city", "state"),
    sep = ",\\s*",
    remove = FALSE,
    extra = "merge",
    fill = "right"
  ) %>%
  mutate(
    search_place = paste(city, state, sep = ", ")
  ) %>%
  geocode(search_place, method = "osm", lat = lat, long = lon) %>%
  mutate(
    distance_miles = if_else(
      is.na(lat) | is.na(lon),
      NA_real_,
      distHaversine(
        cbind(lon, lat),
        c(monticello_lon, monticello_lat)
      ) / 1609.34
    ),
    distance_miles = round(distance_miles, 1),
    distance_note = if_else(
      is.na(distance_miles),
      "unknown",
      "matched"
    )
  ) %>%
  select(city_state, students, distance_miles, distance_note)

write_csv(cities_dist, "~/Desktop/Project/allerton-project/analysis1/cities_dist.csv")
