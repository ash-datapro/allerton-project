# --- FIX Illinois bubble map (blue theme + better city coordinates) ---------
# This replaces your current IL map block.
# Key change: use the `uscities` package dataset (has small towns), so the
# “unmatched locations” problem goes away.

if (!requireNamespace("uscities", quietly = TRUE)) install.packages("uscities")
library(uscities)
library(tidyverse)
library(maps)
library(ggrepel)
library(scales)
library(stringr)

# 1) Read your city table again
q1_cities = readr::read_csv(file.path(tables_dir, "q1_top_50_cities.csv"), show_col_types = FALSE) %>%
  transmute(
    city_state = as.character(city_state),
    n = as.numeric(n),
    state_abbr = str_extract(city_state, "(?<=,\\s)[A-Z]{2}$"),
    city_input = str_trim(str_remove(city_state, ",\\s*[A-Z]{2}\\s*$"))
  ) %>%
  filter(state_abbr == "IL", !is.na(n))

# 2) Pull Illinois city coordinates (this dataset includes small towns)
il_places = uscities::uscities %>%
  as_tibble() %>%
  filter(state_id == "IL") %>%
  transmute(
    city_input = city,
    lat = lat,
    lon = long
  ) %>%
  mutate(
    city_norm = str_to_lower(str_replace_all(city_input, "[^a-z0-9\\s]", "")) %>% str_squish()
  )

q1_cities = q1_cities %>%
  mutate(
    city_norm = str_to_lower(str_replace_all(city_input, "[^a-z0-9\\s]", "")) %>% str_squish()
  )

# 3) Join coordinates (should match basically all)
city_points = q1_cities %>%
  left_join(il_places %>% select(city_norm, lat, lon), by = "city_norm") %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(label = paste0(str_to_title(city_input), ", IL"))

# If anything still unmatched, write it out
still_unmatched = q1_cities %>% anti_join(city_points, by = c("city_state", "n", "state_abbr", "city_input", "city_norm"))
if (nrow(still_unmatched) > 0) {
  readr::write_csv(still_unmatched, file.path(plots_dir, "01_il_city_map_still_unmatched.csv"))
}

# 4) Illinois background (counties + state outline)
il_counties = map_data("county") %>% filter(region == "illinois")
il_state = map_data("state") %>% filter(region == "illinois")

# Tight crop to Illinois
pad = 0.15
xlim = range(il_state$long, na.rm = TRUE) + c(-pad, pad)
ylim = range(il_state$lat, na.rm = TRUE) + c(-pad, pad)

# 5) Blue-theme bubble map
p_il_bubble = ggplot() +
  geom_polygon(
    data = il_counties,
    aes(long, lat, group = group),
    fill = "#EAF2FB",      # light blue
    color = "white",
    linewidth = 0.25
  ) +
  geom_polygon(
    data = il_state,
    aes(long, lat, group = group),
    fill = NA,
    color = "#1F5DA8",     # deep blue outline
    linewidth = 0.8
  ) +
  geom_point(
    data = city_points,
    aes(x = lon, y = lat, size = n),
    shape = 21,
    fill = "#2B6CB0",
    color = "white",
    stroke = 0.7,
    alpha = 0.75
  ) +
  ggrepel::geom_text_repel(
    data = city_points,
    aes(x = lon, y = lat, label = label),
    size = 3.3,
    color = "#123B6D",
    min.segment.length = 0,
    box.padding = 0.35,
    point.padding = 0.20,
    seed = 1,
    max.overlaps = 200
  ) +
  scale_size_continuous(
    name = "Registrations",
    range = c(2.5, 18),
    breaks = pretty_breaks(n = 5),
    labels = comma
  ) +
  coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(
    title = "Where registrants come from (Illinois city-level bubble map)",
    subtitle = "Bubbles sized by registrations; labels show city, IL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave(file.path(plots_dir, "01_il_city_bubble_map.png"), p_il_bubble, width = 13, height = 8, dpi = 300)