# --- Illinois city bubble map using tigris (labels only for top 10) ----------

# Packages
pkgs = c("tidyverse", "readr", "stringr", "scales", "ggrepel", "tigris", "sf")
to_install = pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(scales)
  library(ggrepel)
  library(tigris)
  library(sf)
})

options(tigris_use_cache = TRUE)

# Paths
tables_dir = "~/Desktop/Project/allerton-project/analysis1/analysis_tables"
plots_dir  = "~/Desktop/Project/allerton-project/analysis1/analysis_plots_focus"
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# 1) Read city table
q1_cities = readr::read_csv(file.path(tables_dir, "q1_top_50_cities.csv"), show_col_types = FALSE) %>%
  transmute(
    city_state = as.character(city_state),
    n = as.numeric(n),
    state_abbr = str_extract(city_state, "(?<=,\\s)[A-Z]{2}$"),
    city = str_trim(str_remove(city_state, ",\\s*[A-Z]{2}\\s*$"))
  ) %>%
  filter(state_abbr == "IL", !is.na(n)) %>%
  mutate(
    city_norm = str_to_lower(str_replace_all(city, "[^a-z0-9\\s]", "")) %>% str_squish()
  )

# 2) Get Illinois places from Census via tigris (sf)
il_places = tigris::places(state = "IL", year = 2023, class = "sf") %>%
  st_transform(4326) %>%
  mutate(
    place_name = str_remove(NAME, "\\s+city$|\\s+village$|\\s+town$|\\s+CDP$"),
    place_norm = str_to_lower(str_replace_all(place_name, "[^a-z0-9\\s]", "")) %>% str_squish()
  )

# Use centroids for bubble locations
il_centroids = il_places %>%
  st_point_on_surface() %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(place_norm, lon, lat)

# 3) Join city counts to centroids
city_points = q1_cities %>%
  left_join(il_centroids, by = c("city_norm" = "place_norm"))

# Save any unmatched to inspect
unmatched = city_points %>% filter(is.na(lon) | is.na(lat))
if (nrow(unmatched) > 0) {
  readr::write_csv(unmatched, file.path(plots_dir, "01_il_city_map_unmatched_locations.csv"))
}

city_points = city_points %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  mutate(label = paste0(str_to_title(city), ", IL")) %>%
  arrange(desc(n)) %>%
  mutate(label_top10 = if_else(row_number() <= 10, label, NA_character_))

# 4) Illinois outline + counties (sf)
il_state = tigris::states(cb = TRUE, year = 2023, class = "sf") %>%
  filter(STUSPS == "IL") %>%
  st_transform(4326)

il_counties = tigris::counties(state = "IL", cb = TRUE, year = 2023, class = "sf") %>%
  st_transform(4326)

# 5) Blue-theme map (labels ONLY for top 10)
p_il_bubble = ggplot() +
  geom_sf(data = il_counties, fill = "#EAF2FB", color = "white", linewidth = 0.25) +
  geom_sf(data = il_state, fill = NA, color = "#1F5DA8", linewidth = 0.9) +
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
    data = city_points %>% filter(!is.na(label_top10)),
    aes(x = lon, y = lat, label = label_top10),
    size = 4,
    color = "#123B6D",
    fontface = "bold",
    min.segment.length = 0,
    box.padding = 0.35,
    point.padding = 0.20,
    seed = 1,
    max.overlaps = 200
  )+
  scale_size_continuous(
    name = "Registrations",
    range = c(2.5, 18),
    breaks = pretty_breaks(n = 5),
    labels = comma
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Where registrants come from (Illinois city-level bubble map)",
    subtitle = "Bubbles sized by registrations; labels shown for top 10 cities only"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave(
  file.path(plots_dir, "01_il_city_bubble_map.png"),
  p_il_bubble,
  width = 13, height = 8, dpi = 300
)