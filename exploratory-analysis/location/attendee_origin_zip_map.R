library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(maps)
library(zipcodeR)

zip_map_data = folkschool23_26combined_1_ %>%
  mutate(
    ZIP = str_pad(as.character(student_zip), width = 5, side = "left", pad = "0")
  ) %>%
  filter(!is.na(student_zip), ZIP != "NA") %>%
  group_by(ZIP) %>%
  summarise(
    attendees = n_distinct(student_id),
    .groups = "drop"
  ) %>%
  left_join(
    zipcodeR::zip_code_db %>%
      transmute(
        ZIP = zipcode,
        lat = lat,
        lng = lng,
        state = state
      ),
    by = "ZIP"
  ) %>%
  filter(!is.na(lat), !is.na(lng))

us_map = map_data("state")

p = ggplot() +
  geom_polygon(
    data = us_map,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    color = "grey75",
    linewidth = 0.4
  )+
  geom_point(
    data = zip_map_data,
    aes(x = lng, y = lat, size = attendees),
    color = "#08519c",
    alpha = 0.65
  ) +
  coord_map() +
  scale_size_continuous(
    range = c(2, 12),
    breaks = pretty_breaks(n = 5),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = "Attendee Origin by ZIP",
    subtitle = "Bubble size shows number of unique attendees",
    x = NULL,
    y = NULL,
    size = "Unique attendees"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )



ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/location/plots/attendee_origin_zip_map.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 300
)