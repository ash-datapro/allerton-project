library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

cities_dist_clean = cities_dist %>%
  mutate(
    city_state = str_squish(city_state),
    city_state = str_replace(city_state, ", [A-Z][a-z], ", ", "),
    city_state = str_replace(city_state, ", Il, IL$", ", IL")
  ) %>%
  group_by(city_state) %>%
  summarise(
    distance_miles = min(distance_miles, na.rm = TRUE),
    .groups = "drop"
  )

attendee_spend = folkschool23_26combined_1_ %>%
  mutate(
    City = str_squish(str_to_title(str_to_lower(student_city))),
    State = str_to_upper(str_squish(student_state)),
    City = case_when(
      City == "Champaign" ~ "Champaign",
      TRUE ~ City
    ),
    State = case_when(
      City == "Champaign" ~ "IL",
      TRUE ~ State
    ),
    city_state = paste0(City, ", ", State),
    registration_total_num = as.numeric(registration_total)
  ) %>%
  left_join(
    cities_dist_clean,
    by = "city_state"
  ) %>%
  mutate(
    distance_bucket = case_when(
      is.na(distance_miles) ~ "Unknown distance",
      distance_miles <= 15 ~ "Local (0–15 miles)",
      distance_miles <= 30 ~ "Nearby regional (16–30 miles)",
      distance_miles <= 60 ~ "Extended regional (31–60 miles)",
      distance_miles > 60 ~ "Destination (60+ miles)"
    ),
    distance_bucket = factor(
      distance_bucket,
      levels = c(
        "Local (0–15 miles)",
        "Nearby regional (16–30 miles)",
        "Extended regional (31–60 miles)",
        "Destination (60+ miles)",
        "Unknown distance"
      )
    )
  ) %>%
  group_by(student_id, distance_bucket) %>%
  summarise(
    attendee_total_spend = sum(registration_total_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(distance_bucket) %>%
  summarise(
    avg_spend_per_attendee = mean(attendee_total_spend, na.rm = TRUE),
    n_attendees = n(),
    .groups = "drop"
  )

p = ggplot(attendee_spend, aes(x = distance_bucket, y = avg_spend_per_attendee)) +
  geom_col(fill = "#3182bd", width = 0.7) +
  geom_text(
    aes(label = dollar(avg_spend_per_attendee, accuracy = 1)),
    vjust = -0.3,
    color = "#08306b",
    fontface = "bold",
    size = 4
  ) +
  scale_y_continuous(
    labels = label_dollar(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Distance band",
    y = "Average total spend per attendee",
    title = "Distance Band vs Average Spend",
    subtitle = "Average total spend per attendee within each distance band"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.x = element_text(color = "#08306b", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "#08519c"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#deebf7")
  )

p

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/location/plots/distance_band_avg_spend_per_attendee.png",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)