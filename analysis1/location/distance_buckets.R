library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(scales)


sus_values = c(
  "Unit #1102, IL",
  "Xx, IL",
  "God, IL",
  "Apt 1, IL",
  "Da, IL"
)

cities_dist = cities_dist %>%
  mutate(
    distance_miles = case_when(
      city_state %in% c("Saint Joseph, IL", "Mt Zion, IL", "Mt. Zion, IL") ~ 35,
      city_state %in% sus_values ~ NA_real_,
      TRUE ~ distance_miles
    ),
    distance_note = case_when(
      city_state %in% c("Saint Joseph, IL", "Mt Zion, IL", "Mt. Zion, IL") ~ "corrected",
      city_state %in% sus_values ~ "unknown",
      TRUE ~ distance_note
    )
  )

distance_buckets = cities_dist %>%
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
  group_by(distance_bucket) %>%
  summarise(
    students = sum(students, na.rm = TRUE),
    .groups = "drop"
  )

p = ggplot(distance_buckets, aes(x = distance_bucket, y = students)) +
  geom_col(fill = "#3182bd", width = 0.7) +
  geom_text(
    aes(label = comma(students)),
    vjust = -0.3,
    color = "#08306b",
    fontface = "bold",
    size = 4
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Distance bucket",
    y = "Number of students",
    title = "Students by Distance from Allerton"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.x = element_text(color = "#08306b", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "#08519c"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#deebf7")
  )


ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/focused_plots/student_distance_buckets.png",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)

#cities_dist %>%arrange(desc(distance_miles)) %>%slice_head(n = 20)
