library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(forcats)

repeat_rate_by_first_subcategory = folkschool23_26combined_1_ %>%
  mutate(
    class_subcategory = str_squish(class_subcategory),
    class_subcategory = if_else(
      is.na(class_subcategory) | class_subcategory == "",
      "Unknown",
      class_subcategory
    )
  ) %>%
  arrange(student_id, class_begins, registered_date, registered_time) %>%
  group_by(student_id) %>%
  mutate(
    registration_order = row_number(),
    total_registrations = n()
  ) %>%
  filter(registration_order == 1) %>%
  ungroup() %>%
  mutate(
    first_subcategory = class_subcategory,
    returned_again = total_registrations > 1
  ) %>%
  group_by(first_subcategory) %>%
  summarise(
    attendees = n(),
    returned_attendees = sum(returned_again, na.rm = TRUE),
    repeat_rate = returned_attendees / attendees,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      percent(repeat_rate, accuracy = 0.1),
      " (",
      comma(returned_attendees),
      "/",
      comma(attendees),
      ")"
    )
  ) %>%
  arrange(desc(repeat_rate))

p = ggplot(
  repeat_rate_by_first_subcategory,
  aes(
    x = repeat_rate,
    y = fct_reorder(first_subcategory, repeat_rate)
  )
) +
  geom_segment(
    aes(x = 0, xend = repeat_rate, yend = first_subcategory),
    linewidth = 1,
    color = "#9ecae1"
  ) +
  geom_point(
    aes(size = attendees),
    color = "#08519c"
  ) +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    color = "#08306b",
    fontface = "bold",
    size = 3.6
  ) +
  scale_x_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 1.12),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_size_continuous(
    range = c(3, 8),
    breaks = pretty_breaks(n = 4),
    labels = label_number(accuracy = 1)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Percent who ever returned",
    y = "First class subcategory",
    title = "Repeat Rate by First Subcategory",
    subtitle = "Labels show returned attendees / total attendees; point size shows cohort size",
    size = "Attendees"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.y = element_text(color = "#08306b"),
    axis.text.x = element_text(color = "#08519c"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#deebf7"),
    plot.margin = margin(10, 80, 10, 10)
  )

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/repeat-attendance/repeat_rate_by_first_subcategory_weighted.png",
  plot = p,
  width = 11,
  height = 7,
  dpi = 300
)