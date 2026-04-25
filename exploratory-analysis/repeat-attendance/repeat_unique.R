library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

attendee_type_summary = folkschool23_26combined_1_ %>%
  mutate(
    registration_total_num = as.numeric(registration_total)
  ) %>%
  group_by(student_id) %>%
  summarise(
    registrations = n(),
    revenue = sum(registration_total_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    attendee_type = case_when(
      registrations == 1 ~ "First-time",
      registrations > 1 ~ "Repeat"
    )
  ) %>%
  group_by(attendee_type) %>%
  summarise(
    unique_attendees = n(),
    registrations = sum(registrations, na.rm = TRUE),
    revenue = sum(revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    attendee_type = factor(attendee_type, levels = c("First-time", "Repeat"))
  )

p1 = ggplot(attendee_type_summary, aes(x = attendee_type, y = unique_attendees)) +
  geom_col(fill = "#3182bd", width = 0.7) +
  geom_text(
    aes(label = comma(unique_attendees)),
    vjust = -0.3,
    color = "#08306b",
    fontface = "bold",
    size = 4.5
  ) +
  scale_y_continuous(
    labels = label_number(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = NULL,
    y = "Unique attendees",
    title = "First-Time vs Repeat Attendees",
    subtitle = "By unique attendee count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.x = element_text(color = "#08306b"),
    axis.text.y = element_text(color = "#08519c"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#deebf7")
  )


ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/repeat-attendance/first_time_vs_repeat_unique_attendees.png",
  plot = p1,
  width = 7,
  height = 5,
  dpi = 300
)


p2 = ggplot(attendee_type_summary, aes(x = attendee_type, y = registrations)) +
  geom_col(fill = "#6baed6", width = 0.7) +
  geom_text(
    aes(label = comma(registrations)),
    vjust = -0.3,
    color = "#08306b",
    fontface = "bold",
    size = 4.5
  ) +
  scale_y_continuous(
    labels = label_number(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = NULL,
    y = "Registrations",
    title = "First-Time vs Repeat Attendees",
    subtitle = "By registrations"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.x = element_text(color = "#08306b"),
    axis.text.y = element_text(color = "#08519c"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#deebf7")
  )


ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/repeat-attendance/first_time_vs_repeat_registrations.png",
  plot = p2,
  width = 7,
  height = 5,
  dpi = 300
)

p3 = ggplot(attendee_type_summary, aes(x = attendee_type, y = revenue)) +
  geom_col(fill = "#9ecae1", width = 0.7) +
  geom_text(
    aes(label = dollar(revenue, accuracy = 1)),
    vjust = -0.3,
    color = "#08306b",
    fontface = "bold",
    size = 4.2
  ) +
  scale_y_continuous(
    labels = label_dollar(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = NULL,
    y = "Revenue",
    title = "First-Time vs Repeat Attendees",
    subtitle = "By revenue"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.x = element_text(color = "#08306b"),
    axis.text.y = element_text(color = "#08519c"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#deebf7")
  )



ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/repeat-attendance/first_time_vs_repeat_revenue.png",
  plot = p3,
  width = 7,
  height = 5,
  dpi = 300
)