library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

cohort_retention = folkschool23_26combined_1_ %>%
  group_by(student_id) %>%
  summarise(
    first_class_date = min(class_begins, na.rm = TRUE),
    total_registrations = n(),
    returned_again = total_registrations > 1,
    .groups = "drop"
  ) %>%
  mutate(
    cohort_month = floor_date(first_class_date, unit = "month")
  ) %>%
  group_by(cohort_month) %>%
  summarise(
    attendees = n(),
    returned_attendees = sum(returned_again, na.rm = TRUE),
    return_rate = returned_attendees / attendees,
    .groups = "drop"
  )

p = ggplot(cohort_retention, aes(x = cohort_month, y = return_rate)) +
  geom_col(fill = "#3182bd", width = 25) +
  geom_text(
    aes(label = percent(return_rate, accuracy = 0.1)),
    vjust = -0.3,
    color = "#08306b",
    fontface = "bold",
    size = 3.8
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "2 months",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "First class cohort month",
    y = "Returned again rate",
    title = "Cohort Retention by First Class Month",
    subtitle = "Percent of attendees in each cohort who registered again later"
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



ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/repeat-attendance/cohort_retention_monthly.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)