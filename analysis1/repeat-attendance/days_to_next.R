library(dplyr)
library(ggplot2)
library(scales)

days_to_next_registration = folkschool23_26combined_1_ %>%
  arrange(student_id, registered_date, registered_time) %>%
  group_by(student_id) %>%
  mutate(
    registration_order = row_number()
  ) %>%
  filter(registration_order <= 2) %>%
  summarise(
    first_registration = min(registered_date, na.rm = TRUE),
    second_registration = max(registered_date, na.rm = TRUE),
    n_registrations_kept = n(),
    .groups = "drop"
  ) %>%
  filter(n_registrations_kept == 2) %>%
  mutate(
    days_to_second_registration = as.numeric(second_registration - first_registration)
  )

timing_summary = days_to_next_registration %>%
  summarise(
    median_days = median(days_to_second_registration, na.rm = TRUE),
    q1_days = quantile(days_to_second_registration, 0.25, na.rm = TRUE),
    q3_days = quantile(days_to_second_registration, 0.75, na.rm = TRUE)
  )

days_to_next_registration_nonzero = days_to_next_registration %>%
  filter(days_to_second_registration > 0)

timing_summary_nonzero = days_to_next_registration_nonzero %>%
  summarise(
    median_days = median(days_to_second_registration, na.rm = TRUE),
    q1_days = quantile(days_to_second_registration, 0.25, na.rm = TRUE),
    q3_days = quantile(days_to_second_registration, 0.75, na.rm = TRUE)
  )



p = ggplot(days_to_next_registration_nonzero, aes(x = "", y = days_to_second_registration)) +
  geom_boxplot(
    fill = "#9ecae1",
    color = "#08519c",
    width = 0.35,
    outlier.alpha = 0.35
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      ceiling(max(days_to_next_registration_nonzero$days_to_second_registration, na.rm = TRUE) / 10) * 10,
      by = 50
    ),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    x = NULL,
    y = "Days from first to second registration",
    title = "Days to Next Registration",
    subtitle = "Excludes same-day second registrations"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "#08519c"),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#deebf7")
  )



ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/repeat-attendance/days_to_next_registration_boxplot_nonzero.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)