library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

revenue_by_visit_frequency = folkschool23_26combined_1_ %>%
  mutate(
    registration_total_num = as.numeric(registration_total)
  ) %>%
  group_by(student_id) %>%
  summarise(
    registrations = n(),
    total_revenue = sum(registration_total_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    visit_bucket = case_when(
      registrations == 1 ~ "1",
      registrations == 2 ~ "2",
      registrations == 3 ~ "3",
      registrations %in% 4:5 ~ "4–5",
      registrations >= 6 ~ "6+"
    ),
    visit_bucket = factor(
      visit_bucket,
      levels = c("1", "2", "3", "4–5", "6+")
    )
  ) %>%
  group_by(visit_bucket) %>%
  summarise(
    total_revenue = sum(total_revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_revenue = total_revenue / sum(total_revenue, na.rm = TRUE)
  )

p = ggplot(revenue_by_visit_frequency, aes(x = visit_bucket, y = total_revenue)) +
  geom_col(fill = "#3182bd", width = 0.7) +
  geom_text(
    aes(label = paste0(dollar(total_revenue, accuracy = 1), " (", percent(pct_revenue, accuracy = 0.1), ")")),
    vjust = -0.3,
    color = "#08306b",
    fontface = "bold",
    size = 3.6
  )+
  scale_y_continuous(
    labels = label_dollar(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Visit frequency bucket",
    y = "Total revenue",
    title = "Revenue Contribution by Visit Frequency",
    subtitle = "Labels show share of total revenue"
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

p

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/repeat-attendance/revenue_by_visit_frequency.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)