library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

monthly_revenue = folkschool23_26combined_1_ %>%
  mutate(
    revenue_num = as.numeric(amount_paid_to_date),
    revenue_month = floor_date(class_begins, unit = "month")
  ) %>%
  group_by(revenue_month) %>%
  summarise(
    total_revenue = sum(revenue_num, na.rm = TRUE),
    .groups = "drop"
  )

p = ggplot(monthly_revenue, aes(x = revenue_month, y = total_revenue)) +
  geom_line(linewidth = 1, color = "#08519c") +
  geom_point(size = 2.2, color = "#08519c") +
  geom_text(
    aes(label = dollar(total_revenue, accuracy = 1)),
    vjust = -0.7,
    color = "#08306b",
    fontface = "bold",
    size = 3.2
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "2 months",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_dollar(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    x = "Month of class begins",
    y = "Total amount paid to date",
    title = "Monthly Revenue Over Time",
    subtitle = "Revenue summed by class month"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.x = element_text(color = "#08519c", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "#08519c"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#deebf7")
  )

p

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/general/outputs/monthly_revenue_over_time.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)