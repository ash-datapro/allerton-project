library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

class_count_by_month = folkschool23_26combined_1_ %>%
  mutate(
    class_month = floor_date(class_begins, unit = "month")
  ) %>%
  distinct(class, class_begins, class_month) %>%
  group_by(class_month) %>%
  summarise(
    class_count = n(),
    .groups = "drop"
  )

p = ggplot(class_count_by_month, aes(x = class_month, y = class_count)) +
  geom_line(linewidth = 1, color = "#08519c") +
  geom_point(size = 2.2, color = "#08519c") +
  geom_text(
    aes(label = comma(class_count)),
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
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    x = "Month of class begins",
    y = "Distinct class count",
    title = "Class Count by Month",
    subtitle = "Distinct class instances based on class + class_begins"
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
  filename = "~/Desktop/Project/allerton-project/analysis1/general/outputs/class_count_by_month.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)