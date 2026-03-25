library(dplyr)
library(ggplot2)
library(scales)

class_price_fill_rate = folkschool23_26combined_1_ %>%
  mutate(
    price_num = as.numeric(price)
  ) %>%
  group_by(class, class_begins, price_num, max_seats, registrants) %>%
  summarise(.groups = "drop") %>%
  mutate(
    fill_rate = registrants / max_seats,
    class_revenue = registrants * price_num
  ) %>%
  filter(
    !is.na(price_num),
    !is.na(fill_rate),
    !is.na(max_seats),
    max_seats > 0
  )

p = ggplot(
  class_price_fill_rate,
  aes(x = price_num, y = fill_rate)
) +
  geom_point(
    aes(size = class_revenue),
    color = "#08519c",
    alpha = 0.6
  ) +
  scale_x_continuous(
    labels = label_dollar(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_size_continuous(
    labels = label_dollar(accuracy = 1),
    breaks = pretty_breaks(n = 4)
  ) +
  labs(
    x = "Class price",
    y = "Fill rate",
    size = "Revenue",
    title = "Class Price vs Fill Rate",
    subtitle = "Each point is a class instance; point size shows class revenue"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.x = element_text(color = "#08519c"),
    axis.text.y = element_text(color = "#08519c"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#deebf7")
  )

p

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/price-points/class_price_vs_fill_rate.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)