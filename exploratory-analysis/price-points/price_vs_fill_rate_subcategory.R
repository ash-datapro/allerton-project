library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(forcats)

class_price_fill_rate_subcat = folkschool23_26combined_1_ %>%
  mutate(
    price_num = as.numeric(price),
    class_subcategory = str_squish(class_subcategory),
    class_subcategory = if_else(
      is.na(class_subcategory) | class_subcategory == "",
      "Unknown",
      class_subcategory
    )
  ) %>%
  group_by(class, class_begins, class_subcategory, price_num, max_seats, registrants) %>%
  summarise(.groups = "drop") %>%
  mutate(
    fill_rate = registrants / max_seats,
    class_revenue = registrants * price_num
  ) %>%
  filter(
    !is.na(price_num),
    !is.na(fill_rate),
    !is.na(max_seats),
    max_seats > 0,
    !is.na(class_subcategory)
  ) %>%
  group_by(class_subcategory) %>%
  mutate(
    subcategory_n = n()
  ) %>%
  ungroup() %>%
  filter(subcategory_n >= 20)

p = ggplot(
  class_price_fill_rate_subcat,
  aes(x = price_num, y = fill_rate)
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#cb181d",
    linewidth = 0.9
  ) +
  geom_point(
    aes(size = class_revenue),
    color = "#08519c",
    alpha = 0.55
  ) +
  facet_wrap(~ class_subcategory, scales = "free_x") +
  scale_x_continuous(
    labels = label_dollar(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_size_continuous(
    labels = label_dollar(accuracy = 1),
    breaks = pretty_breaks(n = 3)
  ) +
  labs(
    x = "Class price",
    y = "Fill rate",
    size = "Revenue",
    title = "Price vs Fill Rate by Subcategory",
    subtitle = "Each point is a class instance; trend lines highlight how pricing behaves differently by subcategory"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.x = element_text(color = "#08519c"),
    axis.text.y = element_text(color = "#08519c"),
    strip.text = element_text(face = "bold", color = "#08306b"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#deebf7")
  )

p

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/price-points/price_vs_fill_rate_by_subcategory.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300
)