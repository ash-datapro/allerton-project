library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

fill_rate_by_price_band = folkschool23_26combined_1_ %>%
  mutate(
    price_num = as.numeric(price),
    fill_rate = registrants / max_seats,
    price_band = case_when(
      price_num < 25 ~ "<25",
      price_num >= 25 & price_num <= 49 ~ "25–49",
      price_num >= 50 & price_num <= 74 ~ "50–74",
      price_num >= 75 & price_num <= 99 ~ "75–99",
      price_num >= 100 & price_num <= 149 ~ "100–149",
      price_num >= 150 ~ "150+",
      TRUE ~ NA_character_
    ),
    price_band = factor(
      price_band,
      levels = c("<25", "25–49", "50–74", "75–99", "100–149", "150+")
    )
  ) %>%
  filter(
    !is.na(price_band),
    !is.na(fill_rate),
    !is.na(max_seats),
    max_seats > 0
  ) %>%
  group_by(price_band) %>%
  summarise(
    avg_fill_rate = mean(fill_rate, na.rm = TRUE),
    classes = n(),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(percent(avg_fill_rate, accuracy = 0.1), " • n=", comma(classes))
  )

p = ggplot(fill_rate_by_price_band, aes(x = price_band, y = avg_fill_rate)) +
  geom_col(fill = "#3182bd", width = 0.7) +
  geom_text(
    aes(label = label),
    vjust = -0.3,
    color = "#08306b",
    fontface = "bold",
    size = 3.8
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "Price band",
    y = "Average fill rate",
    title = "Fill Rate by Price Band",
    subtitle = "Fill rate = registrants / max seats; labels show average fill rate and class count"
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
  filename = "~/Desktop/Project/allerton-project/analysis1/price-points/fill_rate_by_price_band.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)