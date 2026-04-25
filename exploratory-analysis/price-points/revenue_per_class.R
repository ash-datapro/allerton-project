library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

revenue_per_class_by_price_band = folkschool23_26combined_1_ %>%
  mutate(
    price_num = as.numeric(price)
  ) %>%
  group_by(class, class_begins, price_num, registrants, price) %>%
  summarise(.groups = "drop") %>%
  mutate(
    class_revenue = registrants * price_num,
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
  filter(!is.na(price_band), !is.na(class_revenue)) %>%
  group_by(price_band) %>%
  summarise(
    avg_class_revenue = mean(class_revenue, na.rm = TRUE),
    median_class_revenue = median(class_revenue, na.rm = TRUE),
    class_instances = n(),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      "Avg ", dollar(avg_class_revenue, accuracy = 1),
      #" • Med ", dollar(median_class_revenue, accuracy = 1),
      " • n=", comma(class_instances)
    )
  )

p = ggplot(revenue_per_class_by_price_band, aes(x = price_band, y = avg_class_revenue)) +
  geom_col(fill = "#3182bd", width = 0.7) +
  geom_text(
    aes(label = label),
    vjust = -0.3,
    color = "#08306b",
    fontface = "bold",
    size = 3.5
  ) +
  scale_y_continuous(
    labels = label_dollar(accuracy = 1),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    x = "Price band",
    y = "Average revenue per class",
    title = "Revenue per Class by Price Band",
    subtitle = "Labels show average revenue, median revenue, and class count"
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
  filename = "~/Desktop/Project/allerton-project/analysis1/price-points/revenue_per_class_by_price_band.png",
  plot = p,
  width = 9,
  height = 5.5,
  dpi = 300
)