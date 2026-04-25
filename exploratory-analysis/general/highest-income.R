library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(readr)

highest_income_classes = folkclasses23_25 %>%
  mutate(
    price_num = parse_number(Price),
    income = price_num * Registrants #formula
  ) %>%
  group_by(Class_clean) %>%
  summarise(
    total_income = sum(income, na.rm = TRUE),
    total_registrations = sum(Registrants, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_income)) %>%
  slice_head(n = 15)

x_max = ceiling(max(highest_income_classes$total_income, na.rm = TRUE) / 100) * 100

p = ggplot(
  highest_income_classes,
  aes(x = total_income, y = fct_reorder(Class_clean, total_income))
) +
  geom_segment(
    aes(x = 0, xend = total_income, yend = Class_clean),
    linewidth = 1,
    color = "#9ecae1"
  ) +
  geom_point(
    size = 3.5,
    color = "#08519c"
  ) +
  scale_x_continuous(
    breaks = seq(0, x_max, by = 2000),
    limits = c(0, x_max),
    expand = expansion(mult = c(0, 0.02)),
    labels = label_dollar(accuracy = 1)
  ) +
  labs(
    x = "Total income",
    y = "Class name",
    title = "Highest-Income Classes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.y = element_text(color = "#08306b"),
    axis.text.x = element_text(color = "#08519c"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#deebf7")
  )

p

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/focused_plots/highest_income_classes.png",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)