library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(stringr)

zip_registrations = folkschool23_26combined %>%
  mutate(
    ZIP = str_pad(as.character(student_zip), width = 5, side = "left", pad = "0")
  ) %>%
  filter(!is.na(student_zip), ZIP != "NA") %>%
  count(ZIP, name = "registrations", sort = TRUE) %>%
  mutate(
    pct_total = registrations / sum(registrations)
  )

top_15_zips = zip_registrations %>%
  slice_head(n = 15)

x_max = ceiling(max(top_15_zips$registrations, na.rm = TRUE) / 10) * 10

p = ggplot(
  top_15_zips,
  aes(x = registrations, y = fct_reorder(ZIP, registrations))
) +
  geom_col(fill = "#3182bd", width = 0.75) +
  geom_text(
    aes(label = paste0(registrations, " (", percent(pct_total, accuracy = 0.1), ")")),
    hjust = -0.15,
    color = "#08306b",
    fontface = "bold",
    size = 3.3
  ) +
  scale_x_continuous(
    breaks = seq(0, x_max, by = 50),
    limits = c(0, x_max * 1.12),
    expand = expansion(mult = c(0, 0.02)),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    x = "Registrations",
    y = "ZIP",
    title = "Top 15 ZIP Codes by Registrations",
    subtitle = "Labels show share of total registrations"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#08519c"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.y = element_text(color = "#08306b"),
    axis.text.x = element_text(color = "#08519c"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#deebf7")
  )

p

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/location/plots/top15_zipcodes.png",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)