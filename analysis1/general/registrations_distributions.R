library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

registrations_over_time = folktrans23_25 %>%
  mutate(
    registered_on = mdy_hm(registered_on),
    registered_date = as_date(registered_on)
  ) %>%
  count(registered_date, name = "registrations")

p = ggplot(registrations_over_time, aes(x = registered_date, y = registrations)) +
  geom_line(linewidth = 0.9, color = "#08519c") +
  geom_point(size = 1.8, color = "#08519c") +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "2 months",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      ceiling(max(registrations_over_time$registrations, na.rm = TRUE) / 50) * 50,
      by = 50
    ),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Registration date",
    y = "Number of registrations",
    title = "Registrations Over Time"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.x = element_text(color = "#08519c", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "#08519c"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#deebf7")
  )
p

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/focused_plots/registrations_over_time.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)