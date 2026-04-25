

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

registrations_over_time = folkschool23_26combined %>%
  filter(!is.na(registered_date)) %>%
  count(registered_date, name = "registrations") %>%
  complete(
    registered_date = seq(min(registered_date), max(registered_date), by = "day"),
    fill = list(registrations = 0)
  )

p = ggplot(registrations_over_time, aes(x = registered_date, y = registrations)) +
  geom_line(linewidth = 0.9, color = "#08519c") +
  geom_point(size = 1.5, color = "#08519c") +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "2 months",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 8),
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
    axis.text.x = element_text(color = "#08519c", angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(color = "#08519c", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#deebf7")
  )


ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/general/outputs//registrations_over_time.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)


