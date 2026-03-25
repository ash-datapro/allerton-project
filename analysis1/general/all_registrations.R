library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

registration_classes_plot = registration_classes %>%
  arrange(desc(registrations))

x_max = ceiling(max(registration_classes_plot$registrations, na.rm = TRUE) / 10) * 10

p = ggplot(
  registration_classes_plot,
  aes(
    x = registrations,
    y = fct_reorder(Class_clean, registrations)
  )
) +
  geom_segment(
    aes(x = 0, xend = registrations, yend = Class_clean),
    linewidth = 0.8,
    color = "#9ecae1"
  ) +
  geom_point(
    size = 2.8,
    color = "#08519c"
  ) +
  scale_x_continuous(
    breaks = seq(0, x_max, by = 20),
    limits = c(0, x_max),
    expand = expansion(mult = c(0, 0.02)),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    x = "Number of registrations",
    y = "Class name",
    title = "All Classes by Registrations"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.y = element_text(size = 7, color = "#08306b"),
    axis.text.x = element_text(color = "#08519c"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#deebf7")
  )

p

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/focused_plots/all_registrations.png",
  plot = p,
  width = 10,
  height = 18,
  dpi = 300
)