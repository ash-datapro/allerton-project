library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

best_selling_classes = folkclasses23_25 %>%
  group_by(Class_clean) %>%
  summarise(registrations = sum(Registrants, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(registrations)) %>%
  slice_head(n = 15)

x_max = ceiling(max(best_selling_classes$registrations, na.rm = TRUE) / 10) * 10

p = ggplot(
  best_selling_classes,
  aes(x = registrations, y = fct_reorder(Class_clean, registrations))
) +
  geom_segment(
    aes(x = 0, xend = registrations, yend = Class_clean),
    linewidth = 1,
    color = "#9ecae1"
  ) +
  geom_point(
    size = 3.5,
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
    title = "Best-Selling Classes"
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
  filename = file.path(plots_dir, "best_selling_classes_lollipop_blue.png"),
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)





library(dplyr)
library(ggplot2)
library(forcats)

zero_registration_classes = folkclasses23_25 %>%
  group_by(Class_clean) %>%
  summarise(registrations = sum(Registrants, na.rm = TRUE), .groups = "drop") %>%
  filter(registrations == 0)

p = ggplot(
  zero_registration_classes,
  aes(x = registrations, y = fct_reorder(Class_clean, registrations))
) +
  geom_point(
    size = 3.5,
    color = "#cb181d"
  ) +
  scale_x_continuous(
    breaks = 0,
    limits = c(-0.1, 0.1)
  ) +
  labs(
    x = "Number of registrations",
    y = "Class name",
    title = "Classes with Zero Registrations"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#67000d"),
    axis.title = element_text(face = "bold", color = "#cb181d"),
    axis.text.y = element_text(color = "#67000d"),
    axis.text.x = element_text(color = "#cb181d"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#fee5d9")
  )

p

ggsave(
  "~/Desktop/Project/allerton-project/analysis1/focused_plots/zero_registration_classes.png",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)


registration_classes = folkclasses23_25 %>%
  group_by(Class_clean) %>%
  summarise(registrations = sum(Registrants, na.rm = TRUE), .groups = "drop") %>%
  filter(registrations >=0)