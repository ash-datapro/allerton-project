library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(forcats)

registrations_by_subcategory = folkschool23_26combined_1_ %>%
  mutate(
    class_subcategory = str_squish(class_subcategory),
    class_subcategory = if_else(
      is.na(class_subcategory) | class_subcategory == "",
      "Unknown",
      class_subcategory
    )
  ) %>%
  group_by(class_subcategory) %>%
  summarise(
    registrations = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(registrations)) %>%
  mutate(
    pct_registrations = registrations / sum(registrations, na.rm = TRUE)
  )

p = ggplot(
  registrations_by_subcategory,
  aes(
    x = registrations,
    y = fct_reorder(class_subcategory, registrations)
  )
) +
  geom_col(fill = "#3182bd", width = 0.7) +
  geom_text(
    aes(label = paste0(comma(registrations), " (", percent(pct_registrations, accuracy = 0.1), ")")),
    hjust = -0.15,
    color = "#08306b",
    fontface = "bold",
    size = 3.8
  ) +
  scale_x_continuous(
    labels = label_number(),
    limits = c(0, max(registrations_by_subcategory$registrations, na.rm = TRUE) * 1.15),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Registrations",
    y = "Subcategory",
    title = "Registrations by Subcategory",
    subtitle = "Labels show registrations and share of total"
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
  filename = "~/Desktop/Project/allerton-project/analysis1/general/outputs/registrations_by_subcategory.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)