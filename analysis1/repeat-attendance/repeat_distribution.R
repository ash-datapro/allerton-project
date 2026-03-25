library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

visit_frequency = folkschool23_26combined_1_ %>%
  group_by(student_id) %>%
  summarise(
    registrations = n(),
    .groups = "drop"
  ) %>%
  mutate(
    visit_bucket = case_when(
      registrations == 1 ~ "1",
      registrations == 2 ~ "2",
      registrations == 3 ~ "3",
      registrations %in% 4:5 ~ "4–5",
      registrations >= 6 ~ "6+"
    ),
    visit_bucket = factor(
      visit_bucket,
      levels = c("1", "2", "3", "4–5", "6+")
    )
  ) %>%
  count(visit_bucket, name = "attendees")

p = ggplot(visit_frequency, aes(x = visit_bucket, y = attendees)) +
  geom_col(fill = "#3182bd", width = 0.7) +
  geom_text(
    aes(label = comma(attendees)),
    vjust = -0.3,
    color = "#08306b",
    fontface = "bold",
    size = 4.5
  ) +
  scale_y_continuous(
    labels = label_number(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Number of registrations per attendee",
    y = "Number of attendees",
    title = "Distribution of Visit Frequency",
    subtitle = "Attendees grouped by registration count"
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



ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/repeat-attendance/distribution_visit_frequency.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)