library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(stringr)

all_cities = folkstudents23_25 %>%
  mutate(
    City = str_trim(City),
    State = str_trim(State),
    City = str_to_title(str_to_lower(City)),
    City = case_when(
      str_detect(City, "^Champaign$") ~ "Champaign",
      TRUE ~ City
    ),
    State = case_when(
      City == "Champaign" ~ "IL",
      TRUE ~ State
    ),
    city_state = paste0(City, ", ", State)
  ) %>%
  filter(!is.na(City), City != "", !is.na(State), State != "") %>%
  count(city_state, name = "students", sort = TRUE) #%>%
  #slice_head(n=15)

x_max = ceiling(max(all_cities$students, na.rm = TRUE) / 10) * 10

p = ggplot(
  all_cities,
  aes(x = students, y = fct_reorder(city_state, students))
) +
  geom_segment(
    aes(x = 0, xend = students, yend = city_state),
    linewidth = 0.8,
    color = "#9ecae1"
  ) +
  geom_point(
    size = 2.8,
    color = "#08519c"
  ) +
  scale_x_continuous(
    breaks = seq(0, x_max, by = 50),
    limits = c(0, x_max),
    expand = expansion(mult = c(0, 0.02)),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    x = "Number of students",
    y = "City",
    title = "All Cities"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", color = "#08306b"),
    axis.title = element_text(face = "bold", color = "#08519c"),
    axis.text.y = element_text(size = 6, color = "#08306b"),
    axis.text.x = element_text(color = "#08519c"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#deebf7")
  )

p

ggsave(
  filename = "~/Desktop/Project/allerton-project/analysis1/focused_plots/all_cities.png",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)

write_csv(all_cities, "~/Desktop/Project/allerton-project/analysis1/all_cities.csv")
