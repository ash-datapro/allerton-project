library(dplyr)
library(scales)
library(gt)
library(tidyr)

program_kpi_summary = folkschool23_26combined_1_ %>%
  mutate(
    revenue_num = as.numeric(amount_paid_to_date),
    price_num = as.numeric(price)
  ) %>%
  summarise(
    total_registrations = n(),
    unique_attendees = n_distinct(student_id),
    total_revenue = sum(revenue_num, na.rm = TRUE)
  )

class_level_kpis = folkschool23_26combined_1_ %>%
  mutate(
    revenue_num = as.numeric(amount_paid_to_date),
    price_num = as.numeric(price)
  ) %>%
  group_by(class, class_begins, price_num, registrants, max_seats, waiting_list) %>%
  summarise(
    class_revenue = sum(revenue_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(max_seats), max_seats > 0)

program_kpi_table_data = class_level_kpis %>%
  summarise(
    unique_classes = n(),
    average_revenue_per_class = mean(class_revenue, na.rm = TRUE),
    average_class_price = mean(price_num, na.rm = TRUE),
    average_fill_rate = mean(registrants / max_seats, na.rm = TRUE),
    average_waitlist_size = mean(waiting_list, na.rm = TRUE)
  ) %>%
  bind_cols(program_kpi_summary) %>%
  select(
    total_registrations,
    unique_attendees,
    unique_classes,
    total_revenue,
    average_revenue_per_class,
    average_class_price,
    average_fill_rate,
    average_waitlist_size
  ) %>%
  mutate(
    total_registrations = comma(total_registrations),
    unique_attendees = comma(unique_attendees),
    unique_classes = comma(unique_classes),
    total_revenue = dollar(total_revenue, accuracy = 1),
    average_revenue_per_class = dollar(average_revenue_per_class, accuracy = 1),
    average_class_price = dollar(average_class_price, accuracy = 1),
    average_fill_rate = percent(average_fill_rate, accuracy = 0.1),
    average_waitlist_size = number(average_waitlist_size, accuracy = 0.1)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      total_registrations = "Total registrations",
      unique_attendees = "Unique attendees",
      unique_classes = "Unique classes",
      total_revenue = "Total revenue",
      average_revenue_per_class = "Average revenue per class",
      average_class_price = "Average class price",
      average_fill_rate = "Average fill rate",
      average_waitlist_size = "Average waitlist size"
    )
  )

program_kpi_table = program_kpi_table_data %>%
  gt() %>%
  cols_label(
    metric = "Program KPI",
    value = "Value"
  ) %>%
  tab_header(
    title = "Program KPI Table",
    subtitle = "Overall program performance summary"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#deebf7"),
      cell_text(weight = "bold", color = "#08306b")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(color = "#08306b", weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(color = "#08519c"),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(10)
  )

gtsave(
  data = program_kpi_table,
  filename = "~/Desktop/Project/allerton-project/analysis1/general/outputs/program_kpi_table.png"
)