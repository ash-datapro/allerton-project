library(dplyr)
library(scales)
library(gt)

retention_kpi = folkschool23_26combined_1_ %>%
  mutate(
    registration_total_num = as.numeric(registration_total)
  ) %>%
  group_by(student_id) %>%
  summarise(
    registrations = n(),
    revenue = sum(registration_total_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    attendee_type = case_when(
      registrations == 1 ~ "One-time",
      registrations > 1 ~ "Repeat"
    )
  )

retention_kpi_summary = retention_kpi %>%
  summarise(
    unique_attendees = n(),
    one_time_attendees = sum(attendee_type == "One-time", na.rm = TRUE),
    repeat_attendees = sum(attendee_type == "Repeat", na.rm = TRUE),
    repeat_pct = repeat_attendees / unique_attendees,
    repeat_registrations_share = sum(registrations[attendee_type == "Repeat"], na.rm = TRUE) / sum(registrations, na.rm = TRUE),
    repeat_revenue_share = sum(revenue[attendee_type == "Repeat"], na.rm = TRUE) / sum(revenue, na.rm = TRUE)
  ) %>%
  mutate(
    unique_attendees = comma(unique_attendees),
    one_time_attendees = comma(one_time_attendees),
    repeat_attendees = comma(repeat_attendees),
    repeat_pct = percent(repeat_pct, accuracy = 0.1),
    repeat_registrations_share = percent(repeat_registrations_share, accuracy = 0.1),
    repeat_revenue_share = percent(repeat_revenue_share, accuracy = 0.1)
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      unique_attendees = "Unique attendees",
      one_time_attendees = "One-time attendees",
      repeat_attendees = "Repeat attendees",
      repeat_pct = "Repeat %",
      repeat_registrations_share = "Repeat registrations share",
      repeat_revenue_share = "Repeat revenue share"
    )
  )

retention_kpi_table = retention_kpi_summary %>%
  gt() %>%
  cols_label(
    metric = "Retention KPI",
    value = "Value"
  ) %>%
  tab_header(
    title = "Retention KPI Table",
    subtitle = "First-time and repeat attendee summary"
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
  data = retention_kpi_table,
  filename = "retention_kpi_table.png"
)