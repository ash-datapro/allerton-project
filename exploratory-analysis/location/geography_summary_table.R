library(dplyr)
library(stringr)
library(scales)
library(gt)

cities_dist_clean = cities_dist %>%
  mutate(
    city_state = str_squish(city_state),
    city_state = str_replace(city_state, ", [A-Z][a-z], ", ", "),
    city_state = str_replace(city_state, ", Il, IL$", ", IL")
  ) %>%
  group_by(city_state) %>%
  summarise(
    distance_miles = min(distance_miles, na.rm = TRUE),
    .groups = "drop"
  )

geo_summary = folkschool23_26combined_1_ %>%
  mutate(
    City = str_squish(str_to_title(str_to_lower(student_city))),
    State = str_to_upper(str_squish(student_state)),
    city_state = paste0(City, ", ", State),
    registration_total_num = as.numeric(registration_total)
  ) %>%
  left_join(
    cities_dist_clean,
    by = "city_state"
  ) %>%
  mutate(
    distance_bucket = case_when(
      is.na(distance_miles) ~ "Unknown distance",
      distance_miles <= 15 ~ "Local (0–15 miles)",
      distance_miles <= 30 ~ "Nearby regional (16–30 miles)",
      distance_miles <= 60 ~ "Extended regional (31–60 miles)",
      distance_miles > 60 ~ "Destination (60+ miles)"
    ),
    distance_bucket = factor(
      distance_bucket,
      levels = c(
        "Local (0–15 miles)",
        "Nearby regional (16–30 miles)",
        "Extended regional (31–60 miles)",
        "Destination (60+ miles)",
        "Unknown distance"
      )
    )
  ) %>%
  group_by(distance_bucket, student_id) %>%
  summarise(
    attendee_registrations = n(),
    attendee_revenue = sum(registration_total_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(distance_bucket) %>%
  summarise(
    registrations = sum(attendee_registrations, na.rm = TRUE),
    unique_attendees = n(),
    revenue = sum(attendee_revenue, na.rm = TRUE),
    average_spend = revenue / unique_attendees,
    repeat_rate = mean(attendee_registrations > 1, na.rm = TRUE),
    .groups = "drop"
  )

geo_table = geo_summary %>%
  gt() %>%
  cols_label(
    distance_bucket = "Geography bucket",
    registrations = "Registrations",
    unique_attendees = "Unique attendees",
    revenue = "Revenue",
    average_spend = "Average spend",
    repeat_rate = "Repeat rate"
  ) %>%
  fmt_number(
    columns = c(registrations, unique_attendees),
    decimals = 0
  ) %>%
  fmt_currency(
    columns = c(revenue, average_spend),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = repeat_rate,
    decimals = 1
  ) %>%
  tab_header(
    title = "Geography Summary",
    subtitle = "Distance band performance overview"
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(8)
  )

gtsave(
  data = geo_table,
  filename = "~/Desktop/Project/allerton-project/analysis1/location/plots/geography_summary_table.png"
)