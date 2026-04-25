library(dplyr)
library(scales)
library(gt)
library(tidyr)

price_band_summary = folkschool23_26combined_1_ %>%
  mutate(
    price_num = as.numeric(price)
  ) %>%
  group_by(class, class_begins, price_num, max_seats, registrants, waiting_list) %>%
  summarise(.groups = "drop") %>%
  mutate(
    fill_rate = registrants / max_seats,
    sellout = registrants >= max_seats,
    class_revenue = registrants * price_num,
    price_band = case_when(
      price_num < 25 ~ "<25",
      price_num >= 25 & price_num <= 49 ~ "25–49",
      price_num >= 50 & price_num <= 74 ~ "50–74",
      price_num >= 75 & price_num <= 99 ~ "75–99",
      price_num >= 100 & price_num <= 149 ~ "100–149",
      price_num >= 150 ~ "150+",
      TRUE ~ NA_character_
    ),
    price_band = factor(
      price_band,
      levels = c("<25", "25–49", "50–74", "75–99", "100–149", "150+")
    )
  ) %>%
  filter(
    !is.na(price_band),
    !is.na(price_num),
    !is.na(max_seats),
    max_seats > 0
  ) %>%
  group_by(price_band) %>%
  summarise(
    class_count = n(),
    registrations = sum(registrants, na.rm = TRUE),
    avg_registrants = mean(registrants, na.rm = TRUE),
    avg_fill_rate = mean(fill_rate, na.rm = TRUE),
    sellout_rate = mean(sellout, na.rm = TRUE),
    avg_waitlist = mean(waiting_list, na.rm = TRUE),
    total_revenue = sum(class_revenue, na.rm = TRUE),
    avg_revenue_per_class = mean(class_revenue, na.rm = TRUE),
    .groups = "drop"
  )

price_band_table = price_band_summary %>%
  gt() %>%
  cols_label(
    price_band = "Price band",
    class_count = "Class count",
    registrations = "Registrations",
    avg_registrants = "Avg registrants",
    avg_fill_rate = "Avg fill rate",
    sellout_rate = "Sellout rate",
    avg_waitlist = "Avg waitlist",
    total_revenue = "Total revenue",
    avg_revenue_per_class = "Avg revenue per class"
  ) %>%
  fmt_number(
    columns = c(class_count, registrations),
    decimals = 0
  ) %>%
  fmt_number(
    columns = c(avg_registrants, avg_waitlist),
    decimals = 1
  ) %>%
  fmt_percent(
    columns = c(avg_fill_rate, sellout_rate),
    decimals = 1
  ) %>%
  fmt_currency(
    columns = c(total_revenue, avg_revenue_per_class),
    decimals = 0
  ) %>%
  tab_header(
    title = "Price-Band Summary Table",
    subtitle = "Demand and economics by class price band"
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
    table.font.size = px(12),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(8)
  )

gtsave(
  data = price_band_table,
  filename = "~/Desktop/Project/allerton-project/analysis1/price-points/price_band_summary_table.png"
)