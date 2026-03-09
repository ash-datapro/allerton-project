###############################################################################
# Allerton Folk School (2023–2025) — Plots for 3 Key Questions (table-driven)
#
# This script DOES NOT rely on any data objects in the environment.
# It reads the CSV tables from a directory and writes PNG plots to an output dir.
#
# Inputs:
#   tables_dir = "~/Desktop/Project/allerton-project/analysis1/analysis_tables/"
# Output:
#   plots_dir  = "~/Desktop/Project/allerton-project/analysis1/analysis_plots/"
###############################################################################

# ------------------------------- Packages ------------------------------------
packages_needed = c("tidyverse", "scales", "forcats", "stringr", "readr")
to_install = packages_needed[!packages_needed %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(forcats)
  library(stringr)
  library(readr)
})

# ------------------------------- Paths ---------------------------------------
tables_dir = "~/Desktop/Project/allerton-project/analysis1/analysis_tables"
plots_dir = "~/Desktop/Project/allerton-project/analysis1/analysis_plots"

stopifnot(dir.exists(tables_dir))
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# ---------------------------- Helper functions -------------------------------
read_tbl = function(file) {
  path = file.path(tables_dir, file)
  stopifnot(file.exists(path))
  readr::read_csv(path, show_col_types = FALSE)
}

save_png = function(p, filename, width = 9, height = 5) {
  ggsave(
    filename = file.path(plots_dir, filename),
    plot = p,
    width = width,
    height = height,
    dpi = 300
  )
}

theme_report = function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

ensure_col = function(df, col, default = NA) {
  if (!col %in% names(df)) df[[col]] = default
  df
}

# ------------------------------- Q1 Plots ------------------------------------
# Q1: Where are individuals from?

# 1) Unique people by state (Top 15)
q1_people_by_state = read_tbl("q1_people_by_state.csv") %>%
  ensure_col("state", "Unknown") %>%
  ensure_col("n", 0) %>%
  mutate(state = replace_na(state, "Unknown")) %>%
  arrange(desc(n))

p_q1_state_people = q1_people_by_state %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = fct_reorder(state, n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Where registrants come from (Unique Individuals) — Top 15 States",
    x = "State",
    y = "Unique individuals"
  ) +
  theme_report()

save_png(p_q1_state_people, "q1_people_by_state_top15.png", width = 8, height = 5)

# 2) Registrations by state (Top 15)
q1_regs_by_state = read_tbl("q1_registrations_by_state.csv") %>%
  ensure_col("state", "Unknown") %>%
  ensure_col("n", 0) %>%
  mutate(state = replace_na(state, "Unknown")) %>%
  arrange(desc(n))

p_q1_state_regs = q1_regs_by_state %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = fct_reorder(state, n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Where registrations come from (All Registrations) — Top 15 States",
    x = "State",
    y = "Registrations"
  ) +
  theme_report()

save_png(p_q1_state_regs, "q1_registrations_by_state_top15.png", width = 8, height = 5)

# 3) Top cities (Top 20)
q1_top_cities = read_tbl("q1_top_50_cities.csv") %>%
  ensure_col("city_state", "Unknown") %>%
  ensure_col("n", 0) %>%
  mutate(city_state = replace_na(city_state, "Unknown")) %>%
  arrange(desc(n))

p_q1_cities = q1_top_cities %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = fct_reorder(city_state, n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top Cities by Registrations (Top 20)",
    x = "City, State",
    y = "Registrations"
  ) +
  theme_report()

save_png(p_q1_cities, "q1_top_cities_top20.png", width = 9, height = 6)

# 4) Zip concentration (Top 20 zips by unique people)
q1_top_zip_people = read_tbl("q1_top_50_zip_by_people.csv") %>%
  ensure_col("zip5", "Unknown") %>%
  ensure_col("n", 0) %>%
  mutate(
    zip5 = as.character(zip5),
    zip5 = if_else(is.na(zip5) | zip5 == "", "Unknown", zip5)
  ) %>%
  arrange(desc(n))

p_q1_zip = q1_top_zip_people %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = fct_reorder(zip5, n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top ZIP Codes by Unique Individuals (Top 20)",
    x = "ZIP",
    y = "Unique individuals"
  ) +
  theme_report()

save_png(p_q1_zip, "q1_top_zip_people_top20.png", width = 8, height = 6)

# ------------------------------- Q2 Plots ------------------------------------
# Q2: Repeat attendees and distinguishing characteristics

# 1) Repeat vs one-time counts
q2_repeat_counts = read_tbl("q2_repeat_counts.csv") %>%
  ensure_col("repeat_flag", FALSE) %>%
  ensure_col("n", 0) %>%
  mutate(
    repeat_flag = as.logical(repeat_flag),
    group = if_else(repeat_flag, "Repeat (2+)", "One-time (1)")
  ) %>%
  arrange(desc(n))

p_q2_repeat_counts = ggplot(q2_repeat_counts, aes(x = group, y = n)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Repeat vs One-time Attendees",
    x = NULL,
    y = "Individuals"
  ) +
  theme_report()

save_png(p_q2_repeat_counts, "q2_repeat_vs_one_time.png", width = 7, height = 5)

# 2) Characteristic comparison (selected metrics)
q2_repeat_characteristics = read_tbl("q2_repeat_characteristics.csv") %>%
  ensure_col("repeat_flag", FALSE) %>%
  ensure_col("n_people", NA_real_) %>%
  ensure_col("avg_ticket", NA_real_) %>%
  ensure_col("avg_total_spend", NA_real_) %>%
  ensure_col("avg_unique_classes", NA_real_) %>%
  ensure_col("avg_tenure_days", NA_real_) %>%
  ensure_col("avg_cancel_rate", NA_real_) %>%
  mutate(
    repeat_flag = as.logical(repeat_flag),
    group = if_else(repeat_flag, "Repeat (2+)", "One-time (1)")
  ) %>%
  select(group, n_people, avg_ticket, avg_total_spend, avg_unique_classes, avg_tenure_days, avg_cancel_rate) %>%
  pivot_longer(cols = -group, names_to = "metric", values_to = "value") %>%
  mutate(
    metric = recode(
      metric,
      n_people = "People",
      avg_ticket = "Avg ticket ($)",
      avg_total_spend = "Avg total spend ($)",
      avg_unique_classes = "Avg unique classes",
      avg_tenure_days = "Avg tenure (days)",
      avg_cancel_rate = "Avg cancel rate"
    )
  )

p_q2_metrics = ggplot(q2_repeat_characteristics, aes(x = metric, y = value, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  labs(
    title = "How Repeat Attendees Differ (Selected Averages)",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_report()

save_png(p_q2_metrics, "q2_repeat_vs_one_time_characteristics.png", width = 10, height = 6)

# 3) Top Folk Schools among repeat attendees
q2_repeat_top_folk_school = read_tbl("q2_repeat_top_folk_school.csv") %>%
  ensure_col("folk_school", "Unknown") %>%
  ensure_col("n", 0) %>%
  mutate(folk_school = replace_na(folk_school, "Unknown")) %>%
  arrange(desc(n))

p_q2_repeat_school = q2_repeat_top_folk_school %>%
  slice_head(n = 12) %>%
  ggplot(aes(x = fct_reorder(folk_school, n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Repeat Attendees — Registrations by Folk School (Top 12)",
    x = "Folk School",
    y = "Registrations (repeat attendees only)"
  ) +
  theme_report()

save_png(p_q2_repeat_school, "q2_repeat_top_folk_school_top12.png", width = 9, height = 5)

# 4) Concentration among top repeat attendees (Top 50)
q2_top_repeat = read_tbl("q2_top_50_repeat_attendees.csv")
q2_top_repeat = ensure_col(q2_top_repeat, "n_registrations", NA_real_) %>%
  filter(!is.na(n_registrations)) %>%
  mutate(rank = row_number())

if (nrow(q2_top_repeat) > 0) {
  p_q2_top_repeat = ggplot(q2_top_repeat, aes(x = rank, y = n_registrations)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Top Repeat Attendees — Registrations Concentration (Top 50)",
      x = "Rank (Top 50)",
      y = "Registrations"
    ) +
    theme_report()
  
  save_png(p_q2_top_repeat, "q2_top_repeat_attendees_concentration.png", width = 9, height = 5)
}

# ------------------------------- Q3 Plots ------------------------------------
# Q3: Which individuals are buying at which price points?

# 1) Registrations by total price bin
q3_regs_by_total_bin = read_tbl("q3_registrations_by_total_price_bin.csv") %>%
  ensure_col("total_bin", NA_character_) %>%
  ensure_col("n", 0) %>%
  filter(!is.na(total_bin)) %>%
  mutate(total_bin = as.factor(total_bin))

p_q3_regs_by_bin = ggplot(q3_regs_by_total_bin, aes(x = total_bin, y = n)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Registrations by Transaction Total (Price Bins)",
    x = "Price bin (registration total)",
    y = "Registrations"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_png(p_q3_regs_by_bin, "q3_registrations_by_total_price_bin.png", width = 10, height = 5)

# 2) People by median ticket bin
q3_people_by_median = read_tbl("q3_people_by_median_total_price_bin.csv") %>%
  ensure_col("med_total_bin", NA_character_) %>%
  ensure_col("n", 0) %>%
  filter(!is.na(med_total_bin)) %>%
  mutate(med_total_bin = as.factor(med_total_bin))

p_q3_people_by_median = ggplot(q3_people_by_median, aes(x = med_total_bin, y = n)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Individuals by Typical Transaction Total (Median Ticket Bin)",
    x = "Median ticket bin (per person)",
    y = "Individuals"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_png(p_q3_people_by_median, "q3_people_by_median_total_price_bin.png", width = 10, height = 5)

# 3) Price bins by Folk School (heatmap)
q3_by_school = read_tbl("q3_total_price_bin_by_folk_school.csv") %>%
  ensure_col("folk_school", "Unknown") %>%
  ensure_col("total_bin", NA_character_) %>%
  ensure_col("n", 0) %>%
  ensure_col("pct_within_school", NA_real_) %>%
  filter(!is.na(total_bin)) %>%
  mutate(
    folk_school = as.character(folk_school),
    total_bin = as.character(total_bin)
  )

top_schools = q3_by_school %>%
  group_by(folk_school) %>%
  summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_n)) %>%
  slice_head(n = 10) %>%
  pull(folk_school)

q3_by_school_plot = q3_by_school %>%
  filter(folk_school %in% top_schools)

p_q3_heat = ggplot(
  q3_by_school_plot,
  aes(
    x = total_bin,
    y = fct_reorder(folk_school, n, .fun = sum),
    fill = pct_within_school
  )
) +
  geom_tile() +
  scale_fill_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Price Mix by Folk School (Top 10 Schools)",
    x = "Transaction total price bin",
    y = "Folk School",
    fill = "% within school"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_png(p_q3_heat, "q3_price_mix_heatmap_top10_schools.png", width = 11, height = 6)

# 4) People by total spend tier
q3_spend_tiers = read_tbl("q3_people_by_total_spend_tier.csv") %>%
  ensure_col("spend_tier", NA_character_) %>%
  ensure_col("n", 0) %>%
  filter(!is.na(spend_tier)) %>%
  mutate(spend_tier = as.factor(spend_tier))

p_q3_spend_tiers = ggplot(q3_spend_tiers, aes(x = spend_tier, y = n)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Individuals by Total Spend Tier",
    x = "Total spend tier (per person)",
    y = "Individuals"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_png(p_q3_spend_tiers, "q3_people_by_total_spend_tier.png", width = 10, height = 5)

# 5) Optional: summary table plot (high-price cutoff)
q3_high_price = read_tbl("q3_high_price_people_summary.csv")
q3_high_price = ensure_col(q3_high_price, "n_people", NA_real_)
q3_high_price = ensure_col(q3_high_price, "cutoff_ticket_total_95pct", NA_real_)

if (nrow(q3_high_price) > 0) {
  p_q3_high_price = q3_high_price %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    ggplot(aes(x = metric, y = value)) +
    geom_col() +
    labs(
      title = "High-price segment summary (Top 5% ticket cutoff)",
      x = NULL,
      y = NULL
    ) +
    theme_report() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
  save_png(p_q3_high_price, "q3_high_price_people_summary.png", width = 8, height = 5)
}

message("Plots saved to: ", plots_dir)