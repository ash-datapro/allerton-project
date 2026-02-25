###############################################################################
# Allerton Folk School (2023–2025) — Tables for 3 Key Questions
###############################################################################

# ------------------------------- Packages ------------------------------------
packages_needed = c("tidyverse", "lubridate", "stringr", "readr", "scales", "forcats")
to_install = packages_needed[!packages_needed %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(readr)
  library(scales)
  library(forcats)
})

# ----------------------------- Preconditions ---------------------------------
stopifnot(exists("trans"))
stopifnot(exists("classes"))
stopifnot(exists("students"))
stopifnot(exists("out_tables"))

# ---------------------------- Helper functions -------------------------------
na_if_blank = function(x) {
  y = as.character(x)
  y = stringr::str_trim(y)
  y[y == ""] = NA_character_
  y
}

make_name_key = function(first, last) {
  str_to_lower(str_squish(paste0(na_if_blank(first), " ", na_if_blank(last))))
}

price_bins = function(x) {
  cut(
    x,
    breaks = c(-Inf, 0, 25, 50, 75, 100, 150, 200, 300, 500, Inf),
    labels = c(
      "Free/Refund/0", "$1–$25", "$26–$50", "$51–$75", "$76–$100",
      "$101–$150", "$151–$200", "$201–$300", "$301–$500", "$500+"
    ),
    right = TRUE,
    ordered_result = TRUE
  )
}

pick_first_existing = function(df, candidates) {
  existing = candidates[candidates %in% names(df)]
  if (length(existing) == 0) return(rep(NA_character_, nrow(df)))
  df[[existing[1]]]
}

pick_existing_name = function(df, candidates) {
  existing = candidates[candidates %in% names(df)]
  if (length(existing) == 0) return(NA_character_)
  existing[1]
}

mode_chr = function(x) {
  v = as.character(x)
  v = v[!is.na(v) & v != ""]
  if (length(v) == 0) return(NA_character_)
  names(sort(table(v), decreasing = TRUE))[1]
}

make_cancel_flag = function(x) {
  y = str_to_lower(str_trim(as.character(x)))
  dplyr::case_when(
    y %in% c("yes", "y", "true", "t", "1") ~ TRUE,
    y %in% c("no", "n", "false", "f", "0") ~ FALSE,
    y == "" ~ NA,
    is.na(y) ~ NA,
    TRUE ~ NA
  )
}

# --------------------------- Build analysis table ----------------------------
trans_people = trans %>%
  mutate(
    student_name_key = make_name_key(student_first_name, student_last_name),
    trans_person_key = coalesce(student_email, student_phone, paste0(student_name_key, "_", student_zip)),
    state = coalesce(na_if_blank(student_state), na_if_blank(customer_state)),
    city = coalesce(na_if_blank(student_city), na_if_blank(customer_city)),
    zip5 = coalesce(student_zip, customer_zip)
  )

if (exists("trans_classes")) {
  trans_people_classes = trans_classes %>%
    mutate(
      student_name_key = make_name_key(student_first_name, student_last_name),
      trans_person_key = coalesce(student_email, student_phone, paste0(student_name_key, "_", student_zip)),
      state = coalesce(na_if_blank(student_state), na_if_blank(customer_state)),
      city = coalesce(na_if_blank(student_city), na_if_blank(customer_city)),
      zip5 = coalesce(student_zip, customer_zip)
    )
} else {
  trans_tmp = trans %>%
    mutate(
      class_join_key = str_squish(coalesce(
        na_if_blank(pick_first_existing(., c("class_clean"))),
        na_if_blank(pick_first_existing(., c("class"))),
        na_if_blank(pick_first_existing(., c("class_name")))
      ))
    )
  
  classes_tmp = classes %>%
    mutate(
      class_join_key = str_squish(coalesce(
        na_if_blank(pick_first_existing(., c("class_clean"))),
        na_if_blank(pick_first_existing(., c("class_name"))),
        na_if_blank(pick_first_existing(., c("class")))
      ))
    )
  
  trans_people_classes = trans_tmp %>%
    left_join(
      classes_tmp %>%
        select(
          class_join_key,
          any_of(c("class_name", "class_clean", "instructor", "folk_school", "location", "room", "price_num"))
        ) %>%
        distinct(),
      by = "class_join_key"
    ) %>%
    mutate(
      student_name_key = make_name_key(student_first_name, student_last_name),
      trans_person_key = coalesce(student_email, student_phone, paste0(student_name_key, "_", student_zip)),
      state = coalesce(na_if_blank(student_state), na_if_blank(customer_state)),
      city = coalesce(na_if_blank(student_city), na_if_blank(customer_city)),
      zip5 = coalesce(student_zip, customer_zip)
    )
}

# ----------------- Standardize fields (avoid missing-column / closure issues) ----------------
trans_people_classes = trans_people_classes %>%
  mutate(
    # Always-available class identifier for counting distinct classes
    class_label = coalesce(
      na_if_blank(pick_first_existing(., c("class_clean"))),
      na_if_blank(pick_first_existing(., c("class_name"))),
      na_if_blank(pick_first_existing(., c("class")))
    ),
    
    # Safe versions of columns that may/may not exist in your join/export
    registered_on_dt_safe = suppressWarnings(as.Date(pick_first_existing(., c("registered_on_dt", "registered_on")))),
    registration_total_num_safe = suppressWarnings(as.numeric(pick_first_existing(., c("registration_total_num", "registration_total")))),
    amount_paid_to_date_num_safe = suppressWarnings(as.numeric(pick_first_existing(., c("amount_paid_to_date_num", "amount_paid_to_date")))),
    
    folk_school_safe = na_if_blank(pick_first_existing(., c("folk_school", "folk_school_class"))),
    location_safe = na_if_blank(pick_first_existing(., c("location", "location_class")))
  )

# Canceled flag: check multiple possible column names
cancel_col = pick_existing_name(trans_people_classes, c("canceled", "canceled_trans", "canceled_class"))
if (!is.na(cancel_col)) {
  trans_people_classes = trans_people_classes %>%
    mutate(canceled_flag = make_cancel_flag(.data[[cancel_col]]))
} else {
  trans_people_classes = trans_people_classes %>%
    mutate(canceled_flag = NA)
}

###############################################################################
# Q1) Where are the individuals from who are registering?
###############################################################################
q1_people_by_state = trans_people %>%
  filter(!is.na(trans_person_key)) %>%
  mutate(state = str_to_upper(state)) %>%
  distinct(trans_person_key, state) %>%
  count(state, sort = TRUE) %>%
  mutate(pct = n / sum(n))

write_csv(q1_people_by_state, file.path(out_tables, "q1_people_by_state.csv"))

q1_regs_by_state = trans_people %>%
  mutate(state = str_to_upper(state)) %>%
  count(state, sort = TRUE) %>%
  mutate(pct = n / sum(n))

write_csv(q1_regs_by_state, file.path(out_tables, "q1_registrations_by_state.csv"))

q1_top_cities = trans_people %>%
  filter(!is.na(city)) %>%
  mutate(
    state = str_to_upper(state),
    city_state = paste0(str_to_title(city), ", ", state)
  ) %>%
  count(city_state, sort = TRUE) %>%
  slice_head(n = 50)

write_csv(q1_top_cities, file.path(out_tables, "q1_top_50_cities.csv"))

q1_top_zip_people = trans_people %>%
  filter(!is.na(trans_person_key), !is.na(zip5)) %>%
  distinct(trans_person_key, zip5) %>%
  count(zip5, sort = TRUE) %>%
  slice_head(n = 50)

write_csv(q1_top_zip_people, file.path(out_tables, "q1_top_50_zip_by_people.csv"))

###############################################################################
# Q2) Which individuals are repeat attendees and what characteristics distinguish them?
###############################################################################
person_summary = trans_people_classes %>%
  filter(!is.na(trans_person_key)) %>%
  group_by(trans_person_key) %>%
  summarise(
    n_registrations = n(),
    n_distinct_classes = n_distinct(.data[["class_label"]], na.rm = TRUE),
    
    first_registration = suppressWarnings(min(.data[["registered_on_dt_safe"]], na.rm = TRUE)),
    last_registration  = suppressWarnings(max(.data[["registered_on_dt_safe"]], na.rm = TRUE)),
    
    avg_total = mean(.data[["registration_total_num_safe"]], na.rm = TRUE),
    med_total = median(.data[["registration_total_num_safe"]], na.rm = TRUE),
    total_spend = sum(.data[["amount_paid_to_date_num_safe"]], na.rm = TRUE),
    
    pct_canceled = mean(.data[["canceled_flag"]], na.rm = TRUE),
    
    state_mode = mode_chr(str_to_upper(state)),
    zip_mode = mode_chr(zip5),
    top_folk_school = mode_chr(.data[["folk_school_safe"]]),
    top_location = mode_chr(.data[["location_safe"]]),
    
    .groups = "drop"
  ) %>%
  mutate(
    repeat_flag = n_registrations >= 2,
    tenure_days = as.numeric(difftime(last_registration, first_registration, units = "days"))
  )

write_csv(person_summary, file.path(out_tables, "q2_person_summary.csv"))

q2_repeat_counts = person_summary %>%
  count(repeat_flag) %>%
  mutate(pct = n / sum(n))

write_csv(q2_repeat_counts, file.path(out_tables, "q2_repeat_counts.csv"))

q2_repeat_characteristics = person_summary %>%
  group_by(repeat_flag) %>%
  summarise(
    n_people = n(),
    avg_regs = mean(n_registrations),
    med_regs = median(n_registrations),
    avg_total_spend = mean(total_spend, na.rm = TRUE),
    med_total_spend = median(total_spend, na.rm = TRUE),
    avg_ticket = mean(avg_total, na.rm = TRUE),
    med_ticket = median(med_total, na.rm = TRUE),
    avg_unique_classes = mean(n_distinct_classes, na.rm = TRUE),
    med_unique_classes = median(n_distinct_classes, na.rm = TRUE),
    avg_tenure_days = mean(tenure_days, na.rm = TRUE),
    med_tenure_days = median(tenure_days, na.rm = TRUE),
    avg_cancel_rate = mean(pct_canceled, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(q2_repeat_characteristics, file.path(out_tables, "q2_repeat_characteristics.csv"))

q2_repeat_top_folk_school = trans_people_classes %>%
  filter(!is.na(trans_person_key)) %>%
  mutate(repeat_flag = trans_person_key %in% person_summary$trans_person_key[person_summary$repeat_flag]) %>%
  filter(repeat_flag) %>%
  count(.data[["folk_school_safe"]], sort = TRUE) %>%
  rename(folk_school = .data[["folk_school_safe"]])

write_csv(q2_repeat_top_folk_school, file.path(out_tables, "q2_repeat_top_folk_school.csv"))

q2_top_repeat_attendees = person_summary %>%
  arrange(desc(n_registrations), desc(total_spend)) %>%
  slice_head(n = 50)

write_csv(q2_top_repeat_attendees, file.path(out_tables, "q2_top_50_repeat_attendees.csv"))

###############################################################################
# Q3) Which individuals are buying at which price points?
###############################################################################
trans_prices = trans_people_classes %>%
  filter(!is.na(trans_person_key)) %>%
  mutate(
    ticket_total = .data[["registration_total_num_safe"]],
    ticket_class_price = suppressWarnings(as.numeric(pick_first_existing(., c("class_price_num", "price_num", "class_price", "price")))),
    total_bin = price_bins(ticket_total),
    class_price_bin = price_bins(ticket_class_price)
  )

q3_regs_by_total_bin = trans_prices %>%
  count(total_bin, sort = FALSE) %>%
  mutate(pct = n / sum(n))

write_csv(q3_regs_by_total_bin, file.path(out_tables, "q3_registrations_by_total_price_bin.csv"))

q3_people_by_median_total_bin = trans_prices %>%
  group_by(trans_person_key) %>%
  summarise(
    med_ticket_total = median(ticket_total, na.rm = TRUE),
    avg_ticket_total = mean(ticket_total, na.rm = TRUE),
    n_registrations = n(),
    .groups = "drop"
  ) %>%
  mutate(med_total_bin = price_bins(med_ticket_total)) %>%
  count(med_total_bin, sort = FALSE) %>%
  mutate(pct = n / sum(n))

write_csv(q3_people_by_median_total_bin, file.path(out_tables, "q3_people_by_median_total_price_bin.csv"))

q3_total_bin_by_folk_school = trans_prices %>%
  mutate(folk_school = fct_lump_n(as.factor(.data[["folk_school_safe"]]), n = 10, other_level = "Other/Unknown")) %>%
  count(folk_school, total_bin) %>%
  group_by(folk_school) %>%
  mutate(pct_within_school = n / sum(n)) %>%
  ungroup()

write_csv(q3_total_bin_by_folk_school, file.path(out_tables, "q3_total_price_bin_by_folk_school.csv"))

q3_spend_tiers = person_summary %>%
  mutate(
    spend_tier = cut(
      total_spend,
      breaks = c(-Inf, 0, 50, 100, 200, 500, 1000, Inf),
      labels = c("$0", "$1–$50", "$51–$100", "$101–$200", "$201–$500", "$501–$1,000", "$1,000+"),
      right = TRUE,
      ordered_result = TRUE
    )
  ) %>%
  count(spend_tier, sort = FALSE) %>%
  mutate(pct = n / sum(n))

write_csv(q3_spend_tiers, file.path(out_tables, "q3_people_by_total_spend_tier.csv"))

q3_high_price_cut = quantile(trans_prices$ticket_total, probs = 0.95, na.rm = TRUE)

q3_high_price_people = trans_prices %>%
  filter(!is.na(ticket_total), ticket_total >= q3_high_price_cut) %>%
  distinct(trans_person_key) %>%
  summarise(
    n_people = n(),
    cutoff_ticket_total_95pct = q3_high_price_cut
  )

write_csv(q3_high_price_people, file.path(out_tables, "q3_high_price_people_summary.csv"))

message("Question tables created in: ", out_tables)
###############################################################################