###############################################################################
# Allerton Folk School EDA Script (2023–2025)
# Inputs expected in your environment:
#   folktrans23_25   (registrations / transactions; ~6,401 x 57)
#   folkclasses23_25 (class offerings; ~484 x 24)
#   folkstudents23_25 (student/customer roster; ~4,185 x 16)
#
# Output:
#   /output/eda_tables/*.csv
#   /output/eda_plots/*.png
###############################################################################

# ------------------------------- Packages ------------------------------------
packages_needed = c(
  "tidyverse", "lubridate", "janitor", "skimr", "stringr", "readr",
  "scales", "ggplot2", "forcats"
)

to_install = packages_needed[!packages_needed %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(skimr)
  library(stringr)
  library(readr)
  library(scales)
  library(ggplot2)
  library(forcats)
})

# ---------------------------- Helper functions -------------------------------
make_dir = function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

iconv_utf8 = function(x) {
  # Convert anything to UTF-8; replace invalid bytes with a placeholder
  if (!is.character(x)) return(x)
  # from = "" lets R guess the native encoding; sub replaces invalid bytes
  iconv(x, from = "", to = "UTF-8", sub = "byte")
}

parse_money = function(x) {
  readr::parse_number(as.character(x), locale = locale(grouping_mark = ","))
}

parse_mdy_safe = function(x) {
  suppressWarnings(lubridate::mdy(as.character(x)))
}

parse_ymd_safe = function(x) {
  suppressWarnings(lubridate::ymd(as.character(x)))
}

na_if_blank = function(x) {
  y = iconv_utf8(as.character(x))
  y = stringr::str_trim(y)
  y[y == ""] = NA_character_
  y
}

clean_yes_no = function(x) {
  y = iconv_utf8(as.character(x))
  y = str_to_lower(str_trim(y))
  dplyr::case_when(
    y %in% c("yes", "y", "true", "t", "1") ~ "Yes",
    y %in% c("no", "n", "false", "f", "0") ~ "No",
    y == "" ~ NA_character_,
    TRUE ~ str_to_title(y)
  )
}

std_email = function(x) {
  y = str_to_lower(na_if_blank(x))
  y
}

std_phone = function(x) {
  y = na_if_blank(x)
  str_replace_all(y, "[^0-9]", "")
}

std_zip = function(x) {
  y = na_if_blank(x)
  y = str_replace_all(y, "[^0-9]", "")
  dplyr::if_else(!is.na(y) & nchar(y) >= 5, str_sub(y, 1, 5), y)
}

theme_set(theme_minimal(base_size = 12))

# ------------------------------ Output paths ---------------------------------
out_base = "~/Desktop/Project/allerton-project/eda"
out_tables = file.path(out_base, "eda_tables")
out_plots = file.path(out_base, "eda_plots")

make_dir(out_tables)
make_dir(out_plots)

# --------------------------- 0) Quick data checks ----------------------------
stopifnot(exists("folktrans23_25"))
stopifnot(exists("folkclasses23_25"))
stopifnot(exists("folkstudents23_25"))

# ------------------------- 1) Standardize column names -----------------------
# Normalize ALL character columns to safe UTF-8 immediately after import.
trans_raw = folktrans23_25 %>%
  janitor::clean_names() %>%
  mutate(across(where(is.character), iconv_utf8))

classes_raw = folkclasses23_25 %>%
  janitor::clean_names() %>%
  mutate(across(where(is.character), iconv_utf8))

students_raw = folkstudents23_25 %>%
  janitor::clean_names() %>%
  mutate(across(where(is.character), iconv_utf8))

# ------------------------- 2) Type parsing / cleaning ------------------------
# Transactions / Registrations
trans = trans_raw %>%
  mutate(
    registration_id = na_if_blank(registration_id),
    
    registered_on_dt = coalesce(
      parse_ymd_safe(registered_on),
      parse_mdy_safe(registered_on)
    ),
    
    class_begins_dt = coalesce(
      parse_ymd_safe(class_begins),
      parse_mdy_safe(class_begins)
    ),
    
    fully_paid_on_dt = coalesce(
      parse_ymd_safe(fully_paid_on),
      parse_mdy_safe(fully_paid_on)
    ),
    
    canceled = clean_yes_no(canceled),
    
    class_price_num = parse_money(class_price),
    add_on_total_num = parse_money(add_on_total),
    discount_total_num = parse_money(discount_total),
    registration_total_num = parse_money(registration_total),
    amount_paid_to_date_num = parse_money(amount_paid_to_date),
    amount_remaining_num = parse_money(amount_remaining),
    
    student_email = std_email(student_email),
    customer_email = std_email(customer_email),
    student_phone = std_phone(student_phone),
    customer_phone = std_phone(customer_phone),
    student_zip = std_zip(student_zip),
    customer_zip = std_zip(customer_zip),
    
    class_clean = if ("class_clean" %in% names(trans_raw)) class_clean else NA_character_
  )

# Classes / Offerings
classes = classes_raw %>%
  mutate(
    start_date_dt = coalesce(parse_mdy_safe(start_date), parse_ymd_safe(start_date)),
    end_date_dt = coalesce(parse_mdy_safe(end_date), parse_ymd_safe(end_date)),
    
    price_num = parse_money(price),
    
    featured = clean_yes_no(featured),
    kids_class = clean_yes_no(kids_class),
    register_online = clean_yes_no(register_online),
    continuous_enrollment = clean_yes_no(continuous_enrollment),
    canceled = clean_yes_no(canceled),
    
    visibility = na_if_blank(visibility),
    class_type = na_if_blank(class_type),
    folk_school = na_if_blank(folk_school),
    
    class_clean = na_if_blank(class_clean),
    class_name = na_if_blank(class_name),
    instructor = na_if_blank(instructor),
    location = na_if_blank(location),
    room = na_if_blank(room),
    days_of_week = na_if_blank(days_of_week)
  )

# Students / Roster
students = students_raw %>%
  mutate(
    first_name = na_if_blank(first_name),
    last_name = na_if_blank(last_name),
    email = std_email(email),
    phone = std_phone(phone),
    additional_phone_number_standard_form = std_phone(additional_phone_number_standard_form),
    zip = std_zip(zip),
    
    last_registered_on_dt = last_registered_on,
    
    date_of_birth_dt = coalesce(
      parse_ymd_safe(date_of_birth_standard_form),
      parse_mdy_safe(date_of_birth_standard_form)
    ),
    
    gender_standard_form = na_if_blank(gender_standard_form),
    race_ethnicity_standard_form = na_if_blank(race_ethnicity_standard_form),
    are_you_of_hispanic_latinx_or_spanish_origin_standard_form =
      na_if_blank(are_you_of_hispanic_latinx_or_spanish_origin_standard_form),
    are_you_a_university_of_illinois_student_standard_form =
      na_if_blank(are_you_a_university_of_illinois_student_standard_form)
  )

# --------------------------- 3) High-level summaries -------------------------
skim_trans = skimr::skim(trans)
skim_classes = skimr::skim(classes)
skim_students = skimr::skim(students)

write_csv(as_tibble(skim_trans), file.path(out_tables, "skim_transactions.csv"))
write_csv(as_tibble(skim_classes), file.path(out_tables, "skim_classes.csv"))
write_csv(as_tibble(skim_students), file.path(out_tables, "skim_students.csv"))

miss_tbl = function(df) {
  df %>%
    summarise(across(everything(), ~mean(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
    mutate(pct_missing = round(100 * pct_missing, 1)) %>%
    arrange(desc(pct_missing))
}

write_csv(miss_tbl(trans), file.path(out_tables, "missingness_transactions.csv"))
write_csv(miss_tbl(classes), file.path(out_tables, "missingness_classes.csv"))
write_csv(miss_tbl(students), file.path(out_tables, "missingness_students.csv"))

# ------------------------ 4) Key joins / integrity checks --------------------
trans = trans %>%
  mutate(
    class_join_key = coalesce(na_if_blank(class_clean), na_if_blank(class)),
    class_join_key = str_squish(class_join_key)
  )

classes = classes %>%
  mutate(
    class_join_key = coalesce(na_if_blank(class_clean), na_if_blank(class_name)),
    class_join_key = str_squish(class_join_key)
  )

trans_classes = trans %>%
  left_join(
    classes %>%
      select(
        class_join_key, class_name, class_clean, instructor, folk_school, location, room,
        start_date_dt, end_date_dt, start_time, end_time, max_seats, min_seats,
        registrants, waiting_list, price_num, canceled, visibility, kids_class, featured
      ) %>%
      distinct(),
    by = "class_join_key",
    suffix = c("_trans", "_class")
  )

join_match_rate = trans_classes %>%
  summarise(
    n_trans = n(),
    pct_matched = mean(!is.na(class_name)) * 100
  )

write_csv(join_match_rate, file.path(out_tables, "join_match_rate_trans_to_classes.csv"))

unmatched_keys = trans_classes %>%
  filter(is.na(class_name)) %>%
  count(class_join_key, sort = TRUE) %>%
  slice_head(n = 50)

write_csv(unmatched_keys, file.path(out_tables, "unmatched_class_keys_top50.csv"))

# ---------------------------- 5) EDA: Time trends ----------------------------
regs_by_month = trans %>%
  filter(!is.na(registered_on_dt)) %>%
  mutate(month = floor_date(registered_on_dt, "month")) %>%
  count(month) %>%
  arrange(month)

write_csv(regs_by_month, file.path(out_tables, "registrations_by_month.csv"))

p_regs_month = ggplot(regs_by_month, aes(x = month, y = n)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    title = "Registrations by Month",
    x = "Month",
    y = "Number of registrations"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(out_plots, "registrations_by_month.png"),
  plot = p_regs_month, width = 9, height = 5, dpi = 300
)

classes_by_month = classes %>%
  filter(!is.na(start_date_dt)) %>%
  mutate(month = floor_date(start_date_dt, "month")) %>%
  count(month) %>%
  arrange(month)

write_csv(classes_by_month, file.path(out_tables, "classes_by_month.csv"))

p_classes_month = ggplot(classes_by_month, aes(x = month, y = n)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    title = "Classes Offered by Month (Start Date)",
    x = "Month",
    y = "Number of classes"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(out_plots, "classes_by_month.png"),
  plot = p_classes_month, width = 9, height = 5, dpi = 300
)

# --------------------------- 6) EDA: Folk School -----------------------------
regs_by_school = trans_classes %>%
  mutate(folk_school = fct_lump_n(as.factor(folk_school), n = 10, other_level = "Other/Unknown")) %>%
  count(folk_school, sort = TRUE)

write_csv(regs_by_school, file.path(out_tables, "registrations_by_folk_school.csv"))

p_regs_school = ggplot(regs_by_school, aes(x = fct_reorder(folk_school, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Registrations by Folk School",
    x = "Folk School",
    y = "Number of registrations"
  )

ggsave(
  filename = file.path(out_plots, "registrations_by_folk_school.png"),
  plot = p_regs_school, width = 8, height = 5, dpi = 300
)

classes_by_school = classes %>%
  mutate(folk_school = fct_lump_n(as.factor(folk_school), n = 10, other_level = "Other/Unknown")) %>%
  count(folk_school, sort = TRUE)

write_csv(classes_by_school, file.path(out_tables, "classes_by_folk_school.csv"))

p_classes_school = ggplot(classes_by_school, aes(x = fct_reorder(folk_school, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Classes Offered by Folk School",
    x = "Folk School",
    y = "Number of classes"
  )

ggsave(
  filename = file.path(out_plots, "classes_by_folk_school.png"),
  plot = p_classes_school, width = 8, height = 5, dpi = 300
)

# ---------------------------- 7) EDA: Pricing --------------------------------
p_total_hist = trans %>%
  filter(!is.na(registration_total_num)) %>%
  ggplot(aes(x = registration_total_num)) +
  geom_histogram(bins = 40) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "Distribution of Registration Total",
    x = "Registration total",
    y = "Count"
  )

ggsave(
  filename = file.path(out_plots, "registration_total_hist.png"),
  plot = p_total_hist, width = 8, height = 5, dpi = 300
)

avg_total_by_school = trans_classes %>%
  group_by(folk_school) %>%
  summarise(
    n = n(),
    avg_total = mean(registration_total_num, na.rm = TRUE),
    med_total = median(registration_total_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

write_csv(avg_total_by_school, file.path(out_tables, "avg_registration_total_by_folk_school.csv"))

p_avg_total_school = avg_total_by_school %>%
  filter(!is.na(folk_school)) %>%
  mutate(folk_school = fct_lump_n(as.factor(folk_school), n = 12, other_level = "Other/Unknown")) %>%
  group_by(folk_school) %>%
  summarise(avg_total = mean(avg_total, na.rm = TRUE), n = sum(n), .groups = "drop") %>%
  ggplot(aes(x = fct_reorder(folk_school, avg_total), y = avg_total)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Average Registration Total by Folk School",
    x = "Folk School",
    y = "Average registration total"
  )

ggsave(
  filename = file.path(out_plots, "avg_registration_total_by_folk_school.png"),
  plot = p_avg_total_school, width = 8, height = 5, dpi = 300
)

# ----------------------- 8) EDA: Seats / Fill rates --------------------------
classes_fill = classes %>%
  mutate(
    fill_rate = dplyr::if_else(!is.na(max_seats) & max_seats > 0,
                               registrants / max_seats,
                               NA_real_)
  )

fill_summary = classes_fill %>%
  summarise(
    n_classes = n(),
    pct_with_max = mean(!is.na(max_seats)) * 100,
    avg_fill = mean(fill_rate, na.rm = TRUE),
    med_fill = median(fill_rate, na.rm = TRUE),
    pct_over_capacity = mean(fill_rate > 1, na.rm = TRUE) * 100
  )

write_csv(fill_summary, file.path(out_tables, "fill_rate_summary.csv"))

p_fill_hist = classes_fill %>%
  filter(!is.na(fill_rate)) %>%
  ggplot(aes(x = fill_rate)) +
  geom_histogram(bins = 40) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Distribution of Class Fill Rates (Registrants / Max Seats)",
    x = "Fill rate",
    y = "Count"
  )

ggsave(
  filename = file.path(out_plots, "fill_rate_hist.png"),
  plot = p_fill_hist, width = 8, height = 5, dpi = 300
)

fill_by_school = classes_fill %>%
  group_by(folk_school) %>%
  summarise(
    n = n(),
    avg_fill = mean(fill_rate, na.rm = TRUE),
    med_fill = median(fill_rate, na.rm = TRUE),
    avg_waitlist = mean(waiting_list, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

write_csv(fill_by_school, file.path(out_tables, "fill_rate_by_folk_school.csv"))

# -------------------------- 9) Repeat students / retention -------------------
students_keyed = students %>%
  mutate(
    name_key = str_to_lower(str_squish(paste0(first_name, " ", last_name))),
    person_key = coalesce(email, phone, paste0(name_key, "_", zip))
  )

trans_people = trans %>%
  mutate(
    student_name_key = str_to_lower(str_squish(paste0(student_first_name, " ", student_last_name))),
    trans_person_key = coalesce(student_email, student_phone, paste0(student_name_key, "_", student_zip))
  )

repeat_tbl = trans_people %>%
  filter(!is.na(trans_person_key)) %>%
  count(trans_person_key, name = "n_registrations") %>%
  mutate(repeat_flag = n_registrations >= 2) %>%
  summarise(
    n_people = n(),
    pct_repeat = mean(repeat_flag) * 100,
    avg_regs_per_person = mean(n_registrations),
    med_regs_per_person = median(n_registrations)
  )

write_csv(repeat_tbl, file.path(out_tables, "repeat_registration_summary.csv"))

p_regs_per_person = trans_people %>%
  filter(!is.na(trans_person_key)) %>%
  count(trans_person_key, name = "n_registrations") %>%
  ggplot(aes(x = n_registrations)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "Registrations per Person (Proxy key from email/phone/name+zip)",
    x = "Number of registrations",
    y = "Number of people"
  )

ggsave(
  filename = file.path(out_plots, "registrations_per_person.png"),
  plot = p_regs_per_person, width = 8, height = 5, dpi = 300
)

# -------------------------- 10) Geography (basic) ----------------------------
state_counts = trans %>%
  mutate(state = coalesce(na_if_blank(student_state), na_if_blank(customer_state))) %>%
  mutate(state = str_to_upper(state)) %>%
  count(state, sort = TRUE)

write_csv(state_counts, file.path(out_tables, "registrations_by_state.csv"))

p_state = state_counts %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = fct_reorder(state, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Registrations by State (Top 15)",
    x = "State",
    y = "Number of registrations"
  )

ggsave(
  filename = file.path(out_plots, "registrations_by_state_top15.png"),
  plot = p_state, width = 7, height = 5, dpi = 300
)

# --------------------- 11) Cancellations / payment status --------------------
cancel_rate = trans %>%
  summarise(
    n = n(),
    pct_canceled = mean(canceled == "Yes", na.rm = TRUE) * 100
  )

write_csv(cancel_rate, file.path(out_tables, "cancellation_rate.csv"))

payment_tbl = trans %>%
  mutate(
    fully_paid_flag = !is.na(fully_paid_on_dt) | (amount_remaining_num %in% c(0, 0.00)),
    fully_paid_flag = if_else(is.na(fully_paid_flag), FALSE, fully_paid_flag)
  ) %>%
  summarise(
    n = n(),
    pct_fully_paid = mean(fully_paid_flag) * 100,
    avg_remaining = mean(amount_remaining_num, na.rm = TRUE)
  )

write_csv(payment_tbl, file.path(out_tables, "payment_summary.csv"))

# --------------------------- 12) “Top lists” tables --------------------------
top_classes_by_regs = trans %>%
  mutate(class_label = coalesce(na_if_blank(class_clean), na_if_blank(class))) %>%
  count(class_label, sort = TRUE) %>%
  slice_head(n = 30)

write_csv(top_classes_by_regs, file.path(out_tables, "top_30_classes_by_registrations.csv"))

top_instructors = classes %>%
  filter(!is.na(instructor)) %>%
  group_by(instructor) %>%
  summarise(
    n_classes = n(),
    total_registrants = sum(registrants, na.rm = TRUE),
    avg_registrants = mean(registrants, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_registrants)) %>%
  slice_head(n = 30)

write_csv(top_instructors, file.path(out_tables, "top_30_instructors_by_total_registrants.csv"))

# ------------------------------- 13) Export cleaned data ---------------------
write_csv(trans, file.path(out_tables, "transactions_cleaned_for_eda.csv"))
write_csv(classes, file.path(out_tables, "classes_cleaned_for_eda.csv"))
write_csv(students, file.path(out_tables, "students_cleaned_for_eda.csv"))

message("EDA complete. Tables in: ", out_tables, " | Plots in: ", out_plots)
###############################################################################