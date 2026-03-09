###############################################################################
# Allerton Folk School (2023–2025)
# Key Questions — Insightful Plots + HTML Report (table-driven)
#
# What this does (beyond basic bar charts):
#   Q1 Where are registrants from?
#     - Concentration / Pareto curves (how concentrated are registrations?)
#     - Top-share metrics + HHI (market-concentration style metric)
#   Q2 Who are repeat attendees and what distinguishes them?
#     - Distribution (ECDF / histogram) of registrations per person
#     - Spend comparisons (violin/box + effect size)
#     - Simple predictive model (logistic regression) + coefficient plot
#   Q3 Which individuals buy at which price points?
#     - Price-bin mix + cumulative share
#     - Price mix heatmap by Folk School (already in tables)
#     - Spend tiers vs repeat rate and ticket size
#
# Inputs (ONLY):
#   tables_dir = "~/Desktop/Project/allerton-project/analysis1/analysis_tables"
#
# Outputs:
#   plots_dir  = "~/Desktop/Project/allerton-project/analysis1/analysis_plots"
#   report_dir = "~/Desktop/Project/allerton-project/analysis1/analysis_report"
#     - key_questions_report.html
#
# NOTE:
# - This script reads the CSV tables you already generated.
# - No reliance on objects in the global environment.
###############################################################################

# ------------------------------- Packages ------------------------------------
packages_needed = c(
  "tidyverse", "scales", "forcats", "stringr", "readr",
  "patchwork", "broom", "rmarkdown", "knitr"
)

to_install = packages_needed[!packages_needed %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(forcats)
  library(stringr)
  library(readr)
  library(patchwork)
  library(broom)
  library(rmarkdown)
  library(knitr)
})

# ------------------------------- Paths ---------------------------------------
tables_dir = "~/Desktop/Project/allerton-project/analysis1/analysis_tables"
plots_dir  = "~/Desktop/Project/allerton-project/analysis1/analysis_plots2"
report_dir = "~/Desktop/Project/allerton-project/analysis1/analysis_report"

stopifnot(dir.exists(tables_dir))
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)

# ---------------------------- Helper functions -------------------------------
read_tbl = function(file) {
  path = file.path(tables_dir, file)
  stopifnot(file.exists(path))
  df = readr::read_csv(path, show_col_types = FALSE)
  
  # Fix common type-guess issues
  if ("zip5" %in% names(df)) df = df %>% mutate(zip5 = as.character(zip5))
  if ("repeat_flag" %in% names(df)) df = df %>% mutate(repeat_flag = as.logical(repeat_flag))
  
  df
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

# Concentration metrics
hhi = function(shares) {
  # shares should sum to 1 (or close); HHI = sum(share^2)
  shares = shares[is.finite(shares) & !is.na(shares)]
  if (length(shares) == 0) return(NA_real_)
  sum(shares^2)
}

top_share = function(counts, k = 5) {
  counts = counts[is.finite(counts) & !is.na(counts)]
  if (length(counts) == 0) return(NA_real_)
  counts = sort(counts, decreasing = TRUE)
  sum(counts[seq_len(min(k, length(counts)))]) / sum(counts)
}

# Simple effect size (Cohen's d) for two groups
cohen_d = function(x, g) {
  df = tibble(x = x, g = g) %>% filter(!is.na(x), !is.na(g))
  if (n_distinct(df$g) < 2) return(NA_real_)
  g1 = df %>% filter(g == unique(df$g)[1]) %>% pull(x)
  g2 = df %>% filter(g == unique(df$g)[2]) %>% pull(x)
  m1 = mean(g1); m2 = mean(g2)
  s1 = sd(g1); s2 = sd(g2)
  sp = sqrt(((length(g1) - 1) * s1^2 + (length(g2) - 1) * s2^2) / (length(g1) + length(g2) - 2))
  (m1 - m2) / sp
}

# ------------------------- Read required tables ------------------------------
q1_people_by_state = read_tbl("q1_people_by_state.csv") %>%
  mutate(state = replace_na(state, "Unknown"))

q1_regs_by_state = read_tbl("q1_registrations_by_state.csv") %>%
  mutate(state = replace_na(state, "Unknown"))

q1_top_cities = read_tbl("q1_top_50_cities.csv") %>%
  mutate(city_state = replace_na(city_state, "Unknown"))

q1_top_zip_people = read_tbl("q1_top_50_zip_by_people.csv") %>%
  mutate(zip5 = replace_na(zip5, "Unknown"),
         zip5 = if_else(zip5 == "", "Unknown", zip5))

q2_person_summary = read_tbl("q2_person_summary.csv")
q2_repeat_characteristics = read_tbl("q2_repeat_characteristics.csv")
q2_repeat_counts = read_tbl("q2_repeat_counts.csv")
q2_repeat_top_folk_school = read_tbl("q2_repeat_top_folk_school.csv") %>%
  mutate(folk_school = replace_na(folk_school, "Unknown"))

q2_top_repeat_attendees = read_tbl("q2_top_50_repeat_attendees.csv")

q3_regs_by_total_bin = read_tbl("q3_registrations_by_total_price_bin.csv")
q3_people_by_median_bin = read_tbl("q3_people_by_median_total_price_bin.csv")
q3_spend_tiers = read_tbl("q3_people_by_total_spend_tier.csv")
q3_price_mix_by_school = read_tbl("q3_total_price_bin_by_folk_school.csv")
q3_high_price_summary = read_tbl("q3_high_price_people_summary.csv")

# ------------------------- Derive additional fields --------------------------
# Parse dates if present (person_summary usually has first/last)
if ("first_registration" %in% names(q2_person_summary)) {
  q2_person_summary = q2_person_summary %>%
    mutate(first_registration = suppressWarnings(as.Date(first_registration)),
           last_registration  = suppressWarnings(as.Date(last_registration)))
}

q2_person_summary = q2_person_summary %>%
  mutate(
    repeat_flag = as.logical(repeat_flag),
    repeat_group = if_else(repeat_flag, "Repeat (2+)", "One-time (1)")
  )

# Spend tiers (if you want to use inside report as well)
q2_person_summary = q2_person_summary %>%
  mutate(
    spend_tier = cut(
      total_spend,
      breaks = c(-Inf, 0, 50, 100, 200, 500, 1000, Inf),
      labels = c("$0", "$1–$50", "$51–$100", "$101–$200", "$201–$500", "$501–$1,000", "$1,000+"),
      right = TRUE,
      ordered_result = TRUE
    )
  )

# ------------------------------ Q1 Insights ----------------------------------
# Pareto dataset: states by registrations
q1_state_pareto = q1_regs_by_state %>%
  arrange(desc(n)) %>%
  mutate(
    share = n / sum(n),
    cum_share = cumsum(share),
    rank = row_number()
  )

q1_state_hhi = hhi(q1_state_pareto$share)
q1_state_top5 = top_share(q1_state_pareto$n, k = 5)

# Pareto plot: bars + cumulative line (registrations)
p_q1_pareto_states = ggplot(q1_state_pareto %>% slice_head(n = 20),
                            aes(x = fct_reorder(state, n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Q1: State concentration (Top 20 states by registrations)",
    x = "State",
    y = "Registrations"
  ) +
  theme_report()

p_q1_cum_states = ggplot(q1_state_pareto,
                         aes(x = rank, y = cum_share)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "Q1: Cumulative share of registrations across states",
    subtitle = paste0("Top 5 states share = ", percent(q1_state_top5, 0.1),
                      " | HHI = ", round(q1_state_hhi, 4)),
    x = "States ranked by registrations",
    y = "Cumulative share of registrations"
  ) +
  theme_report()

save_png(p_q1_pareto_states, "q1_states_top20_registrations.png", width = 8, height = 6)
save_png(p_q1_cum_states, "q1_states_cumulative_share.png", width = 9, height = 5)

# ZIP concentration: show how concentrated unique people are by ZIP (top 50 in table)
q1_zip_conc = q1_top_zip_people %>%
  arrange(desc(n)) %>%
  mutate(
    share = n / sum(n),
    cum_share = cumsum(share),
    rank = row_number()
  )

p_q1_zip_cum = ggplot(q1_zip_conc, aes(x = rank, y = cum_share)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Q1: Cumulative share of unique individuals across top ZIP codes",
    subtitle = paste0("Top 10 ZIPs share = ", percent(top_share(q1_zip_conc$n, 10), 0.1)),
    x = "ZIPs ranked by unique individuals (top list)",
    y = "Cumulative share of unique individuals"
  ) +
  theme_report()

save_png(p_q1_zip_cum, "q1_zip_cumulative_share_top50.png", width = 9, height = 5)

# ------------------------------ Q2 Insights ----------------------------------
# Distribution of registrations per person (ECDF + histogram)
p_q2_hist_regs = ggplot(q2_person_summary %>% filter(!is.na(n_registrations)),
                        aes(x = n_registrations, fill = repeat_group)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Q2: Distribution of registrations per person",
    x = "Registrations per person",
    y = "Individuals",
    fill = NULL
  ) +
  theme_report()

p_q2_ecdf_regs = ggplot(q2_person_summary %>% filter(!is.na(n_registrations)),
                        aes(x = n_registrations, color = repeat_group)) +
  stat_ecdf(linewidth = 1) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Q2: ECDF — registrations per person",
    x = "Registrations per person",
    y = "Cumulative % of individuals",
    color = NULL
  ) +
  theme_report()

save_png(p_q2_hist_regs, "q2_registrations_per_person_hist.png", width = 10, height = 5)
save_png(p_q2_ecdf_regs, "q2_registrations_per_person_ecdf.png", width = 9, height = 5)

# Spend comparison (total spend + avg ticket) — violin/box combo
# (More meaningful than counts; shows distribution + differences)
p_q2_spend_violin = ggplot(q2_person_summary %>% filter(!is.na(total_spend)),
                           aes(x = repeat_group, y = total_spend)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Q2: Total spend differs strongly by repeat attendance",
    x = NULL,
    y = "Total spend per person"
  ) +
  theme_report()

p_q2_ticket_violin = ggplot(q2_person_summary %>% filter(!is.na(med_total)),
                            aes(x = repeat_group, y = med_total)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Q2: Typical ticket size (median registration total) by repeat status",
    x = NULL,
    y = "Median registration total (ticket)"
  ) +
  theme_report()

save_png(p_q2_spend_violin, "q2_total_spend_violin.png", width = 9, height = 5)
save_png(p_q2_ticket_violin, "q2_median_ticket_violin.png", width = 9, height = 5)

# Simple predictive model (logistic regression) — what most distinguishes repeat attendees?
# This is not causal; it’s a useful segmentation lens.
model_df = q2_person_summary %>%
  transmute(
    repeat_flag = repeat_flag,
    n_distinct_classes = n_distinct_classes,
    tenure_days = tenure_days,
    total_spend = total_spend,
    med_total = med_total,
    pct_canceled = pct_canceled
  ) %>%
  filter(!is.na(repeat_flag)) %>%
  mutate(across(where(is.numeric), ~ if_else(is.finite(.x), .x, NA_real_))) %>%
  drop_na()

# Guard: only fit if we have both classes present and enough rows
if (nrow(model_df) >= 200 && n_distinct(model_df$repeat_flag) == 2) {
  repeat_glm = glm(
    repeat_flag ~ n_distinct_classes + tenure_days + total_spend + med_total + pct_canceled,
    data = model_df,
    family = binomial()
  )
  
  coef_tbl = broom::tidy(repeat_glm) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      odds_ratio = exp(estimate),
      conf_low = exp(estimate - 1.96 * std.error),
      conf_high = exp(estimate + 1.96 * std.error),
      term = recode(
        term,
        n_distinct_classes = "Unique classes",
        tenure_days = "Tenure (days)",
        total_spend = "Total spend ($)",
        med_total = "Median ticket ($)",
        pct_canceled = "Cancel rate"
      )
    )
  
  p_q2_coef = ggplot(coef_tbl, aes(x = fct_reorder(term, odds_ratio), y = odds_ratio)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.15) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    coord_flip() +
    scale_y_continuous(trans = "log10") +
    labs(
      title = "Q2: Factors associated with repeat attendance (odds ratios, log scale)",
      subtitle = "Interpretation: values > 1 increase odds of being repeat; < 1 decrease odds.",
      x = NULL,
      y = "Odds ratio (log10 scale)"
    ) +
    theme_report()
  
  save_png(p_q2_coef, "q2_repeat_logistic_coefficients.png", width = 10, height = 6)
}

# ------------------------------ Q3 Insights ----------------------------------
# Price bin mix (registrations) + cumulative share
q3_bin_mix = q3_regs_by_total_bin %>%
  filter(!is.na(total_bin)) %>%
  mutate(
    share = n / sum(n)
  )

q3_bin_pareto = q3_bin_mix %>%
  arrange(desc(n)) %>%
  mutate(
    cum_share = cumsum(share),
    rank = row_number()
  )

p_q3_bin_mix = ggplot(q3_bin_mix, aes(x = total_bin, y = n)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Q3: Registrations by transaction total (price bins)",
    x = "Price bin (registration total)",
    y = "Registrations"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_q3_bin_cum = ggplot(q3_bin_pareto, aes(x = rank, y = cum_share)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Q3: Cumulative share across price bins (ranked by volume)",
    x = "Price bins ranked by registrations",
    y = "Cumulative share of registrations"
  ) +
  theme_report()

save_png(p_q3_bin_mix, "q3_registrations_by_price_bin.png", width = 10, height = 5)
save_png(p_q3_bin_cum, "q3_price_bins_cumulative_share.png", width = 9, height = 5)

# Price mix by Folk School (heatmap), top 10 schools by volume
q3_school = q3_price_mix_by_school %>%
  filter(!is.na(folk_school), !is.na(total_bin)) %>%
  mutate(
    folk_school = as.character(folk_school),
    total_bin = as.character(total_bin)
  )

top_schools = q3_school %>%
  group_by(folk_school) %>%
  summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_n)) %>%
  slice_head(n = 10) %>%
  pull(folk_school)

q3_school_plot = q3_school %>% filter(folk_school %in% top_schools)

p_q3_heat = ggplot(
  q3_school_plot,
  aes(
    x = total_bin,
    y = fct_reorder(folk_school, n, .fun = sum),
    fill = pct_within_school
  )
) +
  geom_tile() +
  scale_fill_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Q3: Price mix by Folk School (top 10 by volume)",
    x = "Price bin (registration total)",
    y = "Folk School",
    fill = "% within school"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_png(p_q3_heat, "q3_price_mix_heatmap_top10_schools.png", width = 11, height = 6)

# Spend tiers (people) + repeat rate by tier using person_summary
tier_counts = q2_person_summary %>%
  filter(!is.na(spend_tier)) %>%
  count(spend_tier, name = "n_people") %>%
  mutate(pct_people = n_people / sum(n_people))

tier_repeat = q2_person_summary %>%
  filter(!is.na(spend_tier)) %>%
  group_by(spend_tier) %>%
  summarise(
    repeat_rate = mean(repeat_flag, na.rm = TRUE),
    med_ticket = median(med_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(tier_counts, by = "spend_tier")

p_q3_repeat_by_tier = ggplot(tier_repeat, aes(x = spend_tier, y = repeat_rate)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Q3: Repeat rate by total spend tier",
    x = "Total spend tier (per person)",
    y = "Repeat rate"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_q3_ticket_by_tier = ggplot(tier_repeat, aes(x = spend_tier, y = med_ticket)) +
  geom_col() +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Q3: Typical ticket size by total spend tier",
    x = "Total spend tier (per person)",
    y = "Median ticket ($)"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_png(p_q3_repeat_by_tier, "q3_repeat_rate_by_spend_tier.png", width = 10, height = 5)
save_png(p_q3_ticket_by_tier, "q3_median_ticket_by_spend_tier.png", width = 10, height = 5)

# ------------------------------- HTML Report ---------------------------------
# This report is generated from the tables + the plots you just created.
# It includes concrete metrics + interpretation scaffolding.

report_rmd = file.path(report_dir, "key_questions_report.Rmd")

rmd_text = c(
  '---',
  'title: "Allerton Folk School (2023–2025) — Key Questions Analysis"',
  'output:',
  '  html_document:',
  '    toc: true',
  '    toc_depth: 2',
  '    number_sections: true',
  '---',
  '',
  '```{r setup, include=FALSE}',
  'knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)',
  'library(tidyverse)',
  'library(scales)',
  '```',
  '',
  '## Data inputs',
  'This report is built **only** from the analysis tables exported to disk (no in-memory objects).',
  '',
  '```{r}',
  paste0('tables_dir = "', tables_dir, '"'),
  paste0('plots_dir = "', plots_dir, '"'),
  '```',
  '',
  '## Q1 — Where are individuals from who are registering?',
  'Instead of just counts, we assess **concentration** (how much volume comes from a small number of states / ZIPs).',
  '',
  '```{r}',
  'q1_regs_by_state = readr::read_csv(file.path(tables_dir, "q1_registrations_by_state.csv"), show_col_types = FALSE) %>%',
  '  mutate(state = replace_na(state, "Unknown")) %>%',
  '  arrange(desc(n)) %>%',
  '  mutate(share = n / sum(n), cum_share = cumsum(share), rank = row_number())',
  '',
  'top5_share = sum(head(q1_regs_by_state$n, 5)) / sum(q1_regs_by_state$n)',
  'hhi_val = sum((q1_regs_by_state$share)^2)',
  '',
  'tibble(metric = c("Top 5 states share", "HHI (state concentration)"),',
  '       value = c(scales::percent(top5_share, 0.1), round(hhi_val, 4)))',
  '```',
  '',
  '**Interpretation guide:**',
  '- *Top 5 states share* tells you how much registrations are dominated by the biggest five states.',
  '- *HHI* is a standard concentration index (closer to 1 = extremely concentrated).',
  '',
  '### Visuals',
  paste0('![](', file.path(plots_dir, "q1_states_cumulative_share.png"), ')'),
  '',
  paste0('![](', file.path(plots_dir, "q1_states_top20_registrations.png"), ')'),
  '',
  '## Q2 — Which individuals are repeat attendees and what distinguishes them?',
  'We focus on **behavioral differences** (spend, tenure, breadth of classes) and a simple predictive model (segmentation lens).',
  '',
  '```{r}',
  'person = readr::read_csv(file.path(tables_dir, "q2_person_summary.csv"), show_col_types = FALSE) %>%',
  '  mutate(repeat_flag = as.logical(repeat_flag)) %>%',
  '  mutate(group = if_else(repeat_flag, "Repeat (2+)", "One-time (1)"))',
  '',
  'person %>%',
  '  group_by(group) %>%',
  '  summarise(',
  '    people = n(),',
  '    avg_regs = mean(n_registrations, na.rm = TRUE),',
  '    med_regs = median(n_registrations, na.rm = TRUE),',
  '    avg_total_spend = mean(total_spend, na.rm = TRUE),',
  '    med_total_spend = median(total_spend, na.rm = TRUE),',
  '    avg_ticket = mean(avg_total, na.rm = TRUE),',
  '    med_ticket = median(med_total, na.rm = TRUE),',
  '    .groups = "drop"',
  '  )',
  '```',
  '',
  '### Visuals',
  paste0('![](', file.path(plots_dir, "q2_registrations_per_person_hist.png"), ')'),
  '',
  paste0('![](', file.path(plots_dir, "q2_total_spend_violin.png"), ')'),
  '',
  paste0('![](', file.path(plots_dir, "q2_median_ticket_violin.png"), ')'),
  '',
  'If generated (enough data + both classes present), the coefficient plot summarizes which features are most associated with repeat attendance.',
  paste0('![](', file.path(plots_dir, "q2_repeat_logistic_coefficients.png"), ')'),
  '',
  '## Q3 — Which individuals are buying at which price points?',
  'We describe the **price mix** (transaction totals), how it varies by Folk School, and how it relates to repeat behavior.',
  '',
  '### Visuals',
  paste0('![](', file.path(plots_dir, "q3_registrations_by_price_bin.png"), ')'),
  '',
  paste0('![](', file.path(plots_dir, "q3_price_mix_heatmap_top10_schools.png"), ')'),
  '',
  paste0('![](', file.path(plots_dir, "q3_repeat_rate_by_spend_tier.png"), ')'),
  '',
  '## Notes on approach',
  '- The analysis is **descriptive** and designed for decisions (marketing focus, retention strategy, pricing mix).',
  '- The predictive model is a **segmentation tool**, not causal inference.',
  '- For deeper causal questions (e.g., whether a program increases retention), we would need experimental design or quasi-experimental methods.'
)

writeLines(rmd_text, report_rmd)

rmarkdown::render(
  input = report_rmd,
  output_file = "key_questions_report.html",
  output_dir = report_dir,
  quiet = TRUE
)

message("Plots saved to: ", plots_dir)
message("Report saved to: ", file.path(report_dir, "key_questions_report.html"))