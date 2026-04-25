library(tidyverse)
library(gt)
library(scales)

survey = folkschool23_25_survey_clean

n_responses = nrow(survey)

kpi_table = tibble(
  kpi = c(
    "Survey Responses",
    "Very Satisfied",
    "Satisfied or Better",
    "Learned a Lot",
    "Very Likely to Recommend",
    "Instructor Rating: Top 2 Box",
    "Instructor Excellent Rating",
    "Perfect Satisfaction Courses"
  ),
  result = c(
    comma(n_responses),
    percent(mean(survey$satisfaction_rating == 5, na.rm = TRUE), accuracy = 0.1),
    percent(mean(survey$satisfaction_rating >= 4, na.rm = TRUE), accuracy = 0.1),
    percent(mean(survey$learning_rating == 3, na.rm = TRUE), accuracy = 0.1),
    percent(mean(survey$recommend_rating == 3, na.rm = TRUE), accuracy = 0.1),
    percent(mean(survey$instructor_rating >= 4, na.rm = TRUE), accuracy = 0.1),
    percent(mean(survey$instructor_rating == 5, na.rm = TRUE), accuracy = 0.1),
    survey %>%
      group_by(course_name) %>%
      summarize(
        n = n(),
        avg_satisfaction = mean(satisfaction_rating, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(n >= 5, avg_satisfaction == 5) %>%
      nrow() %>%
      comma()
  ),
  detail = c(
    "Total post-course survey responses",
    "Highest satisfaction rating",
    "Rated 4 or 5 out of 5",
    "Top learning outcome response",
    "Highest recommendation tier",
    "Rated instructor 4 or 5 out of 5",
    "Rated instructor 5 out of 5",
    "Courses with 5+ responses and 5.0 average satisfaction"
  )
)

kpi_gt = kpi_table %>%
  gt() %>%
  tab_header(
    title = "Allerton Folk School KPI Summary",
    subtitle = "Post-course survey results, 2023–2025"
  ) %>%
  cols_label(
    kpi = "KPI",
    result = "Result",
    detail = "Definition"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = kpi)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = px(18)),
    locations = cells_body(columns = result)
  ) %>%
  cols_width(
    kpi ~ px(240),
    result ~ px(130),
    detail ~ px(360)
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.width = px(760),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(14),
    column_labels.font.weight = "bold",
    row.striping.include_table_body = TRUE
  )

gtsave(
  data = kpi_gt,
  filename = "survey_kpi_summary_table.png"
)