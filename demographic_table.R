library(tidyverse)
library(janitor)
library(forcats)
library(scales)
library(stringr)
library(gt)

read_tsv_clean <- function(path) {
  readr::read_tsv(path, show_col_types = FALSE) %>%
    janitor::remove_empty(c("rows","cols"))
}

clinical <- read_tsv_clean("clinical.tsv")

clinical$cases.disease_type <- factor(clinical$cases.disease_type)
clinical$demographic.country_of_residence_at_enrollment <- factor(clinical$demographic.country_of_residence_at_enrollment)
clinical$demographic.race <- factor(clinical$demographic.race)
clinical$demographic.gender <- factor(clinical$demographic.gender)
clinical$demographic.vital_status <- factor(clinical$demographic.vital_status)

.col_map <- list(
  Gender  = "demographic.gender",
  Race    = "demographic.race",
  Country = "demographic.country_of_residence_at_enrollment",
  Vital   = "demographic.vital_status",
  Age     = "demographic.age_at_index"
)

build_table_df <- function(var_name, clinical) {
  stopifnot(var_name %in% names(.col_map))
  src <- .col_map[[var_name]]
  
  demo <- clinical %>%
    transmute(
      Disease   = factor(cases.disease_type),
      value_raw = .data[[src]]
    )
  
  if (var_name == "Age") {
    demo <- demo %>%
      mutate(
        age_num = suppressWarnings(as.numeric(value_raw)),
        value   = cut(
          age_num,
          breaks = c(-Inf, 40, 60, 80, Inf),
          labels = c("<40","40–59","60–79","80+"),
          right = FALSE
        )
      )
  } else {
    demo <- demo %>% mutate(value = value_raw)
  }
  
  if (var_name == "Race") {
    value_chr <- tolower(trimws(as.character(demo$value)))
    bad <- is.na(value_chr) |
      grepl("^(unknown|not[ _-]?reported|not[ _-]?specified|n/?a|na)$", value_chr)
    demo <- demo[!bad, , drop = FALSE]
  }
  
  demo %>%
    mutate(Level = fct_drop(factor(as.character(value)))) %>%
    select(Disease, Level)
}

make_apa_freq_table <- function(var_name, clinical,
                                percent_type = c("row","column"),
                                digits = 1) {
  percent_type <- match.arg(percent_type)
  demo <- build_table_df(var_name, clinical)
  
  validate <- function(cond, msg) if (!cond) stop(msg, call. = FALSE)
  validate(nrow(demo) > 0, "No data available for the selected demographic after cleaning.")

  counts <- demo %>%
    count(Level, Disease, name = "N") %>%
    tidyr::complete(Level, Disease, fill = list(N = 0))
  
  if (percent_type == "column") {
    counts <- counts %>%
      group_by(Disease) %>%
      mutate(sumN = sum(N)) %>%
      mutate(Pct = ifelse(sumN > 0, N / sumN, 0)) %>%
      ungroup() %>%
      select(-sumN)
    subtitle_text <- "Values are n (column % within disease); Overall is % of total."
  } else { # row %
    counts <- counts %>%
      group_by(Level) %>%
      mutate(sumN = sum(N)) %>%
      mutate(Pct = ifelse(sumN > 0, N / sumN, 0)) %>%
      ungroup() %>%
      select(-sumN)
    subtitle_text <- "Values are n (row % within level across diseases); Overall is % of total."
  }
  
  by_dis <- counts %>%
    mutate(Label = sprintf(paste0("%d (%.", digits, "f%%)"), N, 100 * Pct)) %>%
    select(Level, Disease, Label) %>%
    pivot_wider(names_from = Disease, values_from = Label) %>%
    arrange(Level)
  
  overall <- demo %>%
    count(Level, name = "Overall_N") %>%
    mutate(
      Overall_Pct = Overall_N / sum(Overall_N),
      Overall = sprintf(paste0("%d (%.", digits, "f%%)"), Overall_N, 100 * Overall_Pct)
    ) %>%
    select(Level, Overall)
  
  apa_df <- overall %>% left_join(by_dis, by = "Level")

  totals_disease <- demo %>%
    count(Disease, name = "N") %>%
    mutate(Label = sprintf("%d (100.0%%)", N)) %>%
    select(Disease, Label) %>%
    pivot_wider(names_from = Disease, values_from = Label)
  
  total_row <- tibble(
    Level = "Total",
    Overall = sprintf("%d (100.0%%)", nrow(demo))
  ) %>% bind_cols(totals_disease)
  
  apa_df <- bind_rows(apa_df, total_row)
  
  names(apa_df)[names(apa_df) == "Level"] <- var_name
  
  gt_tbl <- apa_df %>%
    gt() %>%
    tab_header(
      title = md(paste0("**Descriptive Statistics of ", var_name, " by Disease Type**")),
      subtitle = md(subtitle_text)
    ) %>%
    cols_align(align = "left", columns = var_name) %>%
    cols_align(align = "center", columns = setdiff(colnames(apa_df), var_name)) %>%
    tab_options(
      table.font.names = "sans",
      table.font.size  = px(12),
      data_row.padding = px(4)
    )
  
  list(data = apa_df, gt = gt_tbl)
}

# Race (row %)
res_race <- make_apa_freq_table("Race", clinical, percent_type = "row")
print(res_race$data)
gtsave(res_race$gt, "apa_table_race.png")

# Age (row %)
res_age <- make_apa_freq_table("Age", clinical, percent_type = "row")
print(res_age$data)
gtsave(res_age$gt, "apa_table_age.png")

# Country (row %)
res_ct <- make_apa_freq_table("Country", clinical, percent_type = "row")
print(res_ct$data)
gtsave(res_ct$gt, "apa_table_country.png")

# Gender (row %)
res_gen <- make_apa_freq_table("Gender", clinical, percent_type = "row")
print(res_gen$data)
gtsave(res_gen$gt, "apa_table_gender.png")

# Vital (row %)
res_vt <- make_apa_freq_table("Vital", clinical, percent_type = "row")
print(res_vt$data)
gtsave(res_vt$gt, "apa_table_vital.png")