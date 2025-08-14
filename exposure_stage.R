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

clinical  <- read_tsv_clean("clinical.tsv")
exposure  <- read_tsv_clean("exposure.tsv")
follow_up <- read_tsv_clean("follow_up.tsv")

merged_view <- {
  clinical_slim <- clinical %>%
    select(`cases.case_id`, `cases.disease_type`, `cases.primary_site`, `diagnoses.ajcc_pathologic_stage`) %>%
    distinct()
  
  exposure_slim <- exposure %>%
    select(`cases.case_id`, `exposures.tobacco_smoking_status`) %>%
    distinct()
  
  followup_slim <- follow_up %>%
    select(`cases.case_id`, `follow_ups.disease_response`) %>%
    distinct()
  
  clinical_slim %>%
    left_join(exposure_slim, by = "cases.case_id") %>%
    left_join(followup_slim, by = "cases.case_id")
}

build_var_df <- function(var_name) {
  var_map <- list(
    "Smoking Status"     = "exposures.tobacco_smoking_status",
    "Pathologic Stage"   = "diagnoses.ajcc_pathologic_stage",
    "Primary Site"       = "cases.primary_site",
    "Follow-up Response" = "follow_ups.disease_response"
  )
  stopifnot(var_name %in% names(var_map))
  src <- var_map[[var_name]]
  
  merged_view %>%
    transmute(
      Disease = factor(`cases.disease_type`),
      value   = .data[[src]]
    ) %>%
    filter(!is.na(Disease), !is.na(value)) %>%                     # drop missing
    filter(trimws(as.character(value)) != "", as.character(value) != "...") %>%  # drop empty/"..."
    mutate(value = fct_drop(as.factor(value))) %>%
    transmute(!!var_name := value, Disease)
}

# APA-style frequency table (n and % within Disease; Overall %)
make_apa_freq_table <- function(var_name) {
  df <- build_var_df(var_name)
  if (nrow(df) == 0) stop("No data available after cleaning.", call. = FALSE)
  
# Per-disease counts and column % (within each Disease)
  by_dis <- df %>%
    count(Level = !!sym(var_name), Disease, name = "N") %>%
    group_by(Disease) %>%
    mutate(Pct = N / sum(N)) %>%
    ungroup() %>%
    mutate(Label = sprintf("%d (%.1f%%)", N, 100 * Pct)) %>%
    select(Level, Disease, Label) %>%
    pivot_wider(names_from = Disease, values_from = Label) %>%
    arrange(Level)
  
# Overall counts and % (of total for this variable)
  overall <- df %>%
    count(Level = !!sym(var_name), name = "Overall_N") %>%
    mutate(Overall_Pct = Overall_N / sum(Overall_N),
           Overall = sprintf("%d (%.1f%%)", Overall_N, 100 * Overall_Pct)) %>%
    select(Level, Overall)
  
  apa_df <- overall %>%
    left_join(by_dis, by = "Level") %>%
    arrange(Level)
  
# Totals row
  totals_disease <- df %>%
    count(Disease, name = "N") %>%
    mutate(Label = sprintf("%d (100.0%%)", N)) %>%
    select(Disease, Label) %>%
    pivot_wider(names_from = Disease, values_from = Label)
  
  total_row <- tibble(
    Level   = "Total",
    Overall = sprintf("%d (100.0%%)", nrow(df))
  ) %>% bind_cols(totals_disease)
  
  apa_df <- bind_rows(apa_df, total_row)
  
  names(apa_df)[names(apa_df) == "Level"] <- var_name
  
  gt_tbl <- apa_df %>%
    gt() %>%
    tab_header(
      title = md(paste0("**Descriptive Frequencies of ", var_name, " by Disease Type**")),
      subtitle = md("Values are *n (column % within disease)*; Overall is % of total.")
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

res2 <- make_apa_freq_table("Smoking Status");     res2$gt
gtsave(res2$gt, "smoke_status.html")
res3 <- make_apa_freq_table("Pathologic Stage");   res3$gt
gtsave(res3$gt, "pathologic_stage.html")
res4 <- make_apa_freq_table("Primary Site");       res4$gt
gtsave(res4$gt, "exposure_site.html")
res5 <- make_apa_freq_table("Follow-up Response"); res5$gt
gtsave(res5$gt, "follow_up_response.html")

.build_block <- function(var_name) {
  df <- build_var_df(var_name)
  if (nrow(df) == 0) return(NULL)
  
# Per-disease counts and column % (within Disease)
  by_dis <- df %>%
    count(Level = !!sym(var_name), Disease, name = "N") %>%
    group_by(Disease) %>%
    mutate(Pct = N / sum(N)) %>%
    ungroup() %>%
    mutate(Label = sprintf("%d (%.1f%%)", N, 100 * Pct)) %>%
    select(Level, Disease, Label) %>%
    tidyr::pivot_wider(names_from = Disease, values_from = Label)
  
# Overall counts and % of total
  overall <- df %>%
    count(Level = !!sym(var_name), name = "Overall_N") %>%
    mutate(Overall_Pct = Overall_N / sum(Overall_N),
           Overall = sprintf("%d (%.1f%%)", Overall_N, 100 * Overall_Pct)) %>%
    select(Level, Overall)
  
# Merge & sort
  block <- overall %>%
    left_join(by_dis, by = "Level") %>%
    arrange(Level)
  
# Totals row for this variable block
  totals_disease <- df %>%
    count(Disease, name = "N") %>%
    mutate(Label = sprintf("%d (100.0%%)", N)) %>%
    select(Disease, Label) %>%
    tidyr::pivot_wider(names_from = Disease, values_from = Label)
  
  total_row <- tibble(
    Level   = "Total",
    Overall = sprintf("%d (100.0%%)", nrow(df))
  ) %>% bind_cols(totals_disease)
  
  block <- bind_rows(block, total_row)
  
# Add the Variable label as a grouping column
  block %>% mutate(Variable = var_name, .before = 1)
}

# Build the combined table across all variables
make_apa_freq_table_combined <- function(vars = c(
  "Smoking Status", "Pathologic Stage", "Primary Site", "Follow-up Response"
)) {
  blocks <- purrr::map(vars, .build_block)
  blocks <- blocks[!purrr::map_lgl(blocks, is.null)]
  if (length(blocks) == 0) stop("No data available after cleaning.", call. = FALSE)
  combined <- dplyr::bind_rows(blocks)
  fixed_cols <- c("Variable", "Level", "Overall")
  disease_cols <- setdiff(names(combined), fixed_cols)
 
  all_cols <- c(fixed_cols, disease_cols)
  combined <- combined[, all_cols]
  combined[disease_cols] <- lapply(combined[disease_cols], function(x) ifelse(is.na(x), "0 (0.0%)", x))
  
  # gt table grouped by Variable
  gt_tbl <- combined %>%
    gt(rowname_col = "Level", groupname_col = "Variable") %>%
    tab_header(
      title = md("**Descriptive Frequencies by Variable and Disease Type (Combined)**"),
      subtitle = md("Values are *n (column % within disease)*; Overall is % of total.")
    ) %>%
    cols_align(align = "left", columns = c("Level")) %>%
    cols_align(align = "center", columns = setdiff(colnames(combined), c("Variable", "Level"))) %>%
    tab_options(
      table.font.names = "sans",
      table.font.size  = px(12),
      data_row.padding = px(4)
    )
  
  list(data = combined, gt = gt_tbl)
}

combined_res <- make_apa_freq_table_combined()
combined_res$gt
gtsave(combined_res$gt, "exposure_site_status.png")
readr::write_csv(combined_res$data, "exposure_site_status.csv")

criteria_df <- merged_view %>%
  transmute(
    Disease         = `cases.disease_type`,
    SmokingStatus   = `exposures.tobacco_smoking_status`,
    PathologicStage = `diagnoses.ajcc_pathologic_stage`,
    PrimarySite     = `cases.primary_site`,
    FollowupResp    = `follow_ups.disease_response`
  ) %>%

  mutate(across(everything(), ~na_if(., ""))) %>%
  mutate(across(everything(), ~na_if(., "...")))

criteria_table <- criteria_df %>%
  tidyr::drop_na() %>%  # keep only rows with all five fields present
  count(Disease, SmokingStatus, PathologicStage, PrimarySite, FollowupResp, name = "n") %>%
  arrange(desc(n))

criteria_table %>% slice_max(n, n = 20) %>% gt()
readr::write_csv(criteria_table, "criteria_table_all_present.csv")