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

clinical %>% count(cases.case_id) %>% filter(n > 1)
exposure %>% count(cases.case_id) %>% filter(n > 1)
follow_up %>% count(cases.case_id) %>% filter(n > 1)

clinical  <- clinical  %>% distinct(cases.case_id, .keep_all = TRUE)
exposure  <- exposure  %>% distinct(cases.case_id, .keep_all = TRUE)
follow_up <- follow_up %>% distinct(cases.case_id, .keep_all = TRUE)

merged_df <- clinical %>%
  full_join(exposure,  by = "cases.case_id") %>%
  full_join(follow_up, by = "cases.case_id")

glimpse(merged_df)

write_csv(merged_df, "exposure_clinical_followup.csv")