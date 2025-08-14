library(tidyverse)
library(janitor)
library(TCGAbiolinks)
library(maftools)
BiocManager::install("ExperimentHub")

add_case_id <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df %>% mutate(case_id = character()))
  nm <- names(df)
  
  d <- df %>% janitor::clean_names()
  cand <- c(
    "cases_submitter_id", "submitter_id", "bcr_patient_barcode",
    "cases_case_id", "patient_barcode", "patient_id",
    "case_id" # in case it already exists
  )
  
  sample_cand <- c("tumor_sample_barcode","aliquot_barcode", "sample_barcode", "barcode")
  have_cand   <- cand[cand %in% names(d)]
  have_sample <- sample_cand[sample_cand %in% names(d)]
  
  if (length(have_cand) > 0) {
    d <- d %>%
      mutate(case_id = coalesce(!!!rlang::syms(have_cand)))
  } else if (length(have_sample) > 0) {
    first_sample_col <- have_sample[1]
    d <- d %>%
      mutate(case_id = .data[[first_sample_col]])
  } else if ("tumor_sample_barcode" %in% names(df)) {
    d <- d %>% mutate(case_id = tumor_sample_barcode)
  } else {
      d <- d %>% mutate(case_id = NA_character_)
  }
    d <- d %>%
    mutate(case_id = ifelse(!is.na(case_id), toupper(substr(as.character(case_id), 1, 12)), NA_character_))
  
  d
}