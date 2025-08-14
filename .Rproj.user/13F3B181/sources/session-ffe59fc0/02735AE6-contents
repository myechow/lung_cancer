library(tidyverse)
library(janitor)
library(broom)
library(nnet) 
library(openxlsx)

df_raw <- readr::read_csv("criteria_table_all_present.csv", show_col_types = FALSE)
na_fix <- function(x) {
  x <- str_trim(x)
  x[x %in% c("--", "", NA)] <- NA
  x
}

df <- df_raw %>%
  mutate(
    Disease          = na_fix(Disease),
    SmokingStatus    = na_fix(SmokingStatus),
    PathologicStage  = na_fix(PathologicStage),
    PrimarySite      = na_fix(PrimarySite),
    FollowupResp     = na_fix(FollowupResp)
  )

top_combos <- df %>%
  count(Disease, SmokingStatus, PathologicStage, FollowupResp, wt = n, name = "count") %>%
  arrange(desc(count))

print(head(top_combos, 20))

xtab_smoke_follow  <- xtabs(n ~ SmokingStatus + FollowupResp, data = drop_na(df, SmokingStatus, FollowupResp))
xtab_stage_follow  <- xtabs(n ~ PathologicStage + FollowupResp, data = drop_na(df, PathologicStage, FollowupResp))
xtab_disease_smoke <- xtabs(n ~ Disease + SmokingStatus,   data = drop_na(df, Disease, SmokingStatus))

xtab_smoke_follow
prop.table(xtab_smoke_follow, 1)  # row-wise proportions
prop.table(xtab_smoke_follow, 2)  # col-wise proportions

df_bin <- df %>%
  mutate(
    tumor_present = case_when(
      FollowupResp == "TF-Tumor Free" ~ 0,
      is.na(FollowupResp)             ~ NA_real_,
      TRUE                            ~ 1
    )
  ) %>% drop_na(tumor_present, SmokingStatus, Disease, PathologicStage)

# Make Smoking Status/Disease factors with explicit baselines (change as desired)
df_bin <- df_bin %>%
  mutate(
    SmokingStatus = fct_infreq(SmokingStatus), # baseline = most frequent
    Disease       = fct_infreq(Disease)
  )

# Weighted logistic regression (odds ratios)
fit_or <- glm(tumor_present ~ SmokingStatus + Disease + PathologicStage,
              data = df_bin, family = binomial(), weights = n)

summary(fit_or)

# Tidy with exponentiated coefficients = odds ratios
or_tbl <- broom::tidy(fit_or, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)
or_tbl  # column 'estimate' is the OR

# Relative risks,simple Poisson (for quick approximation:
fit_rr <- glm(tumor_present ~ SmokingStatus + Disease + PathologicStage,
              data = df_bin, family = poisson(link = "log"), weights = n)
rr_tbl <- broom::tidy(fit_rr, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)
rr_tbl  # 'estimate' approximates RR

readr::write_csv(or_tbl, "odds_ratios.csv")
readr::write_csv(rr_tbl, "relative_risks.csv")

# Creating Excel with multiple sheets
wb <- createWorkbook()
addWorksheet(wb, "Odds Ratios")
writeData(wb, "Odds Ratios", or_tbl)

addWorksheet(wb, "Relative Risks")
writeData(wb, "Relative Risks", rr_tbl)

saveWorkbook(wb, "OR_RR_stats.xlsx", overwrite = TRUE)

# Plot: Pathologic Stage (x), Follow-up (facet), Smoking Status (fill)
ggplot(df, aes(x = PathologicStage, y = n, fill = SmokingStatus)) +
  geom_bar(stat = "identity", position = "fill") +  # "fill" makes proportions; use "stack" for raw counts
  facet_wrap(~ FollowupResp) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of Smoking Status by Pathologic Stage and Follow-up Response",
    x = "Pathologic Stage",
    y = "Proportion",
    fill = "Smoking Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines")
  )

ggsave(
  filename = "status_tumor.png",
  width = 10,   # inches
  height = 6,   # inches
  dpi = 300,     # high resolution for print; try 600 for super-sharp
  bg = "white" )