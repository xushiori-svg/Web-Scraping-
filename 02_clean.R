# 02_clean.R
# Input : data/raw/ooh_quick_facts_raw.csv  (from 01_scrape.R)
# Output: data/clean/ooh_clean.csv

library(tidyverse)
library(here)

ooh_raw <- read_csv(here("data", "raw", "ooh_quick_facts_raw.csv"), show_col_types = FALSE)
dir.create(here("data", "clean"), recursive = TRUE, showWarnings = FALSE)

dollars_to_num <- function(x) {
  annual <- x |> str_extract("(?i)\\$[0-9,]+(?=\\s*per year)") |> str_remove_all("[$,]") |> as.numeric()
  hourly <- x |> str_extract("(?i)\\$[0-9.]+(?=\\s*per hour)") |> str_remove_all("[$,]") |> as.numeric()
  ifelse(!is.na(annual), annual, hourly * 2080)
}

dollars_simple <- function(x) {
  x |> str_extract("\\$[0-9,]+") |> str_remove_all("[$,]") |> as.numeric()
}

pct_to_num <- function(x) {
  x |> str_extract("-?[0-9.]+(?=%)") |> as.numeric()
}

int_to_num <- function(x) {
  x |> str_extract("-?[0-9,]+") |> str_remove_all(",") |> as.numeric()
}

education_levels <- c(
  "No formal educational credential",
  "High school diploma or equivalent",
  "Postsecondary nondegree award",
  "Some college, no degree",
  "Associate's degree",
  "Bachelor's degree",
  "Master's degree",
  "Doctoral or professional degree"
)

# cleaning
ooh_clean <- ooh_raw |>
  mutate(
    median_pay_usd     = dollars_to_num(median_pay),
    growth_pct         = pct_to_num(growth),
    jobs_2025_n        = int_to_num(jobs_2025),
    employment_change_n = int_to_num(change),
    openings_per_year_n = int_to_num(openings_per_year),
    all_occ_median_wage_usd = dollars_simple(all_occ_median_wage),
    all_occ_growth_pct_n    = pct_to_num(all_occ_growth_pct)
  )

get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
national_wage_benchmark   <- get_mode(ooh_clean$all_occ_median_wage_usd)
national_growth_benchmark <- get_mode(ooh_clean$all_occ_growth_pct_n)
message("National benchmarks used to fill gaps: $", national_wage_benchmark,
        " median wage, ", national_growth_benchmark, "% growth")

ooh_clean <- ooh_clean |>
  mutate(
    all_occ_median_wage_usd = coalesce(all_occ_median_wage_usd, national_wage_benchmark),
    all_occ_growth_pct_n    = coalesce(all_occ_growth_pct_n, national_growth_benchmark),
    education_clean = str_squish(education),
    education_level  = factor(education_clean, levels = education_levels, ordered = TRUE),

    openings_rate_pct = 100 * openings_per_year_n / jobs_2025_n,      # openings per 100 jobs, per year
    growth_jobs_per_year = employment_change_n / 10,                  # 2025-35 change, annualized
    replacement_share = pmin(1, pmax(0, 1 - (growth_jobs_per_year / openings_per_year_n))),
    pay_vs_national    = median_pay_usd - all_occ_median_wage_usd,
    above_median_pay   = pay_vs_national > 0
  )

#data quality report
n_total <- nrow(ooh_clean)
report <- ooh_clean |>
  summarise(
    n = n(),
    missing_pay      = sum(is.na(median_pay_usd)),
    missing_growth   = sum(is.na(growth_pct)),
    missing_jobs     = sum(is.na(jobs_2025_n)),
    missing_openings = sum(is.na(openings_per_year_n)),
    missing_education = sum(is.na(education_level))
  )
print(report)

ooh_clean |>
  filter(is.na(median_pay_usd) | is.na(growth_pct) | is.na(openings_per_year_n)) |>
  select(slug, median_pay, growth, openings_per_year) |>
  print(n = 20)

ooh_analysis <- ooh_clean |>
  filter(!is.na(median_pay_usd), !is.na(growth_pct),
         !is.na(jobs_2025_n), !is.na(openings_per_year_n))

message("Kept ", nrow(ooh_analysis), " of ", n_total,
        " occupations with complete core fields.")

write_csv(ooh_clean, here("data", "clean", "ooh_clean.csv"))
write_csv(ooh_analysis, here("data", "clean", "ooh_analysis.csv"))
