# 03_analysis.R --------------------------------------------------------------
# Input : data/clean/ooh_analysis.csv
# Output: results/figures/*.png, results/tables/*.csv

library(tidyverse)
library(here)

ooh <- read_csv(here("data", "clean", "ooh_analysis.csv"), show_col_types = FALSE)

dir.create(here("results", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("results", "tables"),  recursive = TRUE, showWarnings = FALSE)

# 1. Do growth rate and openings rank occupations the same way? --------------
cor_pearson  <- cor(ooh$growth_pct, ooh$openings_per_year_n, use = "complete.obs")
cor_spearman <- cor(ooh$growth_pct, ooh$openings_per_year_n,
                     method = "spearman", use = "complete.obs")

message("Pearson correlation (growth % vs openings/year): ", round(cor_pearson, 2))
message("Spearman rank correlation: ", round(cor_spearman, 2))

top_growth   <- ooh |> slice_max(growth_pct, n = 20) |> pull(page_title)
top_openings <- ooh |> slice_max(openings_per_year_n, n = 20) |> pull(page_title)
overlap_n    <- length(intersect(top_growth, top_openings))
message("Overlap between top-20 by growth and top-20 by openings: ", overlap_n, " / 20")

write_csv(
  tibble(
    metric = c("pearson_cor", "spearman_cor", "top20_overlap"),
    value  = c(cor_pearson, cor_spearman, overlap_n)
  ),
  here("results", "tables", "growth_vs_openings_correlation.csv")
)

# 2. Scatter: growth rate vs openings, sized by current employment -----------
p_scatter <- ooh |>
  ggplot(aes(x = growth_pct, y = openings_per_year_n, size = jobs_2025_n)) +
  geom_point(alpha = 0.5) +
  scale_y_log10(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma, name = "Current jobs (2025)") +
  labs(
    title = "Fast-growing occupations are not always the ones hiring the most",
    subtitle = "Each point is one BLS occupation, 2025-35 projections",
    x = "Projected growth rate, 2025-35 (%)",
    y = "Projected annual openings (log scale)",
    caption = "Source: U.S. Bureau of Labor Statistics, Occupational Outlook Handbook"
  ) +
  theme_minimal(base_size = 13)

ggsave(here("results", "figures", "growth_vs_openings_scatter.png"),
       p_scatter, width = 8, height = 6, dpi = 200)

# 3. Openings decomposition: how much comes from growth vs. replacement ------
p_decomp <- ooh |>
  filter(openings_per_year_n > 0) |>
  slice_max(openings_per_year_n, n = 15) |>
  mutate(
    growth_share = pmin(1, pmax(0, growth_jobs_per_year / openings_per_year_n)),
    page_title = fct_reorder(page_title, openings_per_year_n)
  ) |>
  select(page_title, openings_per_year_n, growth_share) |>
  mutate(
    from_growth      = openings_per_year_n * growth_share,
    from_replacement = openings_per_year_n * (1 - growth_share)
  ) |>
  pivot_longer(c(from_growth, from_replacement), names_to = "source", values_to = "n") |>
  ggplot(aes(x = page_title, y = n, fill = source)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(from_growth = "#2c7fb8", from_replacement = "#bdbdbd"),
    labels = c("New jobs from growth", "Replacing workers who leave")
  ) +
  labs(
    title = "Where do the 15 occupations with the most openings get their openings?",
    x = NULL, y = "Projected annual openings", fill = NULL,
    caption = "Source: U.S. Bureau of Labor Statistics, Occupational Outlook Handbook"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(here("results", "figures", "openings_decomposition.png"),
       p_decomp, width = 15, height = 10, dpi = 200)

# 4. Actionable table: by education level, best-openings occupations ---------
# "best" = above-median pay AND above-median openings rate for that education tier
recommend_table <- ooh |>
  filter(!is.na(education_level), above_median_pay) |>
  group_by(education_level) |>
  slice_max(openings_rate_pct, n = 5, with_ties = FALSE) |>
  ungroup() |>
  select(education_level, page_title, median_pay_usd, growth_pct,
         openings_per_year_n, openings_rate_pct) |>
  arrange(education_level, desc(openings_rate_pct))

write_csv(recommend_table, here("results", "tables", "top_openings_by_education.csv"))

# 5. Sanity print -------------------------------------------------------------
print(recommend_table, n = 40)
message("Figures written to results/figures/, tables to results/tables/")
