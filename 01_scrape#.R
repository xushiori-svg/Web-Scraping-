# 01_scrape.R -------------------------------------------------------------
# Question : Across ALL occupations in the BLS Occupational Outlook Handbook,
#            does a high projected growth RATE actually mean a lot of yearly
#            job OPENINGS -- or are these two different signals?
# Source   : U.S. Bureau of Labor Statistics, Occupational Outlook Handbook (OOH)
#            - A-Z Index page (to get the full list of occupation profile URLs)
#            - one Quick Facts table per occupation profile page
#
# Output:
#   data/raw/bls_robots.txt              robots.txt, saved for documentation
#   data/raw/occupation_list.csv         every occupation profile URL found on the A-Z index
#   data/raw/html/az_index.html          the A-Z index page itself
#   data/raw/html/<slug>.html            one cached page per occupation
#   data/raw/scrape_log.csv              request status per occupation page
#   data/raw/ooh_quick_facts_raw.csv     parsed RAW text fields (cleaning happens in 02_clean.R)

library(tidyverse)
library(rvest)
library(httr2)
library(here)
library(robotstxt)

# 0. Settings
USER_AGENT    <- "AEDS6400-class-project (Clara Xu; jiayinxu@sas.upenn.edu)"
DELAY_SECONDS <- 5
AZ_INDEX_URL  <- "https://www.bls.gov/ooh/a-z-index.htm"

dir.create(here("data", "raw", "html"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "clean"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("results", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("results", "tables"), recursive = TRUE, showWarnings = FALSE)

fetch_html <- function(url, path, user_agent = USER_AGENT) {
  # Downloads url path
  if (file.exists(path)) return("cached")

  resp <- request(url) |>
    req_user_agent(user_agent) |>
    req_timeout(30) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()

  status <- resp_status(resp)
  if (status == 200) writeLines(resp_body_string(resp), path, useBytes = TRUE)
  Sys.sleep(DELAY_SECONDS)
  as.character(status)
}

# 1. Ethics check: robots.txt
robots_resp <- request("https://www.bls.gov/robots.txt") |>
  req_user_agent(USER_AGENT) |>
  req_error(is_error = \(resp) FALSE) |>
  req_perform()
writeLines(resp_body_string(robots_resp), here("data", "raw", "bls_robots.txt"))
message("robots.txt HTTP status: ", resp_status(robots_resp))

check_allowed <- function(paths) {
  tryCatch(
    paths_allowed(paths = paths, domain = "www.bls.gov", bot = "*", warn = FALSE),
    error = \(e) NA
  )
}

if (!isTRUE(all(check_allowed(c("/ooh/a-z-index.htm", "/ooh/"))))) {
  stop("robots.txt appears to disallow /ooh/ pages, or the check failed. ",
       "Read data/raw/bls_robots.txt by hand before continuing.")
}

# 2. Get the full occupation list from the A-Z index
az_path <- here("data", "raw", "html", "az_index.html")
fetch_html(AZ_INDEX_URL, az_path)

az_page <- read_html(az_path)

# BLS uses RELATIVE hrefs like "/ooh/construction-and-extraction/electricians.htm".
# Occupation profile links always have exactly two path segments after /ooh/:
# a group folder and an occupation slug ending in .htm. Non-occupation pages
# under /ooh/ (home.htm, print/, about/, how-to-find-a-job/, occupation-finder.htm,
# a-z-index.htm, ooh-site-map.htm) only have ONE segment after /ooh/, or live
# under an excluded folder, so the two-segment pattern already excludes them.
# "See:" cross-references point to the SAME href as the canonical entry, so
# de-duplicating by href automatically collapses them into one row per occupation.
occ_links <- az_page |>
  html_elements("a") |>
  (\(nodes) tibble(
    text = html_text2(nodes),
    href = html_attr(nodes, "href")
  ))() |>
  filter(!is.na(href)) |>
  mutate(href = url_absolute(href, AZ_INDEX_URL)) |>
  filter(str_detect(href, "^https://www\\.bls\\.gov/ooh/[^/]+/[^/]+\\.htm$")) |>
  filter(!str_detect(href, "/ooh/(print|about|how-to-find-a-job)/")) |>
  distinct(href, .keep_all = TRUE) |>
  mutate(
    group = str_match(href, "/ooh/([^/]+)/")[, 2],
    slug  = str_match(href, "/([^/]+)\\.htm$")[, 2]
  ) |>
  rename(occupation_link_text = text, url = href) |>
  filter(!group %in% c("about", "a-z-index", "occupation-finder"))

write_csv(occ_links, here("data", "raw", "occupation_list.csv"))
message("Occupations found on A-Z index: ", nrow(occ_links))

# 3. Download every occupation profile page -----------------------------------
occ_links <- occ_links |>
  mutate(html_path = here("data", "raw", "html", paste0(slug, ".html")))

occ_links <- occ_links |>
  mutate(status = map2_chr(url, html_path, fetch_html))

write_csv(
  occ_links |> select(group, slug, url, status),
  here("data", "raw", "scrape_log.csv")
)

failed <- occ_links |> filter(!status %in% c("cached", "200"))
if (nrow(failed) > 0) {
  warning(nrow(failed), " page(s) failed to download. See data/raw/scrape_log.csv.")
}

# 4. Parse the Quick Facts table on each page with rvest -----------------------
parse_profile <- function(path) {
  page      <- read_html(path)
  body_text <- page |> html_element("body") |> html_text2()

  tables  <- page |> html_elements("table")
  qf_node <- tables[str_detect(html_text2(tables), "^Quick Facts")]

  get_fact <- function(qf, pattern) {
    hit <- qf$value[str_detect(qf$label, regex(pattern, ignore_case = TRUE))]
    if (length(hit) == 0) NA_character_ else hit[1]
  }

  if (length(qf_node) == 0) {
    return(tibble(
      page_title = page |> html_element("h1") |> html_text2(),
      median_pay = NA, education = NA, experience = NA, training = NA,
      jobs_2025 = NA, growth = NA, change = NA,
      openings_per_year = NA, all_occ_median_wage = NA,
      all_occ_growth_pct = NA, page_last_modified = NA
    ))
  }

  qf <- html_table(qf_node[[1]], header = FALSE)[, 1:2]
  names(qf) <- c("label", "value")

  tibble(
    page_title    = page |> html_element("h1") |> html_text2(),
    median_pay    = get_fact(qf, "Median Pay"),
    education     = get_fact(qf, "Entry-Level Education"),
    experience    = get_fact(qf, "Work Experience"),
    training      = get_fact(qf, "On-the-job Training"),
    jobs_2025     = get_fact(qf, "^Number of Jobs"),
    growth        = get_fact(qf, "^Job Outlook"),
    change        = get_fact(qf, "^Employment Change"),
    openings_per_year   = str_match(body_text,
                                    regex("about\\s+([0-9,]+)\\s+openings", ignore_case = TRUE))[, 2],
    all_occ_median_wage = str_match(body_text,
      "median annual wage for all workers was \\$([0-9,]+)")[, 2],
    all_occ_growth_pct  = str_match(body_text,
      "average growth rate for all occupations is\\s+([0-9.]+)\\s+percent")[, 2],
    page_last_modified  = str_match(body_text,
      "Last modified date:\\s*([A-Za-z]+ [0-9]{1,2}, [0-9]{4})")[, 2]
  )
}

ooh_raw <- occ_links |>
  filter(status %in% c("cached", "200")) |>
  mutate(facts = map(html_path, safely(parse_profile)))

# report any pages that failed to parse (not the same as failing to download)
parse_errors <- ooh_raw |> filter(map_lgl(facts, \(f) !is.null(f$error)))
if (nrow(parse_errors) > 0) {
  warning(nrow(parse_errors), " page(s) downloaded but failed to parse. ",
          "See parse_errors in the R console for slugs.")
}

ooh_raw <- ooh_raw |>
  filter(map_lgl(facts, \(f) is.null(f$error))) |>
  mutate(facts = map(facts, "result")) |>
  unnest(facts) |>
  select(group, slug, url, page_title, median_pay, education, experience,
         training, jobs_2025, growth, change, openings_per_year,
         all_occ_median_wage, all_occ_growth_pct, page_last_modified)

write_csv(ooh_raw, here("data", "raw", "ooh_quick_facts_raw.csv"))

# 5. Quick sanity checks --------------------------------------------------
glimpse(ooh_raw)
message("Occupations scraped: ", nrow(ooh_raw), " of ", nrow(occ_links))
message("Missing median_pay: ", sum(is.na(ooh_raw$median_pay)))
message("Missing openings_per_year: ", sum(is.na(ooh_raw$openings_per_year)))
message("Groups found: ", n_distinct(ooh_raw$group))
