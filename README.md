# Growth Rate vs. Annual Openings: Which One Tells You Where the Jobs Are?

## Question

Does a high growth rate mean a job is easy to find? Or should you look
at annual openings instead? This project scrapes every occupation in the
BLS Occupational Outlook Handbook and checks how growth rate and
openings relate.

## Data

-   Source: [BLS Occupational Outlook
    Handbook](https://www.bls.gov/ooh/)
-   343 occupations, scraped from BLS's own A-Z index (nothing
    hand-picked)
-   For each occupation: median pay, current jobs, growth rate
    (2025–35), job change, and projected annual openings

## Ethics

-   Public pages only. No login, no paywall.
-   `robots.txt` checked before scraping (saved in
    `data/raw/bls_robots.txt`)
-   Used a real user agent, waited several seconds between requests
-   Each page downloaded once and cached — re-running the code doesn't
    re-download anything

## Repo structure

```         
.
├── README.md
├── data/
│   ├── raw/          # scraped HTML, robots.txt, scrape log, raw CSV
│   └── clean/         # cleaned data (from 02_clean.R)
├── scripts/
│   ├── 01_scrape.R    # get occupation list, scrape + parse each page
│   ├── 02_clean.R     # turn text into numbers, build metrics
│   └── 03_analysis.R  # correlation, plots, ranking table
├── results/
    ├── figures/       # saved plots
    └── tables/        # saved tables
```

## How to run this

1.  Install R packages:

    ``` r
    install.packages(c("tidyverse", "rvest", "httr2", "here", "robotstxt", "scales"))
    ```

2.  Open `scripts/01_scrape.R`. Replace `USER_AGENT` with your own name
    and email.

3.  Run in order:

    ``` r
    source("scripts/01_scrape.R")
    source("scripts/02_clean.R")
    source("scripts/03_analysis.R")
    ```

    First run downloads \~343 pages (\~5 sec apart, \~25–30 min). Later
    runs use the saved HTML files and finish in seconds.

## Limitations

-   These are national numbers, not local ones. Actual demand varies by
    state and city.
-   Annual openings mix new jobs with jobs left open by workers who quit
    or retire. That mix is the point of this post, not a problem to fix.
-   2 occupations (fishing workers, military careers) don't have a
    standard salary listed and were dropped.
-   36 occupations don't have one clear education level (broad groups
    like "Water Transportation Occupations") and were left out of the
    by-education table, but are still in the correlation numbers.
