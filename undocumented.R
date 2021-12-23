library(dplyr)
library(rvest)

base_url <- "https://www.migrationpolicy.org/data/unauthorized-immigrant-population"

state_aa_count <- function(state) {
  file.path(base_url, "state", state) %>%
    read_html() %>%
    html_table() %>%
    .[[1]] %>%
    mutate(state = state) %>%
    filter(grepl("Asia", X1)) %>%
    select(state, count = X2, pct = X3)
}


state_aa_count("CA")


# TODO: 
# 1. Site doesn't appear to have _detailed_ AA breakdowns 
# 2. If we do scrape the site, get links to counties from this page
# https://www.migrationpolicy.org/programs/us-immigration-policy-program-data-hub/unauthorized-immigrant-population-profiles
