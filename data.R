library(dplyr)
library(readr)

# N.B. 2020 ACS isn't yet available
# https://www.census.gov/programs-surveys/acs/news/data-releases/2020/release.html#ABC
# Also, the url links for CVAP and undocumented stats
# (below) need to be updated as well
acs_year <- 2019
cvap_url <- "https://www2.census.gov/programs-surveys/decennial/rdo/datasets/2019/2019-cvap/CVAP_2015-2019_ACS_csv_files.zip"

# Various geo units to consider
# RHS: the data label
# LHS: the value passed to get_acs(geo = ...)
geo_map <- c(
  "us" = "national", 
  "state" = "state", 
  "county" = "county", 
  "congressional district" = "district", 
  "metropolitan statistical area/micropolitan statistical area" = "metro"
)

# state name -> abbreviations
state_map <- c(
  setNames(state.abb, state.name), 
  "District of Columbia" = "DC"
)


# --------------------------------------------------------
# CVAP
# --------------------------------------------------------

cvap_raw <- withr::with_tempdir({
  download.file(cvap_url, "cvap.zip")
  unzip("cvap.zip")
  bind_rows(
    read_csv("Nation.csv") %>% 
      mutate(geo = "national"),
    read_csv("State.csv") %>% 
      mutate(geo = "state"), 
    read_csv("County.csv") %>% 
      mutate(geo = "county"),
    read_csv("CD.csv") %>% 
      mutate(geo = "district")
  )
})

cvap <- cvap_raw %>%
  filter(lnnumber %in% c("4", "6")) %>% 
  filter(!grepl("Puerto Rico", geoname)) %>%
  mutate(
    race = if_else(lntitle == "Asian Alone", "Asian American Alone", "NHPI Alone"),
    name = if_else(
      grepl("Ana County, New Mexico", geoname), 
      "Dona Ana County, New Mexico", geoname
    ),
    topic = "cvap",
    variable = "CVAP",
    pct = if_else(
      tot_est > 0, round(cvap_est/tot_est, digits = 3), NA_real_
    )
  ) %>% 
  select(name, race, geo, variable, count = cvap_est, pct, moe = cvap_moe, topic)


# --------------------------------------------------------
# Undocumented
# --------------------------------------------------------

#undoc_url <- "https://www.migrationpolicy.org/sites/default/files/datahub/Unauthorized-Profiles_ACS%202015-19_State-County-Topline-Estimates.xlsx"
#undoc_file <- tempfile(fileext = ".xlsx")
#download.file(undoc_url, undoc_file)
#
#states <- undoc_file %>%
#  readxl::read_excel(sheet = "U.S. and States", range = "A3:C55") %>%
#  setNames(c("name", "count", "pct")) %>%
#  mutate(
#    geo = if_else(name == "United States", "us", "state"),
#    state = if_else(name == "United States", NA_character_, name)
#  )
#
#county <- undoc_file %>%
#  readxl::read_excel(sheet = "U.S. and Counties", range = "A3:D277") #%>%
#  setNames(c("state", "name", "count", "pct")) %>%
#  filter(name != "United States") %>%
#  mutate(geo = "county")
#
#undocumented <- bind_rows(states, county)


# --------------------------------------------------------
# Helpers for downloading ACS data
# --------------------------------------------------------

get_tables <- function(tables) {
  # Get every geo breakdown for every table
  args <- expand.grid(tables, names(geo_map))
  args <- split(args, seq_len(nrow(args)))
  tbls <- lapply(args, function(x) get_table(x[[1]], x[[2]]))
  
  # Clean, aggregate, and format
  bind_rows(tbls) %>%
    filter(!is.na(variable)) %>%
    filter(!grepl("Puerto Rico", NAME)) %>% 
    filter(!grepl("PR Metro Area", NAME)) %>%
    # N.B. we still have sex splits, so sum them together
    group_by(GEOID, race, variable) %>% 
    mutate(
      count = sum(estimate, na.rm = TRUE),
      moe = sum(moe, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(GEOID, race) %>%
    mutate(
      # N.B. this assumes group_by(GEOID, group)
      pct = if_else(
        summary_est > 0, 
        round(count/summary_est, digits = 3), 
        NA_real_
      ),
      geo = recode(geo, !!!geo_map),
      name = if_else(
        grepl("Ana County, New Mexico", NAME), 
        "Dona Ana County, New Mexico",
        NAME
      )
    ) %>%
    ungroup() %>%
    select(name, race, geo, variable, count, pct, moe) %>%
    distinct()
}

get_table <- function(table, geo) {
  
  acs_dat <- tidycensus::get_acs(
    table = table, 
    year = acs_year,
    geography = geo,
    cache_table = TRUE,
    # This assumption may not hold for detailed population breakdowns
    summary_var = paste0(table, "_001"),
    survey = "acs5"
  )
  
  acs_dat %>%
    mutate(
      race = case_when(
        grepl("D_", variable) ~ "Asian American Alone",
        grepl("E_", variable) ~ "NHPI Alone",
        variable == "B02015_001" ~ "Asian American Alone",
        variable == "B02018_001" ~ "NHPI Alone",
        TRUE ~ NA_character_
      ),
      geo = geo,
      # Here's a data dictionary mapping codes to human-readable labels
      # https://www2.census.gov/programs-surveys/acs/summary_file/2019/documentation/user_tools/ACS2019_Table_Shells.xlsx
      variable = case_when(
        # education labels
        grepl("C15002(D|E)_00(3|8)", variable) ~ "Less than HS",
        grepl("C15002(D|E)_00(4|9)", variable) ~ "HS or GED",
        grepl("C15002(D|E)_0(05|10)", variable) ~ "Some College or AA",
        grepl("C15002(D|E)_0(06|11)", variable) ~ "BA or higher",
        # insurance labels
        grepl("C27001(D|E)_00(3|6|9)", variable) ~ "covered",
        #grepl("C27001(D|E)_00(4|7|10)", variable) ~ "uncovered",
        # lep labels
        grepl("B16005(D|E)_00(4|9)", variable) ~ "Speak another language",
        grepl("B16005(D|E)_0(06|11)", variable) ~ "LEP",
        # Poverty labels
        grepl("B17001(D|E)_002", variable) ~ "Below poverty",
        # Nativity labels
        grepl("B05003(D|E)_0(05|10|16|21)", variable) ~ "Foreign-born",
        # Population labels
        variable == "B02015_001" ~ "Asian American Alone",
        variable == "B02018_001" ~ "NHPI Alone",
        # TODO: the detailed ethniticy population stats might require
        # a call to load_variables() to get the ethnicities right
        #variable == "B02016_001" ~ "Asian American Alone or in Combo",
        #variable == "B02019_001" ~ "NHPI Alone or in Combo",
        #grepl("B0201(6,9)_00(2,3,4,5)", variable) ~ "Polynesian",
        #grepl("B0201(6,9)_00(6,7,8)", variable) ~ "Micronesian",
        #grepl("B0201(6,9)_00(9,10)", variable) ~ "Melanesian",
        TRUE ~ NA_character_
      )
    )
}

# --------------------------------------------------------
# Download ACS data
# --------------------------------------------------------

edu <- get_tables(c("C15002D", "C15002E")) %>%
  mutate(topic = "edu")
insurance <- get_tables(c("C27001D", "C27001E")) %>%
  mutate(topic = "With insurance")
lep <- get_tables(c("B16005D", "B16005E")) %>%
  mutate(topic = "LEP")
poverty <- get_tables(c("B17001D", "B17001E")) %>%
  mutate(topic = "poverty")
nativity <- get_tables(c("B05003D", "B05003E")) %>%
  mutate(topic = "nativity")

# TODO: population estimates can get complicated quick...
# * Should national estimates come from ACS1 not ACS5?
# * How to implement detailed ethnicity breakdown?
#    * This wasn't specified in statement of work...
population <- get_tables(c("B02015", "B02018")) %>%
  mutate(topic = "population")

acs <- bind_rows(edu, insurance, lep, poverty, nativity, population)

# ------------------------------------------------
# Combine and try our best to extract the state (abbreviation) from the name 
# (metro makes this harder than it needs to be...and they can also span multiple states)
# ------------------------------------------------

# TODO: should we strip the state from the name?
dat <- mutate(
  bind_rows(cvap, acs),
  state = sapply(strsplit(name, ",\\s+"), function(x) x[[length(x)]]),
  # geo="metro" uses state abbreviations instead of the full name and 
  # also appends Metro Area after the state
  state = sub("\\s+$", "", sub("^\\s+", "", sub("(Metro|Micro) Area", "", state))),
  state = recode(state, !!!state_map)
)

count(dat, state)

readr::write_csv(dat, file.path("data", paste0(acs_year, ".csv")))