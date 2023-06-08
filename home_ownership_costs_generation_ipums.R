library(tidyverse)
library(ipumsr)
library(readxl)
library(lubridate)
library(httr)

### Functions ###
# Custom function to fetch BLS data from flat file database
get_bls_data <- function(url, email) {
  bls_res <- GET(url = url, user_agent(email))
  stop_for_status(bls_res)
  
  bls_content <- content(bls_res, 
                         as = "parsed",
                         type = "text/tab-separated-values",
                         encoding = "UTF-8",
                         col_names = T,
                         col_types = cols(.default = col_character()),
                         trim_ws = T
  )
  return(bls_content)
  
}

### Analysis ###
# Reading in IPUMS CPS microdata. Change this filename to whatever
# CPS extract number your data is i.e. "cps_00XX.xml"
ddi <- read_ipums_ddi("cps_00013.xml")
data <- read_ipums_micro(ddi) 

# Creating generational grouping variable based on age and survey year
# and filtering to just Baby Boomers, Gen X, and Millennials
three_gens <- data %>% 
  mutate(generation = case_when(
    YEAR - AGE < 1901 ~ "Pre-War",
    between((YEAR - AGE), 1901, 1927) ~ "Greatest",
    between((YEAR - AGE), 1928, 1945) ~ "Silent",
    between((YEAR - AGE), 1946, 1964) ~ "Baby Boomer",
    between((YEAR - AGE), 1965, 1980) ~ "Gen X",
    between((YEAR - AGE), 1981, 1996) ~ "Millennial",
    YEAR - AGE > 1996 ~ "Gen Z"
  )) %>% 
  filter(generation %in% c("Baby Boomer", "Gen X", "Millennial"))

# Filtering to only householder rows and doing grouped summary of 
# weighted average of homeownership for each generation for each year.
# Householder filtering per IPUMS staff advice:
# https://forum.ipums.org/t/calculating-home-ownership-rates-by-generation/5279/2
home_ownership_by_gen <- three_gens %>% 
  filter(RELATE == 101) %>% 
  group_by(YEAR, generation) %>% 
  summarize(OWN_HOME = weighted.mean(OWNERSHP == 10, ASECWTH)) %>% 
  ungroup()

# Creating "Adult Year" variable for x-axis of chart to compare
# homeownership rates across time for same age period for each 
# generation.
home_own_gen_dw <- home_ownership_by_gen %>% 
  mutate(adult_yr = case_when(generation == "Baby Boomer" ~ YEAR - 1964,
                              generation == "Gen X" ~ YEAR - 1980,
                              generation == "Millennial" ~ YEAR - 1996),
         OWN_HOME = round(OWN_HOME * 100, 1)) %>% 
  filter(adult_yr >= 18) %>% 
  select(-YEAR) %>% 
  pivot_wider(names_from = generation, values_from = OWN_HOME) %>% 
  select(-adult_yr) %>% 
  mutate(total_gen_adult_yr = row_number())

write_csv(home_own_gen_dw, "./visualizations/home_own_gen_dw.csv")

# Calculating weighted average household income by year and generation
# from the same methodology done for homeownership rates.
three_gens_income <- data %>% 
  filter(HHINCOME != 99999999) %>% 
  mutate(generation = case_when(
    YEAR - AGE < 1901 ~ "Pre-War",
    between((YEAR - AGE), 1901, 1927) ~ "Greatest",
    between((YEAR - AGE), 1928, 1945) ~ "Silent",
    between((YEAR - AGE), 1946, 1964) ~ "Baby Boomer",
    between((YEAR - AGE), 1965, 1980) ~ "Gen X",
    between((YEAR - AGE), 1981, 1996) ~ "Millennial",
    YEAR - AGE > 1996 ~ "Gen Z"
  )) %>% 
  filter(generation %in% c("Baby Boomer", "Gen X", "Millennial"))

hh_income_by_gen <- three_gens_income %>% 
  filter(RELATE == 101) %>% 
  group_by(YEAR, generation) %>% 
  summarize(AVG_HH_INCOME = weighted.mean(HHINCOME, ASECWTH)) %>% 
  ungroup() %>% 
  mutate(adult_yr = case_when(generation == "Baby Boomer" ~ YEAR - 1964,
                              generation == "Gen X" ~ YEAR - 1980,
                              generation == "Millennial" ~ YEAR - 1996)) %>% 
  filter(adult_yr >= 18)

# Reading in annual median new home prices from Census Bureau's New 
# Residential Sales product from Survey of Construction: 
# https://www.census.gov/construction/nrs/data.html
home_prices <- read_excel("./data/usprice_cust.xls",
                          col_names = T,
                          col_types = c("numeric", "numeric", "numeric"),
                          skip = 4,
                          sheet = "Price Ann"
                          ) %>% 
  filter(!is.na(Period)) %>% 
  select(-Average) %>% 
  rename(YEAR = Period, median_home_price = Median)

# Calculating proportion of 10% home downpayment for each generation's average 
# household income in 9th year of adulthood

ninth_year_hh_income_gen <- hh_income_by_gen %>% 
  filter(adult_yr == 26L)

gen_downpmt_prop <- home_prices %>% 
  filter(YEAR %in% c(1990, 2006, 2022)) %>% 
  inner_join(ninth_year_hh_income_gen, by = "YEAR") %>% 
  mutate(twenty_pct_downpmt_prop = (median_home_price * .2) / AVG_HH_INCOME)

write_csv(gen_downpmt_prop, "./data/gen_downpmt_prop.csv")

# Getting 30-year fixed mortgage interest rate data from Freddie Mac via FRED
# https://fred.stlouisfed.org/series/MORTGAGE30US
# and calculating yearly average.

mortgage_rates <- read_csv("./data/MORTGAGE30US.csv",
                          col_names = T,
                          col_types = "Dd") %>% 
  mutate(YEAR = year(DATE)) %>% 
  group_by(YEAR) %>% 
  summarize(interest_rate = mean(MORTGAGE30US) / 100) %>% 
  ungroup()

# Using same home affordability index methodology as the HOAM index from the 
# ATL fed, just not including insurance and tax data
# https://www.atlantafed.org/center-for-housing-and-policy/data-and-tools/home-ownership-affordability-monitor
# Make new home affordability index by generation by year from generational
# household income, median home price, and average annual 30-year fixed mortgage interest rate.

home_cost_dw <- inner_join(home_prices, mortgage_rates, by = "YEAR") %>% 
  inner_join(hh_income_by_gen, by = "YEAR") %>% 
  mutate(
    home_cost_index = (AVG_HH_INCOME / (
      ((median_home_price * .9 * (interest_rate / 12) / (1-(1/(1 + interest_rate / 12)^360))) + ((median_home_price * .9) * .00558)) * 3.33 * 12
      )) * 100
    ) %>% 
  select(adult_yr, generation, home_cost_index) %>% 
  pivot_wider(names_from = generation, values_from = home_cost_index) %>% 
  select(-adult_yr) %>% 
  mutate(total_gen_adult_yr = row_number())


write_csv(home_cost_dw, "./visualizations/home_cost_dw.csv")

# Getting BLS CPI data to adjust household income and home prices for inflation
# from annual CPI-U average. https://www.bls.gov/cpi/data.htm
all_items_cpi <- get_bls_data("https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems", 
                              "anesta@dotdashmdp.com")

ann_cpi_inf <- all_items_cpi %>% 
  filter(series_id == "CUUR0000SA0", period == "M13") %>% 
  rename(YEAR = year) %>% 
  select(YEAR, value) %>% 
  mutate(YEAR = as.numeric(YEAR),
         value = as.numeric(value)) %>% 
  arrange(desc(YEAR)) %>% 
  mutate(inf_pct = value[1] / value) %>% 
  select(-value)

# Seeing largest year-over-year changes in 30-year fixed mortgage interest rates.
mortgage_rate_changes <- read_csv("./data/MORTGAGE30US.csv",
         col_names = T,
         col_types = "Dd") %>% 
  arrange(desc(DATE)) %>% 
  mutate(yoy_chg = MORTGAGE30US - lead(MORTGAGE30US, n = 52)) %>% 
  arrange(desc(yoy_chg))

# Making Median household income growth vs. home prices comparison viz.
median_household_income <- read_csv("./data/MEHOINUSA646N.csv",
         col_names = T,
         col_types = "Dd") %>% 
  mutate(YEAR = year(DATE)) %>% 
  select(-DATE) %>% 
  rename(median_household_income = MEHOINUSA646N)

hh_income_vs_home_prices_dw <- home_prices %>% 
  inner_join(median_household_income, by = "YEAR") %>%
  inner_join(ann_cpi_inf, by = "YEAR") %>% 
  mutate(median_home_price = median_home_price * inf_pct,
         median_household_income = median_household_income * inf_pct) %>%
  select(-inf_pct) %>% 
  mutate(across(-YEAR, function(x) round(((x / x[1]) - 1) * 100, 2))) %>% 
  rename(`Median Home Price` = median_home_price,
         `Median Household Income` = median_household_income)

write_csv(hh_income_vs_home_prices_dw, 
          "./visualizations/hh_income_vs_home_prices_dw.csv")

