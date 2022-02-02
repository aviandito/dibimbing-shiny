# Helper functions for Shiny Vaccination Dashboard Class Day 2 v1
# Created by Mohammad Aviandito & Calvin Sibarani
# Last updated: April 2021

world_vac_prep <- function(){
  # Read World Bank population data. Latest is 2019 population
  wb_pop_data_dir <- 'API_SP.POP.TOTL_DS2_en_csv_v2_2163507'
  wb_pop_data_filename <- 'API_SP.POP.TOTL_DS2_en_csv_v2_2163507.csv'
  wb_pop_file_full <- paste0(wb_pop_data_dir, '/', wb_pop_data_filename) 
  
  wb_pop <- read_csv(wb_pop_file_full, skip = 3) %>%
    select(`Country Name`, `Country Code`, `2019`) %>%
    rename(country_name = `Country Name`,
           iso_code = `Country Code`,
           pop_2019 = `2019`)
  
  # Read Our World In Data vaccination data and metadata for ISO code
  owid_dir <- 'owid' # should be the location of vaccination-country data data set from OWID repository
  owid_vac_dir <- paste0(owid_dir, '/country_data')
  owid_loc <- paste0(owid_dir, '/locations.csv')
  
  locations <- read_csv(owid_loc)
  
  owid_vac <- list.files(path = owid_vac_dir, pattern = "*.csv", full.names = TRUE) %>% 
    map_df(~read_csv(.)) %>%
    left_join(locations) %>%
    select(location, iso_code, date, total_vaccinations, people_vaccinated, people_fully_vaccinated)
  
  # Final data used in the Shiny app
  world_vac <- owid_vac %>%
    left_join(wb_pop) %>%
    select(-country_name) %>%
    mutate(pct_pop_fully_vaccinated = people_fully_vaccinated / pop_2019) %>%
    # renaming for clarity
    rename(doses_given = total_vaccinations,
           at_least_one_dose = people_vaccinated,
           fully_vaccinated = people_fully_vaccinated)
  
  return(world_vac)
}

region_prep <- function(){
  region <- read_csv('country_centroids_az8.csv') %>%
    rename(location = brk_name,
           scope = continent) %>%
    select(location, scope) %>%
    mutate(scope = case_when(scope == 'North America' ~ 'north america',
                             scope == 'South America' ~ 'south america',
                             scope == 'Asia' ~ 'asia',
                             scope == 'Africa' ~ 'africa',
                             scope == 'Europe' ~ 'europe',
                             scope == 'South America' ~ 'south america',
                             TRUE ~ 'world'))
  return(region)
}


value_box_prep <- function(world_vac){
  value_box_summary_df <- world_vac %>%
    group_by(location) %>%
    summarise(doses_given = max(doses_given, na.rm = T),
              fully_vaccinated = max(fully_vaccinated, na.rm = T),
              pop_2019 = max(pop_2019, na.rm = T)) %>% 
    mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
    ungroup() %>%
    summarise(doses_given = sum(doses_given),
              fully_vaccinated = sum(fully_vaccinated),
              pop_2019 = sum(pop_2019),
              pct_fully_vac = fully_vaccinated / pop_2019)
  
  value_box_summary <- c(value_box_summary_df$doses_given,
                         value_box_summary_df$fully_vaccinated,
                         value_box_summary_df$pct_fully_vac)
  
  return(value_box_summary)
}

# Grouping by location to get the latest data
table_prep <- function(world_vac){
  world_vac_table <- world_vac %>%
    group_by(location) %>%
    summarise(doses_given = max(doses_given, na.rm = T),
              fully_vaccinated = max(fully_vaccinated, na.rm = T),
              pop_2019 = max(pop_2019, na.rm = T)) %>%
    mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
    mutate(pct_pop_fully_vaccinated = fully_vaccinated / pop_2019) %>%
    select(-pop_2019)
  
  return(world_vac_table)
}

timeseries_prep <- function(world_vac){
  ts_df <- world_vac %>%
    group_by(date) %>%
    summarise(at_least_one_dose = sum(at_least_one_dose, na.rm = T),
              fully_vaccinated = sum(fully_vaccinated, na.rm = T)) %>%
    pivot_longer(cols = c(at_least_one_dose,fully_vaccinated), values_to = "val", names_to = "var") %>%
    ungroup()
  
  return(ts_df)
}

map_prep <- function(world_vac){
  df_map_viz <- world_vac %>%
    group_by(location, iso_code) %>%
    summarise(doses_given = max(doses_given, na.rm = T),
              fully_vaccinated = max(fully_vaccinated, na.rm = T),
              pop_2019 = max(pop_2019, na.rm = T)) %>%
    mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
    mutate(pct_pop_fully_vaccinated = fully_vaccinated / pop_2019) %>%
    ungroup()
  
  return(df_map_viz)
}

# MIT License
# 
# Copyright (c) 2021 Mohammad Aviandito & Calvin Sibarani
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#     
#     The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.