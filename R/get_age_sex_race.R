library(tidyverse)
library(tidycensus)


#get counties to regions mapping
regions <- read_rds("./data/county_regions.rds")


##### MAKE VARIABLES ####################
#variables - removes the total, total male, and total female
vars_allraces <- c(paste0("B01001_00", c(3:9)), paste0("B01001_0", c(10:25, 27:49)))

#races/ethnicty
races <- c("white" = "B01001A", "black" = "B01001B",
           "ai_an" = "B01001C", "asian" = "B01001D", "hawaiian_pi" = "B01001E",
           "other" = "B01001F", "two_or_more" = "B01001G", "hispanic" = "B01001I" )

vars_byraces <- map(races, function(x){
  c(paste0(x, "_00", c(3:9)), paste0(x, "_0", c(10:16, 18:31)))
}) %>% unlist() %>% unname()

#################################################################
######GET ACS DEMOGRAPHIC DATA BY RACE, SEX, AND AGE GROUP#######
#################################################################

#all races - age groups
agegroups <- c("0-17", "0-17", "0-17", "0-17", "18-24", "18-24", "18-24", 
               "18-24", "25-34", "25-34", "35-44", "35-44", "45-54", "45-54", 
               "55-64", "55-64", "55-64", "65+", "65+", "65+", "65+", 
               "65+", "65+")

agesex <- c(paste("Male", agegroups), 
            paste("Female", agegroups))

#get census info for all races
allraces <- get_acs(geography = "county",
                    variables = vars_allraces,
                    state = 12)

allraces <- allraces %>% 
  left_join(regions[c("region", "fips")], by = c("GEOID" = "fips")) 

total_pop <- allraces %>% 
  group_by(region) %>% 
  summarise(pop = sum(estimate))

#join english var names for the census variables
allraces$group <- rep(agesex, length(unique(allraces$NAME)))

#demog POSSIBLE groupings
groupings <- c("group", "region", "race", "pop")

#sum the population estimates and recalculate moe's
allraces_4groups <- allraces %>% 
  left_join(total_pop) %>% 
  group_by_at(vars(any_of(groupings))) %>% 
  summarise(sumest = sum(estimate), 
            summoe = moe_sum(moe, estimate),
            percent = round((sumest/unique(pop))*100, 2)) %>% 
  mutate(race = "all_races") %>% 
  select(-summoe)

#################### GET ACS DATA BY RACES ##########################
#age groups aligned with race specific tables (13 instead of 18)
agegroups_races <- c("0-17", "0-17", "0-17", "0-17", "18-24", "18-24",
                     "25-34", "25-34", "35-44", "45-54", "55-64", "65+",
                     "65+", "65+")

agesex_races <- c(paste("Male", agegroups_races),
                  paste("Female", agegroups_races))

byraces <- get_acs(geography = "county",
                   variables = vars_byraces,
                   state = 12) 
byraces <- byraces %>% 
  left_join(regions[c("region", "fips")], by = c("GEOID" = "fips")) %>% 
  mutate(race = str_extract(variable, "^[A-Z][0-9]+[A-Z]"))

#join english var names for the census variables
byraces$group <- rep(agesex_races, length(unique(byraces$race)) * length(unique(byraces$NAME)))

#sum the population estimates and recalculate moe's
byraces_4groups <- byraces %>% 
  left_join(total_pop) %>% 
  group_by_at(vars(any_of(groupings))) %>%
  summarise(sumest = sum(estimate), 
            summoe = moe_sum(moe, estimate),
            percent = round((sumest/unique(pop))*100, 2)) %>% 
  select(-summoe)

#join back in race names
byraces_4groups <- tibble(table_num = races, race = names(races)) %>%
  left_join(byraces_4groups, by = c("table_num" = "race")) %>% 
  select(-table_num)

#bind rows
demog_regions <- bind_rows(allraces_4groups, byraces_4groups)

write_rds(demog_regions, "./data/demog_regions.rds")


