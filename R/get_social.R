library(tidyverse)
library(tidycensus)

#agreggate fl counties by region 
#source: https://www.floridanurse.org/page/Regions

regions <- bind_rows(
  tibble(region = "Northwest",
         county = c("Bay", "Calhoun","Escambia","Franklin","Gulf","Holmes",
                    "Jackson","Liberty","Okaloosa","Santa Rosa", "Walton",
                    "Washington")),
  tibble(region = "North Central",
         county = c("Alachua","Bradford","Citrus","Columbia","Dixie",
                    "Gadsden","Gilchrist","Hamilton","Jefferson",
                    "Lafayette","Leon","Levy","Madison","Marion",
                    "Suwannee","Taylor","Union","Wakulla")),
  tibble(region = "Northeast",
         county = c("Baker","Clay","Duval","Nassau","Putnam",
                    "St. Johns")),
  tibble(region = "East Central",
         county = c("Brevard","Flagler","Lake","Orange","Osceola",
                    "Seminole","Sumter","Volusia")),
  tibble(region = "West Central",
         county = c("DeSoto","Hardee","Hernando","Highlands",
                    "Hillsborough","Manatee","Pasco","Pinellas",
                    "Polk","Sarasota")),
  tibble(region = "Southeast",
         county = c("Indian River","Martin","Okeechobee",
                    "Palm Beach","St. Lucie")),
  tibble(region = "Southwest",
         county = c("Charlotte","Collier","Glades","Hendry",
                    "Lee")),
  tibble(region = "South",
         county = c("Broward","Miami-Dade","Monroe"))
) %>% 
  arrange(county) %>% 
  mutate(fips = c(seq(1, 23, by = 2), seq(27, 85, by = 2),
                  86, seq(87, 133, by = 2))) %>% 
  mutate(fips = case_when(county == "St. Johns" ~ 109,
                          county == "St. Lucie" ~ 111,
                          county == "Santa Rosa" ~ 113,
                          county == "Sarasota" ~ 115,
                          county == "Seminole" ~ 117,
                          TRUE ~ fips)) %>% 
  mutate(fips = paste0("12", str_pad(fips, 3, side = "left", pad = "0")))

write_rds(regions, "./data/county_regions.rds")

get_regions <- function(var_name, numerator, denominator){
  #browser()
 
  numerator <- c(numerator = numerator)
  denominator <- c(denominator = denominator)
  
  acs <- get_acs(geography = "county",
                 state = 12,
                 variables = c(numerator, denominator),)

 
  acs_var <- acs %>% 
    left_join(regions[c("region", "fips")], by = c("GEOID" = "fips")) %>% 
    group_by(variable, region) %>% 
    summarise(est = mean(estimate, na.rm = TRUE)) %>% 
    spread(key = variable, value = est) %>% 
    mutate(var_est = rowSums(.[,str_detect(colnames(.), "numerator")])) %>% 
    if(var_name == "Median Income"){
      mutate(var_pop = var_est)
    }else{
      rename("var_pop" = "denominator")
    }
  
  #add variable name and percentage
  acs_var <- acs_var %>% 
    mutate(variable = var_name,
           percent = round((var_est/var_pop)*100, 2))
  
  #remove numerator variables
  acs_var <- acs_var[,-which(str_detect(colnames(acs_var), "numerator"))]
  
}

foreign <- tibble(var = "Foreign Born", denominator = "B05012_001", 
                  numerator = list( "B05012_003"))
employed <- tibble(var = "Employed", denominator = "B23025_003", 
                   numerator = list( "B23025_004"))
unemployed <- tibble(var = "Unemployed", denominator = "B23025_003", 
                     numerator = list( "B23025_005"))
medinc <- tibble(var = "Median Income", denominator = "S1903_C03_001", 
                 numerator = list( "S1903_C03_001"))
private_insurance <- tibble(var = "Private Insurance", denominator = "B27020_001", 
                            numerator = list(c("B27020_004", "B27020_010", "B27020_015")))
public_insurance <- tibble(var = "Public Insurance", denominator = "B27020_001", 
                           numerator = list(c("B27020_005", "B27020_011", "B27020_016")))
no_insurance <- tibble(var = "No Insurance", denominator = "B27020_001", 
                       numerator = list(c("B27020_006", "B27020_012", "B27020_017")))

vars <- bind_rows(foreign, employed, unemployed, medinc, private_insurance,
                  public_insurance, no_insurance)
#vars <- private_insurance

social_regions <- vars %>%
  pmap_dfr(function(...) {
    current <- tibble(...)
    # do cool stuff and access content from current row with
    print(unique(current$var))
    # return
    get_regions(var_name = unique(current$var), numerator = current$numerator, 
                  denominator = unique(current$denominator))
  })

write_rds(social_regions, "./data/social_regions.rds")
