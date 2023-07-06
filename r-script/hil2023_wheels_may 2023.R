
# ------------------------------------ R-script for the HIL2023 wheels ---------------------------------------------------


# Mónica Quinzá Armenta
# May 2023

#Empty everything in the environment
rm(list = ls())
# Or remove all data frames in the workspace
#ls()
#rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"])

# Installs if necessary and loads packages
list.of.packages <- c("tidyverse", "dplyr", "sf", "broom", "reshape2", "lubridate", "stargazer", "ggpubr", "fontawesome", "htmltools", "reactable", "webshot2", "htmlwidgets", "viridis", "tinytex", "upstartr", "Rfast", "data.table", "readstata13", "stringr", "ggpattern", "jpeg", "cowplot", "magick")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))

# If running into issues download Rfast package from Github
install.packages("devtools")
library(devtools)
devtools::install_github('RfastOfficial/Rfast')

# Import file
library("readstata13")
df <- read.dta13("data/Final_dataset.dta")

# Keep only vars for wheel (keep order as in HIL2020 and country profiles):
# Household income 1_1 average
# Household wealth 1_3
# S80/S20 income share ratio 1_2 VER
# Housing affordability 3_2
# Overcrowding rate 3_1 DEP
# Employment rate 2_1
# Gender wage gap 2_2
# Long hours in paid work 2_7 DEP
# Life expectancy 5_1 
# Gap in life expectancy by education (men) 5_1 HOR
# Students skill in science 6_3
# Students with low skills 6_3 DEP
# Access to green space 9_1
# Exposure to outdoor air pollution 9_2 DEP
# Life satisfaction 11_1 
# Negative affect balance 11_2 DEP
# Homicides 10_1
# Gender gap in feeling safe 10_2 HOR
# Time off 4_1
# Gender gap in hours worked 4_3
# Social interactions 7_2
# Lack of social support 7_1 DEP
# Voter turnout 8_2
# Having no say in government 8_1 DEP

# Set up and clean data

# Average vars
wheels.avg <- df %>%
  filter_all(all_vars(var %in% c("1_1", "1_3", "2_2", "2_7", "2_1", "3_2", "5_1", "6_3", "9_1", "11_1", "10_1", "4_1", "4_3", "7_2", "8_2"))) %>%
  subset(., type_var == "average") %>%
  select(., country, type_var, var, year, value) %>%
  relocate(type_var, var) %>%
  group_by(var, country) %>%
  slice_max(year, n = 1) %>%
  ungroup()

# Vertical vars
wheels.ver <- df %>%
  subset(., var == "1_2") %>%
  select(., country, type_var, var, year, value) %>%
  relocate(type_var, var) %>%
  group_by(var, country) %>%
  slice_max(year, n = 1) %>%
  ungroup()

# Deprivation vars
wheels.dep <- df %>%
  filter_all(all_vars(var %in% c("2_7", "3_1", "6_3", "9_2", "11_2", "7_1", "8_1"))) %>%
  subset(., type_var == "DEP") %>%
  select(., country, type_var, var, year, value) %>%
  relocate(type_var, var) %>%
  group_by(var, country) %>%
  slice_max(year, n = 1) %>%
  ungroup()

# Gap in life expectancy by education (men) 5_1 HOR between primary- and tertiary-educated
# Primary 
life.prim <- df %>%
  subset(., var == "5_1" & type_var == "HOR" & educ== "primary" & sex == "Male") %>%
  rename(value.p = value) %>%
  select(., country, year, value.p) 

# Tertiary
life.ter <- df %>%
  subset(., var == "5_1" & type_var == "HOR" & educ== "tertiary" & sex == "Male") %>%
  rename(value.t = value) %>%
  select(., country, year, value.t) 

# Join
wheels.life <- left_join(life.ter, life.prim, by = c("country" = "country", "year" = "year"), keep = FALSE)

wheels.life <- wheels.life %>%
  mutate(value = value.t - value.p,
         var = "5_1",
         type_var = "HOR") %>%
  select(., -value.t, -value.p) %>%
  relocate(type_var, var) %>%
  group_by(var, country) %>%
  slice_max(year, n = 1) %>%
  ungroup()

# Gender gap in feeling safe 10_2 HOR
# Male data
safe.male <- df %>%
  subset(., var == "10_2" & type_var == "HOR" & sex == "Male") %>%
  rename(value.m = value) %>% 
  select(., country, year, value.m) 

# Female data
safe.female <- df %>%
  subset(., var == "10_2" & type_var == "HOR" & sex == "Female") %>%
  rename(value.f = value) %>% 
  select(., country, year, value.f) 

# Join 
wheels.safe <- left_join(safe.female, safe.male, by = c("country" = "country", "year" = "year"), keep = FALSE)

wheels.safe <- wheels.safe %>%
  mutate(value = value.f - value.m,
         var = "10_2", 
         type_var = "HOR") %>%
  select(., -value.f, -value.m) %>%
  relocate(type_var, var) %>%
  group_by(var, country) %>%
  slice_max(year, n = 1) %>%
  ungroup()

# Append all
wheels <- bind_rows(list(wheels.avg, wheels.ver, wheels.dep, wheels.life, wheels.safe))

# Remove all except wheels 
rm(wheels.avg, wheels.dep, wheels.life, wheels.safe, wheels.ver, life.prim, life.ter, safe.female, safe.male)

# Remove partner countries
wheels <- wheels %>%
  filter(!(country %in% c("BRA","RUS","ZAF")))

# Read indicators direction to define type of change
direction <- read.dta13("data/wb_indicators_direction.dta")

# Add indicator direction
wheels <- left_join(wheels, direction, by = "var")

# Check if all values are negative for gender gap in feeling safe
summary(wheels$value[wheels$var == "10_2"])
 # yes, do not change direction (keep as positive) since all values are negative, less negative values (higher values) are better

# Reverse direction for gap in life expectancy by education for men
wheels$direction <- ifelse(wheels$var == "5_1" & wheels$type_var == "HOR", "negative", wheels$direction)

# Add direction for missing vars & correct for deprivation
wheels$direction <- ifelse(wheels$var == "4_3", "negative", wheels$direction)
wheels$direction <- ifelse(wheels$var == "1_2", "negative", wheels$direction)
wheels$direction <- ifelse(wheels$var == "2_2", "negative", wheels$direction)

wheels$direction <- ifelse(wheels$var == "6_3" & wheels$type_var == "DEP", "negative", wheels$direction)
wheels$direction <- ifelse(wheels$var == "7_1" & wheels$type_var == "DEP", "negative", wheels$direction)
wheels$direction <- ifelse(wheels$var == "8_1" & wheels$type_var == "DEP", "negative", wheels$direction)


#Add order variable by indicator
wheels <- wheels %>%
  mutate (order = ifelse(var == "1_1", 1, 
                         ifelse(var == "1_3", 2, 
                                ifelse(var == "1_2", 3, 
                                       ifelse(var == "3_2", 4, 
                                              ifelse(var == "3_1", 5,
                                                     ifelse(var == "2_1", 6, 
                                                            ifelse(var == "2_2", 7,
                                                                   ifelse(var == "2_7", 8,
                                                                          ifelse(var == "5_1", 9,
                                                                                 ifelse(var == "6_3", 11, 
                                                                                        ifelse(var == "9_1", 13,
                                                                                               ifelse(var == "9_2", 14, 
                                                                                                      ifelse(var == "11_1", 15, 
                                                                                                             ifelse(var == "11_2", 16, 
                                                                                                                    ifelse(var == "10_1", 17,
                                                                                                                           ifelse(var == "10_2", 18,
                                                                                                                                  ifelse(var == "4_1", 19, 
                                                                                                                                         ifelse(var == "4_3", 20,
                                                                                                                                                ifelse(var == "7_2", 21,
                                                                                                                                                       ifelse(var == "7_1", 22, 
                                                                                                                                                              ifelse(var == "8_2", 23, 24))))))))))))))))))))))

# Add order for life expectancy gap by education for men
wheels$order <- ifelse(wheels$var == "5_1" & wheels$type_var ==  "HOR", 10, wheels$order)

# Add order for students with low skills
wheels$order <- ifelse(wheels$var == "6_3" & wheels$type_var ==  "DEP", 12, wheels$order)

# Sort data frame by order variable
wheels <- arrange(wheels, order)

# Sum the number of vars per country
library(data.table)
vars <- setDT(wheels)[order(country, var),.(count = uniqueN(var)) , by = country]

# Fix country codes 
wheels$country <- ifelse(wheels$country == "BEL-VLG", "BEL", wheels$country)
wheels$country <- ifelse(wheels$country == "GBR-ENG", "GBR", wheels$country)

# Drop data for GBR-NIR
wheels <- filter(wheels, country != "GBR-NIR")

# Sum the number of vars per country (again), but by order now
vars <- setDT(wheels)[order(country, order),.(count = uniqueN(order)) , by = country]

# Change var labels same as wheel and add * for reverse scored indicators
wheels <- wheels %>%
  mutate(var_label = ifelse(var == "1_2" & type_var == "VER", "S80/S20 income share ratio*", 
                            ifelse(var == "3_1" & type_var == "DEP", "Overcrowding rate*", 
                                   ifelse(var == "2_2" & type_var == "average", "Gender wage gap*", 
                                          ifelse(var == "2_7" & type_var == "DEP", "Long hours in paid work*", 
                                                  ifelse(var == "5_1" & type_var == "HOR", "Gap in life expectancy by education (men)*", 
                                                         ifelse(var == "6_3" & type_var == "average", "Student skills in science",
                                                                ifelse(var == "6_3" & type_var == "DEP", "Students with low skills*",
                                                                       ifelse(var == "9_2" & type_var == "DEP", "Exposure to outdoor air pollution*",
                                                                              ifelse(var == "11_2" & type_var == "DEP", "Negative affect balance*",
                                                                                     ifelse(var == "10_1" & type_var == "average", "Homicides*", 
                                                                                            ifelse(var == "10_2" & type_var == "HOR", "Gender gap in feeling safe", 
                                                                                                   ifelse(var == "4_3" & type_var == "average", "Gender gap in hours worked*", 
                                                                                                          ifelse(var == "7_1" & type_var == "DEP", "Lack of social support*", 
                                                                                                                 ifelse(var == "8_1" & type_var == "DEP", "Having no say in government*", var_label)))))))))))))))


# Find the 3rd highest and 3rd lowest values per variable
wheels <- wheels %>%
  group_by(order) %>%
  mutate(third.max = (Rfast::nth(value, 3, descending = T))) %>%
  mutate(third.min = (Rfast::nth(value, 3, descending = F))) %>%
  ungroup()

#Replace top 1-2 values for top 3rd value and bottom 1-2 values for lowest 3rd value
wheels$v.replaced <- ifelse(wheels$value > wheels$third.max, wheels$third.max, 
                            ifelse(wheels$value < wheels$third.min, wheels$third.min, wheels$value))

# Normalize values and reverse score for negative indicators
wheels <- wheels %>%
  mutate(v.norm = ifelse(direction == "positive", (((v.replaced - third.min)/(third.max - third.min))*10), 10-(((v.replaced - third.min)/(third.max - third.min))*10)))

#Then multiply normalized value by 100 again
wheels$v.norm <- wheels$v.norm*10

# Replace values that are lower than 5 for for 5 (to make clear in bars that data is not missing, but rather that countries are not performing well)
wheels$v.norm.r <- wheels$v.norm
wheels$v.norm.r <- ifelse(wheels$v.norm.r < 5, 5, wheels$v.norm.r)

# Add empty vars for countries missing vars (based on wheel data) - this code will have to be revised next year
unique(vars$country[vars$count < 24])

wheels$year <- as.numeric(wheels$year)

# Add empty rows for missing vars by country (use latest year to add line) to show this in wheel
wheels <- wheels %>%
  #AUS
  add_row(country = "AUS", var = "3_1", type_var = "DEP", direction = "negative", var_label = "Overcrowding rate*", year = 2020, order = 5 ) %>%
  add_row(country = "AUS", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  
  #BEL
  add_row(country = "BEL", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  
  #CAN
  add_row(country = "CAN", var = "3_1", type_var = "DEP", direction = "negative", var_label = "Overcrowding rate*", year = 2020, order = 5 ) %>%
  add_row(country = "CAN", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  
  #CHE
  add_row(country = "CHE", var = "1_3", type_var = "average", direction = "positive", var_label = "Household net wealth", year = 2019, order = 2 ) %>%
  add_row(country = "CHE", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "CHE", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "CHE", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "CHE", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  add_row(country = "CHE", var = "8_1", type_var = "DEP", direction = "negative", var_label = "Having no say in government*", year = 2017, order = 24 ) %>%
  
  #CHL
  add_row(country = "CHL", var = "1_1", type_var = "average", direction = "positive", var_label = "Household income", year = 2021, order = 1 ) %>%
  add_row(country = "CHL", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  add_row(country = "CHL", var = "11_1", type_var = "average", direction = "positive", var_label = "Life satisfaction", year = 2022, order = 15 ) %>%
  add_row(country = "CHL", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "CHL", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "CHL", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  
  #COL
  add_row(country = "COL", var = "1_1", type_var = "average", direction = "positive", var_label = "Household income", year = 2021, order = 1 ) %>%
  add_row(country = "COL", var = "1_3", type_var = "average", direction = "positive", var_label = "Household net wealth", year = 2019, order = 2 ) %>%
  add_row(country = "COL", var = "1_2", type_var = "VER", direction = "negative", var_label = "S80/S20 income share ratio*", year = 2021, order = 3 ) %>%
  add_row(country = "COL", var = "3_2", type_var = "average", direction = "positive", var_label = "Housing affordability", year = 2021, order = 4 ) %>%
  add_row(country = "COL", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "COL", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  add_row(country = "COL", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "COL", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "COL", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  add_row(country = "COL", var = "8_1", type_var = "DEP", direction = "negative", var_label = "Having no say in government*", year = 2017, order = 24 ) %>%
  
  #CRI
  add_row(country = "CRI", var = "1_3", type_var = "average", direction = "positive", var_label = "Household net wealth", year = 2019, order = 2 ) %>%
  add_row(country = "CRI", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "CRI", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  add_row(country = "CRI", var = "11_1", type_var = "average", direction = "positive", var_label = "Life satisfaction", year = 2022, order = 15 ) %>%
  add_row(country = "CRI", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "CRI", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "CRI", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  add_row(country = "CRI", var = "8_1", type_var = "DEP", direction = "negative", var_label = "Having no say in government*", year = 2017, order = 24 ) %>%
  
  #CZE
  add_row(country = "CZE", var = "1_3", type_var = "average", direction = "positive", var_label = "Household net wealth", year = 2019, order = 2 ) %>%
  add_row(country = "CZE", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "CZE", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "CZE", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "CZE", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  
  #DEU
  add_row(country = "DEU", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  
  #DNK
  add_row(country = "DNK", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "DNK", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "DNK", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  
  #ESP
  add_row(country = "ESP", var = "6_3", type_var = "DEP", direction = "negative", var_label = "Students with low skills*", year = 2018, order = 12 ) %>%
  
  #GRC
  add_row(country = "GRC", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  
  #IRL
  add_row(country = "IRL", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  
  #ISL
  add_row(country = "ISL", var = "1_1", type_var = "average", direction = "positive", var_label = "Household income", year = 2021, order = 1 ) %>%
  add_row(country = "ISL", var = "1_3", type_var = "average", direction = "positive", var_label = "Household net wealth", year = 2019, order = 2 ) %>%
  add_row(country = "ISL", var = "3_2", type_var = "average", direction = "positive", var_label = "Housing affordability", year = 2021, order = 4 ) %>%
  add_row(country = "ISL", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "ISL", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "ISL", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "ISL", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  add_row(country = "ISL", var = "8_1", type_var = "DEP", direction = "negative", var_label = "Having no say in government*", year = 2017, order = 24 ) %>%
  
  #ISR
  add_row(country = "ISR", var = "1_1", type_var = "average", direction = "positive", var_label = "Household income", year = 2021, order = 1 ) %>%
  add_row(country = "ISR", var = "1_3", type_var = "average", direction = "positive", var_label = "Household net wealth", year = 2019, order = 2 ) %>%
  add_row(country = "ISR", var = "3_2", type_var = "average", direction = "positive", var_label = "Housing affordability", year = 2021, order = 4 ) %>%
  add_row(country = "ISR", var = "3_1", type_var = "DEP", direction = "negative", var_label = "Overcrowding rate*", year = 2020, order = 5 ) %>%
  add_row(country = "ISR", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  add_row(country = "ISR", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "ISR", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "ISR", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  
  #JPN
  add_row(country = "JPN", var = "2_7", type_var = "DEP", direction = "negative", var_label = "Long hours in paid work*", year = 2021, order = 8 ) %>%
  add_row(country = "JPN", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "JPN", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  
  #KOR
  add_row(country = "KOR", var = "2_7", type_var = "DEP", direction = "negative", var_label = "Long hours in paid work*", year = 2021, order = 8 ) %>%
  add_row(country = "KOR", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "KOR", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  
  #LTU
  add_row(country = "LTU", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "LTU", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "LTU", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "LTU", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  
  #LUX
  add_row(country = "LUX", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "LUX", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "LUX", var = "8_1", type_var = "DEP", direction = "negative", var_label = "Having no say in government*", year = 2017, order = 24 ) %>%
  
  #LVA
  add_row(country = "LVA", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "LVA", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "LVA", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  add_row(country = "LVA", var = "8_1", type_var = "DEP", direction = "negative", var_label = "Having no say in government*", year = 2017, order = 24 ) %>%
  
  #MEX
  add_row(country = "MEX", var = "1_3", type_var = "average", direction = "positive", var_label = "Household net wealth", year = 2019, order = 2 ) %>%
  add_row(country = "MEX", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "MEX", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  add_row(country = "MEX", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "MEX", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "MEX", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  
  #NLD
  add_row(country = "NLD", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  
  #NZL
  add_row(country = "NZL", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  
  #PRT
  add_row(country = "PRT", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "PRT", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "PRT", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "PRT", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  add_row(country = "PRT", var = "8_1", type_var = "DEP", direction = "negative", var_label = "Having no say in government*", year = 2017, order = 24 ) %>%
  
  #SVK
  add_row(country = "SVK", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "SVK", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "SVK", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "SVK", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  
  #SVN
  add_row(country = "SVN", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  add_row(country = "SVN", var = "4_3", type_var = "average", direction = "negative", var_label = "Gender gap in hours worked*", year = 2018, order = 20 ) %>%
  add_row(country = "SVN", var = "7_2", type_var = "average", direction = "positive", var_label = "Social interactions", year = 2018, order = 21 ) %>%
  
  #SWE
  add_row(country = "SWE", var = "1_3", type_var = "average", direction = "positive", var_label = "Household net wealth", year = 2019, order = 2 ) %>%
  add_row(country = "SWE", var = "4_1", type_var = "average", direction = "positive", var_label = "Time off", year = 2019, order = 19 ) %>%
  
  #TUR
  add_row(country = "TUR", var = "1_1", type_var = "average", direction = "positive", var_label = "Household income", year = 2021, order = 1 ) %>%
  add_row(country = "TUR", var = "1_3", type_var = "average", direction = "positive", var_label = "Household net wealth", year = 2019, order = 2 ) %>%
  add_row(country = "TUR", var = "5_1", type_var = "HOR", direction = "negative", var_label = "Gap in life expectancy by education (men)*", year = 2018, order = 10 ) %>%
  add_row(country = "TUR", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  
  #USA
  add_row(country = "USA", var = "9_1", type_var = "average", direction = "positive", var_label = "Access to green space", year = 2018, order = 13 ) %>%
  add_row(country = "USA", var = "11_1", type_var = "average", direction = "positive", var_label = "Life satisfaction", year = 2022, order = 15 )
  
# Check the sum of the number of vars per country again
check <- setDT(wheels)[order(country, order),.(count = uniqueN(order)) , by = country]

# Sort data frame by order variable
wheels <- arrange(wheels, country, order)

# Create dimension (considering also inequalities within dimensions)
wheels <- wheels %>%
  mutate(dim = ifelse(order <= 2, "Income and Wealth",
                      ifelse(order == 3, "IW inequality", 
                             ifelse(order == 4, "Housing Affordability",
                                    ifelse(order == 5, "HA inequality", 
                                           ifelse(order == 6, "Work and Job Quality", 
                                                  ifelse(order < 9 & order >6 , "WJQ inequality",
                                                         ifelse(order== 9, "Health", 
                                                                ifelse(order == 10, "Health inequality",
                                                                       ifelse(order== 11, "Knowledge and Skills", 
                                                                              ifelse(order == 12, "KS inequality",
                                                                                     ifelse(order == 13, "Environmental Quality",
                                                                                            ifelse(order == 14, "EQ inequality",
                                                                                                   ifelse(order == 15, "Subjective Well-being",
                                                                                                          ifelse(order == 16, "SW inequality",
                                                                                                                 ifelse(order == 17, "Safety",
                                                                                                                        ifelse(order == 18, "Safety inequality",
                                                                                                                               ifelse(order == 19, "Work-life Balance",
                                                                                                                                      ifelse(order == 20, "WLB inequality",
                                                                                                                                             ifelse(order == 21, "Social Connections", 
                                                                                                                                                    ifelse(order == 22, "SC inequality",
                                                                                                                                                           ifelse(order == 23, "Civic Engagement", "CE inequality"))))))))))))))))))))))


# Create order variable by dimension (and inequalities)
wheels <- wheels %>%
  mutate(dim.order = ifelse(dim == "Income and Wealth", 1,
                            ifelse(dim == "IW inequality", 2,
                                   ifelse(dim == "Housing Affordability", 3,
                                          ifelse(dim == "HA inequality", 4,
                                                 ifelse(dim == "Work and Job Quality", 5,
                                                        ifelse(dim == "WJQ inequality", 6,
                                                               ifelse(dim == "Health", 7,
                                                                      ifelse(dim== "Health inequality", 8,
                                                                             ifelse(dim == "Knowledge and Skills", 9,
                                                                                    ifelse(dim == "KS inequality", 10,
                                                                                           ifelse(dim == "Environmental Quality", 11,
                                                                                                  ifelse(dim == "EQ inequality", 12,
                                                                                                         ifelse(dim == "Subjective Well-being", 13,
                                                                                                                ifelse(dim == "SW inequality", 14,
                                                                                                                       ifelse(dim == "Safety", 15,
                                                                                                                              ifelse(dim == "Safety inequality", 16,
                                                                                                                                     ifelse(dim == "Work-life Balance", 17,
                                                                                                                                            ifelse(dim == "WLB inequality", 18,
                                                                                                                                                   ifelse(dim == "Social Connections", 19,
                                                                                                                                                          ifelse(dim == "SC inequality", 20, 
                                                                                                                                                                 ifelse(dim == "Civic Engagement", 21, 22))))))))))))))))))))))

# Add colour variable by dimension (and inequalities = white) to obtain Hex code
wheels <- wheels %>%
  mutate(colours = ifelse(dim == "Income and Wealth", rgb(44, 163, 224, maxColorValue = 255),
                          ifelse(dim == "IW inequality", rgb(255, 255, 255, maxColorValue = 255),
                                 ifelse(dim == "Housing Affordability", rgb(61, 165, 148, maxColorValue = 255),
                                        ifelse(dim == "HA inequality", rgb(255, 255, 255, maxColorValue = 255),
                                               ifelse(dim == "Work and Job Quality", rgb(35, 127, 189, maxColorValue = 255),
                                                      ifelse(dim == "WJQ inequality", rgb(255, 255, 255, maxColorValue = 255),
                                                             ifelse(dim == "Health", rgb(124, 58, 115, maxColorValue = 255),
                                                                    ifelse(dim == "Health inequality", rgb(255, 255, 255, maxColorValue = 255),
                                                                           ifelse(dim == "Knowledge and Skills",  rgb(126, 169, 67, maxColorValue = 255),
                                                                                  ifelse(dim == "KS inequality", rgb(255, 255, 255, maxColorValue = 255),
                                                                                         ifelse(dim == "Environmental Quality", rgb(48, 164, 87, maxColorValue = 255),
                                                                                                ifelse(dim == "EQ inequality", rgb(255, 255, 255, maxColorValue = 255),
                                                                                                       ifelse(dim == "Subjective Well-being", rgb(226, 98, 55, maxColorValue = 255),
                                                                                                              ifelse(dim == "SW inequality", rgb(255, 255, 255, maxColorValue = 255),
                                                                                                                     ifelse(dim == "Safety", rgb(96, 96, 96, maxColorValue = 255), 
                                                                                                                            ifelse(dim == "Safety inequality", rgb(255, 255, 255, maxColorValue = 255),
                                                                                                                                   ifelse(dim == "Work-life Balance", rgb(150, 40, 40, maxColorValue = 255),
                                                                                                                                          ifelse(dim == "WLB inequality", rgb(255, 255, 255, maxColorValue = 255),
                                                                                                                                                 ifelse(dim == "Social Connections", rgb(206, 72, 93, maxColorValue = 255),
                                                                                                                                                        ifelse(dim == "SC inequality", rgb(255, 255, 255, maxColorValue = 255),
                                                                                                                                                               ifelse(dim == "Civic Engagement", rgb(220, 169, 34, maxColorValue = 255), rgb(255, 255, 255, maxColorValue = 255)))))))))))))))))))))))


# Factor variables (still wait to see if need this)
wheels$var_label <- fct_reorder(wheels$var_label, wheels$order)
wheels$dim <- fct_reorder(wheels$dim, wheels$dim.order)
wheels$colours <- fct_reorder(wheels$colours, wheels$dim.order)

#wheels$var_label <- reorder_within(wheels$var_label, wheels$order)
#wheels$dim <- reorder_within(wheels$dim, wheels$dim.order)
#wheels$colours <- reorder_within(wheels$colours, wheels$dim.order)

# Add inequality variable for bars to have stripped pattern
wheels <- wheels %>%
  mutate(inequality = ifelse(order %in% c(3, 5, 7, 8, 10, 12, 14, 16, 18, 20, 22, 24), "inequality", "average"))

# Read dimension icons (image files)
# Income and Wealth
library(jpeg)
img1 <-  readJPEG("images/income.jpg")
dim1 <- grid::rasterGrob(img1, interpolate=TRUE)

#Housing
img2 <-  readJPEG("images/housing.jpg")
dim2 <- grid::rasterGrob(img2, interpolate=TRUE)

# Work and Job Quality
img3 <-  readJPEG("images/workjob.jpg")
dim3 <- grid::rasterGrob(img3, interpolate=TRUE)

# Health
img4 <-  readJPEG("images/health.jpg")
dim4 <- grid::rasterGrob(img4, interpolate=TRUE)

# Knowledge and Skills
img5 <-  readJPEG("images/knowledge.jpg")
dim5 <- grid::rasterGrob(img5, interpolate=TRUE)

#Environmental Quality
img6 <-  readJPEG("images/environment.jpg")
dim6 <- grid::rasterGrob(img6, interpolate=TRUE)

# Subjective Well-being
img7 <-  readJPEG("images/subwellbeing.jpg")
dim7 <- grid::rasterGrob(img7, interpolate=TRUE)

# Safety
img8 <-  readJPEG("images/safety.jpg")
dim8 <- grid::rasterGrob(img8, interpolate=TRUE)

# Work-life Balance
img9 <-  readJPEG("images/worklife.jpg")
dim9 <- grid::rasterGrob(img9, interpolate=TRUE)

# Social connections
img10 <-  readJPEG("images/connections.jpg")
dim10 <- grid::rasterGrob(img10, interpolate=TRUE)

# Civic Engagement
img11 <-  readJPEG("images/civic.jpg")
dim11 <- grid::rasterGrob(img11, interpolate=TRUE)

# Changes to show missing indicators in wheel as request by DO
# Need to add a new dimension and update normalized value for missing indicators (to distinguish them from non missing indicators in terms of bar lenght)
wheels$dim.na <- wheels$dim
wheels$dim.na <- ifelse(is.na(wheels$v.norm.r), "Missing", as.character(wheels$dim.na))

wheels$v.norm.na <- wheels$v.norm.r
wheels$v.norm.na <- ifelse(is.na(wheels$v.norm.r), 129, wheels$v.norm.na)

#Make sure stripped pattern is not assigned to missing indicators (average = none)
wheels$inequality.na <- wheels$inequality
wheels$inequality.na <- ifelse(is.na(wheels$v.norm.r), "average", wheels$inequality.na)


# Plot two final options for the wheel (options A and B)

# Option A

#library(ggpattern)
#library(cowplot)

countries <- sort(unique(wheels$country))

for (c in countries) {
  
  # Wheel data frame for country
  country <- filter(wheels, country == c)
  
  # Start plot
  wheel <- ggplot() +
  
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0, 101)), #the outer wheel was at 160 before
    color = "grey",
    linetype = "dashed"
  ) +
  
  #Add outer circle per dimension
  
  # Income and Wealth
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 0.50,
    xend = 3.49,
    linetype = "solid",
    size = 5,
    colour = "#2CA3E0",
    alpha = 0.6
  ) + 
  
  # Housing
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 3.50,
    xend = 5.49,
    linetype = "solid",
    size = 5,
    colour = "#3DA594",
    alpha = 0.6
  ) +  
  
  # Work and Job Quality
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 5.50,
    xend = 8.49,
    linetype = "solid",
    size = 5,
    colour = "#237FBD",
    alpha = 0.6
  ) +  
  
  # Health
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 8.50,
    xend = 10.49,
    linetype = "solid",
    size = 5,
    colour = "#7C3A73",
    alpha = 0.6
  ) +  
  
  # Knowledge and Skills
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 10.50,
    xend = 12.49,
    linetype = "solid",
    size = 5,
    colour = "#7EA943",
    alpha = 0.6
  ) +  
  
  # Environmental Quality
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 12.50,
    xend = 14.49,
    linetype = "solid",
    size = 5,
    colour = "#30A457",
    alpha = 0.6
  ) +  
  
  # Subjective Well-being
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 14.50,
    xend = 16.49,
    linetype = "solid",
    size = 5,
    colour = "#E26237",
    alpha = 0.6
  ) +  
  
  # Safety
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 16.50,
    xend = 18.49,
    linetype = "solid",
    size = 5,
    colour = "#606060",
    alpha = 0.6
  ) +  
  
  # Work-life balance
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 18.50,
    xend = 20.49,
    linetype = "solid",
    size = 5,
    colour = "#962828",
    alpha = 0.6
  ) + 
  
  # Social connections
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 20.50,
    xend = 22.49,
    linetype = "solid",
    size = 5,
    colour = "#CE485D",
    alpha = 0.6
  ) + 
  
  # Civic Engagement
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 22.50,
    xend = 24.49,
    linetype = "solid",
    size = 5,
    colour = "#DCA922",
    alpha = 0.5
  ) + 
  
  # Use ggpattern to add stripped pattern on inequalities
  geom_bar_pattern(
    data = country,
    aes(
      x = order, 
      y = v.norm.na,
      colour = dim.na,
      fill = dim.na,
      pattern = inequality.na,
      pattern_fill = dim.na
    ),
    stat = "identity",
    position = "dodge2",
    show.legend = TRUE, # Remove legend from plot
    alpha = ifelse(country$dim.na == "Missing", 0.2, 1),
    width = ifelse(country$dim.na == "Missing", 0.8, 0.55),
    pattern_colour = "white", #border colour of stripped pattern
    pattern_angle = 45, #angle of strippes
    pattern_density = 0.3, #Approx. fraction of area the pattern fills
    pattern_spacing = 0.02 #Spacing between repetitions of pattern
  ) + 
  
  # Assign colours to dimensions manually (get colour code from colours variable in data frame)
  scale_fill_manual(values = c("Income and Wealth" = "#2CA3E0",
                               "IW inequality" = "#FFFFFF",
                               "Housing Affordability" = "#3DA594",
                               "HA inequality" = "#FFFFFF",
                               "Work and Job Quality" = "#237FBD",
                               "WJQ inequality" = "#FFFFFF",
                               "Health" = "#7C3A73",
                               "Health inequality" = "#FFFFFF",
                               "Knowledge and Skills" = "#7EA943",
                               "KS inequality" = "#FFFFFF",
                               "Environmental Quality" = "#30A457",
                               "EQ inequality" = "#FFFFFF",
                               "Subjective Well-being" = "#E26237",
                               "SW inequality" = "#FFFFFF",
                               "Safety" = "#606060",
                               "Safety inequality" = "#FFFFFF",
                               "Work-life Balance" = "#962828",
                               "WLB inequality" = "#FFFFFF",
                               "Social Connections" =  "#CE485D",
                               "SC inequality" = "#FFFFFF",
                               "Civic Engagement" = "#DCA922",
                               "CE inequality" = "#FFFFFF",
                               "Missing" = "#787974"
  ),
  guide = "none" # Remove legend
  ) +
  
  # Assign colours to borders
  scale_colour_manual(values = c("Income and Wealth" = "#2CA3E0",
                                 "IW inequality" = "#2CA3E0",
                                 "Housing Affordability" = "#3DA594",
                                 "HA inequality" = "#3DA594",
                                 "Work and Job Quality" = "#237FBD",
                                 "WJQ inequality" = "#237FBD",
                                 "Health" = "#7C3A73",
                                 "Health inequality" = "#7C3A73",
                                 "Knowledge and Skills" = "#7EA943",
                                 "KS inequality" = "#7EA943",
                                 "Environmental Quality" = "#30A457",
                                 "EQ inequality" = "#30A457",
                                 "Subjective Well-being" = "#E26237",
                                 "SW inequality" = "#E26237",
                                 "Safety" = "#606060",
                                 "Safety inequality" = "#606060",
                                 "Work-life Balance" = "#962828",
                                 "WLB inequality" = "#962828",
                                 "Social Connections" =  "#CE485D",
                                 "SC inequality" = "#CE485D",
                                 "Civic Engagement" = "#DCA922",
                                 "CE inequality" = "#DCA922",
                                 "Missing" = "#FFFFFF"
  ),
  guide = "none" # Remove legend
  ) +
  
  # Pattern for stripes
  scale_pattern_manual(values = c("inequality" = "stripe",
                                  "average" = "none"),
                       name = ""
  ) +
  
  # Colours for stripped pattern (same as colour border)
  scale_pattern_fill_manual(values = c("Income and Wealth" = "#2CA3E0",
                                       "IW inequality" = "#2CA3E0",
                                       "Housing Affordability" = "#3DA594",
                                       "HA inequality" = "#3DA594",
                                       "Work and Job Quality" = "#237FBD",
                                       "WJQ inequality" = "#237FBD",
                                       "Health" = "#7C3A73",
                                       "Health inequality" = "#7C3A73",
                                       "Knowledge and Skills" = "#7EA943",
                                       "KS inequality" = "#7EA943",
                                       "Environmental Quality" = "#30A457",
                                       "EQ inequality" = "#30A457",
                                       "Subjective Well-being" = "#E26237",
                                       "SW inequality" = "#E26237",
                                       "Safety" = "#606060",
                                       "Safety inequality" = "#606060",
                                       "Work-life Balance" = "#962828",
                                       "WLB inequality" = "#962828",
                                       "Social Connections" =  "#CE485D",
                                       "SC inequality" = "#CE485D",
                                       "Civic Engagement" = "#DCA922",
                                       "CE inequality" = "#DCA922",
                                       "Missing" = "#787974"
  ),
  guide = "none" # Remove legend
  ) +
  
  # Limits of the plot size = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-35, 250) +
  
  #Custom the plot, no axis title and no Cartesian grid
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm"), # Adjust margin for labels
    legend.text = element_text(size = 18),
    legend.key.size = unit(1, "cm"),
    legend.box.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
    
  ) + 
  
  # Coordinate polar for circular barplot
  coord_polar() +
  
  # Add indicator labels
  geom_text(data = country,
            aes(x = order,
                y = ifelse(order == 10, 170, 160),
                label = ifelse(order == 10, str_wrap(var_label, 5), str_wrap(var_label, 7))
            ),
            size = 5.5,
            colour = "black"
  ) +
  
  # Add dimension labels  
  annotate("text",
           x = 2, 
           y = 228, 
           label = str_wrap("Income and Wealth", 10),
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 4.7, 
           y = 240, 
           label = "Housing", 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 7, 
           y = 245, 
           label = str_wrap("Work and Job Quality", 11), 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 9.3, 
           y = 240, 
           label ="Health", 
           color = "black", 
           size = 7,
           fontface = 2
  ) + 
  
  annotate("text",
           x = 11.5, 
           y = 230, 
           label = str_wrap("Knowledge and Skills", 12), 
           color = "black", 
           size = 7,
           fontface = 2
  )  +
  
  annotate("text",
           x = 13.5, 
           y = 230, 
           label = str_wrap("Environmental Quality", 12), 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 15.5, 
           y = 230, 
           label = str_wrap("Sujective Well-being", 12), 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 17.5, 
           y = 225, 
           label = "Safety", 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 19.5, 
           y = 230, 
           label = str_wrap("Work-life Balance", 12), 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 21.5, 
           y = 230, 
           label = str_wrap("Social Connections", 12), 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 23.5, 
           y = 220, 
           label = str_wrap("Civic Engagement", 12), 
           color = "black", 
           size = 7, 
           fontface = 2
  ) 

# Draw wheel and plot dimension icons
 wheel.final <- ggdraw(wheel) +
  
  # Income and Wealth
  draw_grob(dim1, 
            x = 0.51,
            y = 0.84,
            width = 0.035,
            height = 0.035
  ) +
  
  # Housing
  draw_grob(dim2, 
            x = 0.695,
            y = 0.67,
            width = 0.03,
            height = 0.03
  ) +
  
  # Work and Job Quality
  draw_grob(dim3, 
            x = 0.73,
            y = 0.435,
            width = 0.03,
            height = 0.03
  ) +
  
  # Health
  draw_grob(dim4, 
            x = 0.655,
            y = 0.22,
            width = 0.03,
            height = 0.03
  ) +
  
  # Knowledge and Skills
  draw_grob(dim5, 
            x = 0.4805,
            y = 0.11,
            width = 0.03,
            height = 0.03
  ) +
  
  # Environmental Quality
  draw_grob(dim6, 
            x = 0.31,
            y = 0.11,
            width = 0.03,
            height = 0.03
  ) +
  
  # Subjective Well-being
  draw_grob(dim7, 
            x = 0.1805,
            y = 0.21,
            width = 0.03,
            height = 0.03
  ) +
  
  # Safety
  draw_grob(dim8, 
            x = 0.1205,
            y = 0.39,
            width = 0.03,
            height = 0.03
  ) +
  
  # Work-life balance
  draw_grob(dim9, 
            x = 0.105,
            y = 0.59,
            width = 0.03,
            height = 0.03
  ) +
  
  # Social Connections
  draw_grob(dim10, 
            x = 0.18,
            y = 0.77,
            width = 0.03,
            height = 0.03
  ) +
  
  # Civic Engagement
  draw_grob(dim11, 
            x = 0.32,
            y = 0.85,
            width = 0.03,
            height = 0.03
  )

ggsave2(filename = paste(c,"_wheelA.png",sep=""), plot = wheel.final, width = 25, height = 20, bg = "white")

}


# Option B

for (c in countries) {

# Wheel data frame for country
country <- filter(wheels, country == c)

# Plot wheel 
wheel <-  country %>%
  
  # Start plot
  ggplot() +
  
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0, 101)), #the outer wheel was at 160 before
    color = "grey",
    linetype = "dashed"
  ) +
  
  #Add outer circle per dimension
  
  # Income and Wealth
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 0.50,
    xend = 3.49,
    linetype = "solid",
    size = 5,
    colour = "#2CA3E0",
    alpha = 0.6
  ) + 
  
  # Housing
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 3.50,
    xend = 5.49,
    linetype = "solid",
    size = 5,
    colour = "#3DA594",
    alpha = 0.6
  ) +  
  
  # Work and Job Quality
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 5.50,
    xend = 8.49,
    linetype = "solid",
    size = 5,
    colour = "#237FBD",
    alpha = 0.6
  ) +  
  
  # Health
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 8.50,
    xend = 10.49,
    linetype = "solid",
    size = 5,
    colour = "#7C3A73",
    alpha = 0.6
  ) +  
  
  # Knowledge and Skills
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 10.50,
    xend = 12.49,
    linetype = "solid",
    size = 5,
    colour = "#7EA943",
    alpha = 0.6
  ) +  
  
  # Environmental Quality
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 12.50,
    xend = 14.49,
    linetype = "solid",
    size = 5,
    colour = "#30A457",
    alpha = 0.6
  ) +  
  
  # Subjective Well-being
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 14.50,
    xend = 16.49,
    linetype = "solid",
    size = 5,
    colour = "#E26237",
    alpha = 0.6
  ) +  
  
  # Safety
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 16.50,
    xend = 18.49,
    linetype = "solid",
    size = 5,
    colour = "#606060",
    alpha = 0.6
  ) +  
  
  # Work-life balance
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 18.50,
    xend = 20.49,
    linetype = "solid",
    size = 5,
    colour = "#962828",
    alpha = 0.6
  ) + 
  
  # Social connections
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 20.50,
    xend = 22.49,
    linetype = "solid",
    size = 5,
    colour = "#CE485D",
    alpha = 0.6
  ) + 
  
  # Civic Engagement
  annotate(
    "segment",
    y = 130,
    yend = 130,
    x = 22.50,
    xend = 24.49,
    linetype = "solid",
    size = 5,
    colour = "#DCA922",
    alpha = 0.5
  ) + 
  
  # Use ggpattern to add stripped pattern on inequalities
  geom_bar_pattern(
    data = country,
    aes(
      x = order, 
      y = v.norm.na,
      colour = dim.na,
      fill = dim.na,
      pattern = inequality.na,
      pattern_fill = dim.na
    ),
    stat = "identity",
    position = "dodge2",
    show.legend = TRUE, # Remove legend from plot
    alpha = ifelse(country$dim.na == "Missing", 0.2, 1),
    width = ifelse(country$dim.na == "Missing", 0.8, 0.55),
    pattern_colour = "white", #border colour of stripped pattern
    pattern_angle = 45, #angle of strippes
    pattern_density = 0.3, #Approx. fraction of area the pattern fills
    pattern_spacing = 0.02 #Spacing between repetitions of pattern
  ) + 
  
  # Assign colours to dimensions manually (get colour code from colours variable in data frame)
  scale_fill_manual(values = c("Income and Wealth" = "#2CA3E0",
                               "IW inequality" = "#FFFFFF",
                               "Housing Affordability" = "#3DA594",
                               "HA inequality" = "#FFFFFF",
                               "Work and Job Quality" = "#237FBD",
                               "WJQ inequality" = "#FFFFFF",
                               "Health" = "#7C3A73",
                               "Health inequality" = "#FFFFFF",
                               "Knowledge and Skills" = "#7EA943",
                               "KS inequality" = "#FFFFFF",
                               "Environmental Quality" = "#30A457",
                               "EQ inequality" = "#FFFFFF",
                               "Subjective Well-being" = "#E26237",
                               "SW inequality" = "#FFFFFF",
                               "Safety" = "#606060",
                               "Safety inequality" = "#FFFFFF",
                               "Work-life Balance" = "#962828",
                               "WLB inequality" = "#FFFFFF",
                               "Social Connections" =  "#CE485D",
                               "SC inequality" = "#FFFFFF",
                               "Civic Engagement" = "#DCA922",
                               "CE inequality" = "#FFFFFF",
                               "Missing" = "#787974"
  ),
  guide = "none" # Remove legend
  ) +
  
  # Assign colours to borders
  scale_colour_manual(values = c("Income and Wealth" = "#2CA3E0",
                                 "IW inequality" = "#2CA3E0",
                                 "Housing Affordability" = "#3DA594",
                                 "HA inequality" = "#3DA594",
                                 "Work and Job Quality" = "#237FBD",
                                 "WJQ inequality" = "#237FBD",
                                 "Health" = "#7C3A73",
                                 "Health inequality" = "#7C3A73",
                                 "Knowledge and Skills" = "#7EA943",
                                 "KS inequality" = "#7EA943",
                                 "Environmental Quality" = "#30A457",
                                 "EQ inequality" = "#30A457",
                                 "Subjective Well-being" = "#E26237",
                                 "SW inequality" = "#E26237",
                                 "Safety" = "#606060",
                                 "Safety inequality" = "#606060",
                                 "Work-life Balance" = "#962828",
                                 "WLB inequality" = "#962828",
                                 "Social Connections" =  "#CE485D",
                                 "SC inequality" = "#CE485D",
                                 "Civic Engagement" = "#DCA922",
                                 "CE inequality" = "#DCA922",
                                 "Missing" = "#FFFFFF"
  ),
  guide = "none" # Remove legend
  ) +
  
  # Pattern for stripes
  scale_pattern_manual(values = c("inequality" = "stripe",
                                  "average" = "none"),
                       name = ""
  ) +
  
  # Colours for stripped pattern (same as colour border)
  scale_pattern_fill_manual(values = c("Income and Wealth" = "#2CA3E0",
                                       "IW inequality" = "#2CA3E0",
                                       "Housing Affordability" = "#3DA594",
                                       "HA inequality" = "#3DA594",
                                       "Work and Job Quality" = "#237FBD",
                                       "WJQ inequality" = "#237FBD",
                                       "Health" = "#7C3A73",
                                       "Health inequality" = "#7C3A73",
                                       "Knowledge and Skills" = "#7EA943",
                                       "KS inequality" = "#7EA943",
                                       "Environmental Quality" = "#30A457",
                                       "EQ inequality" = "#30A457",
                                       "Subjective Well-being" = "#E26237",
                                       "SW inequality" = "#E26237",
                                       "Safety" = "#606060",
                                       "Safety inequality" = "#606060",
                                       "Work-life Balance" = "#962828",
                                       "WLB inequality" = "#962828",
                                       "Social Connections" =  "#CE485D",
                                       "SC inequality" = "#CE485D",
                                       "Civic Engagement" = "#DCA922",
                                       "CE inequality" = "#DCA922",
                                       "Missing" = "#787974"
  ),
  guide = "none" # Remove legend
  ) +
  
  # Limits of the plot size = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-35, 250) +
  
  #Custom the plot, no axis title and no Cartesian grid
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm"), # Adjust margin for labels
    legend.text = element_text(size = 19),
    legend.key.size = unit(1.25, "cm"),
    legend.box.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 1)
  ) + 
  
  # Coordinate polar for circular barplot
  coord_polar() +
  
  # Add indicator labels
  geom_text(data = country,
            aes(x = order,
                y = ifelse(order == 10, 170, 160),
                label = ifelse(order == 10, str_wrap(var_label, 5), str_wrap(var_label, 7))
            ),
            size = 5.5,
            colour = "black"
  ) +
  
  # Add dimension labels  
  annotate("text",
           x = 2.5, 
           y = 240, 
           label = str_wrap("Income and Wealth", 10),
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 4.7, 
           y = 240, 
           label = "Housing", 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 7, 
           y = 245, 
           label = str_wrap("Work and Job Quality", 11), 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 9.3, 
           y = 240, 
           label ="Health", 
           color = "black", 
           size = 7,
           fontface = 2
  ) + 
  
  annotate("text",
           x = 10.8, 
           y = 245, 
           label = str_wrap("Knowledge and Skills", 12), 
           color = "black", 
           size = 7,
           fontface = 2
  )  +
  
  annotate("text",
           x = 14, 
           y = 240, 
           label = str_wrap("Environmental Quality", 12), 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 15.5, 
           y = 230, 
           label = str_wrap("Sujective Well-being", 12), 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 17.5, 
           y = 225, 
           label = "Safety", 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 19.5, 
           y = 230, 
           label = str_wrap("Work-life Balance", 12), 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 21.5, 
           y = 230, 
           label = str_wrap("Social Connections", 12), 
           color = "black", 
           size = 7,
           fontface = 2
  ) +
  
  annotate("text",
           x = 23, 
           y = 230, 
           label = str_wrap("Civic Engagement", 12), 
           color = "black", 
           size = 7, 
           fontface = 2
  ) 


# Draw wheel and plot dimension icons
wheel.final <- ggdraw(wheel) +
  
  # Income and Wealth
  draw_grob(dim1, 
            x = 0.55,
            y = 0.83,
            width = 0.035,
            height = 0.035
  ) +
  
  # Housing
  draw_grob(dim2, 
            x = 0.69,
            y = 0.67,
            width = 0.03,
            height = 0.03
  ) +
  
  # Work and Job Quality
  draw_grob(dim3, 
            x = 0.725,
            y = 0.435,
            width = 0.03,
            height = 0.03
  ) +
  
  # Health
  draw_grob(dim4, 
            x = 0.65,
            y = 0.22,
            width = 0.03,
            height = 0.03
  ) +
  
  # Knowledge and Skills
  draw_grob(dim5, 
            x = 0.54,
            y = 0.115,
            width = 0.03,
            height = 0.03
  ) +
  
  # Environmental Quality
  draw_grob(dim6, 
            x = 0.27,
            y = 0.11,
            width = 0.03,
            height = 0.03
  ) +
  
  # Subjective Well-being
  draw_grob(dim7, 
            x = 0.1805,
            y = 0.215,
            width = 0.03,
            height = 0.03
  ) +
  
  # Safety
  draw_grob(dim8, 
            x = 0.1205,
            y = 0.39,
            width = 0.03,
            height = 0.03
  ) +
  
  # Work-life balance
  draw_grob(dim9, 
            x = 0.105,
            y = 0.59,
            width = 0.03,
            height = 0.03
  ) +
  
  # Social Connections
  draw_grob(dim10, 
            x = 0.18,
            y = 0.77,
            width = 0.03,
            height = 0.03
  ) +
  
  # Civic Engagement
  draw_grob(dim11, 
            x = 0.28,
            y = 0.85,
            width = 0.03,
            height = 0.03
  ) +
  
  # Add arrows with no points under dimension labels
  
  # Income and Wealth
  draw_line(x = c(0.55, 0.56, 0.66),
            y = c( 0.80, 0.82, 0.82),
            color = "#2CA3E0",
            size = 1
  ) +
  
  # Housing
  draw_line(x = c(0.68, 0.69, 0.78),
            y = c( 0.645, 0.665, 0.665),
            color = "#3DA594",
            size = 1
  ) +
  
  # Work and Job Quality
  draw_line(x = c(0.72, 0.73, 0.83),
            y = c( 0.45, 0.42, 0.42),
            color = "#237FBD",
            size = 1
  ) + 
  
  # Health
  draw_line(x = c(0.64, 0.65, 0.73),
            y = c( 0.275, 0.255, 0.255),
            color = "#7C3A73",
            size = 1
  ) +
  
  # Knowledge and Skills
  draw_line(x = c(0.53, 0.54, 0.64),
            y = c( 0.18, 0.16, 0.16),
            color = "#7EA943",
            size = 1
  ) +
  
  # Environmental Quality
  draw_line(x = c(0.40, 0.39, 0.28),
            y = c( 0.18, 0.16, 0.16),
            color = "#30A457",
            size = 1
  ) +
  
  # Subjective Well-being
  draw_line(x = c(0.18, 0.28, 0.29),
            y = c( 0.255, 0.255, 0.27),
            color = "#E26237",
            size = 1
  ) +
  
  # Safety
  draw_line(x = c(0.12, 0.19, 0.21),
            y = c( 0.38, 0.38, 0.40),
            color = "#606060",
            size = 1
  ) +
  
  # Work-life balance
  draw_line(x = c(0.10, 0.19, 0.21),
            y = c( 0.57, 0.57, 0.59),
            color = "#962828",
            size = 1
  ) +
  
  # Social Connections
  draw_line(x = c(0.18, 0.29, 0.30),
            y = c( 0.75, 0.75, 0.73),
            color = "#CE485D",
            size = 1
  ) +
  
  # Civic Engagement
  draw_line(x = c(0.28, 0.39, 0.40),
            y = c( 0.83, 0.83, 0.81),
            color = "#DCA922",
            size = 1
  )

ggsave2(filename = paste(c,"_wheelB.png",sep=""), plot = wheel.final, width = 25, height = 20, bg = "white")

}



