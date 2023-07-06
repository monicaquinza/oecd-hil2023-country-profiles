# ------------- R Script for R4FW Table for the HIL2023 country profiles -----------------


#Mónica Quinzá Armenta
#May 2023

# Remove everything from workspace
rm(list=ls())

# installs if necessary and loads packages
list.of.packages <- c("tidyverse", "sf", "haven", "knitr", "broom", "reshape2", "lubridate", "stargazer", "ggpubr", "fontawesome", "htmltools", "reactable", "reactablefmtr", "webshot2", "htmlwidgets", "gt", "remotes", "devtools", "viridis", "magrittr", "sysfonts", "chromote", "data.table", "rvest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))

# Download package dataui (from Github)
# Code below tells the remote package to not treat warnings as errors and keep working instead of breaking executions
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
library(remotes)
remotes::install_github("timelyportfolio/dataui", build = FALSE)
library(dataui)

# If problems when downloading (or with) package reactablefmtr try downloading directly from Github
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
remotes::install_github("kcuilla/reactablefmtr")
library(reactablefmtr)

#Install to save table using reactablefmtr
webshot::install_phantomjs()



# ---------------------------- Spearman regression - change over time ---------------------------


# read files --------------------------------------------------------------

# Read latest Well-being dataset
library("readstata13")
dat <- read.dta13("Final_dataset.dta")
#summary(dat)

# Keep only variables in R4FW table of HIL2023 country profiles

# Greenhouse gas emissions per capita: 12_8
# Material footprint: 12_14
# Red List Index: 12_7
# Produced fixed assets: 15_1
# Financial net worth of gov: 15_7
# Household debt: 15_6
# Edu attainment of young adults: 13_1
# Premature mortality: 13_3
# Labour underutilisation rate: 13_2
# Trust in others: 14_1
# Trust in government: 14_3
# Gender parity in politics: 14_5

dat2 <- dat %>%
  filter_all(all_vars(var %in% c("12_8", "12_14", "12_7", "15_1", "15_7", "15_6", "13_1", "13_3", "13_2", "14_1", "14_3", "14_5")))

#Remove partner countries
dat2 <- dat2 %>%
  filter(!(country %in% c("BRA","RUS","ZAF")))

# Check if we have the vars we wanted -> OK!
table(dat2$var)

# Keep only necessary vars
myvars <- c("country", "var", "year", "value")
dat2 <- dat2[myvars]

summary(dat2)

# Year as numeric
dat2$year <- as.numeric(dat2$year)

# Add NA values for trust in others for GBR, ISR, MEX and TUR since they only have 1 observation (to avoid error when running correlation)
dat2 <- dat2 %>%
  add_row(country = "GBR", var = "14_1", year =2021) %>%
  add_row(country = "GBR", var = "14_1", year =2018) %>%
  add_row(country = "ISR", var = "14_1", year =2018) %>%
  add_row(country = "ISR", var = "14_1", year =2016) %>%
  add_row(country = "MEX", var = "14_1", year =2018) %>%
  add_row(country = "MEX", var = "14_1", year =2016) %>%
  add_row(country = "TUR", var = "14_1", year =2014) %>%
  add_row(country = "TUR", var = "14_1", year =2016)


# loop for getting p value ------------------------------------------------

# Check years
unique(dat2$year)

# Keep 2010-latest available year(2022)
dat1022 <- subset(dat2, year>=2010)
table(dat1022$year)

#No scientific notation
options(scipen=999)

# Split by country
byctry <- split(dat1022, f = dat1022$country)

# initialize final vector containing results for all countries
all_ctry_res <- vector(mode = "list", length = length(byctry))

# loop structure: for all country i, within which variable j
for (i in 1:length(byctry)) {
  
  # Level 1: fix country
  df <- byctry[[i]]
  
  ## within a country, breakdown by variable
  #ctry_long <- melt(df, id = c("country","year")) 
  ctry_var <- split(df, f = df$var)
  
  
  ## initialize variable vector for a fixed country
  res_allvars <- vector(mode = "list", length(ctry_var))
  
  for (j in 1:length(ctry_var)){
    
    # Level 2: fix variable (within a)
    df2 <- ctry_var[[j]]
    
    # exclude any series with less than 3 data points
    if ( length(which(!is.na(df2$value))) < 3 ) { 
      df2$value <- rep(0, length(df2$year))
    }
    
    # correlation test
    ct <- cor.test(df2$value, df2$year, method = "spearman", exact = FALSE)
    #Added exact = F because of ties in data
    
    # result for a fixed variable and fixed country
    res <- data.frame(name = df2$var[[1]], p_value = ct$p.value, corr.coeff = ct$estimate)
    res_allvars[[j]] <- res
    
  }
  
  # combine all variables within a fixed country
  all_var_1ctry <- do.call(rbind, res_allvars)
  all_var_1ctry$country <- df2$country[[1]]
  rownames(all_var_1ctry) <- NULL
  
  # enter into primary list
  all_ctry_res[[i]] <- all_var_1ctry
}


# combine all countries
all_results <- do.call(rbind, all_ctry_res)

# List all values no scientific format
all_results <- format(all_results, scientific = FALSE)

# output
write.csv(all_results, "spearman_p_1022.csv")

all_results <- all_results %>%
  mutate_at(vars(p_value:corr.coeff), funs(as.numeric(.))) %>%
  rename(var = name)

# Identify 10% significance level
summary(all_results$p_value)
all_results$significant <- ifelse(all_results$p_value <= .10, 1, 0)
summary(all_results$significant)

# Read indicators direction to define type of change
direction <- read.dta13("wb_indicators_direction.dta")

# Merge
all_results <- left_join(all_results, direction, by = "var" )

# Type of correlation 
all_results$corr.type <- ifelse(all_results$corr.coeff > 0, "positive", 
                                ifelse(all_results$corr.coeff < 0, "negative", "no trend"))

# Add direction of Spearman results (with 10% significance)
all_results$change <- ifelse(all_results$significant == 1 & all_results$corr.type == "positive" & all_results$direction == "positive", "improvement",
                             ifelse(all_results$significant == 1 & all_results$corr.type == "negative" & all_results$direction == "positive", "deterioration", 
                                    ifelse(all_results$significant == 1 & all_results$corr.type == "negative" & all_results$direction == "negative", "improvement",
                                           ifelse(all_results$significant == 1 & all_results$corr.type == "positive" & all_results$direction == "negative", "deterioration", "no trend"))))



# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Tiers analysis

# Generate new data frame
tiers <- dat2
  
# Keep only latest available year by var and country
tiers <- tiers %>%
  na.omit() %>%
  relocate(var, country, year) %>%
  arrange(var, country, year) %>%
  group_by(var, country) %>%
  slice_max(year, n = 1) %>%
  ungroup()

# Merge with indicators direction
tiers <- left_join(tiers, direction, by = "var" )

# Rank countries based on value by var & find highest rank (to get # of countries )
tiers <- tiers %>%
  group_by(var) %>%
  arrange (var, value) %>%
  mutate(rank = ifelse(direction == "positive", rank(value, ties.method = "random"), rank(-value, ties.method = "random"))) %>%
  mutate (rank.max = max(rank)) %>%
  ungroup()

# Create share of tiers based on ranking
tiers$share <- tiers$rank/tiers$rank.max

# Create tiers
tiers$tiers <- ifelse(tiers$share < 0.33, 3, ifelse(tiers$share > 0.66, 1, 2))


# ------------------------------------------ Table ------------------------------------------------------

# Merge all data frames together
table <- left_join(dat1022, all_results, by = c("var" = "var", "country" = "country"))

# Clean tiers data
tiers2 <- tiers %>%
  subset(select = c(var, country, tiers))

table <- left_join (table, tiers2, by = c("var" = "var", "country" = "country"))

# Need to have 12 variables for all countries
library(data.table)
vars <- setDT(table)[order(country, var),.(count = uniqueN(var)) , by = country]
unique(vars$country[vars$count < 12])
# "AUS" "CAN" "CHE" "CHL" "COL" "CRI" "ISL" "ISR" "JPN" "KOR" "TUR" "USA"

# Missing vars 
unique(table$var[table$country == "AUS"]) #14_1
unique(table$var[table$country == "CAN"]) #14_1
unique(table$var[table$country == "CHE"]) #15_1
unique(table$var[table$country == "CHL"]) #14_1
unique(table$var[table$country == "COL"]) #13_1 13_2 14_1 15_1
unique(table$var[table$country == "CRI"]) #13_2 14_1 15_1 15_6 15_7
unique(table$var[table$country == "ISL"]) #15_1, 15_6
unique(table$var[table$country == "ISR"]) #13_2, 15_6
unique(table$var[table$country == "JPN"]) #13_1, 14_1
unique(table$var[table$country == "KOR"]) #13_2, 14_1
unique(table$var[table$country == "TUR"]) #15_1, 15_6
unique(table$var[table$country == "USA"]) #14_1

# Add rows for missing vars
table <- table %>%
  add_row(country = "AUS", var = "14_1", var_label = "Trust in others", year = full_seq(2013:2022, 1)) %>%
  add_row(country = "CAN", var = "14_1", var_label = "Trust in others", year = full_seq(2013:2022, 1)) %>%
  add_row(country = "CHE", var = "15_1", var_label = "Produced fixed assets", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "CHL", var = "14_1", var_label = "Trust in others", year = full_seq(2013:2022, 1)) %>%
  add_row(country = "COL", var = "13_1", var_label = "Educational attainment among young adults", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "COL", var = "13_2", var_label = "Labour underutilization", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "COL", var = "14_1", var_label = "Trust in others", year = full_seq(2013:2022, 1)) %>%
  add_row(country = "COL", var = "15_1", var_label = "Produced fixed assets", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "CRI", var = "13_2", var_label = "Labour underutilization", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "CRI", var = "14_1", var_label = "Trust in others", year = full_seq(2013:2022, 1)) %>%
  add_row(country = "CRI", var = "15_1", var_label = "Produced fixed assets", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "CRI", var = "15_6", var_label = "Household debt", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "CRI", var = "15_7", var_label = "Financial net wealth of general government", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "ISL", var = "15_1", var_label = "Produced fixed assets", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "ISL", var = "15_6", var_label = "Household debt", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "ISR", var = "13_2", var_label = "Labour underutilization", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "ISR", var = "15_6", var_label = "Household debt", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "JPN", var = "13_1", var_label = "Educational attainment among young adults", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "JPN", var = "14_1", var_label = "Trust in others", year = full_seq(2013:2022, 1)) %>%
  add_row(country = "KOR", var = "13_2", var_label = "Labour underutilization", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "KOR", var = "14_1", var_label = "Trust in others", year = full_seq(2013:2022, 1)) %>%
  add_row(country = "TUR", var = "15_1", var_label = "Produced fixed assets", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "TUR", var = "15_6", var_label = "Household debt", year = full_seq(2010:2021, 1)) %>%
  add_row(country = "USA", var = "14_1", var_label = "Trust in others", year = full_seq(2013:2022, 1)) 

table <- table[with(table, order(var, country)),]

# Check total number of vars
vars2 <- setDT(table)[order(country, var),.(count = uniqueN(var)) , by = country]
unique(vars2$country[vars2$count < 12])

# Add name of capitals
table <- table %>%
  mutate(capitals = ifelse(grepl('^12_', var), "Natural Capital",
                           ifelse(grepl('^13_', var), "Human Capital", 
                                  ifelse(grepl('^14_', var), "Social Capital", "Economic Capital"))))

# Create change as numeric variable
table$change.num <- ifelse(table$change == "improvement", 1, ifelse(table$change == "deterioration", 2, 3))

# Assign icons for change and tiers
table$icons.change <- ifelse(table$change.num == 1, "arrow-trend-up", 
                             ifelse(table$change.num == 2, "arrow-trend-down", "arrows-left-right"))

table$icons.change <- ifelse(is.na(table$change.num), "ellipsis", table$icons.change)

table$icons.tiers <- ifelse(table$tiers == 1, "1", 
                            ifelse(table$tiers == 2, "2", "3"))

table$icons.tiers <- ifelse(is.na(table$tiers), "ellipsis", table$icons.tiers)


# Assign colors
table$colors.change <- ifelse(is.na(table$change.num), "slategray3", "black")
table$colors.tiers <- ifelse(table$tiers == 1, "yellowgreen", 
                             ifelse(table$tiers == 2, "slategray3", "orange"))
table$colors.tiers <- ifelse(is.na(table$tiers), "slategray3", table$colors.tiers)

# Loop to plot & save table for all countries
library(reactable)
library(reactablefmtr)
library(dataui)
library(htmlwidgets)
library(rvest)
library(htmltools)
library(webshot2)

countries <- sort(unique(table$country))

for (c in countries) {

# Create data frame for table for country c
table.country <- table %>%
  filter(country == c) %>%
  select(var_label, change.num, tiers, capitals, value, icons.change, icons.tiers, colors.change, colors.tiers) %>%
  group_by(capitals, var_label, change.num, tiers) %>%
  relocate(capitals) %>%
  mutate(value = ifelse(var_label == "Produced fixed assets", value/1000, value)) %>%
  mutate(value = list(value)) %>%
  unique()

#Fix spelling of labour underutilization
table.country$var_label <- ifelse(table.country$var_label == "Labour underutilization", "Labour underutilisation", table.country$var_label)

# Add asterisk for Red List Index
table.country$var_label <- ifelse(table.country$var_label == "Red List Index of threatened species", "Red List Index of threatened species*", table.country$var_label)

# Add unit of measurement
table.country <- table.country %>%
  mutate(unit = ifelse(var_label == "Produced fixed assets", "(USD at 2015 PPPs, thousands per capita)",
                       ifelse(var_label == "Household debt", "(% of household net disposable income)", 
                              ifelse(var_label == "Financial net wealth of general government", "(% of GDP)", 
                                     ifelse(var_label == "Educational attainment among young adults", "(% of people aged 25-34 who have attained at least an upper secondary education)",
                                            ifelse(var_label == "Labour underutilisation", "(% of unemployed, discouraged and underemployed workers in the total labour force)", 
                                                   ifelse(var_label == "Premature mortality ", "(PYLL per 100 000 population, age-standardised)",
                                                          ifelse(var_label == "Material footprint", "(tonnes per capita)",
                                                                 ifelse(var_label == "Red List Index of threatened species*", "(1.0 = species qualifying as “Least Concern”, 0 = all species having gone extinct)",
                                                                        ifelse(var_label == "Greenhouse gas emissions", "(tonnes per capita, CO2 equivalent, thousands)",
                                                                               ifelse(var_label == "Trust in others", "(mean average, on a scale from 0=you do not trust any other person to 10=most people can be trusted)", 
                                                                                      ifelse(var_label == "Trust in government", "(% of the population responding “yes” to a question about confidence in the national government)", "(%  women in the national lower or single houses of parliament)")))))))))))) %>%
  relocate(unit, value, .after = var_label)


# Plot table
table.country %>%
  reactable(.,
            pagination = FALSE,
            defaultSorted = "capitals", #Sort by capital
            rowStyle = group_border_sort(columns = c("capitals"), border_width = "thin", border_color = "lightblue", border_style = "solid"), #Remove border if capital repeated
            columns = list(
              
              #Define characteristics of capital column
              capitals = colDef(style = group_merge_sort("capitals"), #merge groups by capital
                                name = "Capital",
                                align = "center",
                                #maxWidth = 160,
                                cell = function(value){
                                  img_src <- knitr::image_uri(sprintf("images/capitals/%s.png", value))
                                  image <- img(src = img_src, style = "height: 24px;", alt = value)
                                  tagList(
                                    div(style = "display: inline-block; width: 45px", image),
                                    value
                                  )
                                }), 
              
              #Define characteristics of indicator label column
              var_label = colDef(name = "Indicator",
                                 align = "center",
                                 #maxWidth = 160,
                                 #Merge indicator labels with below with unit of measurement (and place below)
                                 cell = merge_column(., "unit", merged_position = "below", size = 18, merged_size = 15, weight = "normal", merged_style = "italic")),
              
              #Hide unit of measurement column (as already merged above)
              unit = colDef(show = FALSE),
              
              #Define characteristics of type of change column
              change.num = colDef(cell = icon_sets(., 
                                                   icon_ref = "icons.change",
                                                   icon_color_ref = "colors.change",
                                                   icon_size = 20, 
                                                   icon_position = "over"),
                                  align = "center",
                                  name = "Type of change",
                                  maxWidth = 140
              ),
              
              #Define characteristics of tiers column
              tiers = colDef(cell = icon_sets(.,
                                              icon_ref = "icons.tiers",
                                              icon_color_ref = "colors.tiers",
                                              icon_size = 17, 
                                              icon_position = "over"),
                             align = "center",
                             name = "OECD tier",
                             maxWidth = 140
              ),
              
              #Define characteristics for value column and create time series (sparkline)
              value = colDef(cell = react_sparkline(table.country, 
                                                    line_color = rgb(56, 79, 104, maxColorValue = 255),
                                                    highlight_points = highlight_points(first = rgb(242, 86, 2, maxColorValue = 255), last = rgb(242, 86, 2, maxColorValue = 255)),
                                                    labels = c("first", "last"),
                                                    label_size = 8,
                                                    bandline = "innerquartiles", 
                                                    bandline_color = "black", 
                                                    tooltip = FALSE,
                                                    decimals = 0),
                             align = "center",
                             name = "Time series",
                             maxWidth = 150),
              
              #Hide remaining columns
              icons.change = colDef(show = FALSE),
              icons.tiers = colDef(show = FALSE),
              colors.change = colDef(show = FALSE), 
              colors.tiers = colDef(show = FALSE)
            ),
            
            #Remove sort icon from table for capitals
            showSortIcon = FALSE,
            
            #Set theme for table
            theme = reactableTheme(borderColor = "lightblue",
                                   borderWidth = "2px",
                                   headerStyle = list(fontSize = 21,
                                                      centered = TRUE),
                                   cellStyle = list(fontSize = 18)), 
            outlined = TRUE, # Add borders around the table
            #bordered = FALSE,
            borderless = TRUE
            #compact = TRUE,
            #width = 850,
            #height = 1300
  ) %>%
  #google_font(font_family = "Dosis") %>%
  save_reactable(paste(c,"_R4FW.png",sep=""))

}


