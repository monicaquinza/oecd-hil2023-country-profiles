#-------------------  Script for HIL2023 vertical inequalities -------------------------

# Mónica Quinzá Armenta
# April 2023

# Remove all data frames in the workspace
ls()
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"])

# Installs if necessary and loads packages 
list.of.packages <- c("tidyverse", "sf", "broom", "viridis", "tidytext", "upstartr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))

# Import file
library("readstata13")
df <- read.dta13("hil2023_vertical data_april2023.dta")

# Replace variable number by ordering of figures in HIL2022 (which follows chapters in HIL2020)
table(df$var)

df$varorder <- ifelse(df$var == "1_2", 1, 
                 ifelse(df$var=="1_3", 2,
                        ifelse(df$var == "2_8", 3, 
                               ifelse(df$var == "6_3", 4,
                                      ifelse(df$var == "11_1", 5, 
                                             ifelse(df$var=="4_4", 6, 99999))))))


# Order data by var and by value (in descending order by value)
df <- df[order(df$varorder,-df$value),]   

# Create label var for all plots
df$var_label <- ifelse(df$var == "1_2", "Household income of the top 20% relative to the bottom 20%", ifelse(df$var == "1_3", "Share of wealth owned by the top 10%", ifelse(df$var == "2_8", "Earnings of the top 10% relative to the bottom 10%, full-time employees", ifelse(df$var == "6_3", "PISA score in science of the top 10% relative to the bottom 10%", ifelse(df$var == "11_1", "Life satisfaction scores of the top 20% relative to the bottom 20%", ifelse(df$var == "4_4", "Satisfaction with time use scores of the top 20% relative to the bottom 20%", "")))))) 

# Plot faceted bar charts
library(ggplot2)

countries <- sort(unique(df$country))

for (c in countries) {
  df %>%
    mutate(country = reorder_within(country, -value, varorder)) %>%
    ggplot( aes(x = country, y = value)) +  
    geom_col(width = 0.5, fill = ifelse(df$country == c, rgb(242, 86, 2, maxColorValue = 255), ifelse(grepl('^OECD', df$country), rgb(56, 79, 104, maxColorValue = 255), rgb(201, 201, 201, maxColorValue = 255)))) +  #shape of bars
    facet_wrap(~reorder(var_label, varorder), ncol = 2, scales="free") + # facet_wrap creates a facet plot so there's a panel for each var category, and free scales allows the x and y axis to appear in all panels and adjust automatically
    theme_classic() +
    scale_x_reordered() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust = 1, size = 7), strip.background = element_blank(), strip.text = element_text(size = 9, face = "bold")) +  #x-axis labels orientation and characteristics, strip.background and strip.text manages indicator variable labels at the top of each panel
    xlab("") +
    ylab("") +
    geom_text(aes(label = ifelse(df$country == c, round(df$value, digits = ifelse(df$varorder == 4, 2, 1)), 
                                 ifelse(grepl('^OECD', df$country), round(df$value, digits = ifelse(df$varorder == 4, 2, 1)), "" ))),  
              vjust = -0.5, size = 2.5, position = position_dodge(width = 0.75))
  ggsave( filename = paste(c,".png",sep=""),  width = 25, height = 23, units = "cm")
  
}











