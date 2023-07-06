# --------------------------- R Script for HIL2023 lollipops ---------------------------------


# Mónica Quinzá Armenta
# April 2022


#Empty everything in the environment
rm(list = ls())
# Or remove all data frames in the workspace
#ls()
#rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"])

# Installs if necessary and loads packages
list.of.packages <- c("tidyverse", "sf", "broom")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))

#Download lollipops data by sex (dta file)
library("readstata13")
df.sex <- read.dta13("hil2023_lollipops_female_male_final.dta")
#view (df.sex)
#summary(df.sex)

#Create copy of df by sex to keep only OECD vars and OECD countries
df.sex.oecd <- subset(df.sex, oecd == 1)

#Keep only certain vars
myvars <- c("country", "var_label", "var", "oecd", "oecd_female", "oecd_male", "oecd_ratio", "oecd_ratio_label")
df.sex.oecd <- df.sex.oecd[myvars]%>%
  mutate(country = "OECD")

df.sex.oecd <- unique(df.sex.oecd) %>%
  na.omit(df.sex.oecd) %>%
  rename(female = oecd_female, male = oecd_male, ratio = oecd_ratio, ratio_label = oecd_ratio_label)


# --------------------------------- Plot lollipops -------------------------------------

# AUS Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="AUS")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.AUS),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.AUS <- df$ratio_graph.AUS

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.AUS <- ifelse(is.na(df$ratio_country2.AUS), df$ratio_graph.OECD, df$ratio_country2.AUS)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.AUS),]

#Create order variable of country ratio_country2 values
df$order_var.AUS <- order(df$ratio_country2.AUS)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.AUS, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.AUS, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.AUS))

#Rename vars to remove country name
df <- rename(df, 
                     order_var = order_var.AUS,
                     var = var.AUS,
                     female = female.AUS,
                     male = male.AUS,
                     ratio = ratio.AUS,
                     ratio_label = ratio_label.AUS,
                     ratio_label2 = ratio_label2.AUS,
                     dim = dim.AUS,
                     ratio_graph = ratio_graph.AUS, 
                     truncated = truncated.AUS,
                     order = order.AUS
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop NA values in new data frame
#df <- na.omit(df)
#df.sex.aus <- df.sex.aus[rowSums(is.na(df.sex.aus)) != ncol(df.sex.aus), ]

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Sort data frame values for lollipop chart by ratio values regardless of country
#df <- df[order(df$ratio_label),]

#Reorder values for lollipop chart
#df$var_label <- fct_reorder(df$var_label, -df$ratio_label)

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   
#We need a label for the colors to go automatically in label
#df.sex.aus$colors_label <- ifelse(df.sex.aus$colors == "black", "OECD average", ifelse(df.sex.aus$colors == "blue", "Men doing better", ifelse(df.sex.aus$colors == "orange", "Women doing better", "No clear difference between men and women")))

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.6, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.6, xend = 0.6, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("AUS.png",  width = 17.7, height = 18, units = "cm")


# AUT Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="AUT")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.AUT),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.AUT <- df$ratio_graph.AUT

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.AUT <- ifelse(is.na(df$ratio_country2.AUT), df$ratio_graph.OECD, df$ratio_country2.AUT)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.AUT),]

#Create order variable of country ratio_country2 values
df$order_var.AUT <- order(df$ratio_country2.AUT)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.AUT, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.AUT, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.AUT))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.AUT,
             var = var.AUT,
             female = female.AUT,
             male = male.AUT,
             ratio = ratio.AUT,
             ratio_label = ratio_label.AUT,
             ratio_label2 = ratio_label2.AUT,
             dim = dim.AUT,
             ratio_graph = ratio_graph.AUT, 
             truncated = truncated.AUT,
             order = order.AUT
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop NA values in new data frame
#df <- na.omit(df)
#df.sex.aus <- df.sex.aus[rowSums(is.na(df.sex.aus)) != ncol(df.sex.aus), ]

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.6, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.6, xend = 0.6, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("AUT.png",  width = 17.7, height = 18, units = "cm")


# BEL Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="BEL")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.BEL),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.BEL <- df$ratio_graph.BEL

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.BEL <- ifelse(is.na(df$ratio_country2.BEL), df$ratio_graph.OECD, df$ratio_country2.BEL)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.BEL),]

#Create order variable of country ratio_country2 values
df$order_var.BEL <- order(df$ratio_country2.BEL)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.BEL, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.BEL, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.BEL))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.BEL,
             var = var.BEL,
             female = female.BEL,
             male = male.BEL,
             ratio = ratio.BEL,
             ratio_label = ratio_label.BEL,
             ratio_label2 = ratio_label2.BEL,
             dim = dim.BEL,
             ratio_graph = ratio_graph.BEL, 
             truncated = truncated.BEL,
             order = order.BEL
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop NA values in new data frame
#df <- na.omit(df)
#df.sex.aus <- df.sex.aus[rowSums(is.na(df.sex.aus)) != ncol(df.sex.aus), ]

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")
 
#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.6, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.6, xend = 0.6, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("BEL.png",  width = 17.7, height = 18, units = "cm")


# CAN Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="CAN")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.CAN),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.CAN <- df$ratio_graph.CAN

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.CAN <- ifelse(is.na(df$ratio_country2.CAN), df$ratio_graph.OECD, df$ratio_country2.CAN)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.CAN),]

#Create order variable of country ratio_country2 values
df$order_var.CAN <- order(df$ratio_country2.CAN)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.CAN, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.CAN, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.CAN))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.CAN,
             var = var.CAN,
             female = female.CAN,
             male = male.CAN,
             ratio = ratio.CAN,
             ratio_label = ratio_label.CAN,
             ratio_label2 = ratio_label2.CAN,
             dim = dim.CAN,
             ratio_graph = ratio_graph.CAN, 
             truncated = truncated.CAN,
             order = order.CAN
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop NA values in new data frame
#df <- na.omit(df)
#df.sex.aus <- df.sex.aus[rowSums(is.na(df.sex.aus)) != ncol(df.sex.aus), ]

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.6, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.6, xend = 0.6, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("CAN.png",  width = 17.7, height = 18, units = "cm")


# CHE Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="CHE")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.CHE),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.CHE <- df$ratio_graph.CHE

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.CHE <- ifelse(is.na(df$ratio_country2.CHE), df$ratio_graph.OECD, df$ratio_country2.CHE)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.CHE),]

#Create order variable of country ratio_country2 values
df$order_var.CHE <- order(df$ratio_country2.CHE)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.CHE, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.CHE, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.CHE))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.CHE,
             var = var.CHE,
             female = female.CHE,
             male = male.CHE,
             ratio = ratio.CHE,
             ratio_label = ratio_label.CHE,
             ratio_label2 = ratio_label2.CHE,
             dim = dim.CHE,
             ratio_graph = ratio_graph.CHE, 
             truncated = truncated.CHE,
             order = order.CHE
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop NA values in new data frame
#df <- na.omit(df)
#df.sex.aus <- df.sex.aus[rowSums(is.na(df.sex.aus)) != ncol(df.sex.aus), ]

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.6, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.6, xend = 0.6, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("CHE.png",  width = 17.7, height = 18, units = "cm")


# CHL Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="CHL")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.CHL),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.CHL <- df$ratio_graph.CHL

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.CHL <- ifelse(is.na(df$ratio_country2.CHL), df$ratio_graph.OECD, df$ratio_country2.CHL)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.CHL),]

#Create order variable of country ratio_country2 values
df$order_var.CHL <- order(df$ratio_country2.CHL)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.CHL, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.CHL, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.CHL))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.CHL,
             var = var.CHL,
             female = female.CHL,
             male = male.CHL,
             ratio = ratio.CHL,
             ratio_label = ratio_label.CHL,
             ratio_label2 = ratio_label2.CHL,
             dim = dim.CHL,
             ratio_graph = ratio_graph.CHL, 
             truncated = truncated.CHL,
             order = order.CHL
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop NA values in new data frame
#df <- na.omit(df)
#df.sex.aus <- df.sex.aus[rowSums(is.na(df.sex.aus)) != ncol(df.sex.aus), ]

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.6, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.6, xend = 0.6, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("CHL.png",  width = 17.7, height = 18, units = "cm")


# COL Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="COL")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.COL),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.COL <- df$ratio_graph.COL

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.COL <- ifelse(is.na(df$ratio_country2.COL), df$ratio_graph.OECD, df$ratio_country2.COL)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.COL),]

#Create order variable of country ratio_country2 values
df$order_var.COL <- order(df$ratio_country2.COL)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.COL, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.COL, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.COL))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.COL,
             var = var.COL,
             female = female.COL,
             male = male.COL,
             ratio = ratio.COL,
             ratio_label = ratio_label.COL,
             ratio_label2 = ratio_label2.COL,
             dim = dim.COL,
             ratio_graph = ratio_graph.COL, 
             truncated = truncated.COL,
             order = order.COL
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop NA values in new data frame
#df <- na.omit(df)
#df <- df[rowSums(is.na(df)) != ncol(df), ]

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 8),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("COL.png",  width = 17.7, height = 18, units = "cm")
# Bigger bubbbles size 8


# CRI Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="CRI")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.CRI),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.CRI <- df$ratio_graph.CRI

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.CRI <- ifelse(is.na(df$ratio_country2.CRI), df$ratio_graph.OECD, df$ratio_country2.CRI)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.CRI),]

#Create order variable of country ratio_country2 values
df$order_var.CRI <- order(df$ratio_country2.CRI)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.CRI, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.CRI, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.CRI))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.CRI,
             var = var.CRI,
             female = female.CRI,
             male = male.CRI,
             ratio = ratio.CRI,
             ratio_label = ratio_label.CRI,
             ratio_label2 = ratio_label2.CRI,
             dim = dim.CRI,
             ratio_graph = ratio_graph.CRI, 
             truncated = truncated.CRI,
             order = order.CRI
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop NA values in new data frame
#df <- na.omit(df)
#df <- df[rowSums(is.na(df)) != ncol(df), ]

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 8),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("CRI.png",  width = 17.7, height = 18, units = "cm")


# CZE Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="CZE")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.CZE),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.CZE <- df$ratio_graph.CZE

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.CZE <- ifelse(is.na(df$ratio_country2.CZE), df$ratio_graph.OECD, df$ratio_country2.CZE)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.CZE),]

#Create order variable of country ratio_country2 values
df$order_var.CZE <- order(df$ratio_country2.CZE)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.CZE, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.CZE, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.CZE))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.CZE,
             var = var.CZE,
             female = female.CZE,
             male = male.CZE,
             ratio = ratio.CZE,
             ratio_label = ratio_label.CZE,
             ratio_label2 = ratio_label2.CZE,
             dim = dim.CZE,
             ratio_graph = ratio_graph.CZE, 
             truncated = truncated.CZE,
             order = order.CZE
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("CZE.png",  width = 17.7, height = 18, units = "cm")


# DEU Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="DEU")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.DEU),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.DEU <- df$ratio_graph.DEU

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.DEU <- ifelse(is.na(df$ratio_country2.DEU), df$ratio_graph.OECD, df$ratio_country2.DEU)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.DEU),]

#Create order variable of country ratio_country2 values
df$order_var.DEU <- order(df$ratio_country2.DEU)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.DEU, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.DEU, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.DEU))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.DEU,
             var = var.DEU,
             female = female.DEU,
             male = male.DEU,
             ratio = ratio.DEU,
             ratio_label = ratio_label.DEU,
             ratio_label2 = ratio_label2.DEU,
             dim = dim.DEU,
             ratio_graph = ratio_graph.DEU, 
             truncated = truncated.DEU,
             order = order.DEU
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("DEU.png",  width = 17.7, height = 18, units = "cm")


# DNK Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="DNK")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.DNK),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.DNK <- df$ratio_graph.DNK

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.DNK <- ifelse(is.na(df$ratio_country2.DNK), df$ratio_graph.OECD, df$ratio_country2.DNK)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.DNK),]

#Create order variable of country ratio_country2 values
df$order_var.DNK <- order(df$ratio_country2.DNK)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.DNK, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.DNK, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.DNK))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.DNK,
             var = var.DNK,
             female = female.DNK,
             male = male.DNK,
             ratio = ratio.DNK,
             ratio_label = ratio_label.DNK,
             ratio_label2 = ratio_label2.DNK,
             dim = dim.DNK,
             ratio_graph = ratio_graph.DNK, 
             truncated = truncated.DNK,
             order = order.DNK
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 8),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("DNK.png",  width = 17.7, height = 18, units = "cm")



# ESP Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="ESP")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.ESP),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.ESP <- df$ratio_graph.ESP

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.ESP <- ifelse(is.na(df$ratio_country2.ESP), df$ratio_graph.OECD, df$ratio_country2.ESP)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.ESP),]

#Create order variable of country ratio_country2 values
df$order_var.ESP <- order(df$ratio_country2.ESP)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.ESP, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.ESP, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.ESP))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.ESP,
             var = var.ESP,
             female = female.ESP,
             male = male.ESP,
             ratio = ratio.ESP,
             ratio_label = ratio_label.ESP,
             ratio_label2 = ratio_label2.ESP,
             dim = dim.ESP,
             ratio_graph = ratio_graph.ESP, 
             truncated = truncated.ESP,
             order = order.ESP
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("ESP.png",  width = 17.7, height = 18, units = "cm")


# EST Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="EST")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.EST),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.EST <- df$ratio_graph.EST

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.EST <- ifelse(is.na(df$ratio_country2.EST), df$ratio_graph.OECD, df$ratio_country2.EST)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.EST),]

#Create order variable of country ratio_country2 values
df$order_var.EST <- order(df$ratio_country2.EST)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.EST, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.EST, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.EST))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.EST,
             var = var.EST,
             female = female.EST,
             male = male.EST,
             ratio = ratio.EST,
             ratio_label = ratio_label.EST,
             ratio_label2 = ratio_label2.EST,
             dim = dim.EST,
             ratio_graph = ratio_graph.EST, 
             truncated = truncated.EST,
             order = order.EST
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("EST.png",  width = 17.7, height = 18, units = "cm")


# FIN Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="FIN")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.FIN),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.FIN <- df$ratio_graph.FIN

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.FIN <- ifelse(is.na(df$ratio_country2.FIN), df$ratio_graph.OECD, df$ratio_country2.FIN)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.FIN),]

#Create order variable of country ratio_country2 values
df$order_var.FIN <- order(df$ratio_country2.FIN)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.FIN, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.FIN, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.FIN))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.FIN,
             var = var.FIN,
             female = female.FIN,
             male = male.FIN,
             ratio = ratio.FIN,
             ratio_label = ratio_label.FIN,
             ratio_label2 = ratio_label2.FIN,
             dim = dim.FIN,
             ratio_graph = ratio_graph.FIN, 
             truncated = truncated.FIN,
             order = order.FIN
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("FIN.png",  width = 17.7, height = 18, units = "cm")


# FRA Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="FRA")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.FRA),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.FRA <- df$ratio_graph.FRA

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.FRA <- ifelse(is.na(df$ratio_country2.FRA), df$ratio_graph.OECD, df$ratio_country2.FRA)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.FRA),]

#Create order variable of country ratio_country2 values
df$order_var.FRA <- order(df$ratio_country2.FRA)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.FRA, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.FRA, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.FRA))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.FRA,
             var = var.FRA,
             female = female.FRA,
             male = male.FRA,
             ratio = ratio.FRA,
             ratio_label = ratio_label.FRA,
             ratio_label2 = ratio_label2.FRA,
             dim = dim.FRA,
             ratio_graph = ratio_graph.FRA, 
             truncated = truncated.FRA,
             order = order.FRA
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("FRA.png",  width = 17.7, height = 18, units = "cm")


# GBR Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="GBR")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.GBR),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.GBR <- df$ratio_graph.GBR

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.GBR <- ifelse(is.na(df$ratio_country2.GBR), df$ratio_graph.OECD, df$ratio_country2.GBR)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.GBR),]

#Create order variable of country ratio_country2 values
df$order_var.GBR <- order(df$ratio_country2.GBR)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.GBR, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.GBR, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.GBR))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.GBR,
             var = var.GBR,
             female = female.GBR,
             male = male.GBR,
             ratio = ratio.GBR,
             ratio_label = ratio_label.GBR,
             ratio_label2 = ratio_label2.GBR,
             dim = dim.GBR,
             ratio_graph = ratio_graph.GBR, 
             truncated = truncated.GBR,
             order = order.GBR
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4" & var != "8_1")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 8, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 9, yend = 7),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("GBR.png",  width = 17.7, height = 20, units = "cm")


# GRC Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="GRC")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.GRC),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.GRC <- df$ratio_graph.GRC

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.GRC <- ifelse(is.na(df$ratio_country2.GRC), df$ratio_graph.OECD, df$ratio_country2.GRC)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.GRC),]

#Create order variable of country ratio_country2 values
df$order_var.GRC <- order(df$ratio_country2.GRC)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.GRC, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.GRC, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.GRC))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.GRC,
             var = var.GRC,
             female = female.GRC,
             male = male.GRC,
             ratio = ratio.GRC,
             ratio_label = ratio_label.GRC,
             ratio_label2 = ratio_label2.GRC,
             dim = dim.GRC,
             ratio_graph = ratio_graph.GRC, 
             truncated = truncated.GRC,
             order = order.GRC
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 6, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 7, yend = 5),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("GRC.png",  width = 17.7, height = 18, units = "cm")


# HUN Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="HUN")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.HUN),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.HUN <- df$ratio_graph.HUN

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.HUN <- ifelse(is.na(df$ratio_country2.HUN), df$ratio_graph.OECD, df$ratio_country2.HUN)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.HUN),]

#Create order variable of country ratio_country2 values
df$order_var.HUN <- order(df$ratio_country2.HUN)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.HUN, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.HUN, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.HUN))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.HUN,
             var = var.HUN,
             female = female.HUN,
             male = male.HUN,
             ratio = ratio.HUN,
             ratio_label = ratio_label.HUN,
             ratio_label2 = ratio_label2.HUN,
             dim = dim.HUN,
             ratio_graph = ratio_graph.HUN, 
             truncated = truncated.HUN,
             order = order.HUN
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("HUN.png",  width = 17.7, height = 18, units = "cm")


# IRL Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="IRL")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.IRL),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.IRL <- df$ratio_graph.IRL

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.IRL <- ifelse(is.na(df$ratio_country2.IRL), df$ratio_graph.OECD, df$ratio_country2.IRL)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.IRL),]

#Create order variable of country ratio_country2 values
df$order_var.IRL <- order(df$ratio_country2.IRL)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.IRL, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.IRL, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.IRL))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.IRL,
             var = var.IRL,
             female = female.IRL,
             male = male.IRL,
             ratio = ratio.IRL,
             ratio_label = ratio_label.IRL,
             ratio_label2 = ratio_label2.IRL,
             dim = dim.IRL,
             ratio_graph = ratio_graph.IRL, 
             truncated = truncated.IRL,
             order = order.IRL
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("IRL.png",  width = 17.7, height = 18, units = "cm")


# ISL Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="ISL")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.ISL),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.ISL <- df$ratio_graph.ISL

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.ISL <- ifelse(is.na(df$ratio_country2.ISL), df$ratio_graph.OECD, df$ratio_country2.ISL)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.ISL),]

#Create order variable of country ratio_country2 values
df$order_var.ISL <- order(df$ratio_country2.ISL)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.ISL, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.ISL, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.ISL))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.ISL,
             var = var.ISL,
             female = female.ISL,
             male = male.ISL,
             ratio = ratio.ISL,
             ratio_label = ratio_label.ISL,
             ratio_label2 = ratio_label2.ISL,
             dim = dim.ISL,
             ratio_graph = ratio_graph.ISL, 
             truncated = truncated.ISL,
             order = order.ISL
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("ISL.png",  width = 17.7, height = 18, units = "cm")


# ISR Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="ISR")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.ISR),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.ISR <- df$ratio_graph.ISR

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.ISR <- ifelse(is.na(df$ratio_country2.ISR), df$ratio_graph.OECD, df$ratio_country2.ISR)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.ISR),]

#Create order variable of country ratio_country2 values
df$order_var.ISR <- order(df$ratio_country2.ISR)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.ISR, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.ISR, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.ISR))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.ISR,
             var = var.ISR,
             female = female.ISR,
             male = male.ISR,
             ratio = ratio.ISR,
             ratio_label = ratio_label.ISR,
             ratio_label2 = ratio_label2.ISR,
             dim = dim.ISR,
             ratio_graph = ratio_graph.ISR, 
             truncated = truncated.ISR,
             order = order.ISR
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("ISR.png",  width = 17.7, height = 18, units = "cm")


# ITA Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="ITA")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.ITA),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.ITA <- df$ratio_graph.ITA

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.ITA <- ifelse(is.na(df$ratio_country2.ITA), df$ratio_graph.OECD, df$ratio_country2.ITA)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.ITA),]

#Create order variable of country ratio_country2 values
df$order_var.ITA <- order(df$ratio_country2.ITA)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.ITA, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.ITA, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.ITA))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.ITA,
             var = var.ITA,
             female = female.ITA,
             male = male.ITA,
             ratio = ratio.ITA,
             ratio_label = ratio_label.ITA,
             ratio_label2 = ratio_label2.ITA,
             dim = dim.ITA,
             ratio_graph = ratio_graph.ITA, 
             truncated = truncated.ITA,
             order = order.ITA
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("ITA.png",  width = 17.7, height = 18, units = "cm")


# JPN Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="JPN")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.JPN),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.JPN <- df$ratio_graph.JPN

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.JPN <- ifelse(is.na(df$ratio_country2.JPN), df$ratio_graph.OECD, df$ratio_country2.JPN)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.JPN),]

#Create order variable of country ratio_country2 values
df$order_var.JPN <- order(df$ratio_country2.JPN)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.JPN, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.JPN, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.JPN))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.JPN,
             var = var.JPN,
             female = female.JPN,
             male = male.JPN,
             ratio = ratio.JPN,
             ratio_label = ratio_label.JPN,
             ratio_label2 = ratio_label2.JPN,
             dim = dim.JPN,
             ratio_graph = ratio_graph.JPN, 
             truncated = truncated.JPN,
             order = order.JPN
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("JPN.png",  width = 17.7, height = 18, units = "cm")


# KOR Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="KOR")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.KOR),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.KOR <- df$ratio_graph.KOR

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.KOR <- ifelse(is.na(df$ratio_country2.KOR), df$ratio_graph.OECD, df$ratio_country2.KOR)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.KOR),]

#Create order variable of country ratio_country2 values
df$order_var.KOR <- order(df$ratio_country2.KOR)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.KOR, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.KOR, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.KOR))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.KOR,
             var = var.KOR,
             female = female.KOR,
             male = male.KOR,
             ratio = ratio.KOR,
             ratio_label = ratio_label.KOR,
             ratio_label2 = ratio_label2.KOR,
             dim = dim.KOR,
             ratio_graph = ratio_graph.KOR, 
             truncated = truncated.KOR,
             order = order.KOR
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("KOR.png",  width = 17.7, height = 18, units = "cm")


# LTU Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="LTU")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.LTU),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.LTU <- df$ratio_graph.LTU

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.LTU <- ifelse(is.na(df$ratio_country2.LTU), df$ratio_graph.OECD, df$ratio_country2.LTU)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.LTU),]

#Create order variable of country ratio_country2 values
df$order_var.LTU <- order(df$ratio_country2.LTU)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.LTU, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.LTU, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.LTU))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.LTU,
             var = var.LTU,
             female = female.LTU,
             male = male.LTU,
             ratio = ratio.LTU,
             ratio_label = ratio_label.LTU,
             ratio_label2 = ratio_label2.LTU,
             dim = dim.LTU,
             ratio_graph = ratio_graph.LTU, 
             truncated = truncated.LTU,
             order = order.LTU
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("LTU.png",  width = 17.7, height = 18, units = "cm")


# LUX Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="LUX")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.LUX),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.LUX <- df$ratio_graph.LUX

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.LUX <- ifelse(is.na(df$ratio_country2.LUX), df$ratio_graph.OECD, df$ratio_country2.LUX)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.LUX),]

#Create order variable of country ratio_country2 values
df$order_var.LUX <- order(df$ratio_country2.LUX)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.LUX, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.LUX, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.LUX))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.LUX,
             var = var.LUX,
             female = female.LUX,
             male = male.LUX,
             ratio = ratio.LUX,
             ratio_label = ratio_label.LUX,
             ratio_label2 = ratio_label2.LUX,
             dim = dim.LUX,
             ratio_graph = ratio_graph.LUX, 
             truncated = truncated.LUX,
             order = order.LUX
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("LUX.png",  width = 17.7, height = 18, units = "cm")


# LVA Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="LVA")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.LVA),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.LVA <- df$ratio_graph.LVA

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.LVA <- ifelse(is.na(df$ratio_country2.LVA), df$ratio_graph.OECD, df$ratio_country2.LVA)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.LVA),]

#Create order variable of country ratio_country2 values
df$order_var.LVA <- order(df$ratio_country2.LVA)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.LVA, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.LVA, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.LVA))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.LVA,
             var = var.LVA,
             female = female.LVA,
             male = male.LVA,
             ratio = ratio.LVA,
             ratio_label = ratio_label.LVA,
             ratio_label2 = ratio_label2.LVA,
             dim = dim.LVA,
             ratio_graph = ratio_graph.LVA, 
             truncated = truncated.LVA,
             order = order.LVA
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("LVA.png",  width = 17.7, height = 18, units = "cm")

# MEX Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="MEX")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.MEX),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.MEX <- df$ratio_graph.MEX

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.MEX <- ifelse(is.na(df$ratio_country2.MEX), df$ratio_graph.OECD, df$ratio_country2.MEX)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.MEX),]

#Create order variable of country ratio_country2 values
df$order_var.MEX <- order(df$ratio_country2.MEX)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.MEX, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.MEX, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.MEX))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.MEX,
             var = var.MEX,
             female = female.MEX,
             male = male.MEX,
             ratio = ratio.MEX,
             ratio_label = ratio_label.MEX,
             ratio_label2 = ratio_label2.MEX,
             dim = dim.MEX,
             ratio_graph = ratio_graph.MEX, 
             truncated = truncated.MEX,
             order = order.MEX
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("MEX.png",  width = 17.7, height = 18, units = "cm")


# NLD Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="NLD")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.NLD),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.NLD <- df$ratio_graph.NLD

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.NLD <- ifelse(is.na(df$ratio_country2.NLD), df$ratio_graph.OECD, df$ratio_country2.NLD)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.NLD),]

#Create order variable of country ratio_country2 values
df$order_var.NLD <- order(df$ratio_country2.NLD)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.NLD, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.NLD, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.NLD))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.NLD,
             var = var.NLD,
             female = female.NLD,
             male = male.NLD,
             ratio = ratio.NLD,
             ratio_label = ratio_label.NLD,
             ratio_label2 = ratio_label2.NLD,
             dim = dim.NLD,
             ratio_graph = ratio_graph.NLD, 
             truncated = truncated.NLD,
             order = order.NLD
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("NLD.png",  width = 17.7, height = 18, units = "cm")


# NOR Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="NOR")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.NOR),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.NOR <- df$ratio_graph.NOR

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.NOR <- ifelse(is.na(df$ratio_country2.NOR), df$ratio_graph.OECD, df$ratio_country2.NOR)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.NOR),]

#Create order variable of country ratio_country2 values
df$order_var.NOR <- order(df$ratio_country2.NOR)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.NOR, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.NOR, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.NOR))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.NOR,
             var = var.NOR,
             female = female.NOR,
             male = male.NOR,
             ratio = ratio.NOR,
             ratio_label = ratio_label.NOR,
             ratio_label2 = ratio_label2.NOR,
             dim = dim.NOR,
             ratio_graph = ratio_graph.NOR, 
             truncated = truncated.NOR,
             order = order.NOR
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("NOR.png",  width = 17.7, height = 18, units = "cm")


# NZL Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="NZL")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.NZL),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.NZL <- df$ratio_graph.NZL

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.NZL <- ifelse(is.na(df$ratio_country2.NZL), df$ratio_graph.OECD, df$ratio_country2.NZL)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.NZL),]

#Create order variable of country ratio_country2 values
df$order_var.NZL <- order(df$ratio_country2.NZL)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.NZL, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.NZL, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.NZL))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.NZL,
             var = var.NZL,
             female = female.NZL,
             male = male.NZL,
             ratio = ratio.NZL,
             ratio_label = ratio_label.NZL,
             ratio_label2 = ratio_label2.NZL,
             dim = dim.NZL,
             ratio_graph = ratio_graph.NZL, 
             truncated = truncated.NZL,
             order = order.NZL
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("NZL.png",  width = 17.7, height = 18, units = "cm")


# POL Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="POL")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.POL),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.POL <- df$ratio_graph.POL

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.POL <- ifelse(is.na(df$ratio_country2.POL), df$ratio_graph.OECD, df$ratio_country2.POL)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.POL),]

#Create order variable of country ratio_country2 values
df$order_var.POL <- order(df$ratio_country2.POL)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.POL, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.POL, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.POL))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.POL,
             var = var.POL,
             female = female.POL,
             male = male.POL,
             ratio = ratio.POL,
             ratio_label = ratio_label.POL,
             ratio_label2 = ratio_label2.POL,
             dim = dim.POL,
             ratio_graph = ratio_graph.POL, 
             truncated = truncated.POL,
             order = order.POL
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("POL.png",  width = 17.7, height = 18, units = "cm")


# PRT Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="PRT")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.PRT),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.PRT <- df$ratio_graph.PRT

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.PRT <- ifelse(is.na(df$ratio_country2.PRT), df$ratio_graph.OECD, df$ratio_country2.PRT)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.PRT),]

#Create order variable of country ratio_country2 values
df$order_var.PRT <- order(df$ratio_country2.PRT)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.PRT, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.PRT, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.PRT))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.PRT,
             var = var.PRT,
             female = female.PRT,
             male = male.PRT,
             ratio = ratio.PRT,
             ratio_label = ratio_label.PRT,
             ratio_label2 = ratio_label2.PRT,
             dim = dim.PRT,
             ratio_graph = ratio_graph.PRT, 
             truncated = truncated.PRT,
             order = order.PRT
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("PRT.png",  width = 17.7, height = 18, units = "cm")


# SVK Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="SVK")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.SVK),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.SVK <- df$ratio_graph.SVK

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.SVK <- ifelse(is.na(df$ratio_country2.SVK), df$ratio_graph.OECD, df$ratio_country2.SVK)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.SVK),]

#Create order variable of country ratio_country2 values
df$order_var.SVK <- order(df$ratio_country2.SVK)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.SVK, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.SVK, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.SVK))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.SVK,
             var = var.SVK,
             female = female.SVK,
             male = male.SVK,
             ratio = ratio.SVK,
             ratio_label = ratio_label.SVK,
             ratio_label2 = ratio_label2.SVK,
             dim = dim.SVK,
             ratio_graph = ratio_graph.SVK, 
             truncated = truncated.SVK,
             order = order.SVK
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("SVK.png",  width = 17.7, height = 18, units = "cm")


# SVN Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="SVN")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.SVN),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.SVN <- df$ratio_graph.SVN

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.SVN <- ifelse(is.na(df$ratio_country2.SVN), df$ratio_graph.OECD, df$ratio_country2.SVN)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.SVN),]

#Create order variable of country ratio_country2 values
df$order_var.SVN <- order(df$ratio_country2.SVN)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.SVN, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.SVN, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.SVN))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.SVN,
             var = var.SVN,
             female = female.SVN,
             male = male.SVN,
             ratio = ratio.SVN,
             ratio_label = ratio_label.SVN,
             ratio_label2 = ratio_label2.SVN,
             dim = dim.SVN,
             ratio_graph = ratio_graph.SVN, 
             truncated = truncated.SVN,
             order = order.SVN
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("SVN.png",  width = 17.7, height = 18, units = "cm")


# SWE Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="SWE")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.SWE),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.SWE <- df$ratio_graph.SWE

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.SWE <- ifelse(is.na(df$ratio_country2.SWE), df$ratio_graph.OECD, df$ratio_country2.SWE)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.SWE),]

#Create order variable of country ratio_country2 values
df$order_var.SWE <- order(df$ratio_country2.SWE)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.SWE, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.SWE, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.SWE))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.SWE,
             var = var.SWE,
             female = female.SWE,
             male = male.SWE,
             ratio = ratio.SWE,
             ratio_label = ratio_label.SWE,
             ratio_label2 = ratio_label2.SWE,
             dim = dim.SWE,
             ratio_graph = ratio_graph.SWE, 
             truncated = truncated.SWE,
             order = order.SWE
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("SWE.png",  width = 17.7, height = 18, units = "cm")


# TUR Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="TUR")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .5
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_label),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_label.TUR),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.TUR <- df$ratio_label.TUR

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.TUR <- ifelse(is.na(df$ratio_country2.TUR), df$ratio_label.OECD, df$ratio_country2.TUR)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.TUR),]

#Create order variable of country ratio_country2 values
df$order_var.TUR <- order(df$ratio_country2.TUR)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.TUR, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.TUR, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.TUR))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.TUR,
             var = var.TUR,
             female = female.TUR,
             male = male.TUR,
             ratio = ratio.TUR,
             ratio_label = ratio_label.TUR,
             ratio_label2 = ratio_label2.TUR,
             dim = dim.TUR,
             ratio_graph = ratio_graph.TUR, 
             truncated = truncated.TUR,
             order = order.TUR
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("TUR.png",  width = 17.7, height = 18, units = "cm")


# USA Sex

#Create data frame for country without OECD vars and keep same vars as OECD data frame
myvars2 <- c("country", "var_label", "var", "oecd", "female", "male", "ratio", "ratio_label")
df <- df.sex[myvars2] %>% 
  filter(country=="USA")

#Append country and OECD data frames
df <- bind_rows(df, df.sex.oecd)

#Remove OECD var
df <- subset(df, select = -c(oecd))

#Create new var of ratio values for labels with two decimal points
df$ratio_label2 <- df$ratio_label
df$ratio_label2 <- format(round(df$ratio_label2, digits = 2))

#Create flag for colours below or above ratio + grey bubbles
df$dim <- ifelse(df$ratio_label>1, "above", "below")
df$dim[which(df$ratio_label>=.965 & df$ratio_label<=1.034)] = "grey"

#Add variable with ratio values that fit within 0-2 so truncate values lower than .5 and higher than 1.5
df$ratio_graph <- df$ratio_label
df$ratio_graph[which(df$ratio_graph<=.5)] = .52
df$ratio_graph[which(df$ratio_graph>=1.5)] = 1.48

#Create dummy variable for truncation
df$truncated<- ifelse(df$ratio_label<=.5, 1, ifelse(df$ratio_label>=1.5, 1, 0))

#Create variable for country order, OECD always 2 (we want country bubbles to show above OECD)
df$order <- ifelse(df$country == "OECD", 2, 1) 

#Sort data by ratio_graph and then country order
df <- df[order(df$order, df$ratio_graph),]
#df <- df[order(df$ratio_graph, df$order),]

#Reshape data wide
df <- reshape(df, idvar = "var_label", timevar="country", direction = "wide")

#Order values by ratio_graph of country
df <- df[order(df$ratio_graph.USA),]

#Create ratio_country2 variable for country and OECD
df$ratio_country2.USA <- df$ratio_graph.USA

#Replace values of ratio_country2 of country with OECD values whenever country values are missing
df$ratio_country2.USA <- ifelse(is.na(df$ratio_country2.USA), df$ratio_graph.OECD, df$ratio_country2.USA)

#Order values by ratio_country2 of country
df <- df[order(df$ratio_country2.USA),]

#Create order variable of country ratio_country2 values
df$order_var.USA <- order(df$ratio_country2.USA)

#Create dummy for when OECD ratio  > or < country ratio 
df$oecd_spot <- ifelse(df$ratio_label.OECD > df$ratio_label.USA, "higher", ifelse(df$ratio_label.OECD < df$ratio_label.USA, "lower", "same"))

#Reshape values long
df <- reshape(df, idvar = "var_label", direction = "long", sep=".")

#Remove ratio_country2.country variable, will not need it anymore
df <- subset(df, select = -c(ratio_country2.USA))

#Rename vars to remove country name
df <- rename(df, 
             order_var = order_var.USA,
             var = var.USA,
             female = female.USA,
             male = male.USA,
             ratio = ratio.USA,
             ratio_label = ratio_label.USA,
             ratio_label2 = ratio_label2.USA,
             dim = dim.USA,
             ratio_graph = ratio_graph.USA, 
             truncated = truncated.USA,
             order = order.USA
)

#Function reorder var_labels by order and then order_var
df$var_label <- fct_reorder(df$var_label, -df$order)
df$var_label <- fct_reorder(df$var_label, -df$order_var)

#Drop vars 6_4E, 8_1E, 6_4NI, 8_1NI (England and Northern Ireland)
df = filter(df, var != "6_4E" & var != "8_1E" & var!="6_4NI" & var!="8_1NI")

#Create // var to add later on truncated labels
df$bars <- "//"

#Create a new ratio label variable from ratio_label2 but including "//"in label for truncated values
df$ratio_label3 <- as.character(df$ratio_label2)
df$ratio_label3 <- with(df, paste0(bars, ratio_label3))

#Create new truncated OECD dummy
df$truncated_oecd <- ifelse(df$country == "OECD" & df$truncated == 1, 1, 0)

#New variable for truncated values (depending on OECD spot)
df$ratio_graph2 <- df$ratio_graph
df$ratio_graph2 <- ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "lower", .5, ifelse(df$truncated_oecd == 1 & df$dim == "below" & df$oecd_spot == "higher", .54, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "lower", 1.46, ifelse(df$truncated_oecd ==1 & df$dim == "above" & df$oecd_spot == "higher", 1.5, df$ratio_graph))))

#Try to create a variable to define the group of colours
#We need one colour for country&below, country&grey, country$bubble and OECD (black)
df$colors <- ifelse(df$country != "OECD" & df$dim == "below", "blue", ifelse(df$country != "OECD" & df$dim == "grey", "grey", ifelse(df$country !="OECD" & df$dim == "above", "orange", "black")))   

#Lollipop
ggplot(df, aes(x=ratio_graph, y=var_label, group=colors, label=ifelse(df$truncated == 1 & df$country!="OECD", df$ratio_label3, ifelse(df$country=="OECD","", df$ratio_label2)))) +
  geom_errorbarh(aes(xmin = 1, xmax = ratio_graph, color=colors), 
                 size=ifelse(df$country == "OECD", 2, 3),
                 alpha=ifelse(df$country == "OECD", .3, .3), #alpha = 1 = no transparency
                 height = 0, 
                 position = position_dodge(width = .7)) +
  geom_point(aes(color=colors),
             shape = ifelse(df$country == "OECD" & df$truncated ==1, 21, 16), stroke = .7, fill = "white",
             size=ifelse(df$country == "OECD", 2, 7),
             alpha=ifelse(df$country == "OECD" & df$truncated == 1, 0, ifelse(df$country == "OECD" & df$truncated == 0, .9, 1)),
             position = position_dodge(width = .7)) +
  geom_text(color = 'white', size = 2.2, fontface = "bold", 
            position = position_dodge(width = .7)) +
  annotate("text", x = 0.55, y = 14, label = "Men doing better", color = rgb(56, 79, 104, maxColorValue = 255), size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 1.25, y = 7, label = "Women doing better", color = rgb(242, 86, 2, maxColorValue = 255), size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 13, yend = 15),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(56, 79, 104, maxColorValue = 255)) +
  geom_segment(aes(x = 1.25, xend = 1.25 , y = 8, yend = 6),
               arrow = arrow(length = unit(0.1,"cm")), color = rgb(242, 86, 2, maxColorValue = 255)) +
  scale_color_manual(values = c("black" = "black", "blue" = rgb(56, 79, 104, maxColorValue = 255), "grey" = "grey57", "orange" = rgb(242, 86, 2, maxColorValue = 255)), breaks = c("black", "grey"), labels = c("black" = "OECD average", "grey" = "No clear difference*"))+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("") + 
  labs(color = "")
ggsave("USA.png",  width = 17.7, height = 18, units = "cm")


