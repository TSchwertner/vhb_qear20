# ------------------------------------------------------------------------------
# VHB Course - Group 5
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Issue 2 - Solutions
# ------------------------------------------------------------------------------

### Data preparation ###

# Set working directory
# Enter your path in the first line and your windows user name in line 13 (replace your name) then the path will be set automatically based on the user
Paths = c("F:/empirical_accounting_vhb/vhb_qear20", "C:/Users/Simone/Documents/GitHub/vhb_qear20", "C:/Users/tschw/OneDrive/Dokumente/GitHub/vhb_qear20TS/vhb_qear20", "C://Olga/other/path")
names(Paths) = c("pstoczek", "Simone", "tschw", "Olga")
setwd(Paths[Sys.info()[7]])

list.files()


#Load required packages
library(tidyverse)
library(ggplot2)
library(summarytools)
library(sf)
library(tmap)

#Load insolvency dataset
insolvency_data <- read.csv("raw_data/insolvency_filings_de_julaug2020_incomplete.csv", 
                            header = TRUE, 
                            sep = ",", 
                            encoding = "UTF-8")



#Load map dataset with geographical data from Germany
#For downloading the map data, I went to https://gadm.org/download_country_v3.html, searched for "Germany", and saved sf-files in raw_data
#map_federal_states <- readRDS("raw_data/gadm36_DEU_1_sf.rds")
#map_districts <- readRDS("raw_data/gadm36_DEU_2_sf.rds")
map_municipality <- readRDS("raw_data/gadm36_DEU_3_sf.rds")

#combine insolvency dataset and map data on municipality level
insolvency_per_municipality <- left_join(insolvency_data, map_municipality, by = c("insolvency_court" = "NAME_3")) %>%
  select(insolvency_court, NAME_0, NAME_1, NAME_2)

#understand structural differences
insolvency_per_municipality$dummy <- ifelse(is.na(insolvency_per_municipality$NAME_0),1,0)
na_insolvency_per_municipality <- subset(insolvency_per_municipality, insolvency_per_municipality$dummy == 1)
table(na_insolvency_per_municipality$insolvency_court)# get those insolvency_courts which do not match

prepare_join <- function(map){
  map$NAME_3[map$NAME_3 == c("Bad Kreuznach(Verbandsgemeinde)","Bad Kreuznach(Verbandsfreie Gemeinde)")] <- "Bad Kreuznach"
  map$NAME_3[map$NAME_3 == c("Bad Homburg v.d. Höhe")] <- "Bad Homburg v.d.Höhe"
  map$NAME_3[map$NAME_3 == c("Berlin")] <- "Charlottenburg"
  map$NAME_3[map$NAME_3 == c("Esslingen am Neckar")] <- "Esslingen"
  map$NAME_3[map$NAME_3 == c("Frankfurt (Oder)")] <- "Frankfurt/Oder"
  map$NAME_3[map$NAME_3 == c("Freiburg im Breisgau")] <- "Freiburg"
  map$NAME_3[map$NAME_3 == c("Kempten (Allgäu)")] <- "Kempten"
  map$NAME_3[map$NAME_3 == c("Königstein im Taunus")] <- "Königstein/Ts."
  map$NAME_3[map$NAME_3 == c("Leer (Ostfriesland)")] <- "Leer"
  map$NAME_3[map$NAME_3 == c("Limburg a.d. Lahn")] <- "Limburg"
  map$NAME_3[map$NAME_3 == c("Ludwigshafen am Rhein")] <- "Ludwigshafen/Rhein"
  map$NAME_3[map$NAME_3 == c("Marburg")] <- "Marburg/Lahn"
  map$NAME_3[map$NAME_3 == c("Mühldorf a. Inn")] <- "Mühldorf"
  map$NAME_3[map$NAME_3 == c("Neustadt an der Weinstraße")] <- "Neustadt a. d. Wstr."
  map$NAME_3[map$NAME_3 == c("Weiden i.d. OPf.")] <- "Weiden"
  map$NAME_3[map$NAME_3 == c("Weilheim i. OB")] <- "Weilheim"
  map$NAME_3[map$NAME_3 == c("Südtondern")] <- "Niebüll"
  map$NAME_3[map$NAME_3 == c("Mitteldithmarschen")] <- "Meldorf"
  map$NAME_3[map$NAME_3 == c("Mittleres Schussental")] <- "Ravensburg"
  return(map)
}

map_municipality_clean <- prepare_join(map_municipality)

#Join insolvency data with map data to get additional geographical data
joined_insolvency_data <- left_join(insolvency_data, map_municipality_clean, by = c("insolvency_court" = "NAME_3")) %>%
  select(date, insolvency_court, court_file_number, subject, name_debtor, domicile_debtor, NAME_0, NAME_1, NAME_2) %>%
  rename(country = NAME_0,
         federal_state = NAME_1,
         district = NAME_2) %>%
  distinct()

View(joined_insolvency_data)
#Remark: by executing the left_join, we also remove duplicates
#See duplicated.R for further information on duplicate analysis

#Clean insolvency dataset
format_variables <- function(data){
  #transform date from chr to date format
  data$date <- as.Date(data$date)
  #transform insolvency_court to factor variable
  data$insolvency_court <- as.factor(data$insolvency_court)
  #transform subject to factor variable
  data$subject <- as.factor(data$subject)
  #transform country to factor variable
  data$country <- as.factor(data$country)
  #transform federal state to factor variable
  data$federal_state <- as.factor(data$federal_state)
  #transform district to factor variable
  data$district <- as.factor(data$district)
  #remove digits from domicile_debtor as it inconsistent
  data$domicile_debtor_clean <- as.factor(gsub('[0-9]+', '', data$domicile_debtor))
  #order columns by name
  col_order <- c("date", "country", "federal_state", "district", "insolvency_court", "court_file_number", "subject", "name_debtor", "domicile_debtor", "domicile_debtor_clean")
  data <- data[, col_order]
  #return dataframe
  return(data)
}

joined_insolvency_data_clean <- format_variables(joined_insolvency_data)
str(joined_insolvency_data_clean)

## Descriptive analysis ###

## Frequency tables ## 
#Table 1: Frequencies of insolvency filings per insolvency court
table_1 <- summarytools::freq(joined_insolvency_data_clean$insolvency_court, order = "freq", report.nas = FALSE)
print(table_1)

#Table 2: Frequencies of insolvency filings per federal state
table_2 <- summarytools::freq(joined_insolvency_data_clean$federal_state, order = "freq", report.nas = FALSE)
print(table_2)

#Table 3: Frequencies of insolvency filings per subject
table_3 <- summarytools::freq(joined_insolvency_data_clean$subject, order = "freq", report.nas = FALSE)
print(table_3)

#Table 4: Cross-tabulation for pairs of subject and federal states 
table_4 <- summarytools::ctable(x = joined_insolvency_data_clean$subject, y = joined_insolvency_data_clean$federal_state, prop = "r")
print(table_4)

## Basic Bar Charts ##
#Bar chart 1: shows the insolvency filings per subject
bar_chart_1 <- ggplot(data = joined_insolvency_data_clean) + 
  geom_bar(mapping = aes(x = subject, fill = subject))+
  theme(axis.text.x = element_blank())
print(bar_chart_1)

#Bar chart 2:shows the insolvency filings by subject
bar_chart_2  <- joined_insolvency_data_clean %>% 
  group_by(subject) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  ggplot(aes(x = n, y = reorder(subject, n))) + 
  geom_col() + 
  labs(title = "Insolvency filings by subject", x = "Number", y = "Subject")+
  theme()
print(bar_chart_2)

#Bar chart 3: shows absolute number of cases by court
bar_chart_3 <- ggplot(data = joined_insolvency_data_clean, aes(x = insolvency_court, fill = subject)) +
  geom_bar(col = 289)+
  labs(title = "Cases and status of each court", x = "Courts" , y = "Number", fill = "Status") +
  theme(axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "black"))
print(bar_chart_3)

#Bar chart 4: shows development of insolvency subjects over time (TS) 
Jul.1 <- filter(select(joined_insolvency_data_clean, date:subject), date >= "2020-07-01" & date < "2020-07-15")
Jul.1$Month <- "(A) July, First Half " 
Jul.2 <- filter(select(joined_insolvency_data_clean, date:subject), date >= "2020-07-15" & date < "2020-08-01")
Jul.2$Month <- "(B) July, Second Half" 
Aug.1 <- filter(select(joined_insolvency_data_clean, date:subject), date >= "2020-08-01" & date < "2020-08-15")
Aug.1$Month <- "(C) August, First Half" 
Aug.2 <- filter(select(joined_insolvency_data_clean, date:subject), date >= "2020-08-15" & date < "2020-09-01")
Aug.2$Month <- "(D) August, Second Half" 
insolvency_datam <- rbind(Jul.1,Jul.2,Aug.1,Aug.2)

insolvency_datam %>%
  ggplot(aes(x = subject, fill = subject)) + 
  geom_bar(col = 289) +
  facet_grid( ~ Month) +
  labs(title = "Status by Period", x = "Status" , y = "Number", fill = "Status") +
  theme(axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, face ="bold", colour = "black"))

#Bar chart 5: shows the relative frequencies of insolvency subjects by state
joined_insolvency_data_clean %>%
  group_by(federal_state, subject)%>%
  tally() %>%
  ungroup() %>%
  ggplot(aes(x = n, y = federal_state, fill = subject))+
  geom_bar(position="fill", stat="identity")+
  xlab("Relative share of insolvency subjects")+
  ylab("")+
  labs(title="Relative Share of insolvency subjects by state.")+
  theme(legend.title = element_blank())

## Interactive Map ##
# Map: shows the insolvency filing for each insolvency_court
# possibility to filter for specific subjects
count_insolvency <- joined_insolvency_data_clean  %>%
  #filter(subject == "Eröffnungen")%>%
  group_by(insolvency_court)%>%
  count()

#join datasets
map <- left_join(count_insolvency, map_municipality_clean, by = c("insolvency_court" = "NAME_3" )) %>%
  select(insolvency_court, n, geometry)

#map <- left_join(map_municipality_clean, count_insolvency, by = c("NAME_3" = "insolvency_court")) %>%
#  select(NAME_3, n, geometry)

#transform map
object <- st_as_sf(map)

#create interactiv map
tmap_mode("view")
tm_basemap("OpenStreetMap.DE")+
  tm_shape(object) +
  tm_bubbles("n", col = "red")

#Converge the datasets 
#Data withdrawn from my depository 
orbis_data <-  read.csv("raw_data/orbis_wrds_de.csv",
                        header =TRUE,
                        sep=",",
                        encoding = "UTF-8")

orbis_data <- as.tibble(orbis_data)

View(insolvency_data)
View(orbis_data)

# Basic Preparation and Data Cleaning

insolvency_data_ER <- insolvency_data %>%
                      filter(subject=="Eröffnungen")%>%
                      distinct()

# Test whether closing date in 2020 occured prior to filing date

test <- orbis_data %>%
         filter(closdate >="2020-01-01" & closdate < "2020-07-01" )
dim(test)
# So, there are no filings within these dates

# Get newest Orbis data from 2019 and 2018

orbis_data_cur <- orbis_data %>%
               filter(year == "2019" | year =="2018")
              
# Join attempt with inner_join

first_match <- inner_join(insolvency_data_ER, orbis_data_cur, by=c("name_debtor"="name_native"))%>%
                select(everything())

second_match <- inner_join(insolvency_data_ER,orbis_data, by=c("name_debtor"="name_internat"))

# Not able to select highest year in the group --> help(TS)
join_new_1 <- second_match %>%
                    group_by("name_native") %>%
                    filter(year==max(year))

# Alternatively filtered by year:(to old data are not valuable)

join_new_2 <- second_match %>%
              group_by("name_native") %>%
              filter(year == "2019" | year =="2018"| year =="2017"| year =="2016")
