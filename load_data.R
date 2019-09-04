require(magrittr)
require(tidyverse)
require(plyr)
library(rgdal)
library(rgeos)
library(tidycensus)
setwd("S:/Enhanced Surveillance/Shiny/HealthEquityShiny")

############################## MUNICIPALITY DATA ############################################################

year = 2018

#All towns/cities/places in Cook County
CC_towns = read.table("data/municipalities.txt", sep = "\n", strip.white = T, stringsAsFactors = F)[,1]
outside_jurisdiction = c("Burbank", "Evanston", "Forest View", "Oak Park", "Skokie", "Stickney")
remove = which(CC_towns %in% outside_jurisdiction)
CC_towns = CC_towns[-remove]
#CC_towns = c(CC_towns, c("Elmhurst (pt.)", "La Grange Highlands", "Forest Heights", "Hines", 
#  "University Park (pt.)", "Bensenville (pt.)"))
CC_towns = sort(CC_towns)

CC_towns_vector = 0:119
names(CC_towns_vector) = c("Select Municipality",CC_towns)
names(CC_towns_vector)[4] <- "Barrington (pt.)"



############################## DISEASE DATA ############################################################
#Read in table of all cases by disease, town, and year
all_disease_case_counts_long = read.table("data/all_cases_by_city_and_year.txt", sep = "\t", 
                                          strip.white = T, header = T) %>% set_colnames(c("Disease", "Count", "City", "Year")) %>%
                                          filter(Year > (year-5))

#Chicago;Burbank;Evanston;Forest View;Oak Park;Skokie;Stickney;University Park
#diseases to include
disease_list = c("Haemophilus Influenzae Invasive Disease",
        "Hepatitis B Chronic",
        "Mumps",
        "Pertussis",
        "Varicella (Chickenpox)",
        "Campylobacteriosis",
        "Cryptosporidiosis",
        "Shiga toxin-producing E. coli",
        "Hepatitis C Chronic",
        "Histoplasmosis",
        "Legionnaires Disease",
        "Listeria",
        "Lyme Disease",
        "Malaria",
        "Salmonella",
        "Shigella",
        "Streptococcal Disease Invasive Group A",
        "Tuberculosis",
        "West Nile Virus",
        "Chlamydia",
        "Gonorrhea")

edit_disease_names <- function(names){
  #combine neuroinvasive and non-neuroinvasive west nile codes
  names = gsub("West Nile Virus.*$", "West Nile Virus", names)
  
  #combine all shiga toxin-producing E. coli codes
  names = gsub("Shiga toxin-producing E. coli.*$", "Shiga toxin-producing E. coli", names)
  
  #Put in more common names
  names = gsub("Hepatitis C Virus Chronic Infection", "Hepatitis C Chronic", names)
  names = gsub("Legionellosis - Legionnaires Disease", "Legionnaires Disease", names)
  names = gsub("Salmonellosis", "Salmonella", names)
  names = gsub("Shigellosis", "Shigella", names)
  names = gsub("TB Disease", "Tuberculosis", names)
  names = gsub("Listeria Invasive Disease", "Listeria", names)
}


select_disease_case_counts_long = all_disease_case_counts_long %>% mutate(Disease = edit_disease_names(Disease)) %>%
  filter(Disease %in% disease_list)
select_disease_case_counts_long = ddply(select_disease_case_counts_long, .(Disease, City, Year), summarize, Cases = (sum(Count)))

#Split into a separate table for each disease
select_disease_case_counts = split(select_disease_case_counts_long, select_disease_case_counts_long$Disease)

#Get rid of diseases that occurred in 5 or fewer municipalities
keep_disease = sapply(select_disease_case_counts, function(x){
  if(nrow(x) > 5){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}) 
keep_disease = which(keep_disease == TRUE)
select_disease_case_counts = select_disease_case_counts[keep_disease]


transform_disease_crosstab = function(crosstab, towns = CC_towns){
  # Creates formatted crosstab table of city and year for given disease
  #
  # Args:
  #   crosstab: non formatted crosstab with years as columns and towns as rows
  #   towns: character vector of all towns to be included in final table
  #
  # Returns:
  #   a data frame properly formatted to be used in shiny app
  
  d = crosstab
  d[is.na(d)] <- 0 #change NAs to 0s for case counts
  d[,1] = clean_towns_INEDDS(d[,1]) #format town names
  shell = as.data.frame(matrix(ncol = (ncol(d)-1), nrow = length(towns))) #create table shell
  colnames(shell) = colnames(d)[2:length(colnames(d))] #with years as column names
  rownames(shell) = towns #and towns as row names
  for(i in (1:nrow(shell))){ #for each row in table shell, fill in case count data
    town = rownames(shell)[i] 
    rows = which(d[,1] == town) #rows in data that match shell town
    if(length(rows) > 0){ #if there is one or more match in the data for the shell town
      town_rows = d[rows,]
      town_rows = town_rows[,-1]
      sum = colSums(town_rows)
      shell[i,] = sum #fill in the case count sum from data to shell
    }
    else{ #if there is no match in data for shell town
      shell[i,] = rep(0, ncol(shell)) #fill in zeros for case counts
    }
  }
  return(shell) #return final table
}


clean_towns_INEDDS <- function(towns){
  # Cleans town names from I-NEDDS to match final data
  #
  # Args:
  #   towns: character vector of town names to be cleaned
  #
  # Returns:
  #  character vector of cleaned names
  
  #correct common shorthands
  towns = gsub("Hts", "Heights", towns)
  towns = gsub("Vlg", "Village", towns)
  towns = gsub("Pk", "Park", towns)
  towns = gsub("Mt", "Mount", towns)
  towns = gsub("Spgs", "Springs", towns)
  
  #correct observed typos
  towns = gsub("Bridge View", "Bridgeview", towns)
  towns = gsub("Mc Cook", "Mccook", towns)
  towns = gsub("McCook", "Mccook", towns)
  towns = gsub("Matttenson", "Matteson", towns)
  towns = gsub("Argo", "Summit", towns)
  towns = gsub("Summit Argo", "Summit", towns)
  towns = gsub("Summit Summit", "Summit", towns)
  towns = gsub("Chicgo", "Chicago", towns)
  towns = gsub("Desplaines", "Des Plaines", towns)
  towns = gsub("Markjam", "Markham", towns)
  towns = gsub("Matheson", "Matteson", towns)
  towns = gsub("Barrinton", "Barrington", towns)
  
  #add (pt.) to town names that are only partially in Cook County
  towns = gsub("Barrington Hills", "Barrington Hills (pt.)", towns)
  towns = gsub("Bartlett","Bartlett (pt.)", towns)
  towns = gsub("Buffalo Grove","Buffalo Grove (pt.)", towns)
  towns = gsub("Burr Ridge","Burr Ridge (pt.)", towns)
  towns = gsub("Elgin","Elgin (pt.)", towns)
  towns = gsub("Hanover Park","Hanover Park (pt.)", towns)
  towns = gsub("Hinsdale","Hinsdale (pt.)", towns)
  towns = gsub("Hodgkins","Hodgkins (pt.)", towns)
  towns = gsub("Lemont","Lemont (pt.)", towns)
  towns = gsub("Orland Park","Orland Park (pt.)", towns)
  towns = gsub("Park Forest","Park Forest (pt.)", towns)
  towns = gsub("Roselle","Roselle (pt.)", towns)
  towns = gsub("Steger","Steger (pt.)", towns)
  towns = gsub("Tinley Park","Tinley Park (pt.)", towns)
  towns = gsub("Elmhurst","Elmhurst (pt.)", towns)
  towns = gsub("Bensenville","Bensenville (pt.)", towns)
  towns = gsub("University Park","University Park (pt.)", towns)
  
  return(towns)
}



#make formatted crosstabs for each disease with years as columns and towns as rows
select_disease_crosstabs = lapply(select_disease_case_counts, function(x){
  xtab = x %>%
    select(City,Year,Cases) %>%
    filter(Year <= year) %>% 
    spread(Year, Cases) %>% #get count per year per city
    mutate(City = trimws(City)) %>% #trim whitespace from city names
    transform_disease_crosstab() #clean
  
  #If less than 5 cases a year, drop that year from crosstab (may have been user error)
  rm_yrs = which(colSums(xtab) <= 5)
  if(length(rm_yrs) >0){xtab = xtab[,-rm_yrs]}
  
  return(xtab)
})

select_disease_crosstabs$Tuberculosis$`2015`[66] <- 0



######
# finding typos
# inedds_towns = read.table("data/all_cities_inedds.txt", sep = "\n", strip.white = T, stringsAsFactors = F)[,1]
# inedds_towns = clean_towns_INEDDS(inedds_towns)
# inedds_towns[which(!(inedds_towns %in% CC_towns))]

# finding partial towns
# pt_towns = towns[grep("(pt.)", towns)]
# pt_towns = gsub(" \\(pt.\\)", "", pt_towns)
# sapply(pt_towns, function(x){
#   (paste('cities = gsub("', x, '","', x, ' (pt.)", cities)', sep = ""))
# })


############################## SOCIAL INDEX DATA ############################################################


clean_towns_AFF <- function(towns){
  # Cleans town names from American Fact Finder to match final data
  #
  # Args:
  #   towns: character vector of town names to be cleaned
  #
  # Returns:
  #  character vector of cleaned names
  
  towns = gsub(" city, Illinois", "", towns)
  towns = gsub(" village, Illinois", "", towns)
  towns = gsub(" CDP, Illinois", "", towns)
  towns = gsub(" town, Illinois", "", towns)
  
  towns = gsub("McCook", "Mccook", towns)
  
  #add (pt.) to town names that are only partially in Cook County
  towns = gsub("Barrington Hills", "Barrington Hills (pt.)", towns)
  towns = gsub("Bartlett","Bartlett (pt.)", towns)
  towns = gsub("Buffalo Grove","Buffalo Grove (pt.)", towns)
  towns = gsub("Burr Ridge","Burr Ridge (pt.)", towns)
  towns = gsub("Elgin","Elgin (pt.)", towns)
  towns = gsub("Hanover Park","Hanover Park (pt.)", towns)
  towns = gsub("Hinsdale","Hinsdale (pt.)", towns)
  towns = gsub("Hodgkins","Hodgkins (pt.)", towns)
  towns = gsub("Lemont","Lemont (pt.)", towns)
  towns = gsub("Orland Park","Orland Park (pt.)", towns)
  towns = gsub("Park Forest","Park Forest (pt.)", towns)
  towns = gsub("Roselle","Roselle (pt.)", towns)
  towns = gsub("Steger","Steger (pt.)", towns)
  towns = gsub("Tinley Park","Tinley Park (pt.)", towns)
  towns = gsub("Elmhurst","Elmhurst (pt.)", towns)
  towns = gsub("Bensenville","Bensenville (pt.)", towns)
  towns = gsub("University Park","University Park (pt.)", towns)
  
  return(towns)
}


#make social data table with tidycensus
social_data = get_acs(geography = "place", state = "IL",  
                         variables = c("DP03_0062","DP03_0119P","DP03_0009PE", "DP03_0099PE", "DP02_0066PE",
                                       "DP02_0092PE", "DP05_0077PE", "DP05_0078PE",
                                       "DP05_0080PE", "DP05_0081PE", "DP05_0079PE",
                                       "DP05_0083PE", "DP05_0071PE"),
                         output = "wide", geometry = F) %>%
  select(ends_with("E")) %>%
  dplyr::rename("HouseholdIncome_Median" = DP03_0062E, "Poverty" = DP03_0119PE,
                "Unemployment_Rate" = DP03_0009PE, "Uninsured_Perc" = DP03_0099PE, "NoHS25" = DP02_0066PE,
                "ForeignBorn_Perc" = DP02_0092PE, "White_perc" = DP05_0077PE,
                "Black_perc" = DP05_0078PE, "Asian_perc" = DP05_0080PE,
                "NHOPI" = DP05_0081PE, "AIAN" = DP05_0079PE,
                "multirace_perc" = DP05_0083PE, "Hispanic_perc" = DP05_0071PE) %>%
  mutate(NoHS25 = 100-NoHS25) %>%
  mutate(Native_perc = NHOPI+AIAN) %>%
  select(-c(NHOPI, AIAN)) %>%
  mutate(NAME = clean_towns_AFF(NAME)) %>%
  filter(NAME %in% CC_towns) %>%
  select(NAME, HouseholdIncome_Median, Poverty, Unemployment_Rate, 
         Uninsured_Perc, NoHS25, ForeignBorn_Perc, White_perc, 
         Black_perc, Asian_perc, Native_perc, multirace_perc, Hispanic_perc) %>%
  arrange(NAME) %>%
  as.data.frame() %>%
  set_rownames(.$NAME) %>%
  select(-NAME)




social_list = c("Median Household Income","Percent of Households Below Poverty Level", "Unemployment Rate", "Percent Uninsured",
                "Percent 25+ Without High School Degree", "Percent Foreign Born", "Percent Non-Hispanic White", "Percent Non-Hispanic Black",
                "Percent Non-Hispanic Asian", "Percent Native American, Alaska Native, Hawaiian or other Pacific Islander Native",
                "Percent Two or More Races", "Percent Hispanic or Latinx")


social_sources = c(rep(paste0(year-5, "-", year-1, " American Community Survey 5-Year Data"), 12))


#Make table of population size and district for plotting
CC_districts = read.csv("data/districts.csv", stringsAsFactors = F, strip.white = T) %>%
  filter(Municipality %in% CC_towns)


town_size_district = read.csv("data/pop2010_include_partial.csv") %>%
  mutate(Municipality = clean_towns_AFF(Municipality)) %>%
  filter(Municipality %in% CC_towns) %>%
  left_join(CC_districts, by = "Municipality") %>%
  dplyr::rename("Population" = Population2010) %>%
  arrange(Municipality) %>%
  set_rownames(.$Municipality) %>%
  select(-Municipality) %>%
  mutate(District = factor(District, levels = c("North", "West", "Southwest", "South")))
  



############################## MAP DATA ############################################################

cc <- readOGR(dsn = "./data/shape_files", layer = "towns_subcook") %>% spTransform(CRS("+init=epsg:4326"))

cc <- cc[-(which(cc$DIST == "O")),]
cc$DIST = factor(cc$DIST, c("N", "W", "SW", "S"))
CC_towns_clean = gsub(" \\(.*$", "", CC_towns)
cc$CITY = gsub("McCook", "Mccook", cc$CITY)

cc <- cc[cc$CITY %in% CC_towns_clean,]
cc$CITY = clean_towns_INEDDS(cc$CITY)
cc <- cc[order(cc$CITY),]
cc$CITY[3] <- "Barrington (pt.)"

#include OOJ for district map
cc2 <- readOGR(dsn = "./data/shape_files", layer = "towns_subcook") %>% spTransform(CRS("+init=epsg:4326"))


cc2$DIST = factor(cc2$DIST, c("N", "W", "SW", "S", "O"))
CC_towns_clean = gsub(" \\(.*$", "", CC_towns)
cc2$CITY = gsub("McCook", "Mccook", cc2$CITY)

cc2$CITY = clean_towns_INEDDS(cc2$CITY)
cc2 <- cc2[order(cc2$CITY),]
cc2$CITY[3] <- "Barrington (pt.)"

# muni <- readOGR(dsn = "./data/shape_files", layer = "CCmunicipality2014") %>% spTransform(CRS("+init=epsg:4326"))
# muni <- muni[-(which(muni$municipali == "Chicago")),]
# chicago <- readOGR(dsn = "./data/shape_files", layer = "CCmunicipality2014") %>% spTransform(CRS("+init=epsg:4326")) 
# chicago <- chicago[which(chicago$municipali == "Chicago"),]


all_cook <- readOGR(dsn = "./data/shape_files", layer = "CCmunicipality2014") %>% spTransform(CRS("+init=epsg:4326")) 
chicago <- all_cook[which(all_cook$municipali == "Chicago"),]
muni <- all_cook[-(which(all_cook$municipali == "Chicago")),]
#simplify and merge all SCC, so takes less time to plot
muni <- gSimplify(muni, tol = 0.00001)
muni <- gUnionCascaded(muni)

############################## UI FORMATTING ############################################################

disease_choices = 1:length(select_disease_crosstabs)
names(disease_choices) = names(select_disease_crosstabs)
disease_list = names(disease_choices)

social_choices = 1:ncol(social_data)
names(social_choices) = social_list


#disease links
disease_link = c(
  "Campylobacteriosis" =
    "https://www.cdc.gov/campylobacter/index.html",
  "Chlamydia" =
    "https://www.cdc.gov/std/chlamydia/",
  "Cryptosporidiosis" =
    "https://www.cdc.gov/parasites/crypto/index.html",
  "Gonorrhea" =
    "https://www.cdc.gov/std/gonorrhea/default.htm",
  "Haemophilus Influenzae Invasive Disease" =
    "https://www.cdc.gov/hi-disease/index.html",
  "Hepatitis B Chronic" =
    "https://www.cdc.gov/hepatitis/hbv/index.htm",
  "Hepatitis C Chronic" =
    "https://www.cdc.gov/hepatitis/hcv/index.htm",
  "Histoplasmosis" =
    "https://www.cdc.gov/fungal/diseases/histoplasmosis/index.html",
  "Legionnaires Disease" =
    "https://www.cdc.gov/legionella/index.html",
  "Listeria" =
    "https://www.cdc.gov/listeria/index.html",
  "Lyme Disease" =
    "https://www.cdc.gov/lyme/index.html",
  "Malaria" =
    "https://www.cdc.gov/parasites/malaria/index.html",
  "Mumps" =
    "https://www.cdc.gov/mumps/index.html",
  "Pertussis"  =
    "https://www.cdc.gov/pertussis/index.html",
  "Salmonella" =
    "https://www.cdc.gov/salmonella/index.html",
  "Shiga toxin-producing E. coli" = 
    "https://www.cdc.gov/features/ecoliinfection/index.html",
  "Shigella" =
    "https://www.cdc.gov/shigella/index.html",
  "Streptococcal Disease Invasive Group A" =
    "https://www.cdc.gov/groupastrep/diseases-public/index.html",
  "Tuberculosis" =
    "https://www.cdc.gov/tb/topic/basics/default.htm",
  "Varicella (Chickenpox)" =
    "https://www.cdc.gov/chickenpox/about/index.html",
  "West Nile Virus" =
    "https://www.cdc.gov/westnile/index.html"
  
  
)



save(cc, cc2, select_disease_case_counts_long, select_disease_crosstabs, social_data, town_size_district,
     CC_towns, CC_towns_clean, CC_towns_vector, disease_choices, disease_link, disease_list, year,
     social_choices, social_list, social_sources, edit_disease_names, clean_towns_INEDDS, muni, chicago, all_cook,
     file = "equityApp/equityApp.RData")


