require(magrittr)
require(tidyverse)
require(plyr)
library(rgdal)
setwd("S:/_GenCD/HealthEquityShiny")

############################## MUNICIPALITY DATA ############################################################

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
                                          filter(Year > 2012)


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
    filter(Year != 2018) %>% #2018 data not complete yet, so leave out
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

#initialize table


add_social_data <- function(data_file, towns_col = "GEO.display.label", var_col, table = NULL, 
                            towns = CC_towns, col_label = var_col, var_type = "numeric"){
  # Adds social variables to compiled table or start a table
  #
  # Args:
  #   data_file: .csv file where new data is stored
  #   towns_col: column (name or index number) where town names are located in data_file
  #   var_col: column(s) where variables to be added are located in data_file  
  #   table: table to be appended, if NULL- new table will be created. Rownames should be town names.
  #   towns: towns/municipalities to be included in table (if table is not provided)
  #   var_type: "numeric", "factor", or "character"
  #
  # Returns:
  #  table with new variable(s) appended as new columns
  
  #read in data
  data = read.csv(data_file, stringsAsFactors = F, strip.white = T)
  data = data[-1,]
  
  #format town names
  data[,towns_col] = clean_towns_AFF(data[,towns_col]) 
  
  if(!(is.null(table))){
    towns = rownames(table)
  }

  data = data[(which(data[,towns_col] %in% towns)),] #subset data to towns in list

  rownames(data) = data[,towns_col] #make rownames town names
  data = data[,var_col, drop = FALSE] #subset to columns of interest
  colnames(data) = col_label #change column names to variable names
  
  #make sure variable is in correct format
  if(var_type == "numeric"){
    for(i in 1:ncol(data)){
      data[,i] = as.numeric(data[,i])
    }
  }
  else if (var_type == "factor"){
    for(i in 1:ncol(data)){
      data[,i] = as.factor(data[,i])
    }
  }
  else{
    for(i in 1:ncol(data)){
      data[,i] = as.character(data[,i])
    }
  }

  #if table is provided, merge new data
  if(!(is.null(table))){
    output = merge(table, data, by = "row.names", all.x = TRUE, all.y = FALSE)
    rownames(output) = output[,1]
    output = output[,-1, drop = FALSE]
  }
  
  #if table is not provided, make a new one
  else{
    temp = as.data.frame(matrix(nrow = length(towns), ncol = 1))
    rownames(temp) = towns
    output = merge(temp, data, by = "row.names", all.x = TRUE, all.y = FALSE)
    rownames(output) = output[,1]
    output = output[,-c(1,2), drop = FALSE]
    
  }

  return(output)
}


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

#Make social data table
social_data = add_social_data("data/ACS_16_5YR_DP03_economics_IL_places_data.csv", 
                              var_col = c("HC01_VC85", "HC03_VC161", "HC03_VC12"),
                              col_label = c("HouseholdIncome_Median", "Poverty", "Unemployment_Rate"))

social_data = add_social_data("data/ACS_16_5YR_S2701_health_insurance_IL_places_data.csv", var_col = "HC05_EST_VC01",
                              col_label = "Uninsured_Perc", table = social_data)
social_data = add_social_data("data/ACS_16_5YR_S1501_education_IL_places_data.csv", var_col = "HC02_EST_VC17",
                              col_label = "HighSchoolPlus_Perc", table = social_data)
social_data$NoHS25 = 100 - social_data$HighSchoolPlus_Perc
social_data = social_data %>% select(-HighSchoolPlus_Perc)
social_data = add_social_data("data/DEC_00_SF4_QTP14_foreign_born_IL_places_data.csv", var_col = "HC02_VC04",
                              col_label = "ForeignBorn_Perc", table = social_data)
social_data = add_social_data("data/ACS_16_5YR_DP05_population_IL_places_data.csv", 
                              var_col = c("HC03_VC49", "HC03_VC50", "HC03_VC56","HC03_VC51",  "HC03_VC45", "HC03_VC64", "HC03_VC88"),
                              col_label = c("White_perc", "Black_perc", "Asian_perc","Native_perc",  "multirace_perc", "hawaiian", "Hispanic_perc"),
                              table = social_data)
social_data$Native_perc = social_data$Native_perc + social_data$hawaiian
social_data = social_data %>% select(-hawaiian)


social_list = c("Median Household Income","Percent of Households Below Poverty Level", "Unemployment Rate", "Percent Uninsured",
                "Percent 25+ Without High School Degree", "Percent Foreign Born", "Pecent White", "Percent Black",
                "Percent Asian", "Percent Native American, Alaska Native, Hawaiian or other Pacific Islander Native",
                "Percent Two or More Races", "Percent Hispanic or Latino")

# social_descriptions = c("Median Household Income data for each municipality is taken from the 2016 American Community Survey",
#                         "Percent Below Poverty Level data for each municipality is taken from the 2016 American Community Survey",
#                         "Unemplyment Rate data for each municipality is taken from the 2016 American Community Survey",
#                         "Percent Uninsured data for each municipality is taken from the 2016 American Community Survey",
#                         "Percent 25+ Without Highschool Degree data for each municipality is taken from the 2016 American Community Survey,
#                         and represents the percent of the population 25 years of age or older who did not complete high school",
#                         "Percent Foreign Born data for each municipality is taken from the 2010 Census",
#                         rep("Race and ethnicity data for each municipality is taken from the 2016 American Community Survey", 6))

social_sources = c(rep("2012-2016 American Community Survey 5-Year Data", 5), 
                        "2010 United States Census",
                        rep("2012-2016 American Community Survey 5-Year Data", 6))


#Make table of population size and district for plotting
CC_districts = read.csv("data/districts.csv", stringsAsFactors = F, strip.white = T)
remove = which(CC_districts$Municipality %in% outside_jurisdiction)
CC_districts = CC_districts[-remove,]

#town_size_district = add_social_data("data/ACS_16_5YR_DP05_population_IL_places_data.csv", var_col = "HC01_VC03",
#                                     col_label = "Population") #2016 data but partial towns are wrong
town_size_district = add_social_data("data/pop2010_include_partial.csv", var_col = "Population2010",
                                       towns_col = "Municipality", col_label = "Population") #2010 census data
town_size_district = merge(town_size_district, CC_districts, by.x = "row.names", by.y = "Municipality")
rownames(town_size_district) = town_size_district$Row.names
town_size_district = town_size_district[,-1]
town_size_district$District = factor(town_size_district$District, levels = c("North", "West", "Southwest", "South"))




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




############################## UI FORMATTING ############################################################

disease_choices = 1:length(select_disease_crosstabs)
names(disease_choices) = names(select_disease_crosstabs)
disease_list = names(disease_choices)

social_choices = 1:ncol(social_data)
names(social_choices) = social_list

rm(keep_disease, select_disease_case_counts, all_disease_case_counts_long,
  outside_jurisdiction, remove, CC_districts, CC_towns_clean,
   add_social_data, clean_towns_AFF, clean_towns_INEDDS, transform_disease_crosstab)
save.image("equityApp/equityApp.RData")



