library(readxl)
library(tidyr)
library(dplyr)
library(gdata)
library(rjson)
library(jsonlite)

sub_folders <- list.files()
data_location <- grep("data$", sub_folders, value=T)
path_to_data <- (paste0(getwd(), "/", data_location))
all_xls <- dir(path_to_data, pattern = ".xls")

#datasets brought in based on cpes tag
##API call: http http://data.ctdata.org/api/3/action/package_search?fq=tags:cpes >> list.txt
#edit list.txt, give is a carriage return at end of first line

#creates list.json
list_json_file <- "list.txt"
json_data <- fromJSON(paste(readLines(list_json_file), collapse=""))
datasets=json_data[[3]][4] #results
data_list <- datasets$results
#includes dataset source and other attributes needed to dataset table
extras <- data_list %>% select(extras)

#includes dataset name and description needed to dataset table
notes <- data_list %>% select(notes, name, title)

#function to unnest dataframes
unnest_dataframes <- function(x) {
        y <- do.call(data.frame, x)
        if("data.frame" %in% sapply(y, class)) unnest_dataframes(y)
        y
}

#get totoal number of datasets
total_datasets <- nrow(extras)

#all columns that aren't dimensions
non_dim <- c("Default", "Description", "Disabled Views", "Domain", "Frequency", "Full Description", "Geography", 
             "Latest Year Available", "Measure Type", "Socrata", "Socrata Dataset URL", "Source", "Subdomain", "Suppression",          
             "Technical Notes", "Units", "Variable", "Years in Catalog", "Denominator", "Numerator", "Socrata Dataset Name", "Dimensions")

#combine datasets into dataset_table
dataset_table <- data.frame(stringsAsFactors = FALSE)
for (i in 1:total_datasets ) {
  list <- extras[[1]][i]
  df <- unnest_dataframes(list)
  df_wide <- spread(df, key, value)
  #get dimensions
  dimensions <- colnames(df_wide)[!colnames(df_wide) %in% non_dim] 
  dimensions <- paste(dimensions, collapse = ", ")
  fileCols <- names(df_wide)
  #Select year col
  year_col <- fileCols[grepl("Year", fileCols, ignore.case = T) & !grepl("Available", fileCols, ignore.case = T)]
  df_wide <- df_wide %>% select(Source, Description, year_col, `Measure Type`, Geography) 
  #fix geog
  df_wide$Geography <- gsub(";", ", ", df_wide$Geography)
  #fix years available (rename column)
  colnames(df_wide)[grepl('year',colnames(df_wide), ignore.case = T)] <- "Years in Catalog"
  df_wide$`Years in Catalog` <- gsub(";", ", ", df_wide$`Years in Catalog`)
  #fix measures
  df_wide$`Measure Type` <- gsub(";", ", ", df_wide$`Measure Type`)
  #assign dimensions
  df_wide$Dimensions <- dimensions
  dataset_table <- rbind(dataset_table, df_wide)
}

#Merge in data set name by description
dataset_table <- merge(dataset_table, notes, by.x = "Description", by.y = "notes", all=T)

dataset_table <- dataset_table %>% 
  select(-Description)

#Merge in master terms file
dataset_terms <- read.csv(paste0(path_to_data, "/", "dataset_terms.csv"), stringsAsFactors = F, header=T, check.names=F)
dataset_table <- merge(dataset_table, dataset_terms, by.x = "title", by.y = "Data Set", all=T)

indicator_list <- read.xls(paste0(path_to_data, "/", "Updated_IndicatorList_03.01.18_update.xlsx"), sheet = 3)

indicator_list <- data.frame(lapply(indicator_list, as.character), stringsAsFactors=FALSE)

#Recode columns
##Priority Problem
indicator_list$Alcohol[indicator_list$Alcohol == "1"] <- "Alcohol"
indicator_list$Tobacco[indicator_list$Tobacco == "1"] <- "Tobacco"
indicator_list$Marijuana[indicator_list$Marijuana == "1"] <- "Marijuana"
indicator_list$Prescription.Drugs[indicator_list$Prescription.Drugs == "1"] <- "Prescription Drugs"
indicator_list$Heroin[indicator_list$Heroin == "1"] <- "Heroin"
indicator_list$Cocaine[indicator_list$Cocaine == "1"] <- "Cocaine"
indicator_list$Other.Illicit.Drugs[indicator_list$Other.Illicit.Drugs == "1"] <- "Other Illicit Drugs"
indicator_list$Suicide[indicator_list$Suicide == "1"] <- "Suicide"
indicator_list$Problem.Gambling[indicator_list$Problem.Gambling == "1"] <- "Problem Gambling"
indicator_list$Mental.Health[indicator_list$Mental.Health == "1"] <- "Mental Health"
##Age Group
indicator_list$Ages.12.17[indicator_list$Ages.12.17 == "1"] <- "Ages 12-17"
indicator_list$Ages.18.25[indicator_list$Ages.18.25 == "1"] <- "Ages 18-25"
indicator_list$Ages.26.[indicator_list$Ages.26. == "1"] <- "Ages 26+"
##Dimensions
indicator_list$Gender[indicator_list$Gender == "1"] <- "Gender"
indicator_list$Race.Ethnicity[indicator_list$Race.Ethnicity == "1"] <- "Race/Ethnicity"
indicator_list$Age <- NA
indicator_list$Age[!is.na(indicator_list$Ages.12.17)] <- "Age"
indicator_list$Age[!is.na(indicator_list$Ages.18.25)] <- "Age"
indicator_list$Age[!is.na(indicator_list$Ages.26.)] <- "Age"
indicator_list$Age[grepl("\\+", indicator_list$Other)] <- "Age"
indicator_list$Age[grepl("older", indicator_list$Other)] <- "Age"
indicator_list$Grade <- NA
indicator_list$Grade[grepl("Grade", indicator_list$Other)] <- "Grade"
indicator_list$Other2 <- NA
indicator_list$Other2[grepl("income, insurance status, disability, education", indicator_list$Other)] <- "Income, Insurance status, Disability, Education"
indicator_list$Other2[grepl("Other demographics", indicator_list$Other)] <- "Other demographics"
indicator_list$Other2[grepl("Education, Income, Community Type", indicator_list$Other)] <- "Education, Income, Community Type"
indicator_list$Other <- gsub(", income, insurance status, disability, education", "", indicator_list$Other)
indicator_list$Other <- gsub("Other demographics", "", indicator_list$Other)
indicator_list$Other <- gsub(", Education, Income, Community Type", "", indicator_list$Other)

# combine Priority Problem columns
cols <- c("Alcohol", "Tobacco", "Marijuana", "Prescription.Drugs", 
          "Heroin", "Cocaine", "Other.Illicit.Drugs", "Suicide", 
          "Problem.Gambling", "Mental.Health")
indicator_list$`Priority Problem` <- apply( indicator_list[ , cols ] , 1 , paste , collapse = ", " )
indicator_list <- indicator_list[ , !( names( indicator_list ) %in% cols ) ]
indicator_list$`Priority Problem` <- gsub(", NA", "", indicator_list$`Priority Problem`)
indicator_list$`Priority Problem` <- gsub("NA, ", "", indicator_list$`Priority Problem`)
indicator_list$`Priority Problem` <- gsub("NA", NA, indicator_list$`Priority Problem`)

# combine Age Group columns
cols <- c("Ages.12.17", "Ages.18.25", "Ages.26.", "Other")
indicator_list$`Age/Grade Available` <- apply( indicator_list[ , cols ] , 1 , paste , collapse = ", " )
indicator_list <- indicator_list[ , !( names( indicator_list ) %in% cols ) ]
indicator_list$`Age/Grade Available` <- gsub(", NA", "", indicator_list$`Age/Grade Available`)
indicator_list$`Age/Grade Available` <- gsub("NA, ", "", indicator_list$`Age/Grade Available`)
indicator_list$`Age/Grade Available` <- gsub("NA", NA, indicator_list$`Age/Grade Available`)

# combine Dimension columns
cols <- c("Gender", "Race.Ethnicity", "Age", "Grade", "Other2")
indicator_list$Dimensions <- apply( indicator_list[ , cols ] , 1 , paste , collapse = ", " )
indicator_list <- indicator_list[ , !( names( indicator_list ) %in% cols ) ]
indicator_list$Dimensions <- gsub(", NA", "", indicator_list$Dimensions)
indicator_list$Dimensions <- gsub("NA, ", "", indicator_list$Dimensions)
indicator_list$Dimensions <- gsub("NA", NA, indicator_list$Dimensions)

# combine Keywords columns
cols <- c('Key.Word.1', 'Key.Word.2', 'Key.Word.3')
indicator_list$Keywords <- apply( indicator_list[ , cols ] , 1 , paste , collapse = ", " )
indicator_list <- indicator_list[ , !( names( indicator_list ) %in% cols ) ]
indicator_list$Keywords <- gsub(", NA", "", indicator_list$Keywords)
indicator_list$Keywords <- gsub("NA", NA, indicator_list$Keywords)
indicator_list$Keywords <- gsub(", $", "", indicator_list$Keywords)
indicator_list$Keywords <- gsub(",$", "", indicator_list$Keywords)
indicator_list$Keywords <- gsub("\\?", "", indicator_list$Keywords)
indicator_list$Keywords <- gsub("^, ", NA, indicator_list$Keywords)
indicator_list$Keywords <- gsub(", $", "", indicator_list$Keywords)

#Clean up a bit
clean <- indicator_list %>% 
  select(Indicator, Source, Smallest.Geo.Area, Most.Recent.Year..s..Available, Dimensions, `Age/Grade Available`, `Priority Problem`, Keywords, Form2, Link) %>% 
  rename(`Data Set` = Source, `Geography Level` = Smallest.Geo.Area, `Year(s) Available` = Most.Recent.Year..s..Available, Form = Form2)

clean$Link[clean$Link == ""] <- NA

clean$`Data Set` <- as.factor(clean$`Data Set`)
clean$`Geography Level` <- as.factor(clean$`Geography Level`)
clean$Form <- as.factor(clean$Form)




