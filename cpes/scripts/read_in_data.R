library(readxl)
library(tidyr)
library(dplyr)
library(gdata)

sub_folders <- list.files()
data_location <- grep("data$", sub_folders, value=T)
path_to_data <- (paste0(getwd(), "/", data_location))
all_xls <- dir(path_to_data, pattern = ".xls")

dataset_csv <- read.csv(paste0(path_to_data, "/", "dataset_table.csv"), stringsAsFactors = F, header=T, check.names=F)
dataset_csv$slug <- gsub(" ", "-", tolower(dataset_csv$`Data Set`))
dataset_csv$Source <- factor(dataset_csv$Source, levels = c("State Department of Education", 
                                                                "Department of Emergency Services and Public Protection", 
                                                                "Department of Public Health", 
                                                                "Office of the Chief Medical Examiner"))

sources <- unique(dataset_csv$Source)

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




