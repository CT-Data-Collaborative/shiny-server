#Creates csv files
#Runs once when data needs to be generated (new data gets updated)

library(RCurl)
library(dplyr)
library(ggplot2)
library(datapkg)
library(tidyr)
library(plotly)
library(rgdal)
library(censusr)

######################################################################################################################################
mylist <- c("Statewide", "Southwest Region","South Central Region",
                                    "Eastern Region","North Central Region","Western Region","Central Region")
region_list <- c("Connecticut", "Region 1:Southwest",  "Region 2:South Central",  "Region 3:Eastern", 
                 "Region 4:North Central", "Region 5:Western", "Region 6:Central")

region_list2 <- c("Other", "Region 1: Southwest",  "Region 2: South Central", "Region 3: Eastern", 
                  "Region 4: North Central", "Region 5: Western", "Region 6: Central")

##Read in data
#######REGION MAPS############################################################################################################################
CT_towns_shp <- readOGR(dsn = "./Shapefile", layer = "cb_2016_09_cousub_500k", verbose = FALSE)
CT_towns <- fortify(CT_towns_shp, region = "NAME")
dcf_regions <- read.csv(paste0(path_to_data, "/", "dcf-regions.csv"), stringsAsFactors = F, header=T, check.names = F)
dcf_regions_CT<-merge(CT_towns, dcf_regions, by.x="id", by.y = "Town", all=TRUE)
dcf_regions_CT$Region <- mylist[match(dcf_regions_CT$Region, region_list)]

############################################################################
# Write to File
write.table(dcf_regions_CT, file.path(path_to_data, "dcf_regions_CT.csv"), sep = ",", row.names = F)


# dcf_regions_CT <- dcf_regions %>% 
#   mutate(Value = 1)
# dcf_regions_1 <- dcf_regions %>% 
#   mutate(Value = ifelse(grepl("1", Region), 1, 0))
# dcf_regions_2 <- dcf_regions %>% 
#     mutate(Value = ifelse(grepl("2", Region), 1, 0))
# dcf_regions_3 <- dcf_regions %>% 
#     mutate(Value = ifelse(grepl("3", Region), 1, 0))
# dcf_regions_4 <- dcf_regions %>% 
#     mutate(Value = ifelse(grepl("4", Region), 1, 0))
# dcf_regions_5 <- dcf_regions %>% 
#     mutate(Value = ifelse(grepl("5", Region), 1, 0))
# dcf_regions_6 <- dcf_regions %>% 
#     mutate(Value = ifelse(grepl("6", Region), 1, 0))
# dcf_regions_CT<-merge(CT_towns, dcf_regions_CT, by.x="id", by.y = "Town", all=TRUE)
# dcf_regions_1<-merge(CT_towns, dcf_regions_1, by.x="id", by.y = "Town", all=TRUE)
# dcf_regions_2<-merge(CT_towns, dcf_regions_2, by.x="id", by.y = "Town", all=TRUE)
# dcf_regions_3<-merge(CT_towns, dcf_regions_3, by.x="id", by.y = "Town", all=TRUE)
# dcf_regions_4<-merge(CT_towns, dcf_regions_4, by.x="id", by.y = "Town", all=TRUE)
# dcf_regions_5<-merge(CT_towns, dcf_regions_5, by.x="id", by.y = "Town", all=TRUE)
# dcf_regions_6<-merge(CT_towns, dcf_regions_6, by.x="id", by.y = "Town", all=TRUE)
############################################################################
# Write to File
write.table(dcf_regions_CT, file.path(path_to_data, "dcf_regions_CT.csv"), sep = ",", row.names = F)
# write.table(dcf_regions_1, file.path(path_to_data, "dcf_regions_1.csv"), sep = ",", row.names = F)
# write.table(dcf_regions_2, file.path(path_to_data, "dcf_regions_2.csv"), sep = ",", row.names = F)
# write.table(dcf_regions_3, file.path(path_to_data, "dcf_regions_3.csv"), sep = ",", row.names = F)
# write.table(dcf_regions_4, file.path(path_to_data, "dcf_regions_4.csv"), sep = ",", row.names = F)
# write.table(dcf_regions_5, file.path(path_to_data, "dcf_regions_5.csv"), sep = ",", row.names = F)
# write.table(dcf_regions_6, file.path(path_to_data, "dcf_regions_6.csv"), sep = ",", row.names = F)
#######HEALTH############################################################################################################################
print("health")
health <- read.csv("./raw/all_years_FM_UNSUPPRESSED.csv", stringsAsFactors = F, header=T, check.names=F)
health <- health[health$AGGREGATION == "5-Year",]
names(health)[names(health) == "Town/County"] <- "Town"
health_regions <- merge(health, dcf_regions, by = c("Town", "FIPS"), all.y=T)
max_year_health_regions <- "2010-2014"
health_regions$Region <- mylist[match(health_regions$Region, region_list)]
############################################################################
health_rates <- read.csv("./raw/all_years_FM_with_race_rates_UNSUPPRESSED.csv", stringsAsFactors = F, header=T, check.names=F)
health_rates <- health_rates[health_rates$AGGREGATION == "5-Year",]
names(health_rates)[names(health_rates) == "GEOG_name"] <- "Town"
health_rates_regions <- merge(health_rates, dcf_regions, by = c("Town"), all.y=T)
max_year_health_rates_regions <- "2010-2014"
health_rates_regions$Region <- mylist[match(health_rates_regions$Region, region_list)]
############################################################################
#Write to File
write.table(health_regions, file.path(path_to_data, "health_regions.csv"), sep = ",", row.names = F)
write.table(health_rates_regions, file.path(path_to_data, "health_rates_regions.csv"), sep = ",", row.names = F)
#######EARLY CHILDHOOD############################################################################################################################
print("early childhood")
b23 <- read.csv("./raw/birth_to_three_annual_2016_UNSUPPRESSED.csv", stringsAsFactors = F, header=T, check.names=F)
b23_regions <- merge(b23, dcf_regions, by = c("Town", "FIPS"))
b23_regions <- b23_regions[b23_regions$`Measure Period` == "Calendar Year",]
max_year_b23_regions <- max(b23_regions$Year)
b23_regions$Region <- mylist[match(b23_regions$Region, region_list)]
############################################################################
total_births <- read.csv("./raw/maternal_characteristics_UNSUPRESSEDbackground_1-Year.csv", stringsAsFactors = F, header=T, check.names=F)
b23c <- read.csv("./raw/birth_to_three_cohort_2013_UNSUPPRESSED.csv", stringsAsFactors = F, header=T, check.names=F)
b23c_regions <- merge(b23c, dcf_regions, by = c("Town", "FIPS"))
max_year_b23c_regions <- max(b23c_regions$Year)
total_births <- total_births %>% 
  filter(`Birth Weight` == "All",
         `Gestational Age` == "All",
         `Mother's Education` == "All",
         `Mother's Marital Status` == "All",
         `Measure Type` == "Number",
         `Year` == as.numeric(max_year_b23c_regions)) %>% 
  select(Town, Year, Value) %>% 
  rename(`Total Births` = Value)
b23c_regions <- merge(b23c_regions, total_births, by = c("Town", "Year"))
b23c_regions$Region <- mylist[match(b23c_regions$Region, region_list)]
############################################################################
#Write to File
write.table(b23_regions, file.path(path_to_data, "b23_regions.csv"), sep = ",", row.names = F)
write.table(b23c_regions, file.path(path_to_data, "b23c_regions.csv"), sep = ",", row.names = F)
############JUVENILE JUSTICE######################################################################################################################
print("juvenile justice")
jj_data1 <- getURL('http://data.ctdata.org/dataset/17cf405f-ab97-449e-8e1f-12aebbbaa349/resource/3b9fc9e0-ff21-44b1-9c7f-656dbb6b3c77/download/juvenile-arrests2015.csv')
jj <- read.csv(textConnection(jj_data1), header=T, stringsAsFactors = FALSE, check.names=FALSE)
jj_regions <- merge(jj, dcf_regions, by = c("Town", "FIPS"))
max_year_jj_regions <- max(jj_regions$Year)
jj_regions$Region <- mylist[match(jj_regions$Region, region_list)]
############################################################################
#Write to File
write.table(jj_regions, file.path(path_to_data, "jj_regions.csv"), sep = ",", row.names = F)
############CHILD WELFARE###############################################################################################################################################################################
print("child welfare")
cw_data1 <- getURL('http://data.ctdata.org/dataset/a65787c0-4cb0-494d-8537-b8826f31cfa5/resource/5f67003e-91e8-447d-868d-6b397811cfe2/download/children-in-placement-by-age.csv')
cw <- read.csv(textConnection(cw_data1), header=T, stringsAsFactors = FALSE, check.names=FALSE)
max_year_cw <- max(gsub("SFY ", "", cw$Year))
max_year_cw <- paste("SFY", max_year_cw, sep = " ")
cw$Region <- mylist[match(cw$Region, region_list2)]
cw <- cw[cw$Region != "Statewide",]
cw <- cw[cw$Year == max_year_cw,]
#Create statewide values
cw_ct <- cw %>% 
  group_by(`Location of Placement`, `Age Group`, `Type of Placement`) %>% 
  summarise(Value = sum(Value))
cw_ct <- spread(cw_ct, `Age Group`, Value)
cw_ct$Region <- "Statewide"
cw_ct <- cw_ct %>% 
  select(Region, `Location of Placement`, `Type of Placement`, `0 to 3 Years`, `4 to 6 Years`, `7 to 12 Years`, `13 to 17 Years`, `18 Years and Over`, Total) %>% 
  arrange(desc(`Total`))
cw_ct <- as.data.frame(cw_ct)
#Process rest of regions
cw_total <- cw %>% 
  group_by(Region, `Location of Placement`, `Age Group`, `Type of Placement`) %>% 
  summarise(Value = sum(Value))
cw_total <- spread(cw_total, `Age Group`, Value)
cw_total <- cw_total %>% 
  select(Region, `Location of Placement`, `Type of Placement`, `0 to 3 Years`, `4 to 6 Years`, `7 to 12 Years`, `13 to 17 Years`, `18 Years and Over`, Total) %>% 
  arrange(desc(`Total`))
cw_total <- as.data.frame(cw_total)
cw_total <- rbind(cw_total, cw_ct)
############################################################################
cw_data2 <- getURL('http://data.ctdata.org/dataset/bc08903d-a4d9-4094-8ddb-1baad9f99cb4/resource/feb378fc-5287-4e7a-b39a-c0c2f031f5de/download/children-in-placement-by-gender.csv')
cw_gender <- read.csv(textConnection(cw_data2), header=T, stringsAsFactors = FALSE, check.names=FALSE)
max_year_cw_gender <- max(gsub("SFY ", "", cw_gender$Year))
max_year_cw_gender <- paste("SFY", max_year_cw_gender, sep = " ")
cw_gender$Region <- mylist[match(cw_gender$Region, region_list2)]
cw_gender <- cw_gender[cw_gender$Region != "Statewide",]
cw_gender <- cw_gender[cw_gender$Year == max_year_cw_gender,]
#Create statewide values
cw_gender_ct <- cw_gender %>% 
  group_by(`Location of Placement`, Gender, `Type of Placement`) %>% 
  summarise(Value = sum(Value))
cw_gender_ct$Region <- "Statewide"
cw_gender_ct <- cw_gender_ct %>% 
  select(Region, `Location of Placement`, `Type of Placement`, Gender, Value) %>% 
  arrange(desc(`Value`))
cw_gender_ct <- as.data.frame(cw_gender_ct)
#Process rest of regions
cw_gender_total <- cw_gender %>% 
  group_by(Region, `Location of Placement`, Gender, `Type of Placement`) %>% 
  summarise(Value = sum(Value))
cw_gender_total <- cw_gender_total %>% 
  select(Region, `Location of Placement`, `Type of Placement`, Gender, Value) %>% 
  arrange(desc(`Value`))
cw_gender_total <- as.data.frame(cw_gender_total)
cw_gender_total <- rbind(cw_gender_total, cw_gender_ct)
############################################################################
cw_data3 <- getURL('http://data.ctdata.org/dataset/cc67c782-8488-406c-8398-40172deb4f37/resource/c185d000-9b63-4670-b724-cb455648f80c/download/children-in-placement-by-race.csv')
cw_race <- read.csv(textConnection(cw_data3), header=T, stringsAsFactors = FALSE, check.names=FALSE)
max_year_cw_race <- max(gsub("SFY ", "", cw_race$Year))
max_year_cw_race <- paste("SFY", max_year_cw_race, sep = " ")
cw_race$Region <- mylist[match(cw_race$Region, region_list2)]
cw_race <- cw_race[cw_race$Region != "Statewide",]
cw_race <- cw_race[cw_race$Year == max_year_cw_gender,]
#Create statewide values
cw_race_ct <- cw_race %>% 
  group_by(`Location of Placement`, `Race/Ethnicity`, `Type of Placement`) %>% 
  summarise(Value = sum(Value))
cw_race_ct$Region <- "Statewide"
cw_race_ct <- cw_race_ct %>% 
  select(Region, `Location of Placement`, `Type of Placement`, `Race/Ethnicity`, Value) %>% 
  arrange(desc(`Value`))
cw_race_ct <- as.data.frame(cw_race_ct)
#Process rest of regions
cw_race_total <- cw_race %>% 
  group_by(Region, `Location of Placement`, `Race/Ethnicity`, `Type of Placement`) %>% 
  summarise(Value = sum(Value))
cw_race_total <- cw_race_total %>% 
  select(Region, `Location of Placement`, `Type of Placement`, `Race/Ethnicity`, Value) %>% 
  arrange(desc(`Value`))
cw_race_total <- as.data.frame(cw_race_total)
cw_race_total <- rbind(cw_race_total, cw_race_ct)
#backfill all races
races <- c("Black", "Hispanic", "White", "Other")
backfill_races <- expand.grid(
  `Region` = unique(cw_race_total$Region),
  `Location of Placement` = unique(cw_race_total$`Location of Placement`),
  `Type of Placement` = unique(cw_race_total$`Type of Placement`),
  `Race/Ethnicity` = races 
)
cw_backfill <- merge(cw_race_total, backfill_races, all.y=T)
cw_backfill$Value[is.na(cw_backfill$Value)] <- 0
############################################################################
cw_data4 <- getURL('http://data.ctdata.org/dataset/73a25e29-e338-4596-afd7-307f67d7484f/resource/ce572760-8506-42c5-9ead-cddb51ee5458/download/employed-or-enrolled-youth.csv')
cw_eey <- read.csv(textConnection(cw_data4), header=T, stringsAsFactors = FALSE, check.names=FALSE)
max_year_cw_eey <- max(cw_eey$Year)
cw_eey <- cw_eey[cw_eey$Year == max_year_cw_eey & cw_eey$Value != -9999,]
cw_eey_regions <- merge(cw_eey, dcf_regions, by = c("Town", "FIPS"), all.x=T)
cw_eey_regions$Region <- mylist[match(cw_eey_regions$Region, region_list)]
############################################################################
#Write to File
write.table(cw_total, file.path(path_to_data, "cw_total.csv"), sep = ",", row.names = F)
write.table(cw_gender_total, file.path(path_to_data, "cw_gender_total.csv"), sep = ",", row.names = F)
write.table(cw_race_total, file.path(path_to_data, "cw_race_total.csv"), sep = ",", row.names = F)
write.table(cw_backfill, file.path(path_to_data, "cw_backfill.csv"), sep = ",", row.names = F)
write.table(cw_eey_regions, file.path(path_to_data, "cw_eey_regions.csv"), sep = ",", row.names = F)
########DEMOGRAPHICS####################################################################################################################################
print("demographics")
pop_by_age_gender_df <- dir(path_to_raw, recursive=T, pattern = "pop_by_age_gender_regions")
pop_by_race_gender_df <- dir(path_to_raw, recursive=T, pattern = "pop_by_race_gender_regions")
pop_by_age_race_df <- dir(path_to_raw, recursive=T, pattern = "pop_by_age_race_regions")
pop_by_age_gender_regions <- read.csv(paste0(path_to_raw, "/", pop_by_age_gender_df), stringsAsFactors = F, header=T, check.names=F)
pop_by_race_gender_regions <- read.csv(paste0(path_to_raw, "/", pop_by_race_gender_df), stringsAsFactors = F, header=T, check.names=F)
pop_by_age_race_regions <- read.csv(paste0(path_to_raw, "/", pop_by_age_race_df), stringsAsFactors = F, header=T, check.names=F)
max_year_pop_regions <- max(pop_by_age_gender_regions$Year)
############################################################################
mhi_data <- getURL('http://data.ctdata.org/dataset/68296d39-d0f7-4e2d-ba0e-98a3463ab58f/resource/1a962569-c6b3-4031-b7b1-f051e9c12379/download/median-household-income-town-2016.csv')
mhi_df <- read.csv(textConnection(mhi_data), header=T, stringsAsFactors = FALSE, check.names=FALSE)
max_year_mhi_regions <- max(mhi_df$Year)
mhi_df <- mhi_df[mhi_df$Year == max_year_mhi_regions & mhi_df$`Race/Ethnicity` == "All" & mhi_df$`Measure Type` == "Number",]
mhi_df_regions <- merge(mhi_df, dcf_regions, by = c("Town", "FIPS"))
mhi_df_regions$Region <- mylist[match(mhi_df_regions$Region, region_list)]
############################################################################
#Write to File
write.table(pop_by_age_gender_regions, file.path(path_to_data, "pop_by_age_gender_regions.csv"), sep = ",", row.names = F)
write.table(pop_by_race_gender_regions, file.path(path_to_data, "pop_by_race_gender_regions.csv"), sep = ",", row.names = F)
write.table(pop_by_age_race_regions, file.path(path_to_data, "pop_by_age_race_regions.csv"), sep = ",", row.names = F)
write.table(mhi_df_regions, file.path(path_to_data, "mhi_df_regions.csv"), sep = ",", row.names = F)
write.table(mhi_df, file.path(path_to_data, "mhi_df.csv"), sep = ",", row.names = F)
#####BEHAVIORAL HEALTH################################################################################################################################
print("behavioral health")
bh_data <- dir(path_to_raw, recursive=T, pattern = "rh.csv")
bh_df <- read.csv(paste0(path_to_raw, "/", bh_data), stringsAsFactors = FALSE, header=T, check.names=F)
bh_df <- bh_df %>% 
  select(-c(1,3,4,5)) 
bh_df$Age <- as.numeric(bh_df$Age)
bh_df <- bh_df[bh_df$Age < 22,]
bh_df <- bh_df[!is.na(bh_df$full_fips),]
bh_df$`Age Range` <- NA
bh_df$`Age Range`[bh_df$Age <= 5] <- "0 to 5"
bh_df$`Age Range`[bh_df$Age >=6 & bh_df$Age <= 9] <- "6 to 9"
bh_df$`Age Range`[bh_df$Age >= 10 & bh_df$Age <= 15] <- "10 to 15"
bh_df$`Age Range`[bh_df$Age >= 16 & bh_df$Age <= 19] <- "16 to 19"
bh_df$`Age Range`[bh_df$Age >= 20 & bh_df$Age <= 21] <- "20 to 21"
#merge in Region
bh_df_regions <- merge(bh_df, dcf_regions, by.x = "town_name", by.y = "Town")
cols <- c(5:28)
bh_df_regions[cols] <- sapply(bh_df_regions[cols],as.numeric)
bh_df_regions_calc <- bh_df_regions %>% 
  group_by(`Age Range`, Region) %>% 
  summarise(POP_all = sum(POP_all),
            POP_nh = sum(POP_nh),
            POP_hisp = sum(POP_hisp),
            WHITE_all = sum(WHITE_all),
            WHITE_nh = sum(WHITE_nh),
            WHITE_hisp = sum(WHITE_hisp),
            BLACK_all = sum(BLACK_all),
            BLACK_nh = sum(BLACK_nh),
            BLACK_hisp = sum(BLACK_hisp),
            AMIND_all = sum(AMIND_all),
            AMIND_nh = sum(AMIND_nh),
            AMIND_hisp = sum(AMIND_hisp),
            ASIAN_all = sum(ASIAN_all),
            ASIAN_nh = sum(ASIAN_nh),
            ASIAN_hisp = sum(ASIAN_hisp),
            PACISL_all = sum(PACISL_all),
            PACISL_nh = sum(PACISL_nh),
            PACISL_hisp = sum(PACISL_hisp),
            otherrace_all = sum(otherrace_all),
            otherrace_nh = sum(otherrace_nh),
            otherrace_hisp = sum(otherrace_hisp),
            multirace_all = sum(multirace_all),
            multirace_nh = sum(multirace_nh),
            multirace_hisp = sum(multirace_hisp))
#create 0 to 21 age range
x0to21 <- bh_df_regions_calc %>% 
  group_by(Region) %>% 
  summarise(POP_all = sum(POP_all),
            POP_nh = sum(POP_nh),
            POP_hisp = sum(POP_hisp),
            WHITE_all = sum(WHITE_all),
            WHITE_nh = sum(WHITE_nh),
            WHITE_hisp = sum(WHITE_hisp),
            BLACK_all = sum(BLACK_all),
            BLACK_nh = sum(BLACK_nh),
            BLACK_hisp = sum(BLACK_hisp),
            AMIND_all = sum(AMIND_all),
            AMIND_nh = sum(AMIND_nh),
            AMIND_hisp = sum(AMIND_hisp),
            ASIAN_all = sum(ASIAN_all),
            ASIAN_nh = sum(ASIAN_nh),
            ASIAN_hisp = sum(ASIAN_hisp),
            PACISL_all = sum(PACISL_all),
            PACISL_nh = sum(PACISL_nh),
            PACISL_hisp = sum(PACISL_hisp),
            otherrace_all = sum(otherrace_all),
            otherrace_nh = sum(otherrace_nh),
            otherrace_hisp = sum(otherrace_hisp),
            multirace_all = sum(multirace_all),
            multirace_nh = sum(multirace_nh),
            multirace_hisp = sum(multirace_hisp))
x0to21$`Age Range` <- "0 to 21"
bh_df_regions_calc <- as.data.frame(bh_df_regions_calc)
bh_df_regions_calc <- rbind(bh_df_regions_calc, x0to21)
#create final r/e groups
bh_calc <- bh_df_regions_calc %>% 
  mutate(White = WHITE_nh, 
         Black = BLACK_nh,
         Hispanic = POP_hisp,
         Total = POP_all, 
         Other = AMIND_nh + ASIAN_nh + PACISL_nh + otherrace_nh + multirace_nh) %>% 
  select(`Age Range`, Region, White, Black, Hispanic, Total, Other)
#Create CT values
bh_calc_CT <- bh_calc %>% 
  group_by(`Age Range`) %>% 
  summarise(White = sum(White), 
         Black = sum(Black),
         Hispanic = sum(Hispanic),
         Total = sum(Total), 
         Other = sum(Other))
bh_calc_CT$Region <- "Connecticut"
bh_calc <- rbind(bh_calc, bh_calc_CT)
#Calculate treatment needs
bh_calc_long <- gather(bh_calc, `Race/Ethnicity`, Pop, 3:7)
bh_calc_long$Tx <- round(bh_calc_long$Pop * 0.1, 0)
bh_calc_long$Life <- round(bh_calc_long$Pop * 0.2, 0)
bh_calc_long <- gather(bh_calc_long, Label, Value, 4:6)
bh_calc_wide <- spread(bh_calc_long, `Race/Ethnicity`, Value)
bh_calc_wide$`Age Range` <- factor(bh_calc_wide$`Age Range`, 
                                   levels= c("0 to 5", "6 to 9", "10 to 15", "16 to 19", "20 to 21", "0 to 21"))
bh_calc_wide$Label <- factor(bh_calc_wide$Label, 
                                   levels= c("Pop", "Tx", "Life"))
bh_calc_wide <- bh_calc_wide %>% 
  select(Region, `Age Range`, Label, White, Black, Hispanic, Other, Total) %>% 
  arrange(Region, `Age Range`, Label)
bh_plot1 <- bh_calc_wide
bh_plot1$Region <- mylist[match(bh_plot1$Region, region_list)]
bh_plot1 <- bh_plot1[bh_plot1$Label == "Pop",]
#add commas to values
bh_calc_wide$White <- formatC(bh_calc_wide$White, format="d", big.mark=",")
bh_calc_wide$Black <- formatC(bh_calc_wide$Black, format="d", big.mark=",")
bh_calc_wide$Hispanic <- formatC(bh_calc_wide$Hispanic, format="d", big.mark=",")
bh_calc_wide$Other <- formatC(bh_calc_wide$Other, format="d", big.mark=",")
bh_calc_wide$Total <- formatC(bh_calc_wide$Total, format="d", big.mark=",")
bh_CT <- bh_calc_wide[bh_calc_wide$Region == "Connecticut",]
bh_r1 <- bh_calc_wide[grepl("Region 1", bh_calc_wide$Region),]
bh_r2 <- bh_calc_wide[grepl("Region 2", bh_calc_wide$Region),]
bh_r3 <- bh_calc_wide[grepl("Region 3", bh_calc_wide$Region),]
bh_r4 <- bh_calc_wide[grepl("Region 4", bh_calc_wide$Region),]
bh_r5 <- bh_calc_wide[grepl("Region 5", bh_calc_wide$Region),]
bh_r6 <- bh_calc_wide[grepl("Region 6", bh_calc_wide$Region),]
bh_CT$Region <- NULL
bh_r1$Region <- NULL
bh_r2$Region <- NULL
bh_r3$Region <- NULL
bh_r4$Region <- NULL
bh_r5$Region <- NULL
bh_r6$Region <- NULL
bh_CT$`Age Range` <- " "
bh_r1$`Age Range` <- " "
bh_r2$`Age Range` <- " "
bh_r3$`Age Range` <- " "
bh_r4$`Age Range` <- " "
bh_r5$`Age Range` <- " "
bh_r6$`Age Range` <- " "
rownames(bh_r1) <- seq(length=nrow(bh_r1))
rownames(bh_r2) <- seq(length=nrow(bh_r2))
rownames(bh_r3) <- seq(length=nrow(bh_r3))
rownames(bh_r4) <- seq(length=nrow(bh_r4))
rownames(bh_r5) <- seq(length=nrow(bh_r5))
rownames(bh_r6) <- seq(length=nrow(bh_r6))
############################################################################
#Write to File
write.table(bh_CT, file.path(path_to_data, "bh_CT.csv"), sep = ",", row.names = F)
write.table(bh_r1, file.path(path_to_data, "bh_r1.csv"), sep = ",", row.names = F)
write.table(bh_r2, file.path(path_to_data, "bh_r2.csv"), sep = ",", row.names = F)
write.table(bh_r3, file.path(path_to_data, "bh_r3.csv"), sep = ",", row.names = F)
write.table(bh_r4, file.path(path_to_data, "bh_r4.csv"), sep = ",", row.names = F)
write.table(bh_r5, file.path(path_to_data, "bh_r5.csv"), sep = ",", row.names = F)
write.table(bh_r6, file.path(path_to_data, "bh_r6.csv"), sep = ",", row.names = F)
write.table(bh_plot1, file.path(path_to_data, "bh_plot1.csv"), sep = ",", row.names = F)
####EDUCATION########################################################################################################################
print("education")
myfile <- getURL('http://data.ctdata.org/dataset/8cad880f-4db0-4201-afa2-60a471fbc2fd/resource/ffec92b1-6412-40fa-9b05-81daa17ea69a/download/educationalneed2016-2017.csv')
edu <- read.csv(textConnection(myfile), header=T, stringsAsFactors = FALSE, check.names=FALSE)
max_year_edu <- max(edu$Year)
myfile2 <- getURL('http://data.ctdata.org/dataset/960ffb0e-d483-4afb-acf4-f16be9afa057/resource/64a81f36-c468-428a-bc58-32023134d0c2/download/suspensionraterace2010-2016.csv')
edu2 <- read.csv(textConnection(myfile2), header=T, stringsAsFactors = FALSE, check.names=FALSE)
max_year_edu2 <- max(edu2$Year)
myfile3 <- getURL('http://data.ctdata.org/dataset/4ceffd37-a05c-47d4-89da-4e856b514341/resource/abc9a699-b880-4a7d-8b5c-d3e2a8ad2239/download/sanctions2010-2017.csv')
edu3 <- read.csv(textConnection(myfile3), header=T, stringsAsFactors = FALSE, check.names=FALSE)
max_year_edu3 <- max(edu3$Year)
myfile4 <- getURL('http://data.ctdata.org/dataset/f6c789e8-fabc-467c-8441-dc639621ff0b/resource/bdfe5fe1-af80-4500-9e1f-5b842bbcb978/download/incidents2010-2017.csv')
edu4 <- read.csv(textConnection(myfile4), header=T, stringsAsFactors = FALSE, check.names=FALSE)
max_year_edu4 <- max(edu4$Year)
# kei <- read.csv("./raw/kindergarten-entrance-inventory-2017_UNSUPPRESSED.csv", stringsAsFactors = F, header=T, check.names=F)
# max_year_kei <- max(kei$Year)
############################################################################
#Write to File
write.table(bh_CT, file.path(path_to_data, "bh_CT.csv"), sep = ",", row.names = F)
write.table(edu, file.path(path_to_data, "edu.csv"), sep = ",", row.names = F)
write.table(edu2, file.path(path_to_data, "edu2.csv"), sep = ",", row.names = F)
write.table(edu3, file.path(path_to_data, "edu3.csv"), sep = ",", row.names = F)
write.table(edu4, file.path(path_to_data, "edu4.csv"), sep = ",", row.names = F)
print("End reading data")

