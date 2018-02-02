#Reads local csv files in R env for shiny app

#Setup env
sub_folders <- list.files()
data_location <- grep("data$", sub_folders, value=T)
path_to_data <- (paste0(getwd(), "/", data_location))

print("regions")
dcf_regions_CT <- read.csv(paste0(path_to_data, "/", "dcf_regions_CT.csv"), stringsAsFactors=F, header=T, check.names=F)
dcf_regions_1 <- read.csv(paste0(path_to_data, "/", "dcf_regions_1.csv"), stringsAsFactors=F, header=T, check.names=F)
dcf_regions_2 <- read.csv(paste0(path_to_data, "/", "dcf_regions_2.csv"), stringsAsFactors=F, header=T, check.names=F)
dcf_regions_3 <- read.csv(paste0(path_to_data, "/", "dcf_regions_3.csv"), stringsAsFactors=F, header=T, check.names=F)
dcf_regions_4 <- read.csv(paste0(path_to_data, "/", "dcf_regions_4.csv"), stringsAsFactors=F, header=T, check.names=F)
dcf_regions_5 <- read.csv(paste0(path_to_data, "/", "dcf_regions_5.csv"), stringsAsFactors=F, header=T, check.names=F)
dcf_regions_6 <- read.csv(paste0(path_to_data, "/", "dcf_regions_6.csv"), stringsAsFactors=F, header=T, check.names=F)

print("health")
health_regions <- read.csv(paste0(path_to_data, "/", "health_regions.csv"), stringsAsFactors=F, header=T, check.names=F)
health_rates_regions <- read.csv(paste0(path_to_data, "/", "health_rates_regions.csv"), stringsAsFactors=F, header=T, check.names=F)

print("early childhood")
b23_regions <- read.csv(paste0(path_to_data, "/", "b23_regions.csv"), stringsAsFactors=F, header=T, check.names=F)
b23c_regions <- read.csv(paste0(path_to_data, "/", "b23c_regions.csv"), stringsAsFactors=F, header=T, check.names=F)

print("juvenile justice")
jj_regions <- read.csv(paste0(path_to_data, "/", "jj_regions.csv"), stringsAsFactors=F, header=T, check.names=F)

print("child welfare")
cw_total <- read.csv(paste0(path_to_data, "/", "cw_total.csv"), stringsAsFactors=F, header=T, check.names=F)
cw_gender_total <- read.csv(paste0(path_to_data, "/", "cw_gender_total.csv"), stringsAsFactors=F, header=T, check.names=F)
cw_race_total <- read.csv(paste0(path_to_data, "/", "cw_race_total.csv"), stringsAsFactors=F, header=T, check.names=F)
cw_backfill <- read.csv(paste0(path_to_data, "/", "cw_backfill.csv"), stringsAsFactors=F, header=T, check.names=F)
cw_eey_regions <- read.csv(paste0(path_to_data, "/", "cw_eey_regions.csv"), stringsAsFactors=F, header=T, check.names=F)

print("demographics")
pop_by_age_gender_regions <- read.csv(paste0(path_to_data, "/", "pop_by_age_gender_regions.csv"), stringsAsFactors=F, header=T, check.names=F)
pop_by_race_gender_regions <- read.csv(paste0(path_to_data, "/", "pop_by_race_gender_regions.csv"), stringsAsFactors=F, header=T, check.names=F)
pop_by_age_race_regions <- read.csv(paste0(path_to_data, "/", "pop_by_age_race_regions.csv"), stringsAsFactors=F, header=T, check.names=F)
mhi_df_regions <- read.csv(paste0(path_to_data, "/", "mhi_df_regions.csv"), stringsAsFactors=F, header=T, check.names=F)
mhi_df <- read.csv(paste0(path_to_data, "/", "mhi_df.csv"), stringsAsFactors=F, header=T, check.names=F)

print("behavioral health")
bh_CT <- read.csv(paste0(path_to_data, "/", "bh_CT.csv"), stringsAsFactors=F, header=T, check.names=F)
bh_r1 <- read.csv(paste0(path_to_data, "/", "bh_r1.csv"), stringsAsFactors=F, header=T, check.names=F)
bh_r2 <- read.csv(paste0(path_to_data, "/", "bh_r2.csv"), stringsAsFactors=F, header=T, check.names=F)
bh_r3 <- read.csv(paste0(path_to_data, "/", "bh_r3.csv"), stringsAsFactors=F, header=T, check.names=F)
bh_r4 <- read.csv(paste0(path_to_data, "/", "bh_r4.csv"), stringsAsFactors=F, header=T, check.names=F)
bh_r5 <- read.csv(paste0(path_to_data, "/", "bh_r5.csv"), stringsAsFactors=F, header=T, check.names=F)
bh_r6 <- read.csv(paste0(path_to_data, "/", "bh_r6.csv"), stringsAsFactors=F, header=T, check.names=F)
bh_plot1 <- read.csv(paste0(path_to_data, "/", "bh_plot1.csv"), stringsAsFactors=F, header=T, check.names=F)

print("education")
edu <- read.csv(paste0(path_to_data, "/", "edu.csv"), stringsAsFactors=F, header=T, check.names=F)
edu2 <- read.csv(paste0(path_to_data, "/", "edu2.csv"), stringsAsFactors=F, header=T, check.names=F)
edu3 <- read.csv(paste0(path_to_data, "/", "edu3.csv"), stringsAsFactors=F, header=T, check.names=F)
edu4 <- read.csv(paste0(path_to_data, "/", "edu4.csv"), stringsAsFactors=F, header=T, check.names=F)
districts <- read.csv(paste0(path_to_data, "/", "ct-school-district-list-with-fips.csv"), stringsAsFactors = F, header=T, check.names = F)
district_list <- unique(districts$FixedDistrict)
district_list <- district_list[district_list != "Connecticut"]

print("Setting years")
max_year_health_regions <- "2010-2014"
max_year_health_rates_regions <- "2010-2014"
max_year_b23_regions <- max(b23_regions$Year)
max_year_b23c_regions <- max(b23c_regions$Year)
max_year_jj_regions <- max(jj_regions$Year)
max_year_pop_regions <- max(pop_by_age_gender_regions$Year)
max_year_mhi_regions <- max(mhi_df$Year)
max_year_cw_gender <- "SFY 2013-2014"
max_year_cw_race <- "SFY 2013-2014"
max_year_cw_eey <- max(cw_eey_regions$Year)
max_year_edu <- max(edu$Year)
max_year_edu2 <- max(edu2$Year)
max_year_edu3 <- max(edu3$Year)
max_year_edu4 <- max(edu4$Year)


print("Done!")