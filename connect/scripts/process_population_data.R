pop_by_town_data <- getURL('http://data.ctdata.org/dataset/9a508859-a280-41dd-a7c2-aa8f61a084b0/resource/77767f7e-e439-4ad8-8262-399ca3aeadbe/download/population-by-age-town.csv')
pop_by_town_df <- read.csv(textConnection(pop_by_town_data), header=T, stringsAsFactors = FALSE, check.names=FALSE)
max_year_pop_by_town_regions <- max(pop_by_town_regions$Year)
pop_by_town_regions <- merge(pop_by_town_df, dcf_regions, by = c("Town", "FIPS"))
pop_by_town_regions$Region <- mylist[match(pop_by_town_regions$Region, region_list)]
pop_by_age_gender_regions <- pop_by_town_regions[pop_by_town_regions$Year == max_year_pop_by_town_regions & 
                                                   pop_by_town_regions$`Race/Ethnicity` == "All" &
                                                   pop_by_town_regions$`Measure Type` == "Number",]
pop_by_race_gender_regions <- pop_by_town_regions[pop_by_town_regions$Year == max_year_pop_by_town_regions & 
                                                   pop_by_town_regions$`Age Cohort` == "Total" &
                                                   pop_by_town_regions$`Measure Type` == "Number",]
pop_by_age_race_regions <- pop_by_town_regions[pop_by_town_regions$Year == max_year_pop_by_town_regions & 
                                                   pop_by_town_regions$Gender == "Total" &
                                                   pop_by_town_regions$`Measure Type` == "Number",]
write.table(
  pop_by_age_gender_regions,
  file.path(getwd(), "raw", "pop_by_age_gender_regions.csv"),
  sep = ",",
  row.names = F
)

write.table(
  pop_by_race_gender_regions,
  file.path(getwd(), "raw", "pop_by_race_gender_regions.csv"),
  sep = ",",
  row.names = F
)

write.table(
  pop_by_age_race_regions,
  file.path(getwd(), "raw", "pop_by_age_race_regions.csv"),
  sep = ",",
  row.names = F
)
