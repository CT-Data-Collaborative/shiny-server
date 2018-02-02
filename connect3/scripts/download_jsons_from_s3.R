#*****************************************************************************************************************
mylist <- c("Statewide", 
            "Southwest Region",      #1
            "South Central Region",  #2
            "Eastern Region",        #3
            "North Central Region",  #4
            "Western Region",        #5
            "Central Region")        #6

region_list <- c("Connecticut",
                 "Region 1:Southwest", 
                 "Region 2:South Central", 
                 "Region 3:Eastern",                 
                 "Region 4:North Central",
                 "Region 5:Western",
                 "Region 6:Central")

region_list2 <- c("Other",
                 "Region 1: Southwest", 
                 "Region 2: South Central", 
                 "Region 3: Eastern",                 
                 "Region 4: North Central",
                 "Region 5: Western",
                 "Region 6: Central")

sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
data_location <- grep("data$", sub_folders, value=T)
path_to_json <- (paste0(getwd(), "/", data_location, "/", "json"))
path_to_raw <- (paste0(getwd(), "/", raw_location))
CT_towns_shp <- readOGR(dsn = "./Shapefile", layer = "cb_2016_09_cousub_500k", verbose = FALSE)
CT_towns <- fortify(CT_towns_shp, region = "NAME")

districts <- read.csv(paste0(path_to_raw, "/", "ct-school-district-list-with-fips.csv"), stringsAsFactors = F, header=T, check.names = F)
district_list <- unique(districts$FixedDistrict)
district_list <- district_list[district_list != "Connecticut"]

#This comes in from .Rprofile
# Sys.setenv("AWS_ACCESS_KEY_ID" = "key",
#            "AWS_SECRET_ACCESS_KEY" = "secret_key",
#            "AWS_DEFAULT_REGION" = "default_region")

#Read json object from s3
#specify bucketname
s3BucketName <- "connect-ctdata-2"

#get list of all json files in bucket, load them into R, and convert to df
file_names <- grep(".json", grep("2018_test", get_bucket_df(s3BucketName)[["Key"]], value=TRUE), value=T)
#file_name <- grep("dcf_regions_CT.json", get_bucket_df(s3BucketName)[["Key"]], value=TRUE)
for (i in 1:length(file_names)) {
  df_name <- gsub(".*/\\s*|.json.*", "", file_names[i])
  json_name <- gsub(".*/\\s*|$.*", "", file_names[i])
  object <- get_object(file_names[i], s3BucketName) 
  object_data <- readBin(object, "character")
  json_data <- fromJSON(object_data)
  json_file <- lapply(json_data, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  asFrame <- as.data.frame(do.call("rbind", json_data), stringsAsFactors = F, check.names=F)
  asFrame2 <- data.frame(t(asFrame), stringsAsFactors = F, check.names=F)
  if("Value" %in% colnames(asFrame2)) {
      asFrame2$Value <- as.numeric(asFrame2$Value)
  }
  assign(df_name, asFrame2)
}

max_year_edu <- max(edu$Year)
max_year_edu2 <- max(edu2$Year)
max_year_edu3 <- max(edu3$Year)
max_year_edu4 <- max(edu4$Year)

dcf_regions <- read.csv(paste0(path_to_raw, "/", "dcf-regions.csv"), stringsAsFactors = F, header=T, check.names = F)
dcf_regions_CT <- dcf_regions %>% 
  mutate(Value = 1)
dcf_regions_1 <- dcf_regions %>% 
  mutate(Value = ifelse(grepl("1", Region), 1, 0))
dcf_regions_2 <- dcf_regions %>% 
    mutate(Value = ifelse(grepl("2", Region), 1, 0))
dcf_regions_3 <- dcf_regions %>% 
    mutate(Value = ifelse(grepl("3", Region), 1, 0))
dcf_regions_4 <- dcf_regions %>% 
    mutate(Value = ifelse(grepl("4", Region), 1, 0))
dcf_regions_5 <- dcf_regions %>% 
    mutate(Value = ifelse(grepl("5", Region), 1, 0))
dcf_regions_6 <- dcf_regions %>% 
    mutate(Value = ifelse(grepl("6", Region), 1, 0))

dcf_regions_CT<-merge(CT_towns, dcf_regions_CT, by.x="id", by.y = "Town", all=TRUE)
dcf_regions_1<-merge(CT_towns, dcf_regions_1, by.x="id", by.y = "Town", all=TRUE)
dcf_regions_2<-merge(CT_towns, dcf_regions_2, by.x="id", by.y = "Town", all=TRUE)
dcf_regions_3<-merge(CT_towns, dcf_regions_3, by.x="id", by.y = "Town", all=TRUE)
dcf_regions_4<-merge(CT_towns, dcf_regions_4, by.x="id", by.y = "Town", all=TRUE)
dcf_regions_5<-merge(CT_towns, dcf_regions_5, by.x="id", by.y = "Town", all=TRUE)
dcf_regions_6<-merge(CT_towns, dcf_regions_6, by.x="id", by.y = "Town", all=TRUE)

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
