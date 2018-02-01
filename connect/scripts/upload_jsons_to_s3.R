#*****************************************************************************************************************
#Set up environment variables
#install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
library(aws.s3)

#set environment variables (taken from .aws/credentials file)
#This is auto-loaded in from .Rprofile
# Sys.setenv("AWS_ACCESS_KEY_ID" = "key",
#            "AWS_SECRET_ACCESS_KEY" = "secret_key",
#            "AWS_DEFAULT_REGION" = "default_region")

#Run bucketlist() to get list of available buckets on your AWS account
bucketlist()

#List objects in public bucket:
# get_bucket(bucket = 'connect-ctdata-2')
# 
# #List objects in private bucket:
# get_bucket(
#   bucket = 'ctdata-ckan-dumps',
#   key = YOUR_AWS_ACCESS_KEY,
#   secret = YOUR_AWS_SECRET_ACCESS_KEY
# )
#*****************************************************************************************************************

##Add step, archive old data!!!

#*****************************************************************************************************************

# Upload local json file to S3
s3BucketName <- "connect-ctdata-2"
path_to_json <- (paste0(getwd(), "/", data_location, "/", "json"))
json_list <- list.files(path_to_json)
for (i in 1:length(json_list)) {
  json_name <- json_list[i]
  file_path <- paste0(path_to_json, "/", json_list[i])
  put_object(file = file_path, object = paste0("2018_test", "/", json_name), bucket = s3BucketName)
}

