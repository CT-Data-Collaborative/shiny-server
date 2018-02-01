library(acs)
source('./scripts/acsHelpers.R')

tables <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
races <- c("White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
           "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
           "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino")

# Get geography object for CT and subcounty divisions
pop_by_race <- data.frame(stringsAsFactors = FALSE)
for (i in seq_along(tables)) {
  tbl <- tables[i]
  race <- races[i]
  acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2015,
    table = paste0("B01001", tbl)
  ) 
 
  pops <- data.frame(stringsAsFactors = FALSE)
  for (data in acsdata) {
    year <- data@endyear
    pop.total <- acsSum(data, 1, "Total")
    pop.total.m.white <- acsSum(data, 2, "Total Male")
    pop.total.f.white <- acsSum(data, 17, "Total Female")
    datafips <- data.table(fips = getACSFips(data))

    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(pop.total),
        estimate(pop.total.m.white),
        estimate(pop.total.f.white)

    )
    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Race/Ethnicity",
        variable.factor = F,
        value.name = "Pop",
        value.factor = F
    )
    
estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "HD01_VD01.Estimate; Total:")] <- paste0("Total ", race)
estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "HD01_VD02.Estimate; Male:")] <- paste0("Total Male ", race)
estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "HD01_VD17.Estimate; Female:")] <- paste0("Total Female ", race)


    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(pop.total) * 1.645,
        standard.error(pop.total.m.white) * 1.645,
        standard.error(pop.total.f.white) * 1.645
    )

    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Race/Ethnicity",
        variable.factor = F,
        value.name = "MOE",
        value.factor = F
    )
    
moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD01.Estimate; Total:")] <-  paste0("Total ", race)
moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD02.Estimate; Male:")] <-  paste0("Total Male ", race)
moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD17.Estimate; Female:")] <-  paste0("Total Female ", race)


    setkey(estimates, FIPS, Year, `Race/Ethnicity`)
    setkey(moes, FIPS, Year, `Race/Ethnicity`)

    pops <- rbind(pops, estimates[moes])
}

pops <- pops[pops$FIPS != "0900100000",]
pop_by_race <- rbind(pops, pop_by_race)
}

# Write to File
write.table(
    pop_by_race,
    file.path(getwd(), "raw", "populations_by_race.csv"),
    sep = ",",
    row.names = F
)