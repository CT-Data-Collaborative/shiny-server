library(acs)
library(datapkg)
source('./scripts/decHelpers.R')

tables <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
races <- c("White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
           "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
           "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino")

# Get geography object for CT and subcounty divisions
pop_by_race <- data.frame(stringsAsFactors = FALSE)
for (i in seq_along(tables)) {
  tbl <- tables[1]
  race <- races[1]
  acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2010,
    table = paste0("P12", tbl)
  ) 
 
  pops <- data.frame(stringsAsFactors = FALSE)
  for (data in acsdata) {
    year <- data@endyear
    pop.total <- acsSum(data, 1, "Total")
    pop.total.0to4 <- acsSum(data, c(3,27), "Total 0 to 4")
    pop.total.5to9 <- acsSum(data, c(4,28), "Total 5 to 9")
    pop.total.10to14 <- acsSum(data, c(5, 29), "Total 10 to 14")
    pop.total.15to19 <- acsSum(data, c(6,7,30,31), "Total 15 to 19")
    pop.total.20to21 <- acsSum(data, c(8,9,32,33), "Total 20 to 21")
    pop.total.0to21 <- acsSum(data, c(3,4,5,6,7,8,9,27,28,29,30,31,32,33), "Total 0 to 21")
    datafips <- data.table(fips = getACSFips(data))

    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(pop.total),
        estimate(pop.total.0to4),
        estimate(pop.total.5to9),
        estimate(pop.total.10to14),
        estimate(pop.total.15to19),
        estimate(pop.total.20to21),
        estimate(pop.total.0to21)
    )
    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Race/Ethnicity",
        variable.factor = F,
        value.name = "Pop",
        value.factor = F
    )
    
#estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "D001; Total:")] <- paste0("Total ", race)


    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(pop.total) * 1.645,
        standard.error(pop.total.0to4) * 1.645,
        standard.error(pop.total.5to9) * 1.645,
        standard.error(pop.total.10to14) * 1.645,
        standard.error(pop.total.15to19) * 1.645,
        standard.error(pop.total.20to21) * 1.645,
        standard.error(pop.total.0to21) * 1.645
    )

    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Race/Ethnicity",
        variable.factor = F,
        value.name = "MOE",
        value.factor = F
    )
    
#moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD01.Estimate; Total:")] <-  paste0("Total ", race)

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