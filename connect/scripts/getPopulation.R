library(acs)
source('./scripts/acsHelpers.R')

#Get state data
geography=geo.make(state=09)
yearlist=c(2016)
span = 5
col.names="pretty" 
key="ed0e58d2538fb239f51e01643745e83f380582d7"
options(scipen=999)

state_data <- data.table()
variable =list()      
for (k in seq_along(1:49)) {
 number = number=paste0("B01001", "_", sprintf("%03d",k))
 variable = c(variable, number)
 k=k+1
}  
variable <- as.character(variable)    
acsdata <- acs.fetch(geography=geography, endyear=2016, span=span, 
                    variable = variable, key=key)

pops <- data.table()
    year <- acsdata@endyear
    pop.total <- acsSum(acsdata, 1, "Total")
    pop.total.m <- acsSum(acsdata, 2, "Total Male")
    pop.under10.m <- acsSum(acsdata, c(3,4), "0 to 9 years Male") 
    pop.10to19.m <- acsSum(acsdata, c(5,6,7), "10 to 19 years Male")
    pop.20to29.m <- acsSum(acsdata, c(8,9,10,11), "20 to 29 years Male")
    pop.30to44.m <- acsSum(acsdata, c(12,13,14), "30 to 44 years Male")
    pop.45to64.m <- acsSum(acsdata, c(15,16,17,18,19), "45 to 64 years Male")
    pop.65plus.m <- acsSum(acsdata, c(20,21,22,23,24,25), "65 plus years Male")
    pop.total.f <- acsSum(acsdata, 26, "Total Female")
    pop.under10.f <- acsSum(acsdata, c(27,28), "0 to 9 years Female")
    pop.10to19.f <- acsSum(acsdata, c(29,30,31), "10 to 19 years Female")
    pop.20to29.f <- acsSum(acsdata, c(32,33,34,35), "20 to 29 years Female")    
    pop.30to44.f <- acsSum(acsdata, c(36,37,38), "30 to 44 years Female")    
    pop.45to64.f <- acsSum(acsdata, c(39,40,41,42,43), "45 to 64 years Female")    
    pop.65plus.f <- acsSum(acsdata, c(44,45,46,47,48,49), "65 plus years Female")    
    pop.under10 <- acsSum(acsdata, c(3,4,27,28), "0 to 9 years")
    pop.10to19 <- acsSum(acsdata, c(5,6,7,29,30,31), "10 to 19 years")
    pop.20to29 <- acsSum(acsdata, c(8,9,10,11,32,33,34,35), "20 to 29 years")
    pop.30to44 <- acsSum(acsdata, c(12,13,14,36,37,38), "30 to 44 years")
    pop.45to64 <- acsSum(acsdata, c(15,16,17,18,19,39,40,41,42,43), "45 to 64 years")
    pop.65plus <- acsSum(acsdata, c(20,21,22,23,24,25,44,45,46,47,48,49), "65 plus years")
    geo <- acsdata@geography

    estimates <- data.table(
        geo,
        Year = year,
        estimate(pop.total),
        estimate(pop.total.m),
        estimate(pop.under10.m),
        estimate(pop.10to19.m),
        estimate(pop.20to29.m),
        estimate(pop.30to44.m),
        estimate(pop.45to64.m),
        estimate(pop.65plus.m),
        estimate(pop.total.f),
        estimate(pop.under10.f),
        estimate(pop.10to19.f),
        estimate(pop.20to29.f),
        estimate(pop.30to44.f),
        estimate(pop.45to64.f),
        estimate(pop.65plus.f),
        estimate(pop.under10),
        estimate(pop.10to19),
        estimate(pop.20to29),
        estimate(pop.30to44),
        estimate(pop.45to64),
        estimate(pop.65plus)
    )

    estimates <- melt(
        estimates,
        id.vars = c("NAME", "state", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "Pop",
        value.factor = F
    )
    
estimates$`Age Range`[which(estimates$`Age Range` == "B01001_001")] <-  "Total"
estimates$`Age Range`[which(estimates$`Age Range` == "B01001_002")] <-  "Total Male"
estimates$`Age Range`[which(estimates$`Age Range` == "B01001_026")] <-  "Total Female"


    moes <- data.table(
        geo,
        Year = year,
        standard.error(pop.total) * 1.645,
        standard.error(pop.total.m) * 1.645,
        standard.error(pop.under10.m) * 1.645,
        standard.error(pop.10to19.m) * 1.645,
        standard.error(pop.20to29.m) * 1.645,
        standard.error(pop.30to44.m) * 1.645,
        standard.error(pop.45to64.m) * 1.645,
        standard.error(pop.65plus.m) * 1.645,
        standard.error(pop.total.f) * 1.645,
        standard.error(pop.under10.f) * 1.645,
        standard.error(pop.10to19.f) * 1.645,
        standard.error(pop.20to29.f) * 1.645,
        standard.error(pop.30to44.f) * 1.645,
        standard.error(pop.45to64.f) * 1.645,
        standard.error(pop.65plus.f) * 1.645,
        standard.error(pop.under10) * 1.645,
        standard.error(pop.10to19) * 1.645,
        standard.error(pop.20to29) * 1.645,
        standard.error(pop.30to44) * 1.645,
        standard.error(pop.45to64) * 1.645,
        standard.error(pop.65plus) * 1.645        
    )

    moes <- melt(
        moes,
        id.vars = c("NAME", "state", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "MOE",
        value.factor = F
    )
    
moes$`Age Range`[which(moes$`Age Range` == "B01001_001")] <-  "Total"
moes$`Age Range`[which(moes$`Age Range` == "B01001_002")] <-  "Total Male"
moes$`Age Range`[which(moes$`Age Range` == "B01001_026")] <-  "Total Female"


    setkey(estimates, state, Year, `Age Range`)
    setkey(moes, state, Year, `Age Range`)

    pops_state <- merge(estimates, moes, by = c("NAME", "state", "Year", "Age Range"))

#Get town data
geography=geo.make(state=09, county="*", county.subdivision = "*")
yearlist=c(2016)
span = 5
col.names="pretty" 
key="ed0e58d2538fb239f51e01643745e83f380582d7"
options(scipen=999)

town_data <- data.table()
variable =list()      
for (k in seq_along(1:49)) {
 number = number=paste0("B01001", "_", sprintf("%03d",k))
 variable = c(variable, number)
 k=k+1
}  
variable <- as.character(variable)    
acsdata <- acs.fetch(geography=geography, endyear=2016, span=span, 
                    variable = variable, key=key)

pops <- data.table()
    year <- acsdata@endyear
    pop.total <- acsSum(acsdata, 1, "Total")
    pop.total.m <- acsSum(acsdata, 2, "Total Male")
    pop.under10.m <- acsSum(acsdata, c(3,4), "0 to 9 years Male") 
    pop.10to19.m <- acsSum(acsdata, c(5,6,7), "10 to 19 years Male")
    pop.20to29.m <- acsSum(acsdata, c(8,9,10,11), "20 to 29 years Male")
    pop.30to44.m <- acsSum(acsdata, c(12,13,14), "30 to 44 years Male")
    pop.45to64.m <- acsSum(acsdata, c(15,16,17,18,19), "45 to 64 years Male")
    pop.65plus.m <- acsSum(acsdata, c(20,21,22,23,24,25), "65 plus years Male")
    pop.total.f <- acsSum(acsdata, 26, "Total Female")
    pop.under10.f <- acsSum(acsdata, c(27,28), "0 to 9 years Female")
    pop.10to19.f <- acsSum(acsdata, c(29,30,31), "10 to 19 years Female")
    pop.20to29.f <- acsSum(acsdata, c(32,33,34,35), "20 to 29 years Female")    
    pop.30to44.f <- acsSum(acsdata, c(36,37,38), "30 to 44 years Female")    
    pop.45to64.f <- acsSum(acsdata, c(39,40,41,42,43), "45 to 64 years Female")    
    pop.65plus.f <- acsSum(acsdata, c(44,45,46,47,48,49), "65 plus years Female")    
    pop.under10 <- acsSum(acsdata, c(3,4,27,28), "0 to 9 years")
    pop.10to19 <- acsSum(acsdata, c(5,6,7,29,30,31), "10 to 19 years")
    pop.20to29 <- acsSum(acsdata, c(8,9,10,11,32,33,34,35), "20 to 29 years")
    pop.30to44 <- acsSum(acsdata, c(12,13,14,36,37,38), "30 to 44 years")
    pop.45to64 <- acsSum(acsdata, c(15,16,17,18,19,39,40,41,42,43), "45 to 64 years")
    pop.65plus <- acsSum(acsdata, c(20,21,22,23,24,25,44,45,46,47,48,49), "65 plus years")
    geo <- acsdata@geography

    estimates <- data.table(
        geo,
        Year = year,
        estimate(pop.total),
        estimate(pop.total.m),
        estimate(pop.under10.m),
        estimate(pop.10to19.m),
        estimate(pop.20to29.m),
        estimate(pop.30to44.m),
        estimate(pop.45to64.m),
        estimate(pop.65plus.m),
        estimate(pop.total.f),
        estimate(pop.under10.f),
        estimate(pop.10to19.f),
        estimate(pop.20to29.f),
        estimate(pop.30to44.f),
        estimate(pop.45to64.f),
        estimate(pop.65plus.f),
        estimate(pop.under10),
        estimate(pop.10to19),
        estimate(pop.20to29),
        estimate(pop.30to44),
        estimate(pop.45to64),
        estimate(pop.65plus)
    )

    estimates <- melt(
        estimates,
        id.vars = c("NAME", "state", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "Pop",
        value.factor = F
    )
    
estimates$`Age Range`[which(estimates$`Age Range` == "B01001_001")] <-  "Total"
estimates$`Age Range`[which(estimates$`Age Range` == "B01001_002")] <-  "Total Male"
estimates$`Age Range`[which(estimates$`Age Range` == "B01001_026")] <-  "Total Female"


    moes <- data.table(
        geo,
        Year = year,
        standard.error(pop.total) * 1.645,
        standard.error(pop.total.m) * 1.645,
        standard.error(pop.under10.m) * 1.645,
        standard.error(pop.10to19.m) * 1.645,
        standard.error(pop.20to29.m) * 1.645,
        standard.error(pop.30to44.m) * 1.645,
        standard.error(pop.45to64.m) * 1.645,
        standard.error(pop.65plus.m) * 1.645,
        standard.error(pop.total.f) * 1.645,
        standard.error(pop.under10.f) * 1.645,
        standard.error(pop.10to19.f) * 1.645,
        standard.error(pop.20to29.f) * 1.645,
        standard.error(pop.30to44.f) * 1.645,
        standard.error(pop.45to64.f) * 1.645,
        standard.error(pop.65plus.f) * 1.645,
        standard.error(pop.under10) * 1.645,
        standard.error(pop.10to19) * 1.645,
        standard.error(pop.20to29) * 1.645,
        standard.error(pop.30to44) * 1.645,
        standard.error(pop.45to64) * 1.645,
        standard.error(pop.65plus) * 1.645        
    )

    moes <- melt(
        moes,
        id.vars = c("NAME", "state", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "MOE",
        value.factor = F
    )
    
moes$`Age Range`[which(moes$`Age Range` == "B01001_001")] <-  "Total"
moes$`Age Range`[which(moes$`Age Range` == "B01001_002")] <-  "Total Male"
moes$`Age Range`[which(moes$`Age Range` == "B01001_026")] <-  "Total Female"


    setkey(estimates, state, Year, `Age Range`)
    setkey(moes, state, Year, `Age Range`)

    pops_town <- merge(estimates, moes, by = c("NAME", "state", "Year", "Age Range"))


    pops <- rbind(pops_state, pops_town)
    
    
# Write to File
write.table(
    pops,
    file.path(getwd(), "raw", "populations_by_age.csv"),
    sep = ",",
    row.names = F
)