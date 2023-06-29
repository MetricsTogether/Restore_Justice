library(tnum)
library(censusapi)
library(tidyverse)
library(tigris)
library(jsonlite)

tnum.loadLibs()

creds <- "alison@metricstogether.com:brushfire"
ip <- "metrics.truenum.com:8080"
tnum.authorize(ip=ip,creds=creds)
tnum.setSpace("IL-Corrections1")

##Do not run this. 
#tnum.deleteByQuery(query = "subj:*")


# Ingesting the IDOC Data
library(readxl)

IDOC <- read_xls("Prison Stock Reports/December2020.xls",skip = 5)
names(IDOC)[c(1,3,10,11)] <- c("ID Number","Birth Date","Projected Mandatory Supervised Release Date","Projected Discharge Date")
IDOC$`Sentence Months` <- as.numeric(IDOC$`Sentence Months`)

# function to rewrite wrong birthdates (lubridate)
date_redo <- function(x, year=2022){
  m <- year(x)
  n <- year(x) %% 100
  year(x) <- ifelse(m-year > 0, 1900+n, m)
  x
}

# #2020 
IDOC$`Birth Date` <- ymd(IDOC$`Birth Date`)
IDOC$`Birth Date` <- date_redo(IDOC$`Birth Date`)
IDOC$`Current Admission Date` <- ymd(IDOC$`Current Admission Date`)
IDOC$`Projected Mandatory Supervised Release Date` <- ymd(IDOC$`Projected Mandatory Supervised Release Date`)
IDOC$`Projected Discharge Date` <- ymd(IDOC$`Projected Discharge Date`)
IDOC$`Custody Date`<- ymd(IDOC$`Custody Date`)
IDOC$`Sentence Date` <- ymd(IDOC$`Sentence Date`)

#2017
### Need a date checker
# IDOC$`Birth Date` <- mdy(IDOC$`Birth Date`)
# IDOC$`Birth Date` <- date_redo(IDOC$`Birth Date`)
# IDOC$`Current Admission Date` <- mdy(IDOC$`Current Admission Date`)
# IDOC$`Projected Mandatory Supervised Release Date` <- mdy(IDOC$`Projected Mandatory Supervised Release Date`)
# IDOC$`Projected Discharge Date` <- mdy(IDOC$`Projected Discharge Date`)
# IDOC$`Custody Date`<- mdy(IDOC$`Custody Date`)
# IDOC$`Sentence Date` <- mdy(IDOC$`Sentence Date`)


# 
# 
# template <- list(
#   c("Name of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Name)",
#     "IDOC:2021:March, test"))
# 
# tnum.ingestDataFrame(head(IDOC),template = template,"testIngest3.txt")
# 


#   template_loop <- c()
# for (j in 1:length(IDOC)){
#   template_loop[[(length(template_loop) + 1)]] <- c(paste0(names(IDOC)[j],
#                                                     " of inmate $tkn(ID Number) in $tkn(Parent Institution) prison in $(Sentencing County) county is \"$(",names(IDOC)[j],")\"")
#                                                     ,"IDOC:2022:March,ingest:August19")
# }
  

  
template <- list(c("Name of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is \"$(Name)\"", paste(IDOC_date,",",mydate)),
                 c("Birth Date of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Birth Date)", paste(IDOC_date,",",mydate)),
                 c("Sex of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Sex)",paste(IDOC_date,",",mydate)),
                 c("Race of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Race)",paste(IDOC_date,",",mydate)),
                 c("Veteran Status of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Veteran Status)",paste(IDOC_date,",",mydate)),
                 c("Current Admission Date of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Current Admission Date)",paste(IDOC_date,",",mydate)),
                 c("Admission type of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Admission Type)",paste(IDOC_date,",",mydate)),
                 c("Parent institution of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Parent Institution)",paste(IDOC_date,",",mydate)),
                 c("Projected Mandatory Supervised Release Date of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Projected Mandatory Supervised Release Date)",paste(IDOC_date,",",mydate)),
                 c("Projected Discharge Date of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Projected Discharge Date)",paste(IDOC_date,",",mydate)),
                 c("Custody Date of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Custody Date)",paste(IDOC_date,",",mydate)),
                 c("Sentence Date of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Sentence Date)",paste(IDOC_date,",",mydate)),
                 c("Crime Class of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is \"$(Crime Class)\"",paste(IDOC_date,",",mydate)),
                 c("Holding Offense of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is \"$(Holding Offense)\"",paste(IDOC_date,",",mydate)),
                 c("Holding Offense Category of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is \"$(Holding Offense Category)\"",paste(IDOC_date,",",mydate)),
                 c("Offense Type of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is \"$(Offense Type)\"",paste(IDOC_date,",",mydate)),
                 c("Sentence Years of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Sentence Years)",paste(IDOC_date,",",mydate)),
                 c("Sentence Months of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Sentence Months)",paste(IDOC_date,",",mydate)),
                 c("Truth In Sentencing of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is \"$(Truth in Sentencing)\"",paste(IDOC_date,",",mydate)),
                 c("Sentencing County of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is \"$(Sentencing County)\"",paste(IDOC_date,",",mydate))
          
                 )


## template double check 

test <- foreach(i=names(IDOC), .combine='c') %do% grepl(pattern = i,x = template)
test <- data.frame(matrix(test, nrow=20, ncol=19))
t <- sapply(test,sum)
names(t) <- names(IDOC)
uhoh <- ifelse(sum(t)!=76,"THERE'S AN ERROR","You're good, proceed")
print(uhoh)
ifelse(uhoh=="THERE'S AN ERROR",t[!t%in%c(20,1)],)

## ingest!
  
tnum.ingestDataFrame(IDOC,template = template,paste0("ingest_",IDOC2,"_20221114_1.txt"))

#paste0("ingest_",IDOC2,"_20221114_1.txt")


#	IL-DOC/prison:Big_Muddy_River/A02008:inmate:county:Cook
## check what was ingested

`%notin%` <- negate(`%in%`)

IDOC_new <- IDOC[IDOC$`IDOC #`%notin%data_tnum$identifier,]





# functions for ACS data cleaning
yeargeog <- function(year,state,vars){
  data.frame(year = year, 
             getCensus(name="acs/acs5",
                       vintage=year,
                       vars = vars,
                       key=key,
                       region = "tract:*",
                       regionin = paste0("state:",unique(fips_codes$state_code)[state])
             )
  )
  
}

fips_to_acs <- function(data){
  t <- left_join(data,fips_codes,by=c("state"="state_code","county"="county_code"))
  t
}



key <- "095f223bd2d9dd69b8546222bde6c171c540da9b"

race_vars <- c("B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001G_001E","B01001H_001E","B01001I_001E")

acs5_race_17 <- getCensus(name = "acs/acs5",
                          vintage = 2017,
                          vars = race_vars,
                          key = key,
                          region="county:*",
                          regionin = "state:17")
acs5_race_17$year <- 2017

acs5_race_20 <- getCensus(name = "acs/acs5",
                          vintage = 2020,
                          vars = race_vars,
                          key = key,
                          region="county:*",
                          regionin = "state:17")
acs5_race_20$year <- 2020

acs5_race <- rbind(acs5_race_17,acs5_race_20)

acs5_race <- fips_to_acs(acs5_race)
acs5_race <- acs5_race %>% select (year,state_name,county.y,"B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001G_001E","B01001H_001E","B01001I_001E" )

names(acs5_race) <- c("year","state","county", 
                      "white",
                      "black",
                      "american indian or alaska native",
                      "asian",
                      "native hawaiian or pacific islander",
                      "other",
                      "multi racial",
                      "white non hispanic",
                      "hispanic or latino"
)
templates <- c()

for (i in 5:length(acs5_race)-1) {
  templates[[(length(templates) + 1)]] <- c(paste0(" $(county) $(state) has ",names(acs5_race)[i],  " population = $(",names(acs5_race)[i],")"),"USFederal:ACS5:$(year)")
  i <- i+1
}

tnum.ingestDataFrame(acs5_race,templates,"testIngest1.txt")



##mass tag for deletion
for (i in 1:length(thing$id)){
  tnum.addTag(thing$id[i],"delete")
}

