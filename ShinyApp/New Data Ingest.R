### Run this to update prison files - put all new files in folder "prison stock reports"

## before running this script, delete existing files in the Prison Stock Reports folder and replace them with the new files you'd like to add to the full IDOC file

## this file can update multiple reports at the same time 


library(tidyverse)
library(readxl)
library(lubridate)

filename <- list.files(path="Prison Stock Reports")
for (i in 1:length(filename)){
  file <- read_xls(paste0("Prison Stock Reports/",filename[i]),skip=5)
  file$FileName <- filename[i]
  assign(filename[i],file)
}

IDOCnames <- data.frame()
for(j in length(filename)){
  IDOCnames <- rbind(IDOCnames,names(get(filename[j])))
}

IDOCrename <- function(x){
  names(x) <- IDOCnames
  x}

for(k in 1:length(filename)){
    assign(filename[k],IDOCrename(get(filename[k])))
}

fullfile <- c()
for (l in 1:length(filename)){fullfile <- rbind(fullfile,get(c(filename[l])))}


# Creation of year transform variables to calculate life sentence, Defacto Life Sentence, or SDP status 
fullfile$yeartrans <- ifelse(grepl("^[[:digit:]]+$",fullfile$`Sentence Years`),fullfile$`Sentence Years`,NA)
fullfile$yeartrans <- as.numeric(fullfile$yeartrans)
date_redo <- function(x, year=2023){
  m <- year(x)
  n <- year(x) %% 100
  year(x) <- ifelse(m-year > 0, 1900+n, m)
  x
}



fullfile <- fullfile %>% 
  mutate(age=year(Sys.Date())-year(`Date of Birth`),
         life=ifelse(`Sentence Years`=="LIFE",1,(ifelse(yeartrans>49,"De Facto",0))),
         sdp=ifelse(`Sentence Years`=="SDP",1,0),
         "Sentence Age" = year(fullfile$`Sentence Date`)-year(fullfile$`Date of Birth`),
         "Custody Age" = year(fullfile$`Custody Date`)-year(fullfile$`Date of Birth`)) %>% select(-yeartrans)

fullfile$`Date of Birth` <- as.Date(fullfile$`Date of Birth`)
fullfile$`Current Admission Date` <- as.Date(fullfile$`Current Admission Date`)
fullfile$`Projected Mandatory Supervised Release (MSR) Date` <- as.Date(fullfile$`Projected Mandatory Supervised Release (MSR) Date`)
fullfile$`Projected Discharge Date` <- as.Date(fullfile$`Projected Discharge Date`)
fullfile$`Custody Date` <- as.Date(fullfile$`Custody Date`)
fullfile$`Sentence Date` <- as.Date(fullfile$`Sentence Date`)
fullfile$FileName <- my(fullfile$FileName)
fullfile$FileName <- as.Date(fullfile$FileName)


full_IDOC <- read_rds("fullIDOC.rds")

fullfile <- fullfile %>% relocate(c(names(full_IDOC)))

fullIDOC_new <- rbind(full_IDOC,fullfile)



write_rds(fullIDOC_new,"fullIDOC.rds")

