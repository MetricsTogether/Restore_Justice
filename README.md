# Restore_Justice
Data tools to digest IDOC data for Restore Justice

## Important Files 
Simple bullet points • denote files while other bullet point denote folders. 
	ShinyApp
	www
	rsconnect
	Prison Stock Reports
•	IL_Counties.csv
•	OMR.csv
•	filenames.csv
•	app.R
•	fullIDOC.rds
•	New Data Ingest.R
	Website Images
•	Restore_Justice.Rproj
•	Webpage Update.Rmd

## Overview of R and RShiny
R - a freely available programming language and environment for statistical computing and graphics which provides a wide variety of statistical and graphical techniques
R Studio – a user interface used to more easily interact with R including a console, editor, and visual pane for displaying plots, history, debugging and workspace. 
CRAN - network of ftp and web servers around the world that store identical, up-to-date, versions of code and documentation for R.
The beauty of R is that anything you have a question on can usually be googled and there are many free books and resources available online, one of my favorites for how to tidy and wrangle data is R for Data Science. 

### Important things to note
1.	R is an Object based language, meaning, all the things you commit to memory are stored as named “objects”. Naming your objects well can make or break having easily understandable code versus having not so easy to understand code. Objects (and all things in R) are case sensitive so choose your capitals wisely. 

### Installing Packages
One of the greatest things about open-source software like R is the community that makes it better. New people create useful packages for R every day that you can download to do all sorts of things, from statistical analysis, to data visualization, to working with TrueNumbers! There are two sources from which most packages are downloaded, CRAN and GitHub. This document will only focus on downloading CRAN packages as those are the only packages used for the IDOV data dashboard. 

Installing from CRAN

Installing packages from CRAN is easy, all you need to know is the name of the package you’re looking to download. Let’s take downloading the package “tidyverse” as an example:

install.packages(“tidyverse”)

Within the code, all the packages needed are already loaded into the space. When running the dashboard code for the first time, you will be prompted to install required packages. Choose this and the packages will be automatically installed through R studio. 
### Running Code

### R Shiny 
R shiny helps to take R code into an interactive, web enabled space. The data dashboard is hosted on R Shiny 


## About the Data
The full history of IDOC data is pretty large (it spans from 2005 to now at a semi annual or quarterly cadence) so it is stored as a .rds file (an r data file) in both github and in Google drive. All data is pulled from either the Illinois Department of Corrections (IDOC) or the American Community Survey.
From IDOC we use two key files – the Prison Stock data and the Operations and Management Reports (OMR). The prison stock data contains information about each individual within the Illinois corrections system including their name, date of birth, sentence date, offence, and current prison location. The OMR contains information on incidents within institutions, things like hunger strikes and number of individuals who were sick in a given period. 
For prison stock data, once downloaded, file should be saved in the following format “monthYYYY.xls” (example: “December2023.xls”) in the folder Restore_Justice -> ShinyApp -> Prison Stock Reports. It is best practice to clear this file of 

## The Dashboard

### Data Explorer
For the data explorer tab, each of the identified variables pertaining to individuals currently incarcerated are subset by age and race. 

### Incidents Tool
The data for the Incidents Tool comes from the OMR reports 

### Geographic Explorer
Data from the geographic explorer comes from the 2020 American Community Survey (ACS) compared with IDOC data.
 
### Running Reports for the Restore Justice Website

