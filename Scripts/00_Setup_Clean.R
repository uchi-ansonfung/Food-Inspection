# Setup -------------------------------------------------------------------

#load libraries
library(RSocrata)
library(tidyverse)
library(data.table)
library(ggplot2)
library(viridis)
library(ggthemes)
library(scales)
library(rgdal)
library(rnoaa)
library(tidygeocoder)
library(tidycensus)
library(sf)
library(lme4)
library(glmmTMB)
library(sjstats)
library(sjPlot)
library(performance)
library(stringr.tools) #github, requires RTools and package "remote"
library(DataExplorer)
library(dlookr) ##warning: masks diagnose from glmmTMB
library(fastDummies)

#census API key
census_api_key("ADDKEYHERE")

#weather data API key
options(noaakey = "ADDKEYHERE")

#download weather station data, common across all jurisdictions
station_data <- ghcnd_stations()

#load variable table for census data
all_acsvars <- load_variables(2019, "acs5")
#View(all_acsvars)

#set CPU cores for OpenMP to speed up regressions
##X1 Carbon Gen 7 has 6 cores
##Desktop 6700K has 4 cores
options(glmmTMB.cores = 6)
