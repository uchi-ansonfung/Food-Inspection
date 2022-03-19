
# Data Acquisition --------------------------------------------------------

#pull data from Chicago Data Portal with RSocrata
inspectraw <- read.socrata("https://data.cityofchicago.org/resource/4ijn-s7e5.csv", stringsAsFactors = FALSE)
bizraw <- read.socrata("https://data.cityofchicago.org/resource/uupf-x98q.csv", stringsAsFactors = FALSE)
#quick summary to check structure
str(inspectraw)
str(bizraw)


# Data Cleaning -----------------------------------------------------------

##clean up column names
inspectraw <- inspectraw %>%
  rename(license = license_)

##change integers to numeric
inspectraw$inspection_id <- as.numeric(inspectraw$inspection_id)
inspectraw$zip <- as.numeric(inspectraw$zip)
inspectraw$license <- as.numeric(inspectraw$license)

##remove rows without licenses or inspection date
inspect <- inspectraw[!is.na(inspectraw$inspection_date) & !is.na(inspectraw$license),]
##remove rows with license = 0
inspect <- inspect[inspect$license!=0,]
##remove duplicate inspections
inspect <- inspect %>% distinct(inspection_id, .keep_all = TRUE)

##we aren't interested in reinspections, per OBS those are groomed
unique(inspect$inspection_type)
##keeping only "CANVAS" inspections
inspect <- inspect[(inspect$inspection_type == "Canvass") | (inspect$inspection_type == "CANVAS"),]
##check again
unique(inspect$inspection_type)

##not interested in inspections that never happened
unique(inspect$results)
##removing all but Pass, Fail, Pass w/ Conditions
inspect <- inspect[!inspect$results %in% c('Out of Business','Business Not Located','No Entry','Not Ready'),]
##check again
unique(inspect$results)


# Data Mutation -----------------------------------------------------------

##For inspections up to 7/1/18, Chicago used a different rubric for violations.
###split inspect dataset, then recombine.
old <- inspect[inspect$inspection_date<="2018-07-01",]
new <- inspect[inspect$inspection_date>"2018-07-01",]

##Count number of critical, serious, and minor violations per inspection up to 7/1/18.
###critical violations from 1 through 14.
old$criticalct <- str_count(old$violations, "\\b([1-9]|1[0-4])\\.")
###serious violations from 15 through 29.
old$seriousct <- str_count(old$violations, "\\b(1[5-9]|2[0-9])\\.")
###minor violations from 30 through 44, and 70.
old$minorct <- str_count(old$violations, "\\b(3[0-9]|4[0-4]|7[0])\\.")  

##Count number of critical, serious, and minor violations per inspection from 7/2/18 onwards.
###critical violations from 1 through 14.
new$criticalct <- str_count(new$violations, "\\b([1-9]|1[0-4])\\.")
###serious violations from 15-29.
new$seriousct <- str_count(new$violations, "\\b(1[5-9]|2[0-9])\\.")
###minor violations from 30-45.
new$minorct <- str_count(new$violations, "\\b(3[0-9]|4[0-5])\\.")

##recombine old and new.
inspect <- rbind(old,new)

##compute aggregate violations.
inspect$totalct <- inspect$criticalct + inspect$seriousct + inspect$minorct
inspect$notminorct <- inspect$criticalct + inspect$seriousct

##create dummy variable for food trucks.
inspect$type_mobile <- ifelse(str_detect(inspect$facility_type,regex("mobile", ignore_case = TRUE)),1,0)

##link business variables to inspections

bizinspect <- left_join(inspect, bizraw, by = c("license" = "LICENSE.NUMBER"))

##save memory
rm(bizraw, inspect, inspectraw, new, old)
gc()

# Link Weather Data -------------------------------------------------------

##add id column to bizinspect to make rnoaa happy
bizinspect <- tibble::rowid_to_column(bizinspect, "id")
bizinspect$id <- as.character(bizinspect$id)

##get closest weather station to each location
nearby <- meteo_nearby_stations(lat_lon_df = bizinspect, lat_colname = 'latitude', lon_colname = 'longitude', 
                                station_data = station_data, var = "TAVG", limit = 1)
nearby <- Map(as.data.frame, nearby)
nearby <- bind_rows(nearby, .id = "tib")

##bind nearby station id to bizinspect
bizinspect <- left_join(bizinspect, nearby, by = c("id" = "tib"))

##pull weather
weather <- meteo_pull_monitors(monitors = paste(unique(bizinspect$id.y)),
                               date_min = paste(min(bizinspect$inspection_date)),
                               date_max = paste(max(bizinspect$inspection_date)),
                               var = c("prcp","tavg","tmax","tmin"))


#selective join weather data to bizinspect
bizinspect <- left_join(bizinspect,weather, by = c("inspection_date" = "date", "id.y" = "id"))


# Link Census Geography ----------------------------------------------------------------

il <- tracts(state = "IL")

bizinspect <- bizinspect %>% st_as_sf(coords = c("longitude.x","latitude.x"), na.fail = FALSE, crs = st_crs(il))
bizinspect$tract <- as.numeric(st_within(bizinspect, il))
bizinspect <- st_join(bizinspect, il)

bizinspect <- st_drop_geometry(bizinspect)


# Descriptive Visualization -----------------------------------------------

##barplot of total violations per inspection
totalct_hist <- ggplot(bizinspect) + geom_bar(mapping = aes(totalct)) +
  labs(title="Chicago, IL Total Violation Counts", x = "Violations", y = "Establishments") + 
  scale_x_continuous(breaks = pretty_breaks()) + theme_stata()
###heavy skew towards no violations
##display and save output
print(totalct_hist)
ggsave(filename = "Outputs/Graphs/Chi_Bar_TotalViolations.png", plot = totalct_hist)

##barplot of serious and critical violations per inspection
notminorct_hist <- ggplot(bizinspect) + geom_bar(mapping = aes(notminorct)) +
  labs(title="Chicago, IL Serious and Critical Violation Counts", x = "Violations", y = "Establishments") + 
  scale_x_continuous(breaks = pretty_breaks()) + theme_stata()
##heavy skew towards no violations
##display and save output
print(notminorct_hist)
ggsave(filename = "Outputs/Graphs/Chi_Bar_NotMinorViolations.png", plot = notminorct_hist)


# Cleanup and Multi-City Setup --------------------------------------------

##write bizinspect to disk with city prefix
write.csv(bizinspect, "Data/Cleaned_Datasets/Chi_inspect.csv")

##save memory by removing unneeded data
rm(bizinspect, nearby, notminorct_hist, totalct_hist, weather)
gc()




#========================
#MULTILEVEL MIXED EFFECTS
#========================


#preliminary unconditional means model, using License as random effect.
##longitudinal design for multiple inspections per License (Business)
mod1 <- lmer(Results_Binary ~ 1 + (1|License..), ChicagoTrim, REML=F)
summary(mod1)
###results. intercept is 7.353e-01 and logLik is -122067.6. the model has value.

#see if there's intra-class correlation for the unconditional means model
(0.07981^2)/((0.07981^2)+(0.43288))
##results. ICC is 0.0145, questionable case for multilevel modeling, at least for boolean DV