# Did you run 00_Setup.R first?
# Data Acquisition --------------------------------------------------------

#pull data locally with UTF-8 encoded version, server version uses UCS-2-LE-BOM encoding and results in weird errors.  
#Reencoded to UTF-8.
inspectraw <- read_csv("Data/Abq_FoodInspectionsReencode.csv")
#quick summary to check structure
str(inspectraw)
##Data layout is weird.  Result_Desc is for the entire inspection, and there's no binary variable for specific violations
##except for notes in Inspection_Memo.  Question 57 and 58 have 0 points associated and aren't actual violations.

# Data Cleaning ------------------------------------------------

#Keeping only food service establishments and mobile food units
inspect <- inspectraw %>% filter(PROGRAM_CATEGORY_DESCRIPTION == "Food Service Establishment" | 
                                   PROGRAM_CATEGORY_DESCRIPTION == "Food Service Establishment-Bar" |
                                   PROGRAM_CATEGORY_DESCRIPTION == "Mobile Food Unit")

#we aren't interested in reinspections, per OBS those are groomed.
inspect <- inspect %>% filter(!INSPECTION_DESC == "FOOD FOLLOW-UP INSPECTION") %>%
#we also don't care for staff training visit or covid response, since those have different grading rubrics
  filter(!INSPECTION_DESC == "STAFF TRAINING" & !INSPECTION_DESC == "COVID RESPONSE") %>%
# some inspections were "ungraded" or didn't occur and, if left in the dataset, create false perfect scores
  filter(!ACTION_DESC == "NON-GRADED" & !ACTION_DESC == "FACILITY CLOSED AT TIME OF SERVICE")
#check with unique result_desc = shows all successful inspections.
#unique(inspect$RESULT_DESC)


# Data Mutation -----------------------------------------------------------

##CREATE VIOLATION COUNTS
###Inspection Form: https://www.cabq.gov/environmentalhealth/documents/CityofAlbuquerqueFOODINSPECTIONFORM.pdf
###Three tiers of point deductions: 6 (critical), 3 (serious), 1 (minor); define the Qs corresponding to the tiers
###split violations into 3 datasets according to tier

criticalq <- c("04  S2", "04  S3", "04  S6", "04  S7", "04  S8", "04  S9", "04 S10", "04 S11", "04 S12", "04 S13", "04 S14", "04 S15", "04 S16",
               "04 S17", "04 S18", "04 S19", "04 S20", "04 S21", "04 S22")
seriousq <- c("04  S1", "04  S4", "04  S5", "04 S23", "04 S24", "04 S25", "04 S27", "04 S31", "04 S32", "04 S33", "04 S34", "04 S36", "04 S37",
              "04 S38", "04 S39", "04 S40","04 S46", "04 S48", "04 S51")
minorq <- c("04 S30", "04 S35", "04 S41", "04 S42", "04 S43", "04 S45", "04 S47", "04 S52", "04 S53", "04 S54")


critical <- inspect %>% filter(VIOLATION_CODE %in% criticalq)
serious <- inspect %>% filter(VIOLATION_CODE %in% seriousq)
minor <- inspect %>% filter(VIOLATION_CODE %in% minorq)

###for each split dataset, a violation occurs when INSPECTION_MEMO is filled.
critical$criticalviol <- if_else(is.na(critical$INSPECTION_MEMO), 0, 1)
serious$seriousviol <- if_else(is.na(serious$INSPECTION_MEMO), 0, 1)
minor$minorviol <- if_else(is.na(minor$INSPECTION_MEMO), 0, 1)

###recombine split datasets with three violation type columns
inspectct <- bind_rows(critical, serious, minor)

###the NAs in combined dataset violation variables are actually 0, since the NA can never be 1 for those questions.
inspectct <- inspectct %>% mutate(criticalviol = ifelse(is.na(criticalviol),0, criticalviol),
                                    seriousviol = ifelse(is.na(seriousviol),0, seriousviol),
                                    minorviol = ifelse(is.na(minorviol),0, minorviol))


###Violations within each inspection are listed in separate rows.  Aggregate so each row is one inspection with INSPECTION_DATE and SERIAL_NUM.
####mutate is used instead of summarize to preserve other columns--some are still meaningful across each inspection, and we'll save those for later.
by_inspect <- inspectct %>%  
  group_by(INSPECTION_DATE, SERIAL_NUM) %>%
  mutate(criticalct = sum(criticalviol)) %>%
  mutate(seriousct = sum(seriousviol)) %>%
  mutate(minorct = sum(minorviol))
  
###remove intermediate dataframes and garbage collect to save memory
rm(minor, serious, critical)
gc()

####since the dataframe is ungrouped again, we have repeated observations that must be removed.
by_inspect <- by_inspect %>% 
  distinct(across(INSPECTION_DATE, SERIAL_NUM, .fns = NULL), .keep_all = TRUE)


#COMPUTE AGGREGATE VIOLATION COUNTS
by_inspect$totalct <- by_inspect$minorct + by_inspect$seriousct + by_inspect$criticalct
by_inspect$notminorct <- by_inspect$seriousct + by_inspect$criticalct

##with 1 line per inspection and aggregate counts, some columns are not meaningful.
by_inspect <- by_inspect %>% select(-VIOLATION_CODE, -VIOLATION_DESC, -INSPECTION_MEMO)

##SORT ESTABLISHMENT TYPES

###create dummy for food trucks
###food trucks are PROGRAM_CATEGORY 1002 and "Mobile Food Unit"
by_inspect$type_mobile <- ifelse(by_inspect$PROGRAM_CATEGORY == 1002 & by_inspect$PROGRAM_CATEGORY_DESCRIPTION == "Mobile Food Unit", 1, 0)


# Link Weather Data -------------------------------------------------------

##unite addresses into single column
by_inspect<- by_inspect %>% unite("FULL_ADDRESS", c('SITE_ADDRESS', 'CITY', 'STATE', 'ZIP'), sep = ',', remove = FALSE)

##change addresses into lat/long with ARCGIS api
##this will take a while, since arcgis free tier doesn't allow batching, but is the most accurate.
#latlong <- geo(address = by_inspect$FULL_ADDRESS, method = 'arcgis', verbose = TRUE)
##we will save the output to disk so we don't have to wait for the api again
#write.csv(latlong,"Data/Geocode/Abq_latlong.csv")

##add id column to latlong to make rnoaa happy
latlong <- tibble::rowid_to_column(latlong, "id")
latlong$id <- as.character(latlong$id)

##get closest weather station to each location
nearby <- meteo_nearby_stations(lat_lon_df = latlong, lat_colname = 'lat', lon_colname = 'long', 
                                station_data = station_data, var = "TAVG", limit = 1)
nearby <- Map(as.data.frame, nearby)
nearby <- bind_rows(nearby, .id = "tib")

#bind nearby station id to latlong
latlong <- left_join(latlong, nearby, by = c("id" = "tib"))
#bind latlong to by_inspect
latlong <- latlong %>% distinct(address, .keep_all = TRUE)
by_inspect <- left_join(by_inspect, latlong, by = c("FULL_ADDRESS" = "address"))

#change inspect_date format
by_inspect$INSPECTION_DATE <- as.Date.character(by_inspect$INSPECTION_DATE, format = "%m/%d/%Y")

#pull weather
weather <- meteo_pull_monitors(monitors = paste(unique(by_inspect$id.y)),
                      date_min = paste(min(by_inspect$INSPECTION_DATE)),
                      date_max = paste(max(by_inspect$INSPECTION_DATE)),
                      var = c("prcp","tavg","tmax","tmin"))


#selective join weather data to by_inspect
by_inspect <- left_join(by_inspect,weather, by = c("INSPECTION_DATE" = "date", "id.y" = "id"))



# Link Census Geography ----------------------------------------------------------------

nm <- tracts(state = "NM")

by_inspect <- by_inspect %>% st_as_sf(coords = c("long","lat"), crs = st_crs(nm))
by_inspect$tract <- as.numeric(st_within(by_inspect, nm))
by_inspect <- st_join(by_inspect, nm)

by_inspect <- st_drop_geometry(by_inspect)

# Descriptive Visualization -----------------------------------------------

##barplot of total violations per inspection
totalct_hist <- ggplot(by_inspect) + geom_bar(mapping = aes(totalct)) +
  labs(title="Albuquerque, NM Total Violation Counts", x = "Violations", y = "Establishments") + 
  scale_x_continuous(breaks = pretty_breaks()) + theme_stata()
###heavy skew towards no violations
##display and save output
print(totalct_hist)
ggsave(filename = "Outputs/Graphs/Abq_Bar_TotalViolations.png", plot = totalct_hist)

##barplot of serious and critical violations per inspection
notminorct_hist <- ggplot(by_inspect) + geom_bar(mapping = aes(notminorct)) +
  labs(title="Albuquerque, NM Serious and Critical Violation Counts", x = "Violations", y = "Establishments") + 
  scale_x_continuous(breaks = pretty_breaks()) + theme_stata()
##heavy skew towards no violations
##display and save output
print(notminorct_hist)
ggsave(filename = "Outputs/Graphs/Abq_Bar_NotMinorViolations.png", plot = notminorct_hist)


# Cleanup and Multi-City Setup --------------------------------------------

##write by_inspect to disk with city prefix
write.csv(by_inspect, "Data/Cleaned_Datasets/Abq_inspect.csv")

##save memory by removing unneeded data
rm(by_inspect, inspect, inspectct, inspectraw, latlong, nearby, notminorct_hist, totalct_hist, weather, criticalq, minorq, seriousq, nm)
gc()
