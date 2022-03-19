
# Data Acquisition --------------------------------------------------------

#pull data from Louisville Open Access Data Portal with Readr
bizraw <- readr::read_csv("https://lky-open-data.s3.amazonaws.com/LMPHW/Establishments.csv")
inspectraw <- readr::read_csv("https://lky-open-data.s3.amazonaws.com/LMPHW/Inspections.csv")
violraw <- readr::read_csv("https://lky-open-data.s3.amazonaws.com/LMPHW/Inspection_Violations.csv")
#quick summary to check structure
str(bizraw)
str(inspectraw)
str(violraw)

# Data Cleaning -----------------------------------------------------------

##For each inspection, add business attributes
inspectbiz <- left_join(inspectraw, bizraw, by = "EstablishmentID")

##Violation dataset appears incomplete.  For inspections with discrete violation data, add inspection and business attributes.
violraw <- rename(violraw, InspectionID = inspection_id)
violinspectbiz <- left_join(violraw, inspectbiz, by = "InspectionID")

##remove raw data to save memory
rm(bizraw,inspectraw,violraw)

##trim data
###https://louisvilleky.gov/government/health-wellness/public-health-protection-and-food-safety-fee-schedule (Defines RCode/RCodeDesc)
###https://louisvilleky.gov/government/health-wellness/services/street-vendor-information (Explains EstType)

##Trim data to only Food Service Establishments and Retail/Food Service 
inspectbiz <- inspectbiz %>% filter(RCode == 605 | RCode == 607)
violinspectbiz <- violinspectbiz %>% filter (RCode == 605 | RCode == 607)


# Data Mutation -----------------------------------------------------------

##Kentucky violation weights here: https://chfs.ky.gov/agencies/dph/dphps/fsb/FSDocs/dfs208.pdf

##For violation dataset, create count variable of critical and noncritical counts
by_inspect <- violinspectbiz %>% group_by(InspectionID)

noncriticalct <- by_inspect %>% 
  filter(critical_yn == 'No') %>%
  count(critical_yn) %>%
  rename(noncriticalct = n)

criticalct <- by_inspect %>% 
  filter(critical_yn == 'Yes') %>%
  count(critical_yn) %>%
  rename(criticalct = n)

by_inspect <-  left_join(by_inspect,noncriticalct, by = "InspectionID")
by_inspect <- left_join(by_inspect,criticalct, by = "InspectionID")

##remove intermediate dataframes and garbage collect to save memory
rm(noncriticalct, criticalct)
gc()

##since the dataframe is ungrouped again, we have repeated observations that must be removed.
by_inspect <- by_inspect %>% group_by(InspectionID) %>%
  select(-comments.x, -comments.y) %>%
  distinct(across(InspectionID, .fns = NULL),.keep_all = TRUE)

##NA counts in by_inspect noncritical/critical counts are actually 0.
by_inspect <- by_inspect %>% mutate(noncriticalct = ifelse(is.na(noncriticalct),0, noncriticalct),
                                    criticalct = ifelse(is.na(criticalct),0, criticalct))

##compute aggregate violations.
by_inspect$totalct <- by_inspect$noncriticalct + by_inspect$criticalct

##create dummy for food trucks
##food trucks are listed as "SELF-CONTAINED MOBILE FOOD UNITS."
by_inspect$type_mobile <- ifelse(by_inspect$EstType == 'SELF-CONTAINED MOBILE FOOD UNITS', 1, 0)
inspectbiz$type_mobile <- ifelse(inspectbiz$EstType == 'SELF-CONTAINED MOBILE FOOD UNITS', 1, 0)
by_inspect$type_mobile <- as.logical(by_inspect$type_mobile)
inspectbiz$type_mobile <- as.logical(inspectbiz$type_mobile)

##we aren't interested in reinspections, per OBS those are groomed
unique(by_inspect$IsFollowUpInsp)
unique(by_inspect$InspectionType)
unique(inspectbiz$IsFollowUpInsp)
unique(inspectbiz$InspectionType)

##remove followup inspections (double neg filter not needed since no NA from unique result)
by_inspect <- by_inspect %>% filter(IsFollowUpInsp == 'NO' & !InspectionType == 'FOLLOWUP')
inspectbiz <- inspectbiz %>% filter(IsFollowUpInsp == 'NO' & !InspectionType == 'FOLLOWUP')
##check again
unique(by_inspect$InspectionType)
unique(inspectbiz$InspectionType)
unique(by_inspect$IsFollowUpInsp)
unique(inspectbiz$IsFollowUpInsp)

##Louisville repealed its proximity ban on 3/23/2018, which had prohibited food trucks from operating
##within 150ft of a brick and mortar establishment serving food.  Create dummy variable.
###change inspect_date format
by_inspect$InspectionDate <- as.POSIXlt(by_inspect$InspectionDate)
inspectbiz$InspectionDate <- as.POSIXlt(inspectbiz$InspectionDate)
###create dummy variable.
by_inspect <- by_inspect %>% ungroup()  %>% mutate(proxban = InspectionDate < as.POSIXlt("2018-03-23") & type_mobile == TRUE)
inspectbiz <- inspectbiz %>% ungroup()  %>% mutate(proxban = InspectionDate < as.POSIXlt("2018-03-23") & type_mobile == TRUE)

# Link Weather Data -------------------------------------------------------

#change id column to character to make rnoaa happy
by_inspect$id <- as.character(by_inspect$id)
inspectbiz <- tibble::rowid_to_column(inspectbiz, "id")
inspectbiz$id <- as.character(inspectbiz$id)

#get closest weather station to each location
nearby <- meteo_nearby_stations(lat_lon_df = by_inspect, lat_colname = 'latitude', lon_colname = 'longitude', 
                                station_data = station_data, var = "TAVG", limit = 1)
nearby2 <- meteo_nearby_stations(lat_lon_df = inspectbiz, lat_colname = 'latitude', lon_colname = 'longitude', 
                                 station_data = station_data, var = "TAVG", limit = 1)
nearby <- Map(as.data.frame, nearby)
nearby2 <- Map(as.data.frame, nearby2)
nearby <- bind_rows(nearby, .id = "tib")
nearby2 <- bind_rows(nearby, .id = "tib")

#bind station data to by_inspect
by_inspect <- left_join(by_inspect, nearby, by = c("id" = "tib"))
inspectbiz <- left_join(inspectbiz, nearby2, by = c("id" = "tib"))

#pull weather
weather <- meteo_pull_monitors(monitors = paste(unique(by_inspect$id.y)),
                            date_min = paste(min(by_inspect$InspectionDate)), 
                            date_max = paste(max(by_inspect$InspectionDate)),
                            var = c("prcp","tmax", "tavg","tmin"))
weather2 <- meteo_pull_monitors(monitors = paste(unique(inspectbiz$id.y)),
                               date_min = paste(min(inspectbiz$InspectionDate)), 
                               date_max = paste(max(inspectbiz$InspectionDate)),
                               var = c("prcp","tmax", "tavg","tmin"))

#selective join weather data to by_inspect
by_inspect <- left_join(by_inspect,weather, by = c("InspectionDate" = "date", "id.y" = "id"))
inspectbiz <- left_join(inspectbiz,weather2, by = c("InspectionDate" = "date", "id.y" = "id"))

# Link Census Geography ----------------------------------------------------------------

ky <- tracts(state = "KY")

by_inspect <- by_inspect %>% st_as_sf(coords = c("longitude.x","latitude.x"), na.fail = FALSE, crs = st_crs(ky))
by_inspect$tract <- as.numeric(st_within(by_inspect, ky))
by_inspect <- st_join(by_inspect, ky)

by_inspect <- st_drop_geometry(by_inspect)

inspectbiz <- inspectbiz %>% st_as_sf(coords = c("longitude.x","latitude.x"), na.fail = FALSE, crs = st_crs(ky))
inspectbiz$tract <- as.numeric(st_within(inspectbiz, ky))
inspectbiz <- st_join(inspectbiz, ky)

inspectbiz <- st_drop_geometry(inspectbiz)

# Descriptive Visualization -----------------------------------------------

##barplot of total violations per inspection
totalct_hist <- ggplot(by_inspect) + geom_bar(mapping = aes(totalct)) +
  labs(title="Louisville, KY Total Violation Counts", x = "Violations", y = "Establishments") + 
  scale_x_continuous(breaks = pretty_breaks()) + theme_stata()
###heavy skew towards no violations
##display and save output
print(totalct_hist)
ggsave(filename = "Outputs/Graphs/Louisv_Bar_TotalViolations.png", plot = totalct_hist)

##barplot of critical violations per inspection
criticalct_hist <- ggplot(by_inspect) + geom_bar(mapping = aes(criticalct)) +
  labs(title="Louisville, KY Critical Violation Counts", x = "Violations", y = "Establishments") + 
  scale_x_continuous(breaks = pretty_breaks()) + theme_stata()
##heavy skew towards no violations
##display and save output
print(criticalct_hist)
ggsave(filename = "Outputs/Graphs/Louisv_Bar_CriticalViolations.png", plot = criticalct_hist)


# Cleanup and Multi-City Setup --------------------------------------------

##write to disk with city prefix
write.csv(by_inspect, "Data/Cleaned_Datasets/Louisv_inspect.csv")
write.csv(inspectbiz, "Data/Cleaned_Datasets/Louisv_grade.csv")

##save memory by removing unneeded data
rm(list = ls())
gc()
