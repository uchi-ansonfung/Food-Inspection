
# Data Acquisition --------------------------------------------------------

#pull data from Boston Open Access Data Portal with Readr
inspectraw <- readr::read_csv("https://data.boston.gov/dataset/03693648-2c62-4a2c-a4ec-48de2ee14e18/resource/4582bec6-2b4f-4f9e-bc55-cbaa73117f4c/download/tmpakyoz7n0.csv")
#quick summary to check structure
str(inspectraw)
##create_report(inspectraw)


# Data Cleaning -----------------------------------------------------------

##remove Retail Stores
inspectraw <- subset(inspectraw, descript!="Retail Food")
##remove places without property_ids
inspectraw <- subset(inspectraw, !is.na(property_id))

##Boston gov website indicates *** is most severe violation, * is least severe. Recode viollevel to numeric, with 3 as most severe and 1 as least severe.
inspectraw$viollevel <- str_replace(inspectraw$viollevel, "^\\*{1}$", "1")
inspectraw$viollevel <- str_replace(inspectraw$viollevel, "^\\*{2}$", "2")
inspectraw$viollevel <- str_replace(inspectraw$viollevel, "^\\*{3}$", "3")

##Remove nonsense viollevels and NA
unique(inspectraw$viollevel)
inspect <- inspectraw %>%
  filter(!viollevel == '1919') %>%
##WARNING: NAs not completely meaningless here.  A restaurant with perfect inspection (no violations) can be marked as NA for violation category.  
##But NAs also include businesses where no inspection occurred, and those will be removed.  Filter from result var for successful inspections HE_Pass and Pass
  filter(!(is.na(viollevel) & !result == 'HE_Pass') | (is.na(viollevel) & !result == 'Pass'))


# Data Mutation -----------------------------------------------------------

##Violations within each inspection are listed in separate rows.  Aggregate so each row is one inspection with vars licenseno and resultdttm.
by_inspect <- inspect %>% group_by(licenseno, resultdttm)

minorct <- by_inspect %>% 
  filter(viollevel == 1) %>%
  count(viollevel) %>%
  rename(minor = n)

seriousct <- by_inspect %>% 
  filter(viollevel == 2) %>%
  count(viollevel) %>%
  rename(serious = n)

criticalct <- by_inspect %>% 
  filter(viollevel == 3) %>%
  count(viollevel) %>%
  rename(critical = n)

by_inspect <- left_join(by_inspect,minorct, by = c('licenseno','resultdttm')) 
by_inspect <-  left_join(by_inspect,seriousct, by = c('licenseno','resultdttm'))
by_inspect <- left_join(by_inspect,criticalct, by = c('licenseno','resultdttm'))

##remove intermediate dataframes and garbage collect to save memory
rm(minorct, seriousct, criticalct)
gc()

##since the dataframe is ungrouped again, we have repeated observations that must be removed.
by_inspect <- by_inspect %>% group_by(licenseno, resultdttm) %>%
  select(-viollevel.x,-viollevel.y,-viollevel.x.x,-viollevel.y.y,-violdesc,-comments) %>%
  distinct(across(licenseno, resultdttm, .fns = NULL),.keep_all = TRUE)

##NA counts in by_inspect violation counts are actually 0.
by_inspect <- by_inspect %>% mutate(minor = ifelse(is.na(minor),0, minor),
                                    serious = ifelse(is.na(serious),0, serious),
                                    critical = ifelse(is.na(critical),0, critical))

by_inspect <- by_inspect %>%
  rename("minorct" = "minor") %>%
  rename("seriousct" = "serious") %>%
  rename("criticalct" = "critical")

##compute aggregate violations.
by_inspect$totalct <- by_inspect$minorct + by_inspect$seriousct + by_inspect$criticalct
by_inspect$notminorct <- by_inspect$seriousct + by_inspect$criticalct

##create dummy for food trucks
by_inspect$type_mobile <- ifelse(by_inspect$licensecat == 'MFW', 1, 0)


# Link Weather Data -------------------------------------------------------

##split location to discrete lat and long vars
by_inspect <- by_inspect %>% separate(col = location, into = c("lat","long"), sep = "\\,")
##clean lat and long vars
by_inspect$lat <- str_replace(by_inspect$lat, "\\(", "")
by_inspect$long <- str_replace(by_inspect$long, "\\)", "")
by_inspect$long <- str_replace(by_inspect$long, " ", "")

##add id column to make rnoaa happy
by_inspect <- tibble::rowid_to_column(by_inspect, "id")
by_inspect$id <- as.character(by_inspect$id)

##get closest weather station to each location
nearby <- meteo_nearby_stations(lat_lon_df = by_inspect, lat_colname = 'lat', lon_colname = 'long', 
                                var = "TAVG", station_data = station_data, limit = 1)
nearby <- Map(as.data.frame, nearby)
nearby <- bind_rows(nearby, .id = "tib")

#bind nearby station id to inspections
by_inspect <- left_join(by_inspect, nearby, by = c("id" = "tib"))

#save memory
rm(nearby)
gc()

#create date var without time
by_inspect$weather_date <- as.Date(by_inspect$resultdttm)

#pull weather
weather <- meteo_pull_monitors(monitors = paste(unique(by_inspect$id.y)),
                               date_min = paste(min(by_inspect$weather_date, na.rm = TRUE)),
                               date_max = paste(max(by_inspect$weather_date, na.rm = TRUE)),
                               var = c("prcp","tavg","tmax","tmin"))

#selective join weather data to by_inspect
by_inspect <- left_join(by_inspect,weather, by = c("weather_date" = "date", "id.y" = "id"))


# Link Census Geography ----------------------------------------------------------------

ma <- tigris::tracts(state = "MA")

by_inspect <- by_inspect %>% st_as_sf(coords = c("long","lat"), na.fail = FALSE, crs = st_crs(ma))
by_inspect$tract <- as.numeric(st_within(by_inspect, ma))
by_inspect <- st_join(by_inspect, ma)

by_inspect <- st_drop_geometry(by_inspect)

 
# Descriptive Visualization -----------------------------------------------

##barplot of total violations per inspection
totalct_hist <- ggplot(by_inspect) + geom_bar(mapping = aes(totalct)) +
  labs(title="Boston, MA Total Violation Counts", x = "Violations", y = "Establishments") + 
  scale_x_continuous(breaks = pretty_breaks()) + theme_stata()
###heavy skew towards no violations
##display and save output
print(totalct_hist)
ggsave(filename = "Outputs/Graphs/Bos_Bar_TotalViolations.png", plot = totalct_hist)

##barplot of serious and critical violations per inspection
notminorct_hist <- ggplot(by_inspect) + geom_bar(mapping = aes(notminorct)) +
  labs(title="Boston, MA Serious and Critical Violation Counts", x = "Violations", y = "Establishments") + 
  scale_x_continuous(breaks = pretty_breaks()) + theme_stata()
##heavy skew towards no violations
##display and save output
print(notminorct_hist)
ggsave(filename = "Outputs/Graphs/Bos_Bar_NotMinorViolations.png", plot = notminorct_hist)


# Cleanup and Multi-City Setup --------------------------------------------

##write by_inspect to disk with city prefix
write.csv(by_inspect, "Data/Cleaned_Datasets/Bos_Inspect.csv")

##save memory by removing unneeded data
rm(by_inspect, inspect, inspectraw)
gc()
