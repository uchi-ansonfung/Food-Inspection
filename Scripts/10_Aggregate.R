
# Data Acquisition --------------------------------------------------------

abq <- read.csv("Data/Cleaned_Datasets/Abq_inspect.csv")
bos <- read.csv("Data/Cleaned_Datasets/Bos_inspect.csv")
chi <- read.csv("Data/Cleaned_Datasets/Chi_inspect.csv")
louisv <- read.csv("Data/Cleaned_Datasets/Louisv_inspect.csv")

# Data Cleaning -----------------------------------------------------------

##drop useless variables
##rename columns to match
abq <- abq %>% select(FACILITY_NAME, FACILITY_KEY, OWNER_KEY, INSPECTION_DATE, SERIAL_NUM, 
                      criticalct, seriousct, minorct, totalct, notminorct, type_mobile, 
                      prcp, tavg, tmax, tmin, GEOID) %>%
  rename("FacilityName" = "FACILITY_NAME") %>% rename("FacilityID" = "FACILITY_KEY") %>%
  rename("OwnerID" = "OWNER_KEY") %>% rename("InspectDate" = "INSPECTION_DATE") %>%
  rename("InspectID" = "SERIAL_NUM")

bos <- bos %>% select(businessname, zip, licenseno, resultdttm, 
                      criticalct, seriousct, minorct, totalct, notminorct, type_mobile,
                       prcp, tavg, tmax, tmin, GEOID) %>%
  rename("FacilityName" = "businessname") %>% rename("FacilityID" = "zip") %>%
  rename("OwnerID" = "licenseno") %>% rename("InspectDate" = "resultdttm")

chi <- chi %>% select(dba_name, license, ACCOUNT.NUMBER, inspection_date, inspection_id, 
                      criticalct, seriousct, minorct, totalct, notminorct, type_mobile, 
                      prcp, tavg, tmax, tmin, GEOID) %>%
  rename("FacilityName" = "dba_name") %>% rename("FacilityID" = "license") %>%
  rename("OwnerID" = "ACCOUNT.NUMBER") %>% rename("InspectDate" = "inspection_date") %>%
  rename("InspectID" = "inspection_id")

louisv <- louisv %>% select(PremiseName, EstablishmentID, InspectionDate,InspectionID, 
                            criticalct, noncriticalct, totalct, type_mobile, proxban,
                            prcp, tavg, tmax, tmin) %>%
  rename("FacilityName" = "PremiseName") %>% rename("FacilityID" = "EstablishmentID") %>% 
  rename("InspectDate" = "InspectionDate") %>% rename("InspectID" = "InspectionID") 

# Data Aggregation --------------------------------------------------------

##ensure ID rows stay unique to each locale
###abq
abq$FacilityID <- str_prefix(abq$FacilityID, "abq.")
abq$OwnerID <- str_prefix(abq$OwnerID, "abq.")
abq$InspectID <- str_prefix(abq$InspectID, "abq.")
###bos
bos$FacilityID <- str_prefix(bos$FacilityID, "bos.")
bos$OwnerID <- str_prefix(bos$OwnerID, "bos.")
###chi
chi$FacilityID <- str_prefix(chi$FacilityID, "chi.")
chi$OwnerID <- str_prefix(chi$OwnerID, "chi.")
chi$InspectID <- str_prefix(chi$InspectID, "chi.")
###louisv
louisv$FacilityID <- str_prefix(louisv$FacilityID, "louisv.")
louisv$InspectID <- str_prefix(louisv$InspectID, "louisv.")
  
##ensure column types match
###all dates go to Date
abq$InspectDate <- as.Date(abq$InspectDate, "%Y-%m-%d")
bos$InspectDate <- as.Date(bos$InspectDate, "%Y-%m-%d")
chi$InspectDate <- as.Date(chi$InspectDate, "%Y-%m-%d")
louisv$InspectDate <- as.Date(louisv$InspectDate, "%Y-%m-%d")

###all FacilityID go to factor
abq$FacilityID <- as.factor(abq$FacilityID)
bos$FacilityID <- as.factor(bos$FacilityID)
chi$FacilityID <- as.factor(chi$FacilityID)
louisv$FacilityID <- as.factor(louisv$FacilityID)

###all OwnerID go to factor
abq$OwnerID <- as.factor(abq$OwnerID)
bos$OwnerID <- as.factor(bos$OwnerID)
chi$OwnerID <- as.factor(chi$OwnerID)

###all InspectID go to factor
abq$InspectID <- as.factor(abq$InspectID)
chi$InspectID <- as.factor(chi$InspectID)
louisv$InspectID <- as.factor(louisv$InspectID)

##tag each locale before combination
abq$locale <- "abq"
bos$locale <- "bos"
chi$locale <- "chi"
louisv$locale <- "louisv"

##finally, combine into master file
master <- bind_rows(abq, bos, chi, louisv)
master$locale <- as.factor(master$locale)


# Link ACS Data --------------------------------------------------

##pull ACS variables of interest
acsvars <- get_acs(state = c("NM","MA","IL","KY"), geography = "tract", 
                   variables = c("B19013_001", #median income
                                 "B15003_017", #at least high school diploma
                                 "B01002_001", #median age
                                 "B03002_003","B03002_012", #white, hispanic
                                 "B15003_001","B03002_001"), #summary var edu, race
                   survey = "acs5", output = "wide") %>%
  rename("income" = "B19013_001E") %>%  rename("edu_hs" = "B15003_017E") %>% rename("age" = "B01002_001E") %>%
  rename("white" = "B03002_003E") %>% rename("hispanic" = "B03002_012E") %>% 
  rename("sum_edu" = "B15003_001E") %>% rename("sum_race" = "B03002_001E") %>%
  select(GEOID, income, edu_hs, age, white, hispanic, sum_edu, sum_race)

acsvars$pct_white <- (acsvars$white/acsvars$sum_race)
acsvars$pct_hispanic <- (acsvars$hispanic/acsvars$sum_race)
acsvars$pct_highschool <- (acsvars$edu_hs/acsvars$sum_edu)


##link by GEOID to master file
acsvars$GEOID <- as.numeric(acsvars$GEOID)
master <- left_join(master, acsvars) %>% select(-white,-hispanic,-edu_hs)

##split the master file
mabq <- subset(master, locale == "abq")
mbos <- subset(master, locale == "bos")
mchi <- subset(master, locale == "chi")
mlouisv <- subset(master, locale == "louisv")

##create discrete date variables
mabq$dotw <- as.factor(weekdays(mabq$InspectDate))
mbos$dotw <- as.factor(weekdays(mbos$InspectDate))
mchi$dotw <- as.factor(weekdays(mchi$InspectDate))
mlouisv$dotw <- as.factor(weekdays(mlouisv$InspectDate))

mabq$month <- lubridate::month(mabq$InspectDate, label= TRUE)
mbos$month <- lubridate::month(mbos$InspectDate, label= TRUE)
mchi$month <- lubridate::month(mchi$InspectDate, label= TRUE)
mlouisv$month <- lubridate::month(mlouisv$InspectDate, label= TRUE)

mabq$year <- as.factor(format(mabq$InspectDate, "%Y"))
mbos$year <- as.factor(format(mbos$InspectDate, "%Y"))
mchi$year <- as.factor(format(mchi$InspectDate, "%Y"))
mlouisv$year <- as.factor(format(mlouisv$InspectDate, "%Y"))

##dummy dates
mabq <- dummy_cols(mabq, select_columns = c("dotw","month","year"), remove_first_dummy = T)
mbos <- dummy_cols(mbos, select_columns = c("dotw","month","year"), remove_first_dummy = T)
mchi <- dummy_cols(mchi, select_columns = c("dotw","month","year"), remove_first_dummy = T)
mlouisv <- dummy_cols(mlouisv, select_columns = c("dotw","month","year"), remove_first_dummy = T)

# Descriptive Analysis ----------------------------------------------------

##barplot of total violations per inspection
totalct_hist <- ggplot(master) + geom_bar(mapping = aes(totalct)) +
  labs(title="All Jurisdictions Total Violation Counts", x = "Violations", y = "Establishments") + 
  scale_x_continuous(breaks = pretty_breaks()) + theme_stata()
###heavy skew towards no violations
##display and save output
print(totalct_hist)
ggsave(filename = "Outputs/Graphs/Agg_Bar_TotalViolations.png", plot = totalct_hist)

##barplot of serious and critical violations per inspection
notminorct_hist <- ggplot(master) + geom_bar(mapping = aes(notminorct)) +
  labs(title="All Jurisdictions Serious and Critical Violation Counts", x = "Violations", y = "Establishments") + 
  scale_x_continuous(breaks = pretty_breaks()) + theme_stata()
##display and save output
print(notminorct_hist)
ggsave(filename = "Outputs/Graphs/Agg_Bar_NotMinorViolations.png", plot = notminorct_hist)

##bivariate relationship between type_mobile and total violations
##there are weird outliers, all from boston.
master$factor_mobile <- as_factor(master$type_mobile) %>% recode_factor("0" = "Fixed", "1" = "Mobile")
bp_viol <- ggplot(master, aes(x = factor_mobile, y = totalct)) + geom_boxplot(outlier.alpha = 0.1) +
  labs(title="Total Violation Counts", x = "Establishment Type", y = "Violations") + 
  theme_stata()
print(bp_viol)
ggsave(filename = "Outputs/Graphs/Box_TotalViolations.png", plot = bp_viol)

##bivariate relationship between type_mobile and total violations
##again, weird outliers, all from boston.
bp_critical <- ggplot(master, aes(x = factor_mobile, y = notminorct)) + geom_boxplot(outlier.alpha = 0.1) +
  labs(title="Serious and Critical Violation Counts", x = "Establishment Type", y = "Violations") + 
  theme_stata()
print(bp_critical)
ggsave(filename = "Outputs/Graphs/Box_NotMinorViolations.png", plot = bp_critical)

# Cleanup -----------------------------------------------------------------

write.csv(master,"Data/Cleaned_Datasets/master.csv")
write.csv(mabq,"Data/Cleaned_Datasets/mabq.csv")
write.csv(mbos,"Data/Cleaned_Datasets/mbos.csv")
write.csv(mchi,"Data/Cleaned_Datasets/mchi.csv")
write.csv(mlouisv,"Data/Cleaned_Datasets/mlouisv.csv")

rm(abq, bos, chi, louisv, acsvars, all_acsvars, station_data)
gc()
