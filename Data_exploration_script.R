#Install and load packages-----------------------

#install and library packages using pacman 
pacman::p_load(pacman, tidyverse, skimr, broom)

#load .csv files---------

#load .csv files from messy data subfolder
chl_a_samples_messy<-read.csv("Messy_data/chl-a-samples-messy.csv")
stations_messy<-read.csv("Messy_data/stations-messy.csv")
waterbodies_messy<-read.csv("Messy_data/waterbodies-messy.csv")


#visualize and explore data--------

#visualize data
head(chl_a_samples_messy)
summary(chl_a_samples_messy)
skim(chl_a_samples_messy)
#here I can see right away that sample_volume_filtered_ml is a character class and should be numeric, 
unique(chl_a_samples_messy$sample_volume_filtered_ml)
#has one value with units that needs to be removed before converting to numeric
#there is a negative value in extracted_volum_ml and it should all be positive,
#absorbence_663nm is a character and should be numeric,
#chl_a_20ed and chl_a_16ed have outlier. Will flag outlier values using standard rule of thumb: abs(x − median) > 5 × MAD
# will not remove outliers as no justification to remove. Will simply flag for future researchers to decide for their case-use.

chl_a_samples_messy_outliers <- chl_a_samples_messy %>%
  mutate(
    # medians
    med_16 = median(chl_a_16ed, na.rm = TRUE),
    med_20 = median(chl_a_20ed, na.rm = TRUE),
    
    # MAD values
    mad_16 = mad(chl_a_16ed, na.rm = TRUE),
    mad_20 = mad(chl_a_20ed, na.rm = TRUE),
    
    # absolute deviations
    abs_dev_16 = abs(chl_a_16ed - med_16),
    abs_dev_20 = abs(chl_a_20ed - med_20),
    
    # outlier flags
    outlier_16 = abs_dev_16 > 5 * mad_16,
    outlier_20 = abs_dev_20 > 5 * mad_20
  ) %>%
  select(
    -med_16, -med_20,
    -mad_16, -mad_20,
    -abs_dev_16, -abs_dev_20
  )
# test for multivariable outliers between chl-a-16 and chl-a-20 using residuals from liniear model
model <- lm(chl_a_20ed ~ chl_a_16ed, data = chl_a_samples_messy)

chl_a_samples_messy_outliers <- chl_a_samples_messy_outliers %>%
  bind_cols(
    augment(model) %>% 
      select(.resid)   # keep only residuals
  )
#Calculate MAD and flag outliers using |residual| > 5 × MAD.
mad_resid <- mad(chl_a_samples_messy_outliers$.resid, na.rm = TRUE)

chl_a_samples_messy_outliers <- chl_a_samples_messy_outliers %>%
  mutate(
    outlier_resid = abs(.resid) > 5 * mad_resid
  )

#date should be data type: Date and changed to ISO format
#sample_replicate has 12 missing values
#subsample replicate has 1182 missing values


head(stations_messy)
summary(stations_messy)
skim(stations_messy) 
#here I can see right away that a lat and long have been swapped accidentally for station ST052, need to be moved to correct column.
#longitude value for station code ST005 is missing the dash for negative value.
#has 97 station codes, 27 more than the chl_a_samples df.
#87 waterbody codes

head(waterbodies_messy)
summary(waterbodies_messy)
skim(waterbodies_messy)
#has 87 waterbody codes, same as station df. All seems to be good here
waterbodies_tidy<- waterbodies_messy #make new dataframe for tiddy data

#Cleaning Data------

#create pipeline to clean samples dataframe
head(chl_a_samples_messy) #remind myself of column headers

chl_a_samples_tidy<- chl_a_samples_messy_outliers %>% #cleaning pipe
  mutate(
    sample_volume_filtered_ml = parse_number(sample_volume_filtered_ml), #convert from character to numeric, drop any non-number value 
    sample_volume_filtered_ml = if_else(sample_volume_filtered_ml == 10000, 1000, sample_volume_filtered_ml), #remove accidental zero
    absorbance_663nm = str_replace(absorbance_663nm, ",", "."), #convert comma to decimal
    absorbance_663nm = parse_number(absorbance_663nm), #convert from character to numeric
    extract_volume_ml= abs(extract_volume_ml), #make all values positive
    date = parse_date_time(
      date,
      orders = c(
        "Y/m/d", "d/m/Y", "m/d/Y",  # slash formats
        "BdY", "bdy"                # month names
      )
    ),
    date = as_date(date)   # convert to ISO YYYY-MM-DD
  )

#review the changes to ensure they made desired output
skim(chl_a_samples_messy)
skim(chl_a_samples_tidy) 

#create pipeline to clean stations dataframe
skim(stations_messy)

stations_tidy<- stations_messy %>% #cleaning pipe
  mutate(
    temp_lat = latitude,
    latitude  = if_else(station_code == "ST052", longitude, latitude),
    longitude = if_else(station_code == "ST052", temp_lat, longitude),
    longitude = if_else(station_code == "ST005", -longitude,
      longitude)
  ) %>%
  select(-temp_lat)

skim(stations_tidy)

#verification-------
# Find station_codes in chl_a that are missing in stations
missing_codes_s <- setdiff(
  chl_a_samples_tidy$station_code,
  stations_tidy$station_code
)

missing_codes_s
#ST999 is missing in stations_tidy. Should be corrected to ST097, which is missing a subsample replicate
chl_a_samples_tidy <- chl_a_samples_tidy %>% #Pipe to correct incorrect code before joining tables.
  mutate(
    station_code = if_else(chl_a_sample_code == "CHL0271", "ST097", station_code)
  )

# Find waterbody_codes in waterbodies_messy that are missing in stations
missing_codes_w <- setdiff(
  waterbodies_tidy$waterbody_code,
  stations_tidy$waterbody_code
)
missing_codes_w
#no missing codes
#write tidy data to .csv files in subfolder of project directory-------
write_csv(chl_a_samples_tidy, "Tidy_data/chl_a_samples_tidy.csv")
write_csv(stations_tidy, "stations_tidy.csv")
write_csv(waterbodies_tidy, "Tidy_data/waterbodies_tidy.csv")

#Join data tables----------
Combined_Table <- chl_a_samples_tidy %>%
  left_join(stations_tidy, by = "station_code")

Final_Table <- Combined_Table %>%
  left_join(waterbodies_tidy, by = "waterbody_code")

write_csv(Final_Table, "Final_data/Final_Table.csv")




