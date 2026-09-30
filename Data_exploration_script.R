#Install and load packages-----------------------

#install and library packages using pacman 
pacman::p_load(pacman, tidyverse, ggplot2, skimr)

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
#chl_a_20ed has a large outlier
#date should be data type: Date and changed to ISO format
#sample_replicate has 12 missing values
#subsample replicate has 1182 missing values
#has 70 station codes

head(stations_messy)
summary(stations_messy)
skim(stations_messy) 
#here I can see right away there is a negative latitude value, all should be positive
#a positive longitude value, all should be negative.
#has 97 station codes, 27 more than the chl_a_samples df.
#87 waterbody codes

head(waterbodies_messy)
summary(waterbodies_messy)
skim(waterbodies_messy)
#has 87 waterbody codes, same as station df.

#Cleaning Data------

#create pipeline to clean samples dataframe
head(chl_a_samples_messy) #remind myself of column headers

chl_a_samples_tidy<- chl_a_samples_messy %>% #cleaning pipe
  mutate(
    sample_volume_filtered_ml = parse_number(sample_volume_filtered_ml), #convert from character to numeric, drop any non-number value 
    absorbance_663nm = parse_number(absorbance_663nm), #convert from character to numeric
    extract_volume_ml= abs(extract_volume_ml) #make all values positive
  )
#review the changes to ensure they made desired output
skim(chl_a_samples_messy)
skim(chl_a_samples_tidy) 

#create pipeline to clean stations dataframe
head(stations_messy)

stations_tidy<- stations_messy %>% #cleaning pipe
  mutate(
    latitude = abs(latitude), #convert all values to positive as they are all in same geographic region
    longitude = -abs(longitude)
  )
skim(stations_tidy)

#verification-------
unique(chl_a_samples_tidy$sample_volume_filtered_ml)




