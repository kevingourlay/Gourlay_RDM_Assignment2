#Install and load packages-----------------------

#load packages
pacman::p_load(pacman, tidyverse, ggplot2)

#load .csv from folder
chl_a_samples_messy<-read_csv("chl-a-samples-messy.csv")
stations_messy<-read_csv("stations-messy.csv")
waterbodies_messy<-read_csv("waterbodies-messy.csv")


