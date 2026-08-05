# Load packages

library(wildRtrax)
library(dplyr)
library(reticulate)
library(lubridate)


#Set up credentials

Sys.setenv(WT_USERNAME = 'ljpatter', WT_PASSWORD = 'Kingedwardpark13')

# Authenticate

wt_auth()

####PATH VARIABLES####
BUpubilic_prefix<- "P:/"
org<-"ABMI"
project<-"ABMI-EH"
year<-"2023"
visit<-"V1"

####Sampling & Moving Variables####

numberofrecordings<-"151"
file_destination<-"W:/BayneLabWorkSpace/Leonard_workspace" #location you want to move your files

####Reading location list####

master_file<-read.csv("Output/master_recording_list.csv")
locs<-c(master_file$file_name)

renamed_paths<-c()

####Mover Script####


for(i in 1:nrow(master_file)){
  location<-locs[i]
  PATH<-paste("//nfs3.wildtrax.ca/BUpublic/ABMI/ARU/ABMI-EH/2023/V1")
  
  
files<-wt_audio_scanner(renamed_paths[i],file_type = "wav") #Here you can filter for 
files<-files%>%mutate(hour=hour(files$recording_date_time)) # <- not sure how to specify that I want it to only extract recordings that have a file name that matches those in the column "file_name" in the csv file
files<-files%>%filter(hour%in%c(4:10))
}
print("Transfer Complete")




