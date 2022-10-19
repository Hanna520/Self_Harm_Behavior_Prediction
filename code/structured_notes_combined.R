# This script stacks all the notes data for the negative cases that were extracted 30,000 rows at a time, combines the negative cases with positive cases, and extracts the structured data for both.


source("//ochca.com/hcashares/chs/Data/data_science/rscripts/source/master.R")


library(xml2)
# library(tidyverse)
library(tidyr)
library(stringr)
library(lubridate)
library(dplyr)

setwd('Z:/rscripts/users/interns/hlu')

neg_notes_1 <- read.csv('Z:/rscripts/users/interns/hlu/data/quick_soap_all_1-30000_Alex.csv')
neg_notes_2 <- read.csv('Z:/rscripts/users/interns/hlu/data/quick_soap_all_60000_Alex.csv')
neg_notes_3 <- read.csv('Z:/rscripts/users/interns/hlu/data/quick_soap_all_90000_Alex.csv')

names(neg_notes_1)
names(neg_notes_2)
names(neg_notes_3)

neg_notes = rbind(neg_notes_1, neg_notes_2)
neg_notes = rbind(neg_notes, neg_notes_3)

pos_soap <- read.csv('Z:/rscripts/users/interns/hlu/data/model_soap_valid_pos.csv')
pos_quick <- read.csv('Z:/rscripts/users/interns/hlu/data/model_quick_valid_pos.csv')

pos_notes <- merge(pos_soap, pos_quick, by='BookingNumber')

pos_notes <- pos_notes[c("BookingNumber","soap_sub","soap_obj","soap_ass","soap_plan", "quick_notes" )]
neg_notes <- neg_notes[c("BookingNumber","soap_sub","soap_obj","soap_ass","soap_plan", "quick_notes" )]

pos_notes$event = 1
neg_notes$event = 0

notes <- rbind(pos_notes, neg_notes)
######
load2("iat_book",path="iat")
notes_demo <- merge(notes, iat_book, by = 'BookingNumber')
notes_demo <- notes_demo[c('InmateID','BookingNumber','DateOfBirth','Sex','Race',
                          'MaritalStatus','BookingDate','LastUpdateDateTime','ReleaseDate',
                          "soap_sub","soap_obj","soap_ass","soap_plan","quick_notes",'event')]

# Calculate age: BookingDate - DateofBirth
notes_demo$age <- time_length(difftime(as.Date(notes_demo$BookingDate), as.Date(notes_demo$DateOfBirth)),
                             'years')


# find AB109 data
ab109 = .s(INMATE_ADDITIONAL_INFORMATION,InmateID,FieldValue, LastUpdateDateTime, FieldName="BookStatus",FieldValue = c("SPOC","FLSH","REVH","3056","3455"))
neg_ab109_id = ab109$InmateID

notes_demo$ab109 = notes_demo$InmateID %in% neg_ab109_id



write.csv(notes_demo, "Z:/rscripts/users/interns/hlu/data/model_str_notes_Alex_all.csv")

##########################################################
# Clean data
##########################################################
df <- notes_demo
df[df$Sex=="Male",'Sex'] <- "M"
df[df$Sex=="Female",'Sex'] <- "F"
df[is.na(df$MaritalStatus)|df$MaritalStatus=="W", "MaritalStatus"] <- "U" # IF NA or W, then "Unknown"

df$Race <- case_when(
  df$Race %in% c("A","F","K","V","Asian","C","L","Z","D","J") ~ "A",
  df$Race %in% c("Hispanic or Latino", "H")                   ~ "H",
  df$Race %in% c("American Indian or Alaska Native","I")      ~ "I",
  # df$Race == "O"                                              ~ "O", # Combine other with unknown or na
  df$Race %in% c("P","G","S","U")                             ~ "P",
  df$Race %in% c("White","W")                                 ~ "W",
  df$Race == "B"                                              ~ "B",
  TRUE                                                        ~ "U")

df <- within(df, {
  Sex <- factor(Sex)
  Race <- factor(Race)
  MaritalStatus <- factor(MaritalStatus)
  AB109 <- factor(ab109)
})

df$ab109 <- NULL

write.csv(df, "Z:/rscripts/users/interns/hlu/data/cleaned_model_str_notes_Alex.csv")
