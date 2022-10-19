# This script extracts notes data for all negative cases. The progress notes are saved in XML format. For those who have multiple bookings, randomly sample a booking to extract the notes data. Due the database limit on the number of rows allowed to be extracted, we had to extract the notes data for the negative cases 30,000 rows at a time for three times.

source("//ochca.com/hcashares/chs/Data/data_science/rscripts/source/master.R")


library(xml2)
# library(tidyverse)
library(tidyr)
library(stringr)
library(lubridate)
library(dplyr)

setwd('Z:/rscripts/users/interns/hlu')

#############################################################################
##############################################################################

neg = read.csv("Z:/rscripts/users/interns/hlu/data/neg_ids.csv")

suicide <- as.data.frame(read_excel("data/Suicides_combined.xlsx"))

##############################################################################

load2("iat_book", path='iat')# collapsed version of the above iat_book_L
load2("iat_book_L", path='iat') # loads in list of data.frames, each df is per inmate, each row of the df is a different booking

rownames(iat_book)= iat_book$BookingNumber
# temp <- merge(iat_book$BookingNumber, suicide$`INMATE BOOKING #`)
u_pos_InmateIDs =unique(iat_book[suicide$`INMATE BOOKING #`,"InmateID"])

#########################################################################
#########################################################################
soap_noteid = .s(PROGRESS_NOTE_TYPES,"ID",Name="SOAP NOTE")

quick_noteid = .s(PROGRESS_NOTE_TYPES,"ID",Name="QUICK NOTE")

###########
all_soap_ids  = .s(PROGRESS_NOTES, InmateID, NoteType= as.integer(soap_noteid$ID), d=T)[,1]
all_quick_ids = .s(PROGRESS_NOTES, InmateID, NoteType= as.integer(quick_noteid$ID), d=T)[,1]
# prog_note_inmateID_10000 = sample(intersect(names(iat_book_L),intersect(neg$InmateID, intersect(all_soap_ids,all_quick_ids))),10000,replace = F)

# All notes
prog_note_inmateID_all = intersect(names(iat_book_L),intersect(neg$InmateID, intersect(all_soap_ids,all_quick_ids)))

# prog_note_inmateID_all_1 = prog_note_inmateID_all[1:30000]
# prog_note_inmateID_all_1 = prog_note_inmateID_all[30001:60000]
prog_note_inmateID_all_1 = prog_note_inmateID_all[60001:length(prog_note_inmateID_all)]


# Soap notes for the sampled negative inmates
soap_notes_1 = .s(PROGRESS_NOTES,ID,InmateID,Note,Stamp,Role,NoteType= as.integer(soap_noteid$ID),InmateID = prog_note_inmateID_all_1)

# Quick notes for the sampled negative inmates
quick_notes_1 = .s(PROGRESS_NOTES,ID,InmateID,Note,Stamp,Role,NoteType= as.integer(quick_noteid$ID),InmateID = prog_note_inmateID_all_1)

# Adding booking related info to the soap notes
soap_notes_1_2 = addsbn(soap_notes_1,ciatL2 = iat_book_L)# add a bookingnumber, booking date

# Adding booking related info to the quick notes (2 rows are not in iat_book_L)
quick_notes_1_2 = addsbn(quick_notes_1,ciatL2 = iat_book_L)# add a bookingnumber, booking date

note_in_both_inmateID_1=na.omit(intersect(soap_notes_1_2$BookingNumber,quick_notes_1_2$BookingNumber))

soap_notes2 = soap_notes_1_2[soap_notes_1_2$BookingNumber %in% note_in_both_inmateID_1,]
quick_notes2 = quick_notes_1_2[quick_notes_1_2$BookingNumber %in% note_in_both_inmateID_1,]

################# the following 2 lines were used if not splitting the ids file into 2 #############
# soap_notes2_L = split(soap_notes2,soap_notes2$InmateID)
# neg_note_bookingnumbers =sample(sapply(soap_notes2_L,function(x) sample(unique(x[,"BookingNumber"]),1) ),4000,replace = F)
###############################################################
# Randomly choose a booking for each inmateID
soap_notes2_L = split(soap_notes2,soap_notes2$InmateID)
neg_note_bookingnumbers = sapply(soap_notes2_L,function(x) sample(unique(x[,"BookingNumber"]),1) )

# 
soap_notes2 = soap_notes2[soap_notes2$BookingNumber %in% neg_note_bookingnumbers,]
quick_notes2 = quick_notes2[quick_notes2$BookingNumber %in% neg_note_bookingnumbers,]

length(unique(soap_notes2$BookingNumber))

################################################################################
##### Extract Soap Notes (sub and ob) and Quick Notes from PROGRESS_NOTES ######
################################################################################

#WE will use this to parse the ANSWERS TABLE
mdf = function(x){
  xx = tryCatch({xmlTreeParse(x)},error=function(e){iconv(x, to="UTF-8") })
  ll = (xmlToList(xx))
  return(ll)
}

# For SOAP notes subjective (extract the subjective soap notes):
LL = lapply(1:nrow(soap_notes2),function(x){x=mdf(soap_notes2[x,"Note"])$SOAPNOTE$SUBJECTIVE;if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = " "); }})
soap_notes2$notes_sub <- LL


# For SOAP notes objective (extract the objective soap notes):
LL = lapply(1:nrow(soap_notes2),function(x){x=mdf(soap_notes2[x,"Note"])$SOAPNOTE$OBJECTIVE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = " "); }})
soap_notes2$notes_obj <- LL

## For SOAP notes Assessment and Plan
LL = lapply(1:nrow(soap_notes2),function(x){x=mdf(soap_notes2[x,"Note"])$SOAPNOTE$ASSESSMENT; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = " "); }})
soap_notes2$notes_ass <- LL

LL = lapply(1:nrow(soap_notes2),function(x){x=mdf(soap_notes2[x,"Note"])$SOAPNOTE$PLAN; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = " "); }})
soap_notes2$notes_plan <- LL
soap_notes2$Note <- NULL
###############################


# For QUICK notes (extract the quick notes):
LL = lapply(1:nrow(quick_notes2),function(x){x=mdf(quick_notes2[x,"Note"])$QUICKNOTE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = " "); }})
quick_notes2$notes_quick <- LL
quick_notes2$Note <- NULL

# save soap_notes2 because the computer will be forced shut down in a couple hours.
# write.csv(soap_notes2, "Z:/rscripts/users/interns/hlu/data/soap_notes2_allnotes.csv")

##########################################################################################
# Concatenate all notes for the same booking
soap_neg <- soap_notes2 %>%
  group_by(BookingNumber) %>%
  dplyr::summarise(soap_sub=paste(notes_sub, collapse = " "),
                   soap_obj=paste(notes_obj, collapse = " "),
                   soap_ass=paste(notes_ass, collapse = " "),
                   soap_plan=paste(notes_plan, collapse = " ")) %>%
  dplyr::select(BookingNumber,soap_sub,soap_obj,soap_ass,soap_plan)

quick_neg <- quick_notes2 %>%
  group_by(BookingNumber) %>%
  dplyr::summarise(quick_notes=paste(notes_quick, collapse = " ")) %>%
  dplyr::select(BookingNumber,quick_notes)

quick_soap <- merge(soap_neg, quick_neg, by='BookingNumber')

###########################################################################
# write.csv(soap_neg, "Z:/rscripts/users/interns/hlu/data/soap_neg.csv")
# write.csv(quick_neg, "Z:/rscripts/users/interns/hlu/data/quick_neg.csv")
# write.csv(quick_soap, "Z:/rscripts/users/interns/hlu/data/quick_soap.csv")

# write.csv(quick_soap, "Z:/rscripts/users/interns/hlu/data/quick_soap_all_30000_Alex.csv")
# write.csv(quick_soap, "Z:/rscripts/users/interns/hlu/data/quick_soap_all_60000_Alex.csv")
write.csv(quick_soap, "Z:/rscripts/users/interns/hlu/data/quick_soap_all_90000_Alex.csv")
