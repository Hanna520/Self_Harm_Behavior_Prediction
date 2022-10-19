source("//ochca.com/hcashares/chs/Data/data_science/rscripts/source/master.R")


library(xml2)
# library(tidyverse)
library(tidyr)
library(stringr)
library(lubridate)
library(dplyr)

setwd('Z:/rscripts/users/interns/hlu')

##############################################################################
# s(PROGRESS_NOTES,.Note,InmateID.K=c("T","t","x","X"),5)
# lapply(list.files(),function(x) read_excel(x,lkdjs, sheet = 3)) 

##############################################################################
########################### Load Self-harm Data from Excel ###################
##############################################################################
# # Load the self-harm data
# # library(readxl)

suicide <- as.data.frame(read_excel("data/Suicides_combined.xlsx"))
names(suicide) <- c("Inmate_Name", "BookingNumber", "Date_Attempt", "Location",
                    "Method", "Outcome", "Comments", "Entered_by", "ICE_Yes_No")
suicide$BookingNumber = as.character(suicide$BookingNumber)

######
# Some inmates attempted multiple times in the same booking.
# We take the earliest attempt only
suicide <- suicide[order(suicide$BookingNumber, suicide$Date_Attempt),]
suicide <- suicide %>%
  group_by(BookingNumber) %>%
  dplyr::summarise(Stamp = first(Date_Attempt)) %>%
  dplyr::select(BookingNumber,Stamp)

####################################################################################
# Investigate the duration before attempt for positives:
# # suicide <- suicide[,1:6] # delete the last two columns (not useful)

# write.csv(suicide, "Z:/rscripts/users/interns/hlu/data/suicide_clean.csv")
# suicide_clean <- read.csv("Z:/rscripts/users/interns/hlu/data/suicide_clean.csv")
# suicide_clean <- suicide_clean[,2:4]



##############################################################################

load2("iat_book", path='iat')# collapsed version of the above iat_book_L
load2("iat_book_L", path='iat') # loads in list of data.frames, each df is per inmate, each row of the df is a different booking
# load2("iat_L") # list of dataframes per booking

rownames(iat_book)= iat_book$BookingNumber
u_pos_InmateIDs =unique(iat_book[suicide$BookingNumber,"InmateID"])

# Find the InmateID for suicide table
suicide_booking_inmate <- iat_book[suicide$BookingNumber,c("BookingNumber", "InmateID")]

suicide_inmate <- merge(suicide, suicide_booking_inmate, by = "BookingNumber")

# Find the Booking Date based on Attempt Date?
suicide_booking_iat = addsbn(suicide_inmate,ciatL2 = iat_book_L)


# Calculate the Duration before attempt
suicide_booking_iat$Duration <- as.Date(suicide_booking_iat$Stamp) - 
  as.Date(suicide_booking_iat$BookingDate - 8*60*60)

# Outliers are all those attempts that happened on the same day of Booking
outliers <- suicide_booking_iat[suicide_booking_iat$Duration <= 0,]

"Pt is a MOD N deputy referral d/t found trying to hang self in cell. Pt is irritable, 
fairly cooperative, minimal eye contact, impulsive, 
restrictive affect, irritable mood with congruent affect, apethetic, 
denies HI/Ah/VH, +SI. Pt reports the subjective. States the subjecitve. 
Unable to maintain safety. Pt is unpredictable."  
##############################################################################
#########################################################################
#########################################################################
#########################################################################
soap_noteid = .s(PROGRESS_NOTE_TYPES,"ID",Name="SOAP NOTE")

quick_noteid = .s(PROGRESS_NOTE_TYPES,"ID",Name="QUICK NOTE")

# Soap notes for the sampled negative inmates
# soap_notes = .s(PROGRESS_NOTES,ID,InmateID,Note,Stamp,Role,NoteType= as.integer(soap_noteid$ID),InmateID = u_neg_InmateIDs_1000)
# 
# # Quick notes for the sampled negative inmates
# quick_notes = .s(PROGRESS_NOTES,ID,InmateID,Note,Stamp,Role,NoteType= as.integer(quick_noteid$ID),InmateID = u_neg_InmateIDs_1000)
# 
# # Adding booking related info to the soap notes
# soap_notes2 = addsbn(soap_notes,ciatL2 = iat_book_L)# add a bookingnumber, booking date
# 
# 
# # Adding booking related info to the quick notes (2 rows are not in iat_book_L)
# quick_notes2 = addsbn(quick_notes,ciatL2 = iat_book_L)# add a bookingnumber, booking date



################################################################################
##### Extract Soap Notes (sub and ob) and Quick Notes from PROGRESS_NOTES ######
################################################################################

#WE will use this to parse the ANSWERS TABLE
mdf = function(x){
  xx = tryCatch({xmlTreeParse(x)},error=function(e){iconv(x, to="UTF-8") })
  ll = (xmlToList(xx))
  return(ll)
}

# # For SOAP notes subjective (extract the objective soap notes):
# LL = lapply(1:nrow(soap_notes2),function(x){x=mdf(soap_notes2[x,"Note"])$SOAPNOTE$SUBJECTIVE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = "___"); }})
# soap_notes2$notes_sub <- LL
# 
# 
# # For SOAP notes objective (extract the objective soap notes):
# LL = lapply(1:nrow(soap_notes2),function(x){x=mdf(soap_notes2[x,"Note"])$SOAPNOTE$OBJECTIVE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = "___"); }})
# soap_notes2$notes_obj <- LL
# soap_notes2$Note <- NULL
# 
# 
# # For QUICK notes (extract the quick notes):
# LL = lapply(1:nrow(quick_notes2),function(x){x=mdf(quick_notes2[x,"Note"])$QUICKNOTE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = "___"); }})
# quick_notes2$notes_quick <- LL
# quick_notes2$Note <- NULL


# ##############################################################################
# # For Soap Notes:
# count_soap_sub <- soap_notes2 %>%
#   dplyr::group_by(InmateID, BookingNumber) %>%
#   dplyr::summarise(count = dplyr::n())
# 
# # Maximum number of notes among positive inmates
# max(count_soap_sub$count) # = 221 (minimum: 1, mean:5.384, standard deviation: 13.73474)
# 
# unique_booking_neg <- soap_notes2[!duplicated(soap_notes2[,"BookingNumber"]),]
# 
# unique_booking_IM_neg <- unique_booking_neg %>%
#   dplyr::group_by(InmateID) %>%
#   dplyr::summarise(count = dplyr::n())
# 
# # Maximum number of Bookings among positive inmates
# max(unique_booking_IM_neg$count) # = 14 (minimum is 1, mean is 2.016, standard deviation: 1.985)
# 
# 
# ##############################################################################
# # For Quick Notes:
# count_quick <- quick_notes2 %>%
#   dplyr::group_by(InmateID,BookingNumber) %>%
#   dplyr::summarise(count = dplyr::n())
# 
# # Maximum number of notes among positive inmates
# max(count_quick$count) # = 470 (minimum: 1, mean:7.214777, standard deviation: 22.91658)



##############################################################################
########################### For Positive Inmates #############################
##############################################################################

# Soap notes for the positive inmates
soap_notes_pos = .s(PROGRESS_NOTES,ID,InmateID,Note,Stamp,Role,NoteType= as.integer(soap_noteid$ID),InmateID = u_pos_InmateIDs)

# Quick notes for the positive inmates
quick_notes_pos = .s(PROGRESS_NOTES,ID,InmateID,Note,Stamp,Role,NoteType= as.integer(quick_noteid$ID),InmateID = u_pos_InmateIDs)

# Adding booking related info to the soap notes
soap_notes2_pos = addsbn(soap_notes_pos,ciatL2 = iat_book_L)# add a bookingnumber, booking date
quick_notes2_pos = addsbn(quick_notes_pos,ciatL2 = iat_book_L)
######################################################################################
# Remove those that are stamped after the attempt date
soap_notes_pos_merge <- merge(soap_notes2_pos, suicide, by="BookingNumber",all.x = T)
soap_valid <- filter(soap_notes_pos_merge, as.Date(Stamp.x-8*60*60) < Stamp.y)

LL = lapply(1:nrow(soap_valid),function(x){x=mdf(soap_valid[x,"Note"])$SOAPNOTE$SUBJECTIVE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = " "); }})
soap_valid$notes_sub <- LL

LL = lapply(1:nrow(soap_valid),function(x){x=mdf(soap_valid[x,"Note"])$SOAPNOTE$OBJECTIVE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = " "); }})
soap_valid$notes_obj <- LL

## For SOAP notes Assessment and Plan
LL = lapply(1:nrow(soap_valid),function(x){x=mdf(soap_valid[x,"Note"])$SOAPNOTE$ASSESSMENT; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = " "); }})
soap_valid$notes_ass <- LL

LL = lapply(1:nrow(soap_valid),function(x){x=mdf(soap_valid[x,"Note"])$SOAPNOTE$PLAN; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = " "); }})
soap_valid$notes_plan <- LL
soap_valid$Note <- NULL

soap_valid$notes_sub <- vapply(soap_valid$notes_sub, paste, collapse = " ", character(1L))
soap_valid$notes_obj <- vapply(soap_valid$notes_obj, paste, collapse = " ", character(1L))
soap_valid$notes_ass <- vapply(soap_valid$notes_ass, paste, collapse = " ", character(1L))
soap_valid$notes_plan <- vapply(soap_valid$notes_plan, paste, collapse = " ", character(1L))

# For quick notes
quick_notes_pos_merge <- merge(quick_notes2_pos, suicide, by="BookingNumber",how="left")
quick_valid <- filter(quick_notes_pos_merge, as.Date(Stamp.x-8*60*60) < Stamp.y)

LL = lapply(1:nrow(quick_valid),function(x){x=mdf(quick_valid[x,"Note"])$QUICKNOTE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = " "); }})
quick_valid$notes_quick <- LL

quick_valid$notes_quick <- vapply(quick_valid$notes_quick, paste, collapse = " ", character(1L))

##########################################################################################
# Concatenate all notes for the same booking
soap_valid_pos <- soap_valid %>%
  group_by(BookingNumber) %>%
  dplyr::summarise(soap_sub=paste(notes_sub, collapse = " "),
                   soap_obj=paste(notes_obj, collapse = " "),
                   soap_ass=paste(notes_ass, collapse = " "),
                   soap_plan=paste(notes_plan, collapse = " ")) %>%
  dplyr::select(BookingNumber,soap_sub,soap_obj,soap_ass, soap_plan)

soap_valid_pos$num_words_sub <- unlist(lapply(soap_valid_pos$soap_sub, number_words))
quantile(soap_valid_pos$num_words_sub)

soap_valid_pos$num_words_obj <- unlist(lapply(soap_valid_pos$soap_obj, number_words))
quantile(soap_valid_pos$num_words_obj)

# Concatenate all notes for the same booking
quick_valid_pos <- quick_valid %>%
  group_by(BookingNumber) %>%
  dplyr::summarise(quick_notes=paste(notes_quick, collapse = " ")) %>%
  dplyr::select(BookingNumber,quick_notes)

quick_valid_pos$num_words_quick <- unlist(lapply(quick_valid_pos$quick_notes, number_words))
quantile(quick_valid_pos$num_words_quick)

write.csv(soap_valid_pos, "Z:/rscripts/users/interns/hlu/data/model_soap_valid_pos.csv")
write.csv(quick_valid_pos, "Z:/rscripts/users/interns/hlu/data/model_quick_valid_pos.csv")


##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
# soap_valid_pos ready for model
# next step: sample and prepare the negative cases in the same way
##############################################################################################
# Investigate the bookings that notes were created on or after attempt (88 bookings)
setdiff(unique(suicide$BookingNumber),unique(soap_valid_pos$BookingNumber))


temp_all <- merge(soap_notes2_pos, suicide, by="BookingNumber",how="left")
NN = lapply(1:nrow(temp_all),function(x){x=mdf(temp_all[x,"Note"])$SOAPNOTE$SUBJECTIVE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = "___"); }})
temp_all$notes_sub <- NN
NN = lapply(1:nrow(temp_all),function(x){x=mdf(temp_all[x,"Note"])$SOAPNOTE$OBJECTIVE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = "___"); }})
temp_all$notes_obj <- NN

temp_all$notes_sub <- vapply(temp_all$notes_sub, paste, collapse = "___", character(1L))
temp_all$notes_obj <- vapply(temp_all$notes_obj, paste, collapse = "___", character(1L))


#############################
# Remove those that are stamped after the attempt date
quick_notes2_pos = addsbn(quick_notes_pos,ciatL2 = iat_book_L)# add a bookingnumber, booking date
quick_notes_pos_merge <- merge(quick_notes2_pos, suicide, by="BookingNumber",how="left")
quick_valid <- filter(quick_notes_pos_merge, as.Date(Stamp.x-8*60*60) < Stamp.y)

LL = lapply(1:nrow(quick_valid),function(x){x=mdf(quick_valid[x,"Note"])$QUICKNOTE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = "___"); }})
quick_valid$notes_quick <- LL

quick_valid$notes_quick <- vapply(quick_valid$notes_quick, paste, collapse = "___", character(1L))


quick_valid_pos <- quick_valid %>%
  group_by(BookingNumber) %>%
  dplyr::summarise(quick=paste(notes_quick, collapse = "___")) %>%
  dplyr::select(BookingNumber,quick)

quick_valid_pos$num_words_quick <- unlist(lapply(quick_valid_pos$quick, number_words))
quantile(quick_valid_pos$num_words_quick)

# Investigate the bookings that are not found in iat_book
a <- list(unique(soap_notes_pos_merge$BookingNumber))
b <- list(unique(suicide$BookingNumber))
setdiff(unlist(b),unlist(a))


# 3 InmateIDs are not in Progressnotes or iat_book
setdiff(unique(suicide_inmate$InmateID),unique(soap_notes_pos$InmateID))

# After removing the bookings where the notes were created on or after the attempt date, 
# there are 269 unique bookings for soap notes, and 278 for quick notes. There are a total of
# 357 unique bookings in the excel file. Since the sample size is already too small, 
# I will go over the notes created on the attempt date manually.
######################################################################################
# Adding booking related info to the quick notes (2 rows are not in iat_book_L)
quick_notes2_pos = addsbn(quick_notes_pos,ciatL2 = iat_book_L)# add a bookingnumber, booking date


# For SOAP notes subjective (extract the subjective soap notes):
LL = lapply(1:nrow(soap_notes2_pos),function(x){x=mdf(soap_notes2_pos[x,"Note"])$SOAPNOTE$SUBJECTIVE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = "___"); }})
soap_notes2_pos$notes_sub <- LL


# For SOAP notes objective (extract the objective soap notes):
LL = lapply(1:nrow(soap_notes2_pos),function(x){x=mdf(soap_notes2_pos[x,"Note"])$SOAPNOTE$OBJECTIVE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = "___"); }})
soap_notes2_pos$notes_obj <- LL
soap_notes2_pos$Note <- NULL


# For QUICK notes (extract the quick notes):

LL = lapply(1:nrow(quick_notes2_pos),function(x){x=mdf(quick_notes2_pos[x,"Note"])$QUICKNOTE; if(class(x)!="list"){x}else{paste(do.call(c,x),collapse = "___"); }})
quick_notes2_pos$notes_quick <- LL
quick_notes2_pos$Note <- NULL


####################################################################################
# Flatten the lists (notes columns are lists)
soap_notes2_pos$notes_sub <- vapply(soap_notes2_pos$notes_sub, paste, collapse = "___", character(1L))
soap_notes2_pos$notes_obj <- vapply(soap_notes2_pos$notes_obj, paste, collapse = "___", character(1L))
quick_notes2_pos$notes_quick <- vapply(quick_notes2_pos$notes_quick, paste, collapse = "___", character(1L))

soap_notes2$notes_sub <- vapply(soap_notes2$notes_sub, paste, collapse = "___", character(1L))
soap_notes2$notes_obj <- vapply(soap_notes2$notes_obj, paste, collapse = "___", character(1L))
quick_notes2$notes_quick <- vapply(quick_notes2$notes_quick, paste, collapse = "___", character(1L))


write.csv(soap_notes2_pos, "Z:/rscripts/users/interns/hlu/data/soap_notes2_pos.csv")
write.csv(quick_notes2_pos, "Z:/rscripts/users/interns/hlu/data/quick_notes2_pos.csv")
write.csv(soap_notes2, "Z:/rscripts/users/interns/hlu/data/soap_notes2.csv") # for negatives
write.csv(quick_notes2, "Z:/rscripts/users/interns/hlu/data/quick_notes2.csv") # for negatives

####################################################################################
# Count of soap notes by InmateID
count_pos <- soap_notes2_pos %>%
  dplyr::group_by(InmateID, BookingNumber) %>%
  dplyr::summarise(count = dplyr::n())

# Maximum number of notes among positive inmates
max(count_pos$count) # = 882 (minimum: 1, mean: 15.7942, standard deviation: 41.26763)

####################################################################################
# Count of quick notes by InmateID
count_quick_pos <- quick_notes2_pos %>%
  dplyr::group_by(InmateID,BookingNumber) %>%
  dplyr::summarise(count = dplyr::n())

# Maximum number of notes among positive inmates
max(count_quick_pos$count) # = 898 (minimum: 1, mean: 21.57763, standard deviation: 45.14422)

####################################################################################
# For Soap Notes:
unique_booking <- soap_notes2_pos[!duplicated(soap_notes2_pos[,"BookingNumber"]),]

unique_booking_IM <- unique_booking %>%
  dplyr::group_by(InmateID) %>%
  dplyr::summarise(count = dplyr::n())

# Maximum number of Bookings among positive inmates
max(unique_booking_IM$count) # = 43 (minimum: 1, mean: 5.389, standard deviation: 5.836)

####################################################################################

compare <- merge(soap_notes2, quick_notes2, by = "InmateID")
length(unique(compare$InmateID))
compare_pos <- merge(soap_notes_pos, quick_notes_pos, by = "InmateID")
length(unique(compare_pos$InmateID))

# Extract frequency (proportion) of Race
inmates_all = .s(INMATES)
round(t(t(sapply(split(inmates_all, inmates_all$Race),nrow)))/nrow(inmates_all),3)
# split: split the dataframe into sub-dataframes according to the unique values of the the second argument

inmates_pos = .s(INMATES, ID = u_pos_InmateIDs)
round(t(t(sapply(split(inmates_pos, inmates_pos$Race),nrow)))/nrow(inmates_pos),3)


# Extract frequency (proportion) of Sex
round(t(t(sapply(split(inmates_all, inmates_all$Sex),nrow)))/nrow(inmates_all),3)
round(t(t(sapply(split(inmates_pos, inmates_pos$Sex),nrow)))/nrow(inmates_pos),3)

# Extract the notes for positives 1 day before the Date of Attempt (coerce the Date of Attempt to UTC)
# Look at the duration of that booking (up until 1 day before the Date of Attempt)






# Take only the bookings when self-harm was committed
soap_pos_booking <- soap_notes2_pos[soap_notes2_pos$BookingNumber %in% suicide_clean$BookingNumber,]

# Calculate the duration 
soap_pos_boooking_suicide <- merge(soap_pos_booking, suicide_clean, by="BookingNumber")

# Subtract 8 hours from Booking Date (because it's UTC time)
soap_pos_boooking_suicide$duration <- as.Date(soap_pos_boooking_suicide$Date_Attempt) - as.Date(soap_pos_boooking_suicide$BookingDate - 8*60*60)
quantile(soap_pos_boooking_suicide$duration)    

# soap_pos_boooking_suicide$duration <- as.numeric(difftime(soap_pos_boooking_suicide$Date_Attempt, soap_pos_boooking_suicide$BookingDate, units = "days"))
# quantile(soap_pos_boooking_suicide$duration)
outliers <- soap_pos_boooking_suicide[soap_pos_boooking_suicide$duration < 0,]

####################################################################################
.s(INMATES_AUDIT_TRAIL,BookingNumber="3168407")
M = cbind(suicide[,c("BookingNumber", "Date_Attempt")], BookingDate = iat_book[suicide$BookingNumber,"BookingDate"])
new = soap_pos_boooking_suicide[!duplicated(soap_pos_boooking_suicide$BookingNumber),]
new2 = soap_pos_boooking_suicide[,c("BookingDate","Date_Attempt","duration",'BookingNumber')]
####################################################################################
# Next: 
# 1. Subtract 7 or 8 in BookingDate before creating duration
# 2. Investigate negative durations
# 3. Investigate large duration numbers
# 4. Distribution of durations

####################################################################################
# Investigate number of words in notes
library(stringr)
library(dplyr)

# Concatenate all notes for the same booking
soap_suicide_notes <- soap_pos_boooking_suicide %>%
  group_by(BookingNumber) %>%
  dplyr::summarise(soap_sub=paste(notes_sub, collapse = "___"),
                   soap_obj=paste(notes_obj, collapse = "___")) %>%
  dplyr::select(BookingNumber,soap_sub,soap_obj)


# Count the number of words in a concatenated note
number_words <- function(x){
  note_len <- str_count(as.character(x), "\\w+")
  return(note_len)
}
soap_suicide_notes$num_words_sub <- lapply(soap_suicide_notes$soap_sub, number_words)
quantile(unlist(soap_suicide_notes$num_words_sub))

soap_suicide_notes$num_words_obj <- lapply(soap_suicide_notes$soap_obj, number_words)
quantile(unlist(soap_suicide_notes$num_words_obj))

# Next: take the notes before attempt
# Next: investigate cases like this:
# Booking has too many notes (there are duplicates)#************
# Two attempts during one booking 
# Need to remove a duplicate booking (keep the one that has an earlier attempt date)
