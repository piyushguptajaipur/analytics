install.packages("sqldf")
install.packages("knitr")

library("psych")
library("sqldf")
library("ggplot2")
library("dplyr")
library("knitr")


## Please run All_RMF_Func.r first
message("Please run all the function in 'All_RMF_Func.r' and then execute below steps")

#######################   Set parameters ####################################################
#Start Date and End Date: Dates between analysis is to be done (including end and start date)
startDate <- "2017-02-21"
endDate <- "2018-03-27"

################# Validity Parameters #######################################
##InvalidBuyers : vector of starting of customer names not to be taken in analysis,
# InvalidBuyers<-NULL
InvalidBuyers <- c("extra", "recharge", "cash", "lkolp")


## Set Working Directory
#setwd("C:/Great Lakes/Capstone/DataSets/GreatLakes Capstone Project-20180318T035626Z-001/GreatLakes Capstone Project/Retailers/Gifting/v2")

## Read the file1 and check the content
Invset1 <- read.table(file.choose(),sep = "|", header = TRUE, 
                      encoding = "UTF-8", quote = "" , stringsAsFactors = FALSE,
                      na.strings = c("(null)",""))
str(Invset1)
head(Invset1)
tail(Invset1)

## Read the second file and check the content
Invset2 <- read.csv(file.choose(),sep = "|", header = TRUE, 
                    encoding = "UTF-8", quote = "", stringsAsFactors = FALSE
                    ,  na.strings = c("(null)",""))
str(Invset2)
head(Invset2)
tail(Invset2)

## Combine the datasets and check the content
# Invoice <- rbind(Invset1)
Invoice <- read.csv(file.choose())
str(Invoice)
head(Invoice)
tail(Invoice)

## Make IsCancelled , PaymentType and storeId as factor
Invoice$IsCanceled <- as.factor(Invoice$IsCanceled)
Invoice$PaymentType <- as.factor(Invoice$PaymentType)
Invoice$StoreID <- as.factor(Invoice$StoreID)

## Check the blank values
summary(Invoice)
sapply(Invoice, function(x) sum(is.na(x)))

## Remove cancelled Invoices
Invoice <- Invoice[Invoice$IsCanceled=="FALSE",]

## Call the function to mark invalid data based on phone numbers and invalid buyr
## string. The function will enrich the data with date varibles

InvoiceDE <- MarkingValidData(Invoice,startDate,endDate,,,,,,InvalidBuyers)
View(InvoiceDE)
attach(InvoiceDE)
glimpse(InvoiceDE)
#Cleaning done

#### Mohit
df_RFM <- InvoiceDE %>% 
  group_by(as.numeric(CustomerMobileNo)) %>% 
  summarise(recency=as.numeric(as.Date("2018-04-04")-max(as.Date(InvoiceDate))),
            frequenci=n_distinct(InvoiceID), monitery= sum(GrandTotal)/n_distinct(InvoiceID)) 

summary(df_RFM)
kable(head(df_RFM))

hist(df_RFM$recency)
hist(df_RFM$frequenci, breaks = 50)
hist(df_RFM$monitery, breaks = 50)

#### Mohit

InvoiceDE <- CreateDecile(InvoiceDE,"GrandTotal",0.1)
InvoiceDE <- CreateDecile(InvoiceDE,"GrandTotal",0.25)
summary(InvoiceDE)
describe(InvoiceDE$GrandTotal)

## Use InvoiceDE.csv for "Data Analysis" Dashboard
write.csv(InvoiceDE,"InvoiceDE_Salon.csv")



#################################  Preparing RFM Data  ###########################

RFMData <- PrepareRFM(InvoiceDE, endDate,,,,)
NameAndPhone <- RFMData$NameAndPhone
InvoiceRFM <- RFMData$InvoiceRFM

summary(NameAndPhone)
write.csv(NameAndPhone, "NameAndPhone.csv")


#################################################################################
InvoiceRFM <- CreateDecile(InvoiceRFM,"Recency",0.1)
InvoiceRFM <- CreateDecile(InvoiceRFM,"Recency",0.25)
InvoiceRFM <- CreateDecile(InvoiceRFM,"Frequency",0.1)
InvoiceRFM <- CreateDecile(InvoiceRFM,"Frequency",0.25)
InvoiceRFM <- CreateDecile(InvoiceRFM,"MonetaryAvg",0.1)
InvoiceRFM <- CreateDecile(InvoiceRFM,"MonetaryAvg",0.25)

summary(InvoiceRFM)
write.csv(InvoiceRFM, "InvoiceRFM.csv")


### Create RFM Segmentation #####
########### RFM Parameterss  ###############################
#Lowest : (Optional) set valid number if you want to generate specific segment with values less then some specific value
#Highest : (Optional) set valid number if you want to generate specific segment with values greater then some specific value
#r/f/m : Number of segments required. It is mandatory if specific vectors are not given
#rvector/fvector/mvector : This are to be given if sgements with specific values are to be generated. Segment values should be given as vector

rLowest=NULL
rHighest=NULL 
r=3
rvector=NULL 
fLowest=NULL
fHighest=NULL 
f=3
fvector=NULL 
mLowest=NULL
mHighest=NULL 
m=3
mvector=NULL 

##Please change parameres  as per definition mentiones in function doc. No need to change the dataframe(first parameter)
InvoiceRFMScore <- GetRScore(InvoiceRFM[,c(1,2,3,4,5,6,7,8,9)],rLowest,rHighest,r,rvector)
#InvoiceRFMScore <- GetFMScore(InvoiceRFMScore,"Frequency",,,,c(3,12,36))
#InvoiceRFMScore <- GetFMScore(InvoiceRFMScore,"MonetaryAvg",,15000,4,NULL)
InvoiceRFMScore <- GetFMScore(InvoiceRFMScore,"Frequency",fLowest,fHighest,f,fvector)
InvoiceRFMScore <- GetFMScore(InvoiceRFMScore,"MonetaryAvg",mLowest,mHighest,m,NULL)

InvoiceRFMScore$RFMScore <- 100*InvoiceRFMScore$`Score-Recency`+10*InvoiceRFMScore$`Score-Frequency`+InvoiceRFMScore$`Score-MonetaryAvg`
write.csv(InvoiceRFMScore, "InvoiceRFMScore.csv")
