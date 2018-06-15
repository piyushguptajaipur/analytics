library("psych")
library("sqldf")
library("ggplot2")
library("dplyr")
library("xlsx")

## Set Working Directory
setwd("~/Google Drive/GreatLakes Capstone Project/Retailers/02DeptStore/25457")

## Please run All_RMF_Func.r first
message("Please run all the function in 'All_RMF_Func.r' and then execute below steps")

#######################   Set parameters ####################################################
#Start Date and End Date: Dates between analysis is to be done (including end and start date)
startDate <- "2017-02-01"
endDate <- "2018-03-31"

################# Validity Parameters #######################################
##InvalidBuyers : vector of starting of customer names not to be taken in analysis,
#InvalidBuyers<-NULL
InvalidBuyers <- c("extra", "Recharge", "cash", "lkolp")


## Read the file1 and check the content
Invoice <- read.csv(file.choose(), header = TRUE, 
                      encoding = "UTF-8", quote = "" , stringsAsFactors = FALSE,
                      na.strings = c("(null)",""))


str(Invoice)
head(Invoice)
tail(Invoice)
View(Invoice)

## Make IsCancelled , PaymentType and storeId as factor
Invoice$IsCanceled <- as.factor(Invoice$IsCanceled)
Invoice$PaymentType <- as.factor(Invoice$PaymentType)
Invoice$StoreId <- as.factor(Invoice$StoreId)

## Check the blank values
summary(Invoice)
sapply(Invoice, function(x) sum(is.na(x)))

## Remove cancelled Invoices
Invoice <- Invoice[Invoice$IsCanceled=="false" | Invoice$IsCanceled=="FALSE" ,]
View(Invoice)

## Call the function to mark invalid data based on phone numbers and invalid buyr
## string. The function will enrich the data with date varibles


InvoiceDE <- MarkingValidData(Invoice,startDate,endDate,,,,,,InvalidBuyers)
View(InvoiceDE)
InvoiceDE <- CreateDecile(InvoiceDE,"GrandTotal",0.1)
InvoiceDE <- CreateDecile(InvoiceDE,"GrandTotal",0.25)
summary(InvoiceDE[,c(5,7,8,9,10,11,12)])
describe(InvoiceDE$GrandTotal)

## Use InvoiceDE.csv for "Data Analysis" Dashboard
write.csv(InvoiceDE,"InvoiceDE.csv")

InvoiceDEValidData <- CreateDecile(InvoiceDE[InvoiceDE$IsValidData==1,],"GrandTotal",0.1)
InvoiceDEValidData <- CreateDecile(InvoiceDE[InvoiceDE$IsValidData==1,],"GrandTotal",0.25) 
summary(InvoiceDEValidData)
write.csv(InvoiceDEValidData,"InvoiceDEValidData.csv")




#################################  Preparing RFM Data  ###########################

RFMData <- PrepareRFM(InvoiceDE, endDate,,,,)
NameAndPhone <- RFMData$NameAndPhone
InvoiceRFM <- RFMData$InvoiceRFM
rm(RFMData)

summary(NameAndPhone)
write.csv(NameAndPhone, "NameAndPhone.csv")


#################################################################################
InvoiceRFM <- CreateDecile(InvoiceRFM,"Recency",0.1)
InvoiceRFM <- CreateDecile(InvoiceRFM,"Recency",0.25)
InvoiceRFM <- CreateDecile(InvoiceRFM,"Frequency",0.1)
InvoiceRFM <- CreateDecile(InvoiceRFM,"Frequency",0.25)
InvoiceRFM <- CreateDecile(InvoiceRFM,"MonetaryAvg",0.1)
InvoiceRFM <- CreateDecile(InvoiceRFM,"MonetaryAvg",0.25)
InvoiceRFM <- CreateDecile(InvoiceRFM,"MonetaryTotal",0.1)
InvoiceRFM <- CreateDecile(InvoiceRFM,"MonetaryTotal",0.25)
## To analyse cumulative distribution of amount
InvoiceRFM <- GetCumScore(InvoiceRFM, , 10,NULL)

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

fLowest=3
fHighest=NULL 
f=3
fvector=NULL 

mLowest=10000
mHighest=50000
m=3
mvector=NULL 

##Please change parameres  as per definition mentiones in function doc. No need to change the dataframe(first parameter)
InvoiceRFMScore <- GetRScore(InvoiceRFM[,c(1,2,3,4,5,6,7,8,9)],rLowest,rHighest,r,rvector)
#InvoiceRFMScore <- GetFMScore(InvoiceRFMScore,"Frequency",,,,c(3,12,36))
#InvoiceRFMScore <- GetFMScore(InvoiceRFMScore,"MonetaryAvg",,15000,4,NULL)
InvoiceRFMScore <- GetFMScore(InvoiceRFMScore,"Frequency",fLowest,fHighest,f,fvector)
#InvoiceRFMScore <- GetFMScore(InvoiceRFMScore,"MonetaryTotal",mLowest,mHighest,m,NULL)
InvoiceRFMScore <- GetCumScore(InvoiceRFMScore, , ,c(25,25,50))

### Score 1 == Lowest, Score 3 == Best (For all R/F/M)

InvoiceRFMScore$RFMScore <- 100*InvoiceRFMScore$`Score-Recency`+10*InvoiceRFMScore$`Score-Frequency`+InvoiceRFMScore$`Score-MonetaryTotal`
InvoiceRFMScore$LogMonAvg <- NULL
InvoiceRFMScore$LogFreq <- NULL
InvoiceRFMScore$LogRec <- NULL
InvoiceRFMScore$LogMonTot <- NULL

RFM_Segements <- read.csv("RFM_Segements.csv")
View(RFM_Segements)
RFM_Segements$Score <- 100*RFM_Segements$X...R+10*RFM_Segements$F+RFM_Segements$M
sqlstring  <- "select A.*, B.Segment, B.Description,B.Campaign from InvoiceRFMScore A,RFM_Segements B where A.RFMScore=B.Score "
InvoiceRFMScore <- sqldf(sqlstring)

sqlstring <- "select max(InvoiceID) invID,CustomerMobileNo, GrandTotal, InvDate from InvoiceDEValidData group by CustomerMobileNo "
abc <- sqldf(sqlstring)
sqlstring <- "select A.*, B.invId, B.GrandTotal, B.InvDate from InvoiceRFMScore A, abc B where A.CustomerMobileNo=B.CustomerMobileNo "
kkk <- sqldf(sqlstring)

InvoiceRFMScore$LastInvId <- kkk$invID
InvoiceRFMScore$LastPurchase <- kkk$InvDate
InvoiceRFMScore$LastInvAmt <- kkk$GrandTotal

rm(abc)
rm(kkk)
rm(InvoiceDEValidData)
write.csv(InvoiceRFMScore, "InvoiceRFMScore.csv")



