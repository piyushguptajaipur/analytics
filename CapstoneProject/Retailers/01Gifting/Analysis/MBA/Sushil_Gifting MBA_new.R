library(xlsx)
library(plyr)
library(tidyr)
library(SnowballC)
library(qdap)
library(tm)
library(stringr)
library(sqldf)
library(plyr)
library(tidyr)
library(arules)
library(arulesViz)

setwd("~/Google Drive/GreatLakes Capstone Project/Retailers/01Gifting/Analysis")

#Import Invoice Data 
Invoice <- read.table(file.choose(),sep = ",", header = TRUE, 
                       encoding = "UTF-8",  stringsAsFactors = FALSE
                       ,  na.strings = c("(null)",""))



#Import Invoice Details 
InvDtls1 <- read.table(file.choose(),sep = "|", header = TRUE, 
                       encoding = "UTF-8",  stringsAsFactors = FALSE
                       ,  na.strings = c("(null)",""))

InvDtls2 <- read.table(file.choose(),sep = "|", header = TRUE, 
                       encoding = "UTF-8",  stringsAsFactors = FALSE
                       ,  na.strings = c("(null)",""))

InvoiceDetails <- rbind(InvDtls1,InvDtls2)
rm(InvDtls1)
rm(InvDtls2)

str(InvoiceDetails)
head(InvoiceDetails)
tail(InvoiceDetails)

ProductServices <- read.csv(file.choose(), header = TRUE, 
                            encoding = "UTF-8",  stringsAsFactors = FALSE
                            ,  na.strings = c("(null)",""))


str(ProductServices)
head(ProductServices)
tail(ProductServices)



#find number of duplicate rows
count(duplicated(ProductServices$Name))

#7111 unique, 2445 duplicates 


#Add Extra column to ProductServices
ProductServices$NewPdtName <- ProductServices$Name

#Drop NA rows
ProductServices <- ProductServices[!ProductServices$NewPdtName == "NA",]

#Merge into one dataset(Left Join into InvoiceDetails)
Merged <- merge(x=InvoiceDetails,y=ProductServices,by="ProductServicesID")

#Drop NA and Invalid products rows
Merged <- Merged[!Merged$NewPdtName=='NA',]
Merged <- Merged[!Merged$NewPdtName=='Invalid',]

ProdWithDate <- sqldf("select A.NewPdtName, B.InvMonthYr,B.InvDate, A.InvoiceID from Merged A, Invoice B where A.InvoiceID = B.InvoiceID ")
aaa <- sqldf("select NewPdtName,InvMonthYr, count(*)  from ProdWithDate group by NewPdtName,InvMonthYr order by InvMonthYr asc, count(*) desc")

UniqueData <- sqldf("select distinct InvoiceID, NewPdtName from Merged")

rm(Invoice)

#Grocery Merged
FinalData <- NULL
FinalData$TransactionID <- UniqueData$InvoiceID
FinalData$Item <- UniqueData$NewPdtName
FinalData <- split(FinalData$Item, FinalData$TransactionID)
head(FinalData)


#Create Transactions Matrix
txn <- as(FinalData,'transactions')

#Item Frequency Plot (individual)
itemFrequencyPlot(txn,topN=20,type="absolute")

# Get the rules
basket_rules <- apriori(txn, parameter = list(supp = 0.002, conf = 0.001,minlen=2))
quality(basket_rules)$chi <- interestMeasure(basket_rules, measure='chi', significance=T, UniqueData)

summary(basket_rules)
inspect(basket_rules)


basket_rules <- sort(basket_rules, by='confidence', decreasing = TRUE)
write(basket_rules,file = "association_rules.csv", sep=",")


#Visualzation
plot(basket_rules,method="graph",shading=NA)

#Increaing the confidence level
basket_rules <- apriori(txn, parameter = list(supp = 0.002, conf = 0.1,minlen=2))
basket_rules <- sort(basket_rules, by='confidence', decreasing = TRUE)
inspect(basket_rules)
plot(basket_rules,method="graph",shading=NA)
write(basket_rules,file = "association_rules1.csv", sep=",")

