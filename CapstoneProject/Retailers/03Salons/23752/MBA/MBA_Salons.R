# Load requied libraries
library(xlsx)
library(openxlsx)
library(plyr)
library(tidyr)
library(SnowballC)
library(qdap)
library(tm)
library(stringr)
library(tidyr)
library(arulesViz)
library(arules)
library(RColorBrewer)

setwd("~/Google Drive/GreatLakes Capstone Project/Retailers/03Salons/23752/MBA/")

#Import Data in R
InvoiceDetails <- read.csv(file.choose(),header = T)
ProductServices <- read.csv(file.choose(),header = T)

#Total No of Rows for each
nrow(InvoiceDetails)
nrow(ProductServices)

#Clean Product Names
pdt_name <- ProductServices$Name

#Replace punctuation with Spaces
pdt_name <- gsub('([[:punct:]])', ' ',pdt_name)

#Replace all non-alphanumeric with Spaces
pdt_name <- str_replace_all(pdt_name, "[^[:alnum:]]", " ")

#Remove all exta spaces
pdt_name <- trimws(pdt_name,which = c('both'))

#Convert data type for tm functions cleaning
pdt_source <- VectorSource(pdt_name)
pdt_corpus <- Corpus(pdt_source)

#Change all case to lowercase
pdt_corpus = tm_map(pdt_corpus, tolower)

#Specific to Products having quantities mentioned with space seperator
pdt_corpus = tm_map(pdt_corpus, removeWords,c("kg","g","gm","ml","l","ltr"))


#Remove extra spaces from Corpus
pdt_corpus <- tm_map(pdt_corpus,stripWhitespace)

#Create columns for space delimiter in product names
new <- separate(data.frame(text = get("content", pdt_corpus)),col=text,into=c("Col1","Col2","Col3","Col4",'Col5','Col6','Col7','Col8'),sep=" ")
col1 <- new$Col1
col2 <- new$Col2
col3 <- new$Col3
col4 <- new$Col4

pdt_sort <- data.frame(col1,col2,col3,stringsAsFactors = FALSE)
head(pdt_sort)
str(pdt_sort)

#Columns having NA, replace with blank string
pdt_sort[is.na(pdt_sort)] <- ""

#Drop columns having no or 'NA' values
pdt_sort <- pdt_sort[!pdt_sort$col1 == "NA",]

pdt_name_new <- do.call(paste, as.data.frame(pdt_sort, stringsAsFactors=FALSE))
#Clean Extra WhiteSpaces in start/ end
pdt_name_new <- trimws(pdt_name_new,which = c('both'))

write.table(pdt_name_new, file = "Gifting_Products.csv",row.names=FALSE, na="",col.names=c(""), sep=",")

#Total final cleaned Rows
nrow(as.data.frame(pdt_name_new))

#find number of duplicate rows
count(duplicated(pdt_name_new))


#Add Extra column to ProductServices
ProductServices$NewPdtName <- pdt_name_new
#ProductServices <- ProductServices[1:9556,]

#Drop 'invalid' rows
ProductServices <- ProductServices[!ProductServices$NewPdtName == "accesiries",]
View(ProductServices)

#Drop NA rows
ProductServices <- ProductServices[!ProductServices$NewPdtName == "NA",]

#Merge into one dataset(Left Join into InvoiceDetails)
Merged <- merge(x=InvoiceDetails,y=ProductServices,by="ProductServicesID")
View(Merged)

#Drop NA rows
Merged <- Merged[!Merged$NewPdtName=='NA',]

#Drop Cancelled Invoice transactions
#Merged <- subset(Merged,Merged$IsCanceled==FALSE)

#Table Merged
Table_M <- NULL
Table_M$TransactionID <- as.factor(Merged$InvoiceID)
Table_M$Item <- as.factor(Merged$NewPdtName)

Table_M <- split(Table_M$Item, Table_M$TransactionID)


#Create Transactions Matrix
txn <- as(Table_M,'transactions')

#Item Frequency Plot (individual)
itemFrequencyPlot(txn,topN=20,type="absolute")
itemFrequencyPlot(txn,topN=20,type="relative")

#Extended rules
ext_rules <- apriori(txn, parameter = list(supp = 0.00005, conf = 0.1,minlen=3))
length(ext_rules)
tag_line = paste("Plotting Extended Rules: ", length(ext_rules))
plot(ext_rules,control=list(col=brewer.pal(11,"Spectral")),main=tag_line)
arulesViz::plotly_arules(ext_rules)

# Get the rules
basket_rules <- apriori(txn, parameter = list(supp = 0.0009, conf = 0.8,minlen=3))
length(basket_rules)
# Show the top rules, but only 2 digits
options(digits=2)
arules::inspect(basket_rules[1:5])
  
rules_conf <-sort(basket_rules, by="confidence", decreasing=TRUE)
rules_count <- sort(basket_rules, by="count", decreasing=TRUE)
rules_supp <- sort(basket_rules, by="support", decreasing=TRUE)
rules_lift <- sort(basket_rules, by="lift", decreasing=TRUE)

arules::inspect(rules_conf[1:5])
arules::inspect(rules_count[1:5])
arules::inspect(rules_supp[1:5])
arules::inspect(rules_lift[1:5])


#Visualzation
plot(rules_lift[1:5],method = "graph",control = list(type = "items"))
plot(rules_lift[1:5],method = "paracoord",control = list(reorder = TRUE))
plot(rules_lift[1:5],method = "matrix",control = list(reorder = 'measure'))



#Dropping top selling items from Product List (List to be Customized per retailer)
limit_rules <- apriori(txn, parameter = list(supp = 0.0001, conf = 0.8,minlen=3),appearance = list(none = c("invalid"),default="both"))
arules::inspect(limit_rules[1:5])
limit_rules_lift <- sort(limit_rules, by="lift", decreasing=TRUE)
arules::inspect(limit_rules_lift[1:5])
plot(limit_rules[1:5],method = "graph",control = list(type = "items"))
plot(limit_rules_lift[1:5],method = "graph",control = list(type = "items"))
plot(limit_rules_lift[1:5],method = "paracoord",control = list(reorder = TRUE))
