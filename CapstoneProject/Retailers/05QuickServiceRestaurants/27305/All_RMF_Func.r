MarkingValidData <- function(Invoice, startDate, endDate, CustName ="CustomerName", CustIdentification="CustomerMobileNo", IdnentType = "Phone", InvcDate ="InvoiceDate", AmtField ="GrandTotal", InvalidCustomers=NULL){
  print("*******************************************************************************")
  print("Three new columns will be added in the data")
  print("InvDate     : Date on which invoice is created")
  print("ValidNumber : TRUE- If Phone number is valid, FALSE- If Phone Number is Invalid")
  print("ValidBuyer: FALSE- If Data for the customer is not Valid for RFM")
  print("IsValidData : 2- If Date or Phone number or Customer is Invalid, else 1")
  print("InvMonthYr  : Month and Year of Invoice for monthly analysis")
  print("*******************************************************************************")
  ## Create Date Column
  
  ## From Grocery
  # Invoice$InvDate <- 	as.Date(Invoice[,InvcDate],format='%d/%m/%y')
  
  ## For Salon
  Invoice$InvDate <- 	as.Date(Invoice[,InvcDate],format='%Y-%m-%d')
  
  startDate <- as.Date(startDate,"%Y-%m-%d")
  endDate <- as.Date(endDate,"%Y-%m-%d")
  
  print(paste("Data taken between ", startDate, " and ",endDate))
  print(paste("Least Date in Dataset is ", min(Invoice$InvDate),"and Max date is", max(Invoice$InvDate)))
  
  
  ##Get the records between required dates
  Invoice <- Invoice[Invoice$InvDate >= startDate & Invoice$InvDate <=  endDate,]
  print(paste("Invoices between",startDate,"and",endDate, "are",nrow(Invoice)))
  
  Invoice$IsValidData <- 1
  
  if(IdnentType=="Phone")
  {
    
    ## Check the validity of phone number
    ## https://stackoverflow.com/questions/18351553/regular-expression-validation-for-indian-phone-number-and-mobile-number
    ##(?:\s+|)                    // leading spaces
    ##((0|(?:(\+|)91))            // prefixed 0, 91 or +91
    ##  (?:\s|-)*                   // connecting space or dash (-)
    ##  (?:(?:\d(?:\s|-)*\d{9})|    // 1 digit STD code & number with connecting space or dash
    ##    (?:\d{2}(?:\s|-)*\d{8})|    // 2 digit STD code & number with connecting space or dash
    ##    (?:\d{3}(?:\s|-)*\d{7})|    // 3 digit STD code & number with connecting space or dash
    ##    \d{10})                     // plain 10 digit number
    ##  (?:\s+|)                    // trailing spaces
    
    IndPhonNumber <- "(?:\\s+|)((0|(?:(\\+|)91))(?:\\s|-)*(?:(?:\\d(?:\\s|-)*\\d{9})|(?:\\d{2}(?:\\s|-)*\\d{8})|(?:\\d{3}(?:\\s|-)*\\d{7}))|\\d{10})(?:\\s+|)"
    Invoice$ValidNumber <- grepl(IndPhonNumber, Invoice[,CustIdentification])
    ##Marking repeted numbers as invalid
    RepNumber <- "^[0-7](\\d)\\1{5,}\\d*$"
    Invoice$ReptNum <- grepl(RepNumber, Invoice[,CustIdentification])
    Invoice$ValidNumber <- if_else(Invoice$ReptNum==TRUE, FALSE,Invoice$ValidNumber)
  }
  else
  {
    Invoice$ValidNumber <- TRUE
  }
  sqlstring <- paste("select distinct",CustIdentification,",ValidNumber from Invoice" )
  uniqPhn <- sqldf(sqlstring)
  print("*******************************************************************************")
  print(paste("Unique Identification :", nrow(uniqPhn)))
  # print(paste("Valid Identification  :", table(uniqPhn$ValidNumber)[[2]]))
  # print(paste("InValid Identification:",table(uniqPhn$ValidNumber)[[1]]))
  print("*******************************************************************************")
  if(is.null(InvalidCustomers))
  {
    Invoice$ValidBuyer <- TRUE
  }
  else
  {
    InvalidBuyers <- paste("^", InvalidCustomers, sep = "")
    Invoice$ValidBuyer <- TRUE
    for (k in InvalidBuyers)
    {
      Invoice$ValidBuyer <- ifelse(grepl(k, Invoice$CustomerName, ignore.case = TRUE)==TRUE,FALSE,Invoice$ValidBuyer)
    }
  }
  print("*******************************************************************************")
  print(paste("InValid Invoices Based on Customers:",table(Invoice$ValidBuyer)["FALSE"]))
  print(paste("Valid Invoices Based on Customers  :",table(Invoice$ValidBuyer)["TRUE"]))
  
  print("*******************************************************************************")
  Invoice$IsValidData <- as.factor(ifelse(Invoice$ValidNumber==TRUE & Invoice$ValidBuyer==TRUE ,Invoice$IsValidData,2))
  table(Invoice$IsValidData)
  Invoice$InvMonthYr <- as.factor(format(Invoice$InvDate,"%Y-%m"))
  Invoice$ReptNum <- NULL
  print("*******************************************************************************")
  print(paste("InValid Invoices For Analysis:",table(Invoice$IsValidData)[[1]]))
  # print(paste("Valid Invoices               :",table(Invoice$IsValidData)[[2]]))
  print("*******************************************************************************")
  
  return(Invoice)
}

CreateDecile <- function(InvoiceDE, DecileCol ,decnum, decvevtor=NULL){
  
  if(is.null(decnum)==FALSE)
  {
    if(decnum==0.1)
    {
      FieDec <- quantile(InvoiceDE[,DecileCol], seq(0.1,0.9,0.1),na.rm = TRUE)
      ColName <- paste("Dec-",DecileCol,sep="")
      Argmnt <- "Decile"
    }
    else
    {
      FieDec <- quantile(InvoiceDE[,DecileCol], seq(0.25,0.75,0.25),na.rm = TRUE)
      ColName <- paste("Qart-",DecileCol,sep="")
      Argmnt <- "Quartile"
    }
  }
  
  if( is.null(decvevtor) == FALSE)
  {
    FieDec <- decvevtor
    Argmnt <- "Segment"
    ColName <- paste("Score-",DecileCol,sep="")
  }
  print(FieDec)
  
  minval <- min(InvoiceDE[,DecileCol])
  maxval <- max(InvoiceDE[,DecileCol])
  
  print(ColName)
  InvoiceDE[,ColName] <- 0
  reccount <- 0
  for(i in FieDec)
  {
    reccount = reccount+ 1
    if(reccount==1)
    {
      InvoiceDE[,ColName] <- if_else(InvoiceDE[,DecileCol]< as.numeric(i) , reccount, InvoiceDE[,ColName])
      print(paste(Argmnt, reccount, ": Between",minval," and " , as.numeric(i), "with", nrow(InvoiceDE[InvoiceDE[,ColName]==reccount,]),"Records"))
    }
    else
    {
      if(reccount==length(FieDec))
      {
        
        
        InvoiceDE[,ColName] <- ifelse(InvoiceDE[,DecileCol]>= oldrank &  InvoiceDE[,DecileCol]< as.numeric(i) , reccount, InvoiceDE[,ColName])
        print(paste(Argmnt, reccount," : Between " , oldrank , " and " ,as.numeric(i), "with", nrow(InvoiceDE[InvoiceDE[,ColName]==reccount,]),"Records"))
        InvoiceDE[,ColName] <- ifelse(InvoiceDE[,DecileCol]>= as.numeric(i) , reccount+1, InvoiceDE[,ColName])
        print(paste(Argmnt, reccount+1," : Between " , as.numeric(i), "and", maxval, "with", nrow(InvoiceDE[InvoiceDE[,ColName]==reccount+1,]),"Records"))
      }
      else
      {
        InvoiceDE[,ColName] <- ifelse(InvoiceDE[,DecileCol]>= oldrank &  InvoiceDE[,DecileCol]< as.numeric(i) , reccount, InvoiceDE[,ColName])
        print(paste(Argmnt, reccount," : Between " , oldrank , " and " ,as.numeric(i), "with", nrow(InvoiceDE[InvoiceDE[,ColName]==reccount,]),"Records"))
      }
    }
    oldrank <- as.numeric(i)
  }
  return(InvoiceDE)
}

PrepareRFM <- function(InvoiceDE, endDate, CustName ="CustomerName", CustIdentification="CustomerMobileNo", ValidData = "IsValidData", InvcDate ="InvoiceDate", AmtField ="GrandTotal"){
  print("*******************************************************************************")
  print("Two Datasets will be created for valid date")
  print("NameAndPhone : Recency, Frequency and Monetary for the combination of Name and Customet Identification")
  print("InvoiceRFM   : Recency, Frequency and Monetary Customet Identification")
  print("*******************************************************************************")
  ## Create Date Column
  endDate <- as.Date(endDate,"%Y-%m-%d")
  
  ## Difference of invoice date and end date
  InvoiceDE$OrderDays <- floor(as.numeric(difftime(endDate, InvoiceDE$InvDate,units = "days")))
  
  ## Marking orderDays as 999999 if amount id negative so that negative amount related record should not be considered for recency
  InvoiceDE$OrderDays <- ifelse(InvoiceDE$GrandTotal<0,999999,InvoiceDE$OrderDays)
  
  ## Records with negative amounts are for reversed invoice. Negtive Invoice and related invoice should not be considered in ferquency
  InvoiceDE$FreqPoint <- ifelse(InvoiceDE$GrandTotal<0,-1,1)
  
  sqlstring <- paste("select distinct", CustName,",", CustIdentification, ",min(OrderDays) 'Recency', sum(FreqPoint) 'Frequency', avg(GrandTotal) 'MonetaryAvg', sum(GrandTotal) 'MonetaryTotal' from  InvoiceDE where IsValidData=1 group by CustomerName, CustomerMobileNo")
  NameAndPhone <- sqldf(sqlstring)
  ##write.csv(NameAndPhone, "NameAndPhone.csv")
  
  sqlstring <- paste("select distinct", CustIdentification, ",min(OrderDays) 'Recency', sum(FreqPoint) 'Frequency', avg(GrandTotal) 'MonetaryAvg', sum(GrandTotal) 'MonetaryTotal' from  InvoiceDE where IsValidData=1 group by  CustomerMobileNo")
  InvoiceRFM <-sqldf(sqlstring)
  InvoiceRFM <- InvoiceRFM[InvoiceRFM$Frequency>0,]
  InvoiceRFM$LogMonAvg <-log(InvoiceRFM$MonetaryAvg)
  InvoiceRFM$LogFreq <-log(InvoiceRFM$Frequency)
  InvoiceRFM$LogRec <- log(InvoiceRFM$Recency)
  InvoiceRFM$LogMonTot <- log(InvoiceRFM$MonetaryTotal)
  numRecord <- nrow(InvoiceRFM)
  
  print("*******************************************************************************")
  print(paste("Total Number of RFM Customers:",numRecord))
  print("*******************************************************************************")
  
  RetuDF <- list("NameAndPhone"=NameAndPhone,"InvoiceRFM"=InvoiceRFM )
  return(RetuDF)
}

########InvoiceRFM <- DataSet on which fucntion is to be run
#r : Number if segments required
#rvector : vector of values on the basis wich segments are required. r, Lowest, Highest should be passed as null
#Lowest : When a set of least values are to be kept in one segment. r is required, Highest is optional, rvector should be passed as NULL
#Highest: When a set of higher values are to be kept in one segment. r is required, Lowest is optional, rvector should be passed as NULL
GetRScore <- function(InvoiceRFM,  Lowest=NULL, Highest=NULL ,r=NULL,rvector=NULL){
  colname<- "Recency"
  print(summary(InvoiceRFM[,colname]))
  
  if(is.null(r) & is.null(rvector))
  {
    stop('Please provide number of segments or segment vector')
  }
  if(is.null(colname))
  {
    stop('Please provide column Name')
  }
  if(is.null(rvector))
  {
    LowestHighest <- 0
    if(is.null(Lowest)==FALSE)
    {
      
      LowestHighest <- LowestHighest+1
    }
    
    if(is.null(Highest)==FALSE)
    {
      LowestHighest <- LowestHighest+1
    }
    fracsize <- r-LowestHighest
    
    if(LowestHighest ==2 )
    {
      quantvec <- as.vector(quantile(InvoiceRFM[InvoiceRFM[,colname]>Lowest & InvoiceRFM[,colname]<Highest,colname],seq(1/fracsize, (fracsize-1)/fracsize,1/fracsize )))
      rvector <- c(Lowest, quantvec,Highest )
      print(rvector)
    }
    else
    {
      if(LowestHighest==1 & is.null(Lowest)==FALSE)
      {
        quantvec <- as.vector(quantile(InvoiceRFM[InvoiceRFM[,colname]>Lowest ,colname],seq(1/fracsize, (fracsize-1)/fracsize,1/fracsize )))
        rvector <- c(Lowest, quantvec )
        print(rvector)
      }
      else
      {
        if(LowestHighest==1 & is.null(Highest)==FALSE)
        {
          quantvec <- as.vector(quantile(InvoiceRFM[ InvoiceRFM[,colname]<Highest,colname],seq(1/fracsize, (fracsize-1)/fracsize,1/fracsize )))
          rvector <- c(quantvec,Highest )
          print(rvector)
        }
        else
        {
          quantvec <- as.vector(quantile(InvoiceRFM[ ,colname],seq(1/fracsize, (fracsize-1)/fracsize,1/fracsize )))
          rvector <- quantvec
          print(rvector)
        }
        
      }
    }
    
  }
  else
  {
    r<- length(rvector)+1
    print(rvector)
  }
  InvoiceRFM <- CreateDecile(InvoiceRFM,colname,NULL,rvector)
  ScoreColName <- paste("Score-",colname,sep="")
  InvoiceRFM[,ScoreColName] <- -1*InvoiceRFM[,ScoreColName]+(r+1)
  
  return(InvoiceRFM)
  
}

GetFMScore <- function(InvoiceRFM, colname=NULL, Lowest=NULL, Highest=NULL ,r=NULL,rvector=NULL){
  
  if(is.null(r) & is.null(rvector))
  {
    stop('Please provide number of segments or segment vector')
  }
  
  
  if(is.null(colname))
  {
    stop('Please provide column Name')
  }
  
  print(summary(InvoiceRFM[,colname]))
  if(is.null(rvector))
  {
    LowestHighest <- 0
    if(is.null(Lowest)==FALSE)
    {
      
      LowestHighest <- LowestHighest+1
    }
    
    if(is.null(Highest)==FALSE)
    {
      LowestHighest <- LowestHighest+1
    }
    fracsize <- r-LowestHighest
    
    if(LowestHighest ==2 )
    {
      if(fracsize==1)
      {
        rvector <- c(Lowest, Highest )
      }
      else
      {
        quantvec <- as.vector(quantile(InvoiceRFM[InvoiceRFM[,colname]>Lowest & InvoiceRFM[,colname]<Highest,colname],seq(1/fracsize, (fracsize-1)/fracsize,1/fracsize )))
        rvector <- c(Lowest, quantvec,Highest )
      }
      print(rvector)
    }
    else
    {
      if(LowestHighest==1 & is.null(Lowest)==FALSE)
      {
        if(fracsize==1)
        {
          rvector <- c(Lowest )
        }
        else
        {
          quantvec <- as.vector(quantile(InvoiceRFM[InvoiceRFM[,colname]>Lowest ,colname],seq(1/fracsize, (fracsize-1)/fracsize,1/fracsize )))
          rvector <- c(Lowest, quantvec )
        }
        print(rvector)
      }
      else
      {
        if(LowestHighest==1 & is.null(Highest)==FALSE)
        {
          if(fracsize==1)
          {
            rvector <- c(Highest )
          }
          else
          {
            quantvec <- as.vector(quantile(InvoiceRFM[ InvoiceRFM[,colname]<Highest,colname],seq(1/fracsize, (fracsize-1)/fracsize,1/fracsize )))
            rvector <- c(quantvec,Highest )
          }
          print(rvector)
        }
        else
        {
          quantvec <- as.vector(quantile(InvoiceRFM[ ,colname],seq(1/fracsize, (fracsize-1)/fracsize,1/fracsize )))
          rvector <- quantvec
          print(rvector)
        }
        
      }
    }
    
  }
  else
  {
    r<- length(rvector)+1
    print(rvector)
  }
  InvoiceRFM <- CreateDecile(InvoiceRFM,colname,NULL,rvector)
  
  return(InvoiceRFM)
  
}

########GetCumScore   
#InvoiceRFM - Dataset with MMoney column
#level : If levels are given as 5, then function will assigne value as 1 to 5 based on first 20% cumulative sum, then next 20% then next 20%.. This will be required for analysis only
#mvect : Here Score will be done based on cumulative monetary basis. If it is given as (25,25,50) then scoring will be done as 3 for highest monetary values till cum sum is 25% of total. 1 means the least
GetCumScore <- function(InvoiceRFM, colname="MonetaryTotal", level=0, mvect=NULL){
  if(level >0)
  {
    n<-level
    InvoiceRFM <- InvoiceRFM[order(-InvoiceRFM[,colname]),]
    InvoiceRFM$CumMoneyRank <- 0
    for(i in seq(1:n))
    {
      value <- sum(InvoiceRFM[InvoiceRFM$CumMoneyRank==0,colname])
      cutoffPerc <- (1/n)
      cutoffCum <- value*cutoffPerc
      
      #print(paste("value",value,"cutoffPerc",cutoffPerc,"cutoffCum",cutoffCum))
      if (i<n)
      {
        InvoiceRFM$CumMoneyRank <- ifelse(cumsum(InvoiceRFM[InvoiceRFM$CumMoneyRank==0,colname]) <= cutoffCum  , i, InvoiceRFM$CumMoneyRank)
      }
      else
      {
        InvoiceRFM$CumMoneyRank <- ifelse(InvoiceRFM$CumMoneyRank==0,i,InvoiceRFM$CumMoneyRank)
      }
      if(i==1)
      {
        InvoiceRFMNEW <- InvoiceRFM[InvoiceRFM$CumMoneyRank>0,]
      }
      else
      {
        InvoiceRFMNEW <- rbind(InvoiceRFMNEW,InvoiceRFM[InvoiceRFM$CumMoneyRank>0,])
      }
      InvoiceRFM <- InvoiceRFM[InvoiceRFM$CumMoneyRank==0,]
      print(paste("Record for first",i*10 ,"% :" ,print(nrow(InvoiceRFMNEW)), "with Minimum value as",
                  min(InvoiceRFMNEW[,colname]), "and maximum value as",max(InvoiceRFMNEW[,colname]) ))
    }
  }
  if(is.null(mvect) == FALSE)
  {
    InvoiceRFM <- InvoiceRFM[order(-InvoiceRFM[,colname]),]
    InvoiceRFM$`Score-MonetaryTotal` <- 0
    leng <- length(mvect)
    cnt <- 0
    cumperc <- 0
    for (i in mvect)
    {
      cnt<- cnt+1
      value <- sum(InvoiceRFM[InvoiceRFM$`Score-MonetaryTotal`==0,colname])
      cutoffPerc <- i/100
      cutoffCum <- value*cutoffPerc
      cumperc<- cumperc+i
      
      #print(paste("value",value,"cutoffPerc",cutoffPerc,"cutoffCum",cutoffCum))
      if (cnt<leng)
      {
        InvoiceRFM$`Score-MonetaryTotal` <- ifelse(cumsum(InvoiceRFM[InvoiceRFM$`Score-MonetaryTotal`==0,colname]) <= cutoffCum  , leng-cnt+1, InvoiceRFM$`Score-MonetaryTotal`)
      }
      else
      {
        InvoiceRFM$`Score-MonetaryTotal` <- ifelse(InvoiceRFM$`Score-MonetaryTotal`==0,leng-cnt+1,InvoiceRFM$`Score-MonetaryTotal`)
      }
      if(cnt==1)
      {
        InvoiceRFMNEW <- InvoiceRFM[InvoiceRFM$`Score-MonetaryTotal`>0,]
      }
      else
      {
        InvoiceRFMNEW <- rbind(InvoiceRFMNEW,InvoiceRFM[InvoiceRFM$`Score-MonetaryTotal`>0,])
      }
      InvoiceRFM <- InvoiceRFM[InvoiceRFM$`Score-MonetaryTotal`==0,]
      print(paste("Record for first",cumperc ,"% :" ,print(nrow(InvoiceRFMNEW)), "with Minimum value as",
                  min(InvoiceRFMNEW[,colname]), "and maximum value as",max(InvoiceRFMNEW[,colname]) ))
    }
  }
  
  return(InvoiceRFMNEW)
}

