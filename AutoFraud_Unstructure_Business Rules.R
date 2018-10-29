#!/usr/bin/env Rscript

# Order in which Parameters are to be passed to the code in the command line 
# -loss_date 12/20/2013 ; -claim_num C1001

# Code for reading the command line inputs
args <- commandArgs(trailingOnly = TRUE)

# Reading the parameters from the command line and assigning to R varibles

Param1<-args[1]
Loss_date<-as.Date(args[2],format='%m/%d/%Y')
sc1<-args[3]
#Loss_date

Param2<-args[4]
ClaimNum <-args[5]
#ClaimNum

# Connecting to Oracle table
# Extract the record for the specific claim

suppressMessages(require(RODBC))
channel<-odbcConnect("RORACLE",uid="fraudrepo",pwd="fraudrepo",believeNRows=FALSE)
ClaimRecord=sqlQuery(channel,sprintf("select * from RENGINEINVOKE where Claim_id = '%s'",ClaimNum))
#Unstructured Data

unstructureRecord=sqlQuery(channel,sprintf("select * from FAS_POLICE_UNSTRUCTED_DATA where CLAIMID='%s'",ClaimNum))
unstructureRecord$CLAIM_ID = as.character(unstructureRecord$CLAIMID)
unstructureRecord$DATE_OF_INCIDENT = as.Date(as.character(unstructureRecord$DATE_OF_INCIDENT),format='%Y-%m-%d')
unstructureRecord$DATE_OF_POLICE_REPORT = as.Date(as.character(unstructureRecord$DATE_OF_POLICE_REPORT),format='%Y-%m-%d')

options(warn=-1)

# Initializing Fraud Score variable and Triggered business rule list

FraudScore=0
BRList=list();

if(nrow(unstructureRecord)!=0)
{
  
# Business Rule1
  
  if(Loss_date!=unstructureRecord$DATE_OF_INCIDENT)
    { 
      FraudScore=FraudScore+20
      BRList=c(BRList,"Mismatch between Date of incident and Loss date")  
    }
    
#Business Rule2
  
  date_pattern<-parse(file = "unstructureRecord", n = NULL, text = unstructureRecord$DATE_OF_POLICE_REPORT)
  
#CHECK FOR POLICE DATE [UNSTRUCTURED DATA]
  
  CHECK_POLICE_DATE <- is.na(date_pattern)
  
  Rule2a <- ClaimRecord$POLICE_REPORT
  Rule2b <- CHECK_POLICE_DATE
  
  if(Rule2a=="YES" && Rule2b=="TRUE")
    {
      FraudScore=FraudScore+20;
      BRList=c(BRList,"Mismatch in police report data")
    }
  
  if(Rule2a=="NO" && Rule2b=="FALSE")
    {
      FraudScore=FraudScore+20;
      BRList=c(BRList,"Mismatch in police report data")
    } 
  
#Business Rule3
  
  Rule3a<-grep("DIDN'T REALIZE",unstructureRecord$REMARKS,ignore.case=F,fixed = TRUE) 
  
  Rule3b<-grep("Careless",unstructureRecord$REMARKS,ignore.case=F,fixed = TRUE)
  
  Rule3c<-grep("Did not notice",unstructureRecord$REMARKS,ignore.case=F,fixed = TRUE)
  
  BR_CHECK <- Rule13a||Rule13b||Rule13c != 0
  Rule3 <- isTRUE(BR_CHECK)

  if(Rule3==TRUE)
    {
      FraudScore=FraudScore+20;
      BRList=c(BRList,"Unwanted terms were found")
    }
  
#Business Rule4
  
  Rule4 <-is.na(unstructureRecord$REMARKS)
  
  if(Rule4==TRUE)
    {
      FraudScore=FraudScore+20;
      BRList=c(BRList,"Remarks not present")
    }
  
# Printing the final outputs
  cat("\n\n\n")

}

if(FraudScore>0)
{
  cat("Fraud Score =",FraudScore)
  
  cat("\n\n\nThe List of triggered business rules are:\n\n")
  
  paste(BRList)
}

if(FraudScore==0)
{
  cat("Fraud Score =",FraudScore)
  
  cat("\n\n\nNo Fraud Business rules were triggered for this claim\n\n")
}

if(nrow(unstructureRecord)==0)
{
  print("CLAIMID NOT FOUND")
}

odbcClose(channel)
