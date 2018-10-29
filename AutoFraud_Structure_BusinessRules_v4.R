#!/usr/bin/env Rscript

# Order in which Parameters are to be passed to the code in the command line 
# -loss_intimation_date 12/29/2013 ; -loss_date 12/20/2013 ; -policy_effective_date 12/18/2013 ; -policy_expiration_date 12/25/2013 ; -primary_loss_estimate 10000 ; -actual_loss_estimate 2000 ; -claim_type Theft ; -nature_of_damage "collision" ; -claim_num C1001

# Code for reading the command line inputs
args <- commandArgs(trailingOnly = TRUE)

# Reading the parameters from the command line and assigning to R varibles

Param1<-args[1]
Loss_intimation_date<-as.Date(args[2],format='%m/%d/%Y')
sc1<-args[3]
#Loss_intimation_date

Param2<-args[4]
Loss_date<-as.Date(args[5],format='%m/%d/%Y')
sc2<-args[6]
#Loss_date

Param3<-args[7]
Policy_effective_date <- as.Date(args[8],format='%m/%d/%Y')
sc3<-args[9]
#Policy_effective_date

Param4<-args[10]
Policy_expiration_date<- as.Date(args[11],format='%m/%d/%Y')
sc4<-args[12]
#Policy_expiration_date

Param5<-args[13]
Primary_loss_estimate<-as.integer(args[14])
sc5<-args[15]
#Primary_loss_estimate

Param6<-args[16]
Actual_loss_estimate<-as.integer(args[17])
sc6<-args[18]
#Actual_loss_estimate

Param7<-args[19]
Claim_type<-args[20]
sc7<-args[21]
#Claim_type

Param8<-args[22]
Nature_of_damage<-args[23]
sc8<-args[24]
#Nature_of_damage

Param9<-args[25]
ClaimNum <-args[26]
#ClaimNum

# Connecting to Oracle table
# Extract the record for the specific claim

suppressMessages(require(RODBC))

channel<-odbcConnect("RORACLE",uid="fraudrepo",pwd="fraudrepo",believeNRows=FALSE)

ClaimRecord=sqlQuery(channel,sprintf("select * from RENGINEINVOKE where Claim_id = '%s'",ClaimNum))
ClaimRecord$CLAIM_ID = as.character(ClaimRecord$CLAIM_ID)
ClaimRecord$BINDER_EXPIRATION_DATE = as.Date(as.character(ClaimRecord$BINDER_EXPIRATION_DATE),format='%Y-%m-%d')
ClaimRecord$LAST_PREMIUM_DUE_DATE = as.Date(as.character(ClaimRecord$LAST_PREMIUM_DUE_DATE),format='%Y-%m-%d')
ClaimRecord$LAST_PREMIUM_PAY = as.Date(as.character(ClaimRecord$LAST_PREMIUM_PAY),format='%Y-%m-%d')
ClaimRecord$LAST_ENDORSEMENT_DATE = as.Date(as.character(ClaimRecord$LAST_ENDORSEMENT_DATE),format='%Y-%m-%d')

# Initializing Fraud Score variable and Triggered business rule list

FraudScore=0
BRList=list();

#print ("Starting Fraud Score=") 
#FraudScore

Claim_Id_Check <-  ClaimNum  %in%  ClaimRecord$CLAIM_ID

if(Claim_Id_Check == TRUE)
{


# Business Rule1
  
Rule1 <- Loss_intimation_date-Loss_date

if(Rule1 > 15) 
{
  FraudScore=FraudScore+20
  BRList=c(BRList,"Claim reported after 15days of actual loss")  		     
}

# Business Rule2: 

Rule2a<-Loss_date-Policy_effective_date
Rule2b<-Policy_expiration_date-Loss_date

if(Rule2a<5|Rule2b<5)
{
  FraudScore=FraudScore+20
  BRList=c(BRList,"The claim is filed either shortly after coverage becomes effective, or just before cover ceases or shortly after the cover has been increased or the contract provisions are changed")	
}

# Business Rule3

Rule3<-Primary_loss_estimate-Actual_loss_estimate

if(Rule3 > 5000) 
{
  FraudScore=FraudScore+20
  BRList=c(BRList,"Actual loss is far lower than first reported loss")	
}

# Business Rule4

if (Claim_type=="Theft")
{
  FraudScore=FraudScore+20
  BRList=c(BRList,"Loss type is Theft")	
}

# Business Rule5

Result5=grepl("total loss|non drivable|major damage", Nature_of_damage, ignore.case=TRUE)

if ((Claim_type!="Collision") & Result5 == "TRUE")
{
  FraudScore=FraudScore+20
  BRList=c(BRList,"Severe damage is incurred without a collision")
}

# Business Rule6

if (ClaimRecord$PREVIOUS_LOSS_HISTORY=="Yes")
{
  FraudScore=FraudScore+20
  BRList=c(BRList,"The insured was involved in accidents before, with similar circumstances")
}

# Business Rule7

if (ClaimRecord$POLICE_REPORT=="No")
{
  FraudScore=FraudScore+20
  BRList=c(BRList,"After the accident, police or emergency services were not called for")
}

# Business Rule8

Rule8 <- Loss_date - ClaimRecord$LAST_ENDORSEMENT_DATE

if (Rule8<3)
{
  FraudScore=FraudScore+20
  BRList=c(BRList,"The date of modification / endorsement is too close to date of accident")
}

# Business Rule9 

# Rule9a<-ClaimRecord$LAST_PREMIUM_PAY-ClaimRecord$LAST_PREMIUM_DUE_DATE
# Rule9b<- Loss_date-ClaimRecord$LAST_PREMIUM_PAY

Rule9a<-ClaimRecord$LAST_PREMIUM_PAY-ClaimRecord$LAST_PREMIUM_DUE_DATE
Rule9b<- Loss_date-ClaimRecord$LAST_PREMIUM_PAY

if(Rule9a>30 & Rule9b<5)
{
  FraudScore=FraudScore+20
  BRList=c(BRList,"The loss occurs just after payment of premiums that were long overdue")	
}

# Business Rule10

if (Loss_date<ClaimRecord$BINDER_EXPIRATION_DATE)
{
  FraudScore=FraudScore+20
  BRList=c(BRList,"Damage has occurred in the period of provisional cover")
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

if(Claim_Id_Check==FALSE)
{
  print("CLAIMID NOT FOUND!") 
}


odbcClose(channel)