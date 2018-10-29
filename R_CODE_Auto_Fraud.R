   # Getting Data into R

     Autodata_Into_R <- read.csv("autofraud_data.csv", header = T,sep=",")

   # Extracting Claim C1001 Record

     myData  <- read.csv("autofraud_data.csv", nrows=1)

   # PARAMETERS

     Injury_date <-  as.Date(myData$Injury.Date,format='%m/%d/%Y')
     Injury_Reported_Date <-  as.Date(myData$Injury.Reported.Date,format='%m/%d/%Y')

     Policy_effective_date  <- as.Date(myData$Policy.Effective.Date,format='%m/%d/%Y')
     Policy_expiration_date <- as.Date(myData$Policy.Expiration.Date,format='%m/%d/%Y')

     Employment_start_date <- as.Date(myData$Employment.Start.Date,format='%m/%d/%Y')
     Employment_end_date   <- as.Date(myData$Employment.End.Date,format='%m/%d/%Y')

     Injury_Loc       <- myData$Injury.Location          
     Injury_Loc_Check <- myData$Work.Location...Accident.Location

     Family_Members_Getting_Compensation <- myData$Other.family.members.receiving.compensation.benefits

     Is_Currently_Employed_Elsewhere <- myData$Is.currently.employed.elsewhere

     Claim_Amt_Mismatch  <- myData$Maintenance.Claim.Amount.Mismatch
     

   # Initializing Fraud Score to Zero and Triggered business rule list
     
     FV_LIST = list()
     Fraud_Value <- c(0)
     print(Fraud_Value)
     BR_LIST = list()
     

   # Business Rule 1

    Rule1 <- Injury_Reported_Date - Injury_date

    if(Rule1 > 15)
    {
     Fraud_Value <- Fraud_Value+20;
     }
    FV1 <- print(Fraud_Value)
    FV_LIST = c(FV_LIST,FV1) 
    BR_LIST = c(BR_LIST,"BR1 :- Injury occurring late Friday or early Monday")


   # Business Rule 2: 

    Rule2A <- Injury_date - Policy_effective_date
    Rule2B <- Policy_expiration_date - Injury_date

    if(Rule2A < 5 | Rule2B < 5)
    {
     Fraud_Value <- Fraud_Value+20;
    }
    FV2 <-print(Fraud_Value)
    FV_LIST = c(FV_LIST,FV2)
    BR_LIST = c(BR_LIST,"BR2 :- The claim is filed either shortly after coverage becomes effective, or just before cover ceases or shortly after the cover has been increased or the contract provisions are changed")


   # Business Rule 3: 

    Rule3A <- Injury_date - Employment_start_date
    Rule3B <- Employment_end_date - Injury_date

    if(Rule3A < 30 | Rule3B < 30)
    {
     Fraud_Value <- Fraud_Value+20;
    }
    FV3 <-print(Fraud_Value)
    FV_LIST = c(FV_LIST,FV3)
    BR_LIST = c(BR_LIST,"BR3 :- The claim is filed either shortly after employee has joined, or just before employment contract is about to expire")
  

   # Business Rule 4

    Rule4 <- Injury_Reported_Date - Injury_date

    if(Rule4 > 15)
    {
     Fraud_Value <- Fraud_Value+20;
    }
    FV4 <-print(Fraud_Value)
    FV_LIST = c(FV_LIST,FV4)
    BR_LIST = c(BR_LIST,"BR4 :- Late reporting. - The employee delays reporting the claim without a reasonable explanation.
    For example, Injury not reported until a week or more after it supposedly occurred
    Difference between Reporting Date / Time and Accident Date / Time")

     
   # Business Rule 5: 

    Rule5A <- Injury_Loc == "Same as normal work location"
    Rule5B <- Injury_Loc_Check == "Yes"

    if(Rule5A == TRUE | Rule5B == TRUE)
    {
     Fraud_Value <- Fraud_Value+20;
    }
    FV5 <-print(Fraud_Value)
    FV_LIST = c(FV_LIST,FV5)
    BR_LIST = c(BR_LIST,"BR5 :- Injury occurring in a location where the employee would not normally work Employee's normal work location")
  
     
   # Business Rule 6: 

    #No data available


   # Business Rule 7: 

    Rule7 <- Is_Currently_Employed_Elsewhere == "Yes"

    if(Rule7 == TRUE)
    {
     Fraud_Value <- Fraud_Value+20;
    }
    FV7 <-print(Fraud_Value)
    FV_LIST = c(FV_LIST,FV7)
    BR_LIST = c(BR_LIST,"BR7 :- Surveillance or tip indicates that the totally disabled worker is currently employed elsewhere.")
  
     
   # Business Rule 8:

    #No data available

   # Business Rule 9: 

    #No data available

     
   # Business Rule 10: 

    Rule10 <- Claim_Amt_Mismatch == "Yes"

    if(Rule10 == TRUE)
    {
     Fraud_Value <- Fraud_Value+20;
    }
    FV10 <-print(Fraud_Value)
    FV_LIST = c(FV_LIST,FV10)
    BR_LIST = c(BR_LIST,"BR10:- Overstating the amount of income maintenance being paid directly to a worker (Actual amount paid, amount claimed to have been paid")

     
   # Business Rule 11: 

    Rule11 <- Family_Members_Getting_Compensation == "Yes"

    if(Rule11 == TRUE)
    {
     Fraud_Value <- Fraud_Value+20;
    }
    FV11 <-print(Fraud_Value)
    FV_LIST = c(FV_LIST,FV11)
    BR_LIST = c(BR_LIST,"BR11:- Has several other family members also receiving workers compensation benefits or other social insurance benefits or unemployment.")
   
     
   # Business Rule 12:

    #No data available

   ## FINAL FRAUDSCORE VALUE
     
     FINAL_FV <- print(Fraud_Value)
     FV_LIST = c(FV_LIST,FINAL_FV)
