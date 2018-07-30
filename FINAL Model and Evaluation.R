# install.packages("plyr")
# install.packages("dplyr")
# install.packages("Information")
# install.packages("caret")
# install.packages("e1071")
# install.packages("rpart.plot")
# install.packages("unbalanced")
# install.packages("mlr")
# install.packages("randomForest")
# install.packages("ggplot2")
# install.packages("cowplot")
# install.packages("unbalanced")
# install.packages("car")
library(plyr)
library(MASS)
library(dplyr)
library(Information)
library(caret)
library(e1071)
library(rpart.plot)
library(unbalanced)
library(mlr)
library(randomForest)
library(ggplot2)
library(cowplot)
library(unbalanced)
library(car)

#### IMPORTING DATASETS
dem <- read.csv("Demographic data.csv")
cred <- read.csv("Credit Bureau data.csv")

##### DATA CLEANING #####
### checking for duplicate ids in demographic and credit bureau dataset
str(dem)
dem <- dem[!duplicated(dem$Application.ID),]
str(dem)
str(cred)
cred <- cred[!duplicated(cred$Application.ID),]
str(cred)

#MERGING BOTH DATASETS TO CREATE MASTER DATASET.
master <- merge(x=dem,y=cred,by="Application.ID",all=TRUE)
str(master)
summary(master)

#Since Performance tag was in both datasets hence we evaluate if these columns are identical or different.
identical(master[['Performance.Tag.y']],master[['Performance.Tag.x']])

#SINCE BOTH COLUMNS ARE IDENTICAL HENCE WE REMOVE ONE OF THE COLUMNS AND RENAME THE OTHER COLUMN
# master=select(master,-Performance.Tag.x)
master <- master[,-12]
#Removing one Performance Tag column . 
colnames(master)[colnames(master)=="Performance.Tag.y"]="Performance.Tag"
#CHECKING FOR DUPLICATES IN MASTER 
master[duplicated(master$Application.ID),]
#THERE ARE NO DUPLICATES

#NOW WE WILL HANDLE MISSING VALUES, NEGATIVE VALUES AND OUTLIERS IN R
graphics.off()
par(mar=c(2,2,2,2))
boxplot(master$Age)
#master=master[!master$Age<=0,]
table(master$Age[master$Age<=0])
master$Age[master$Age<18] <- NA

#removing blank rows with blank values in gender(only 1 row)
master <- master[!master$Gender=="",]
anyNA(master$Gender)

#removing blank rows with blank values in marital status(only 5 row)
master <- master[!master$Marital.Status..at.the.time.of.application.=="",]
anyNA(master$Marital.Status..at.the.time.of.application.)
anyNA(master$No.of.dependents)
master <- master[!is.na(master$No.of.dependents),]#removing 3 rows

table(master$Income==-0.5)## There 81 values with -0.5 as its income HENCE REPLACING WITH MODE COULD INTRODUCE BIAS
#master$Income[master$Income==-0.5]=0.0
master$Income[master$Income==-0.5] <- NA
master[master$Education=="",]

#SINCE THERE ARE LOT OF ENTRIES WITH BLANK VALUE IN EDUCATION HENCE WE WILL REPLACE THESE BLANK VALUES BY MISSING FOR FUTURE ANALYSIS
master$Education <- as.character(master$Education)
master$Education[master$Education==""] <- "Missing"

summary(master$Profession)
#from the observation we can see that the number of candidates with SAL profession is the highest hence we replace the blank values with SAL.
master$Profession <- as.character(master$Profession)
table(master$Profession=="")#THERE ARE 14 BLANK VALUES IN PROFESSION HENCE WE CAN REPLACE IT WITH MODE
master$Profession[master$Profession==""] <- "SAL"

#CHECK FOR BLANK AND NA VALUES IN TYPE OF RESIDENCE
anyNA(master$Type.of.residence)
table(master$Type.of.residence=="")#There are 8 blank values in   TYPE OF RESIDENCE
master[master$Type.of.residence=="",]
summary(master$Type.of.residence)#MODE IN TYPE OF RESIDENCE IS "RENTED". Hence we replace blank values with RENTED
master$Type.of.residence=as.character(master$Type.of.residence)
master$Type.of.residence[master$Type.of.residence==""] <- "Rented"
anyNA(master$No.of.months.in.current.residence)
master[master$No.of.months.in.current.residence=="",]
boxplot(master$No.of.months.in.current.residence)
summary(master$No.of.months.in.current.residence)
#No of months in current residence is clean

master[master$No.of.months.in.current.company=="",]
boxplot(master$No.of.months.in.current.company)
summary(master$No.of.months.in.current.company)
#no of months in current company is clean
#Now we check missing data in 90,60,30 days past due in last 6 months
master[master$No.of.times.90.DPD.or.worse.in.last.6.months=="",]
boxplot(master$No.of.times.90.DPD.or.worse.in.last.6.months)
summary(master$No.of.times.90.DPD.or.worse.in.last.6.months)
master[master$No.of.times.60.DPD.or.worse.in.last.6.months=="",]
boxplot(master$No.of.times.60.DPD.or.worse.in.last.6.months)
summary(master$No.of.times.60.DPD.or.worse.in.last.6.months)
master[master$No.of.times.30.DPD.or.worse.in.last.6.months=="",]
boxplot(master$No.of.times.30.DPD.or.worse.in.last.6.months)
summary(master$No.of.times.30.DPD.or.worse.in.last.6.months)

#Now we check missing data in 90,60,30 days past due in last 12 months
master[master$No.of.times.90.DPD.or.worse.in.last.12.months=="",]
boxplot(master$No.of.times.90.DPD.or.worse.in.last.12.months)
summary(master$No.of.times.90.DPD.or.worse.in.last.12.months)
master[master$No.of.times.60.DPD.or.worse.in.last.12.months=="",]
boxplot(master$No.of.times.60.DPD.or.worse.in.last.12.months)
summary(master$No.of.times.60.DPD.or.worse.in.last.12.months)
master[master$No.of.times.30.DPD.or.worse.in.last.12.months=="",]
boxplot(master$No.of.times.30.DPD.or.worse.in.last.12.months)
summary(master$No.of.times.30.DPD.or.worse.in.last.12.months)

#Now we check for avg credit card utilization in last 12 months
anyNA(master$Avgas.CC.Utilization.in.last.12.months)
summary(master$Avgas.CC.Utilization.in.last.12.months)

#Now we check for no of trades opened in last six months
boxplot(master$No.of.trades.opened.in.last.6.months)
summary(master$No.of.trades.opened.in.last.6.months)
master <- master[!is.na(master$No.of.trades.opened.in.last.6.months),]
summary(master)
##CLEANING AND EXPLORATORY DATA ANALYSIS COMPLETED

##WOE AND IV ANALYSIS TO IMPUTE MISSING VALUES IN AVG CC UTILIZATION, PRESENCE OF OPEN HOME LOAN, OUTSTANDING BALANCE
#CONVERTING CATEGORICAL INDEPENDENT VARIABLES INTO FACTORS AND DEPENDENT VARIABLE INTO NUMERIC FOR WOE AND IV
sapply(master,class)
master$Education <- as.factor(master$Education)
master$No.of.dependents <- as.factor(master$No.of.dependents)
master$Profession <- as.factor(master$Profession)
master$Type.of.residence <- as.factor(master$Type.of.residence)
master$Presence.of.open.home.loan <- as.factor(master$Presence.of.open.home.loan)
master$Presence.of.open.auto.loan <- as.factor(master$Presence.of.open.auto.loan)
master$Performance.Tag <- as.numeric(master$Performance.Tag)

#SPLITTING THE DATASET ALONG REJECTED AND SELECTED APPLICANTS
rejected <- subset(master,is.na(master$Performance.Tag))
selected <- subset(master,!is.na(master$Performance.Tag))
woe_data <- selected
IV <- create_infotables(data=woe_data, y="Performance.Tag", bins=10, parallel=FALSE)
woe_data$Age <- as.numeric(woe_data$Age)
woe_data$Age <- cut(woe_data$Age,breaks = c(14,30,35,38,41,44,47,50,53,57,65),labels = c("[18,30]", "[31,35]", "[36,38]", "[39,41]", "[42,44]", "[45,47]", "[48,50]", "[51,53]", "[54,57]", "[58,65]"))

#SINCE NA VALUE IN IV TABLE IS IN CHARACTER HENCE WE HAVE TO CONVERT NA VALUES FROM FACTOR TO CHARACTER IN WOE_DATA$AGE TO MAP THE ORIGINAL VALUES TO THE WOE VALUES
woe_data$Age <- addNA(woe_data$Age)
levels(woe_data$Age) <- c(levels(woe_data$Age), "NA")
woe_data$Age[is.na(woe_data$Age)]="NA"
woe_data$Age <- mapvalues(woe_data$Age, from = IV$Tables$Age$Age, to = IV$Tables$Age$WOE)
woe_data$Gender <- mapvalues(woe_data$Gender, from = IV$Tables$Gender$Gender, to = IV$Tables$Gender$WOE)
woe_data$Marital.Status..at.the.time.of.application. <- mapvalues(woe_data$Marital.Status..at.the.time.of.application.,from = IV$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to = IV$Tables$Marital.Status..at.the.time.of.application.$WOE)
woe_data$No.of.dependents <- mapvalues(woe_data$No.of.dependents, from = IV$Tables$No.of.dependents$No.of.dependents, to=IV$Tables$No.of.dependents$WOE)
woe_data$Education <- mapvalues(woe_data$Education,from = IV$Tables$Education$Education, to = IV$Tables$Education$WOE)
woe_data$Profession <- mapvalues(woe_data$Profession,from = IV$Tables$Profession$Profession,to = IV$Tables$Profession$WOE)
woe_data$Type.of.residence <- mapvalues(woe_data$Type.of.residence,from = IV$Tables$Type.of.residence$Type.of.residence, to = IV$Tables$Type.of.residence$WOE)

#SAME WILL BE PERFORMED IN INCOME AS WE DID IN AGE
woe_data$Income <- as.numeric(woe_data$Income)
woe_data$Income <- cut(woe_data$Income,breaks = c(-1,5,10,16,21,26,31,36,41,48,60),labels = c("[0,5]", "[6,10]", "[11,16]", "[17,21]", "[22,26]", "[27,31]", "[32,36]", "[37,41]", "[42,48]", "[49,60]"))
woe_data$Income <- addNA(woe_data$Income)
levels(woe_data$Income) <- c(levels(woe_data$Income), "NA")
woe_data$Income[is.na(woe_data$Income)] <- "NA"
woe_data$Income <- mapvalues(woe_data$Income,from = IV$Tables$Income$Income, to=IV$Tables$Income$WOE)

#CHANGING NO OF MONTHS IN CURRENT RESIDENCE FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.months.in.current.residence <- as.numeric(woe_data$No.of.months.in.current.residence)
woe_data$No.of.months.in.current.residence <- cut(woe_data$No.of.months.in.current.residence,breaks = c(5,9,28,49,72,97,126),labels = c("[6,9]", "[10,28]", "[29,49]", "[50,72]", "[73,97]", "[98,126]"))
woe_data$No.of.months.in.current.residence <- mapvalues(woe_data$No.of.months.in.current.residence,from = IV$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence,to=IV$Tables$No.of.months.in.current.residence$WOE)

#CHANGING NO OF MONTHS IN CURRENT company FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.months.in.current.company <- as.numeric(woe_data$No.of.months.in.current.company)
woe_data$No.of.months.in.current.company <- cut(woe_data$No.of.months.in.current.company,breaks = c(2,5,12,19,26,33,40,47,53,61,133),labels = c("[3,5]", "[6,12]", "[13,19]", "[20,26]", "[27,33]", "[34,40]","[41,47]","[48,53]","[54,61]","[62,133]"))
woe_data$No.of.months.in.current.company <- mapvalues(woe_data$No.of.months.in.current.company,from = IV$Tables$No.of.months.in.current.company$No.of.months.in.current.company,to=IV$Tables$No.of.months.in.current.company$WOE)

#CHANGING NO OF times 90 days past due or more FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.90.DPD.or.worse.in.last.6.months <- as.numeric(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months)
woe_data$No.of.times.90.DPD.or.worse.in.last.6.months <- cut(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months,breaks = c(-1,0,3),labels = c("[0,0]", "[1,3]"))
woe_data$No.of.times.90.DPD.or.worse.in.last.6.months <- mapvalues(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE)

#CHANGING NO OF times 60 days past due or more FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.60.DPD.or.worse.in.last.6.months <- as.numeric(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months)
woe_data$No.of.times.60.DPD.or.worse.in.last.6.months <- cut(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months,breaks = c(-1,0,5),labels = c("[0,0]", "[1,5]"))
woe_data$No.of.times.60.DPD.or.worse.in.last.6.months <- mapvalues(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE)

#CHANGING NO OF times 30 days past due or more FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months <- as.numeric(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months)
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months <- cut(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months,breaks = c(-1,0,1,7),labels = c("[0,0]", "[1,1]","[2,7]"))
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months <- mapvalues(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE)

#CHANGING NO OF times 90 days past due FOR 12 MONTHS or more FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months <- as.numeric(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months)
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months <- cut(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months,breaks = c(-1,0,1,5),labels = c("[0,0]", "[1,1]","[2,5]"))
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months <- mapvalues(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE)

#CHANGING NO OF times 60 days past due or more FOR 12 MONTHS FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months <- as.numeric(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months)
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months <- cut(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months,breaks = c(-1,0,1,7),labels = c("[0,0]", "[1,1]","[2,7]"))
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months <- mapvalues(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE)

#CHANGING NO OF times 30 days past due or more  FOR 12 MONTHS FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months <- as.numeric(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months)
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months <- cut(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months,breaks = c(-1,0,2,9),labels = c("[0,0]", "[1,2]","[3,9]"))
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months <- mapvalues(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE)

#SAME WILL BE PERFORMED IN AVERAGE CREDIT CARD UTILIZATION AS WE DID IN AGE
woe_data$Avgas.CC.Utilization.in.last.12.months <- as.numeric(woe_data$Avgas.CC.Utilization.in.last.12.months)
woe_data$Avgas.CC.Utilization.in.last.12.months <- cut(woe_data$Avgas.CC.Utilization.in.last.12.months,breaks = c(-1,4,6,8,11,14,21,37,51,71,113),labels = c("[0,4]", "[5,6]", "[7,8]", "[9,11]", "[12,14]", "[15,21]", "[22,37]", "[38,51]", "[52,71]", "[72,113]"))
woe_data$Avgas.CC.Utilization.in.last.12.months <- addNA(woe_data$Avgas.CC.Utilization.in.last.12.months)
levels(woe_data$Avgas.CC.Utilization.in.last.12.months) <- c(levels(woe_data$Avgas.CC.Utilization.in.last.12.months), "NA")
woe_data$Avgas.CC.Utilization.in.last.12.months[is.na(woe_data$Avgas.CC.Utilization.in.last.12.months)] <- "NA"
woe_data$Avgas.CC.Utilization.in.last.12.months <- mapvalues(woe_data$Avgas.CC.Utilization.in.last.12.months,from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months,to=IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE)

#MAPPING WOE VALUES FOR NO OF TRADES OPENED IN LAST 6 MONTHS
woe_data$No.of.trades.opened.in.last.6.months <- as.numeric(woe_data$No.of.trades.opened.in.last.6.months)
woe_data$No.of.trades.opened.in.last.6.months <- cut(woe_data$No.of.trades.opened.in.last.6.months,breaks = c(-1,0,1,2,3,4,12),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,12]"))
woe_data$No.of.trades.opened.in.last.6.months <- mapvalues(woe_data$No.of.trades.opened.in.last.6.months,from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months,to=IV$Tables$No.of.trades.opened.in.last.6.months$WOE)

#MAPPING WOE VALUES FOR NO OF TRADES OPENED IN LAST 12 MONTHS
woe_data$No.of.trades.opened.in.last.12.months <- as.numeric(woe_data$No.of.trades.opened.in.last.12.months)
woe_data$No.of.trades.opened.in.last.12.months <- cut(woe_data$No.of.trades.opened.in.last.12.months,breaks = c(-1,0,1,2,3,5,7,9,12,28),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]", "[4,5]", "[6,7]","[8,9]","[10,12]","[13,28]"))
woe_data$No.of.trades.opened.in.last.12.months <- mapvalues(woe_data$No.of.trades.opened.in.last.12.months,from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months,to=IV$Tables$No.of.trades.opened.in.last.12.months$WOE)

#MAPPING WOE VALUES FOR pl OF TRADES OPENED IN LAST 6 MONTHS
woe_data$No.of.PL.trades.opened.in.last.6.months <- as.numeric(woe_data$No.of.PL.trades.opened.in.last.6.months)
woe_data$No.of.PL.trades.opened.in.last.6.months <- cut(woe_data$No.of.PL.trades.opened.in.last.6.months,breaks = c(-1,0,1,2,4),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,6]"))
woe_data$No.of.PL.trades.opened.in.last.6.months <- mapvalues(woe_data$No.of.PL.trades.opened.in.last.6.months,from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months,to=IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE)

#MAPPING WOE VALUES FOR pl OF TRADES OPENED IN LAST 12 MONTHS
woe_data$No.of.PL.trades.opened.in.last.12.months <- as.numeric(woe_data$No.of.PL.trades.opened.in.last.12.months)
woe_data$No.of.PL.trades.opened.in.last.12.months <- cut(woe_data$No.of.PL.trades.opened.in.last.12.months,breaks = c(-1,0,1,2,3,4,5,12),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]","[4,4]","[5,5]","[6,12]"))
woe_data$No.of.PL.trades.opened.in.last.12.months <- mapvalues(woe_data$No.of.PL.trades.opened.in.last.12.months,from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months,to=IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE)

#MAPPING WOE VALUES FOR NO OF INQUIRIES IN LAST SIX MONTHS
woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.numeric(woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- cut(woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,breaks = c(-1,0,1,2,4,10),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,4]","[5,10]"))
woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- mapvalues(woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,to=IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE)

#MAPPING WOE VALUES FOR NO OF INQUIRIES IN LAST 12 MONTHS
woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- as.numeric(woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- cut(woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,breaks = c(-1,0,1,2,3,4,5,8,20),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]","[4,4]","[5,5]","[6,8]","[9,20]"))
woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- mapvalues(woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,to=IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE)

#MAPPING WOE VALUES FOR PRESENCE OF OPEN HOME LOAN
woe_data$Presence.of.open.home.loan <- addNA(woe_data$Presence.of.open.home.loan)
woe_data$Presence.of.open.home.loan <- mapvalues(woe_data$Presence.of.open.home.loan,from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan,to=IV$Tables$Presence.of.open.home.loan$WOE)

#MAPPING WOE VALUES FOR OUTSTANDING BALANCE
woe_data$Outstanding.Balance <- as.numeric(woe_data$Outstanding.Balance)
woe_data$Outstanding.Balance <- cut(woe_data$Outstanding.Balance,breaks = c(-1,6843,25509,386809,585402,774228,972455,1357300,2960987,3282013,5218801),labels = c("[0,6843]", "[6847,25509]", "[25522,386809]", "[386813,585402]", "[585423,774228]", "[774241,972455]", "[972456,1357300]", "[1357399,2960987]", "[2960994,3282013]", "[3282027,5218801]"))
woe_data$Outstanding.Balance <- addNA(woe_data$Outstanding.Balance)
levels(woe_data$Outstanding.Balance) <- c(levels(woe_data$Outstanding.Balance), "NA")
woe_data$Outstanding.Balance[is.na(woe_data$Outstanding.Balance)] <- "NA"
woe_data$Outstanding.Balance <- mapvalues(woe_data$Outstanding.Balance,from = IV$Tables$Outstanding.Balance$Outstanding.Balance,to=IV$Tables$Outstanding.Balance$WOE)

#MAPPING WOE VALUES FOR TOTAL NO OF TRADES
woe_data$Total.No.of.Trades <- as.numeric(woe_data$Total.No.of.Trades)
woe_data$Total.No.of.Trades <- cut(woe_data$Total.No.of.Trades,breaks = c(-1,1,2,3,4,5,6,8,10,19,44),labels = c("[0,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]","[7,8]","[9,10]","[11,19]","[20,44]"))
woe_data$Total.No.of.Trades <- mapvalues(woe_data$Total.No.of.Trades,from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades,to=IV$Tables$Total.No.of.Trades$WOE)

#MAPPING WOE VALUES FOR PRESENCE OF AUTO OPEN HOME LOANS
woe_data$Presence.of.open.auto.loan <- mapvalues(woe_data$Presence.of.open.auto.loan,from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan,to=IV$Tables$Presence.of.open.auto.loan$WOE)

######################   WOE COMPLETED #############################

#CHECK FOR ANY MISSING STILL THERE
anyNA(woe_data)#FALSE. HENCE OUR DATASET DOES NOT CONTAIN ANY MISSING VALUES AND ALL VALUES ARE REPLACED BY WOE VALUES

########################  WOE AND ANALYSIS BY PLOTTING AND LOGISTIC REGRESSION ##################
####MODEL BUILDING ON DEMOGRAPHIC DATA USING LOGISTIC REGRESSION####

woe_dem=woe_data[,c(1,2,3,4,5,6,7,8,9,10,11,29)]
set.seed(3033)
in_train <- createDataPartition(woe_dem$Performance.Tag, p=0.7, list = F)
training <- woe_dem[in_train,]
testing <- woe_dem[-in_train,]
training[["Performance.Tag"]] <- factor(training[["Performance.Tag"]])
logit_model <- glm(Performance.Tag ~ Age+Gender+Marital.Status..at.the.time.of.application.+
                     No.of.dependents+Income+Education+Profession+
                     Type.of.residence+No.of.months.in.current.residence+
                     No.of.months.in.current.company,family = "binomial",data = training)

stepAIC(logit_model)

logit_model_2 <-glm(formula = Performance.Tag ~ Income + Profession + No.of.months.in.current.residence + 
                      No.of.months.in.current.company, family = "binomial", data = training)

summary(logit_model_2)

#EVALUATING THE LOGISTIC MODEL.

pred <- predict(logit_model_2, newdata = testing, type = "response")
y_pred_num <- ifelse(pred > 0.2, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testing$Performance.Tag
mean(y_pred == y_act)
testing[["Performance.Tag"]] <- factor(testing[["Performance.Tag"]])
confusionMatrix(y_pred,testing$Performance.Tag)

####95.82% ACCURACY ACHIEVED. However negative Prediction value is 0% because it has displayed positives for 873 negatives.
## Hence logistic regression may not be as efficient on a new dataset.HOWEVER PREDICTIVE POWER OF DIFFERENT VARIABLES ARE NOTED.


###### BALANCING THE DATASET TO TACKLE IMBALANCED CLASSIFICATION######


woe_master <- woe_data

######MODEL BUILDING AND EVALUATION ON MASTER DATASE USING LOGISTIC REGRESSION ######
# woe_master_balanced=balanced_data[,2:29]

set.seed(3033)
in_train <- createDataPartition(woe_master$Performance.Tag, p=0.7, list = F)
training <- woe_master[in_train,]
testing <- woe_master[-in_train,]

data <- ubBalance(X= training[,-29], Y=as.factor(training[,29]), type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
balanced_data <- cbind(data$X,data$Y)
summary(balanced_data)
colnames(balanced_data)[29] <- "Performance.Tag"


testing[["Performance.Tag"]] <- factor(testing[["Performance.Tag"]])
# training[["Performance.Tag"]]=factor(training[["Performance.Tag"]])


#Considering all the Variables with IV values having Meadium and strong Predicive power . 

logit_model <- glm(Performance.Tag~Avgas.CC.Utilization.in.last.12.months+No.of.trades.opened.in.last.12.months+No.of.PL.trades.opened.in.last.12.months+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+Outstanding.Balance+No.of.times.30.DPD.or.worse.in.last.6.months+Total.No.of.Trades+No.of.PL.trades.opened.in.last.6.months+No.of.times.90.DPD.or.worse.in.last.12.months+No.of.times.60.DPD.or.worse.in.last.6.months+No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
                   ,family = "binomial",data =balanced_data )
logit_model
summary(logit_model)

stepAIC(logit_model)


logit_model_2 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                       No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                       No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                       Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                       Total.No.of.Trades + No.of.times.90.DPD.or.worse.in.last.12.months + 
                       No.of.times.60.DPD.or.worse.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., 
                     family = "binomial", data = balanced_data)
summary(logit_model_2)
vif(logit_model_2)


##removing "No.of.trades.opened.in.last.12.months" as it has high vif
logit_model_3 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                       No.of.PL.trades.opened.in.last.12.months + 
                       No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                       Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                       Total.No.of.Trades + No.of.times.90.DPD.or.worse.in.last.12.months + 
                       No.of.times.60.DPD.or.worse.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., 
                     family = "binomial", data = balanced_data)
summary(logit_model_3)
vif(logit_model_3)


##removing "No.of.PL.trades.opened.in.last.12.months" as it has high vif
logit_model_4 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                       No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                       Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                       Total.No.of.Trades + No.of.times.90.DPD.or.worse.in.last.12.months + 
                       No.of.times.60.DPD.or.worse.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., 
                     family = "binomial", data = balanced_data)
summary(logit_model_4)
vif(logit_model_4)


## removing "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans." as it has high vif
logit_model_5 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                       Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                       Total.No.of.Trades + No.of.times.90.DPD.or.worse.in.last.12.months + 
                       No.of.times.60.DPD.or.worse.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., 
                     family = "binomial", data = balanced_data)
summary(logit_model_5)
vif(logit_model_5)


## removing "Total.No.of.Trades" as it less significant
logit_model_6 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                       Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                       No.of.times.90.DPD.or.worse.in.last.12.months + 
                       No.of.times.60.DPD.or.worse.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., 
                     family = "binomial", data = balanced_data)
summary(logit_model_6)


final_model_balanced <- logit_model_6

#EVALUATING THE LOGISTIC MODEL.

#Model Evaluation: Logistic Regression (test data)

pred <- predict(final_model_balanced, newdata = testing , type = "response")

# Let's use the probability cutoff of 50%.

predicted_Performance.Tag <- factor(ifelse(pred >= 0.5, "yes", "no"))


# Creating confusion matrix for identifying the model evaluation.

levels(testing$Performance.Tag)[levels(testing$Performance.Tag)==0]<-"no"
levels(testing$Performance.Tag)[levels(testing$Performance.Tag)==1]<-"yes"


conf <- confusionMatrix(predicted_Performance.Tag, testing$Performance.Tag, positive = "yes")

conf

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  14427   429
# yes  5652   448
# 
# Accuracy : 0.7098         
# 95% CI : (0.7036, 0.716)
# No Information Rate : 0.9582         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.0596         
# Mcnemar's Test P-Value : <2e-16         
#                                          
#             Sensitivity : 0.51083        
#             Specificity : 0.71851        
#          Pos Pred Value : 0.07344        
#          Neg Pred Value : 0.97112        
#              Prevalence : 0.04185        
#          Detection Rate : 0.02138        
#    Detection Prevalence : 0.29109        
#       Balanced Accuracy : 0.61467        
#                                          
#        'Positive' Class : yes    


# ###CHECKING THE OPTIMAL CUTPOINTS
# 
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_Performance.Tag <- factor(ifelse(pred >= cutoff, "yes", "no"), levels=c("no", "yes"))
  conf <- confusionMatrix(predicted_Performance.Tag, testing$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
cutoff

# Let's choose a cutoff value of 0.4169 for final model

y_pred <- factor(ifelse(pred > cutoff, "yes", "no"))

# y_pred <- factor(y_pred_num, levels=c(0, 1))

conf_final <- conf <- confusionMatrix(y_pred, testing$Performance.Tag)

accuracy <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

accuracy
# Accuracy 
# 0.6299866 
sens
# Sensitivity 
# 0.6304597 
spec
# Specificity 
# 0.6191562 

####################


#For Entire Dataset testing

woe_master$Performance.Tag<- as.factor(woe_master$Performance.Tag)
levels(woe_master$Performance.Tag)[levels(woe_master$Performance.Tag)==0]<-"no"
levels(woe_master$Performance.Tag)[levels(woe_master$Performance.Tag)==1]<-"yes"



predictions_logit_final <- predict(final_model_balanced, newdata = woe_master[, -29], type = "response")

# View(predictions_logit_final)
summary(predictions_logit_final)

predicted_Performance.Tag <- factor(ifelse(predictions_logit_final >= 0.5, "yes", "no"))


# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_Performance.Tag, woe_master$Performance.Tag, positive = "yes")

conf





# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_Performance.Tag <- factor(ifelse(predictions_logit_final >= cutoff, "yes", "no"), levels = c("no", "yes"))
  conf <- confusionMatrix(predicted_Performance.Tag, woe_master$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)]

cutoff

predicted_Performance.Tag <- factor(ifelse(predictions_logit_final >= 0.4158, "yes", "no"))


# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_Performance.Tag, woe_master$Performance.Tag, positive = "yes")

conf

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  41998  1117
# yes 24910  1830
# 
# Accuracy : 0.6274         
# 95% CI : (0.6238, 0.631)
# No Information Rate : 0.9578         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.0512         
# Mcnemar's Test P-Value : <2e-16         
# 
# Sensitivity : 0.62097        
# Specificity : 0.62770        
# Pos Pred Value : 0.06844        
# Neg Pred Value : 0.97409        
# Prevalence : 0.04219        
# Detection Rate : 0.02620        
# Detection Prevalence : 0.38279        
# Balanced Accuracy : 0.62433        
# 
# 'Positive' Class : yes      

#### MODEL BUILDING ON MASTER DATASET USING RANDOM FOREST ######

#Building the Random Forest model on the partioned data earlier and the balanced training data . 

#### Tuning parameters of random forest modelling on the dataset ####

woe_master_balanced=woe_data[,2:29]
set.seed(3033)
in_train <- createDataPartition(woe_master_balanced$Performance.Tag, p=0.7, list = F)
training=woe_master_balanced[in_train,] 
testing=woe_master_balanced[-in_train,]

data<-ubBalance(X= training[,-28], Y=as.factor(training[,28]), type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
balanced_data=cbind(data$X,data$Y)
summary(balanced_data)
colnames(balanced_data)[28]="Performance.Tag"

str(training$Performance.Tag)

balanced_data[["Performance.Tag"]]=factor(balanced_data[["Performance.Tag"]])
testing[["Performance.Tag"]]=factor(testing[["Performance.Tag"]])

levels(testing$Performance.Tag)[levels(testing$Performance.Tag)==0]<-"no"
levels(testing$Performance.Tag)[levels(testing$Performance.Tag)==1]<-"yes"


trainTask <- makeClassifTask(data = balanced_data,target = "Performance.Tag")
testTask <- makeClassifTask(data = testing,target = "Performance.Tag")

rf <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(importance = TRUE)

rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50))
rancontrol <- makeTuneControlRandom(maxit = 50L)

set_cv <- makeResampleDesc("CV",iters = 3L)

rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, 
                      control = rancontrol, measures = acc)

####[Tune] ntree=483; mtry=8; nodesize=20 : acc.test.mean=0.8571753


#Using the tuning Parameters ,and building the Random Forest model . 

fit_Final <- randomForest(Performance.Tag~ . ,balanced_data , ntree=483, mtry=8, nodesize=20)

rf_pred_final <- predict(fit_Final, testing[, -29], type = "prob")

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- factor(ifelse(rf_pred_final[, 2] >= cutoff, "yes", "no"), levels=c("no","yes"))
  conf <- confusionMatrix(predicted_response, as.factor(testing$Performance.Tag))
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]

cutoff_rf

predicted_response <- factor(ifelse(rf_pred_final[, 2] >= 0.247, "yes", "no"), levels=c("no", "yes"))

conf_forest <- confusionMatrix(predicted_response, testing$Performance.Tag)

conf_forest

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  12338   330
# yes  7741   547
# 
# Accuracy : 0.6149          
# 95% CI : (0.6082, 0.6215)
# No Information Rate : 0.9582          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0473          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.62372         
# Specificity : 0.61447         
# Pos Pred Value : 0.06600         
# Neg Pred Value : 0.97395         
# Prevalence : 0.04185         
# Detection Rate : 0.02610         
# Detection Prevalence : 0.39550         
# Balanced Accuracy : 0.61910         


# Predict response for entire woe_master data

rf_pred_final <- predict(fit_Final, woe_master[, -29], type = "prob")

head(rf_pred_final)

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- factor(ifelse(rf_pred_final[, 2] >= cutoff, "yes", "no"), levels = c("no", "yes"))
  conf <- confusionMatrix(predicted_response, woe_master$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]

cutoff_rf

# The plot shows that cutoff value of around 29% optimises sensitivity and accuracy

predicted_response_final <- factor(ifelse(rf_pred_final[, 2] >= 0.2970, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_final, woe_master[, 29])

conf_forest

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  49732   777
# yes 17176  2170
# 
# Accuracy : 0.743           
# 95% CI : (0.7397, 0.7462)
# No Information Rate : 0.9578          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.1311          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.7433          
# Specificity : 0.7363          
# Pos Pred Value : 0.9846          
# Neg Pred Value : 0.1122          
# Prevalence : 0.9578          
# Detection Rate : 0.7119          
# Detection Prevalence : 0.7231          
# Balanced Accuracy : 0.7398          
# 
# 'Positive' Class : no 

#Final Model Selection 

final_model <- fit_Final

####### PREDICTING THE LIKELIHOOD OF DEFAULT ON REJECTED CANDIDATES ########


##### 1. converting rejected data into woe data #########
rejected$Performance.Tag=1
rejected$Age=as.numeric(rejected$Age)
rejected$Age=cut(rejected$Age,breaks = c(14,30,35,38,41,44,47,50,53,57,65),labels = c("[18,30]", "[31,35]", "[36,38]", "[39,41]", "[42,44]", "[45,47]", "[48,50]", "[51,53]", "[54,57]", "[58,65]"))
#SINCE NA VALUE IN IV TABLE IS IN CHARACTER HENCE WE HAVE TO CONVERT NA VALUES FROM FACTOR TO CHARACTER IN rejected$AGE TO MAP THE ORIGINAL VALUES TO THE WOE VALUES
rejected$Age=addNA(rejected$Age)
levels(rejected$Age) <- c(levels(rejected$Age), "NA")
rejected$Age[is.na(rejected$Age)]="NA"
rejected$Age=mapvalues(rejected$Age,from = IV$Tables$Age$Age,to=IV$Tables$Age$WOE)
rejected$Gender=mapvalues(rejected$Gender,from = IV$Tables$Gender$Gender, to=IV$Tables$Gender$WOE)
rejected$Marital.Status..at.the.time.of.application.=mapvalues(rejected$Marital.Status..at.the.time.of.application.,from = IV$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to=IV$Tables$Marital.Status..at.the.time.of.application.$WOE)
rejected$No.of.dependents=mapvalues(rejected$No.of.dependents,from = IV$Tables$No.of.dependents$No.of.dependents, to=IV$Tables$No.of.dependents$WOE)
rejected$Education=mapvalues(rejected$Education,from = IV$Tables$Education$Education,to=IV$Tables$Education$WOE)
rejected$Profession=mapvalues(rejected$Profession,from = IV$Tables$Profession$Profession,to=IV$Tables$Profession$WOE)
rejected$Type.of.residence=mapvalues(rejected$Type.of.residence,from = IV$Tables$Type.of.residence$Type.of.residence,to=IV$Tables$Type.of.residence$WOE)
#SAME WILL BE PERFORMED IN INCOME AS WE DID IN AGE
rejected$Income=as.numeric(rejected$Income)
rejected$Income=cut(rejected$Income,breaks = c(-1,5,10,16,21,26,31,36,41,48,60),labels = c("[0,5]", "[6,10]", "[11,16]", "[17,21]", "[22,26]", "[27,31]", "[32,36]", "[37,41]", "[42,48]", "[49,60]"))
rejected$Income=addNA(rejected$Income)
levels(rejected$Income) <- c(levels(rejected$Income), "NA")
rejected$Income[is.na(rejected$Income)]="NA"
rejected$Income=mapvalues(rejected$Income,from = IV$Tables$Income$Income,to=IV$Tables$Income$WOE)
#CHANGING NO OF MONTHS IN CURRENT RESIDENCE FROM CONTINOUS TO CATEGORICAL
rejected$No.of.months.in.current.residence=as.numeric(rejected$No.of.months.in.current.residence)
rejected$No.of.months.in.current.residence=cut(rejected$No.of.months.in.current.residence,breaks = c(5,9,28,49,72,97,126),labels = c("[6,9]", "[10,28]", "[29,49]", "[50,72]", "[73,97]", "[98,126]"))
rejected$No.of.months.in.current.residence=mapvalues(rejected$No.of.months.in.current.residence,from = IV$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence,to=IV$Tables$No.of.months.in.current.residence$WOE)
#CHANGING NO OF MONTHS IN CURRENT company FROM CONTINOUS TO CATEGORICAL
rejected$No.of.months.in.current.company=as.numeric(rejected$No.of.months.in.current.company)
rejected$No.of.months.in.current.company=cut(rejected$No.of.months.in.current.company,breaks = c(2,5,12,19,26,33,40,47,53,61,133),labels = c("[3,5]", "[6,12]", "[13,19]", "[20,26]", "[27,33]", "[34,40]","[41,47]","[48,53]","[54,61]","[62,133]"))
rejected$No.of.months.in.current.company=mapvalues(rejected$No.of.months.in.current.company,from = IV$Tables$No.of.months.in.current.company$No.of.months.in.current.company,to=IV$Tables$No.of.months.in.current.company$WOE)
#CHANGING NO OF times 90 days past due or more FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.90.DPD.or.worse.in.last.6.months=as.numeric(rejected$No.of.times.90.DPD.or.worse.in.last.6.months)
rejected$No.of.times.90.DPD.or.worse.in.last.6.months=cut(rejected$No.of.times.90.DPD.or.worse.in.last.6.months,breaks = c(-1,0,3),labels = c("[0,0]", "[1,3]"))
rejected$No.of.times.90.DPD.or.worse.in.last.6.months=mapvalues(rejected$No.of.times.90.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE)
#CHANGING NO OF times 60 days past due or more FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.60.DPD.or.worse.in.last.6.months=as.numeric(rejected$No.of.times.60.DPD.or.worse.in.last.6.months)
rejected$No.of.times.60.DPD.or.worse.in.last.6.months=cut(rejected$No.of.times.60.DPD.or.worse.in.last.6.months,breaks = c(-1,0,5),labels = c("[0,0]", "[1,5]"))
rejected$No.of.times.60.DPD.or.worse.in.last.6.months=mapvalues(rejected$No.of.times.60.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE)
#CHANGING NO OF times 30 days past due or more FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.30.DPD.or.worse.in.last.6.months=as.numeric(rejected$No.of.times.30.DPD.or.worse.in.last.6.months)
rejected$No.of.times.30.DPD.or.worse.in.last.6.months=cut(rejected$No.of.times.30.DPD.or.worse.in.last.6.months,breaks = c(-1,0,1,7),labels = c("[0,0]", "[1,1]","[2,7]"))
rejected$No.of.times.30.DPD.or.worse.in.last.6.months=mapvalues(rejected$No.of.times.30.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE)
#CHANGING NO OF times 90 days past due FOR 12 MONTHS or more FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.90.DPD.or.worse.in.last.12.months=as.numeric(rejected$No.of.times.90.DPD.or.worse.in.last.12.months)
rejected$No.of.times.90.DPD.or.worse.in.last.12.months=cut(rejected$No.of.times.90.DPD.or.worse.in.last.12.months,breaks = c(-1,0,1,5),labels = c("[0,0]", "[1,1]","[2,5]"))
rejected$No.of.times.90.DPD.or.worse.in.last.12.months=mapvalues(rejected$No.of.times.90.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE)
#CHANGING NO OF times 60 days past due or more FOR 12 MONTHS FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.60.DPD.or.worse.in.last.12.months=as.numeric(rejected$No.of.times.60.DPD.or.worse.in.last.12.months)
rejected$No.of.times.60.DPD.or.worse.in.last.12.months=cut(rejected$No.of.times.60.DPD.or.worse.in.last.12.months,breaks = c(-1,0,1,7),labels = c("[0,0]", "[1,1]","[2,7]"))
rejected$No.of.times.60.DPD.or.worse.in.last.12.months=mapvalues(rejected$No.of.times.60.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE)
#CHANGING NO OF times 30 days past due or more  FOR 12 MONTHS FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.30.DPD.or.worse.in.last.12.months=as.numeric(rejected$No.of.times.30.DPD.or.worse.in.last.12.months)
rejected$No.of.times.30.DPD.or.worse.in.last.12.months=cut(rejected$No.of.times.30.DPD.or.worse.in.last.12.months,breaks = c(-1,0,2,9),labels = c("[0,0]", "[1,2]","[3,9]"))
rejected$No.of.times.30.DPD.or.worse.in.last.12.months=mapvalues(rejected$No.of.times.30.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE)
#SAME WILL BE PERFORMED IN AVERAGE CREDIT CARD UTILIZATION AS WE DID IN AGE
rejected$Avgas.CC.Utilization.in.last.12.months=as.numeric(rejected$Avgas.CC.Utilization.in.last.12.months)
rejected$Avgas.CC.Utilization.in.last.12.months=cut(rejected$Avgas.CC.Utilization.in.last.12.months,breaks = c(-1,4,6,8,11,14,21,37,51,71,113),labels = c("[0,4]", "[5,6]", "[7,8]", "[9,11]", "[12,14]", "[15,21]", "[22,37]", "[38,51]", "[52,71]", "[72,113]"))
rejected$Avgas.CC.Utilization.in.last.12.months=addNA(rejected$Avgas.CC.Utilization.in.last.12.months)
levels(rejected$Avgas.CC.Utilization.in.last.12.months) <- c(levels(rejected$Avgas.CC.Utilization.in.last.12.months), "NA")
rejected$Avgas.CC.Utilization.in.last.12.months[is.na(rejected$Avgas.CC.Utilization.in.last.12.months)]="NA"
rejected$Avgas.CC.Utilization.in.last.12.months=mapvalues(rejected$Avgas.CC.Utilization.in.last.12.months,from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months,to=IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE)
#MAPPING WOE VALUES FOR NO OF TRADES OPENED IN LAST 6 MONTHS
rejected$No.of.trades.opened.in.last.6.months=as.numeric(rejected$No.of.trades.opened.in.last.6.months)
rejected$No.of.trades.opened.in.last.6.months=cut(rejected$No.of.trades.opened.in.last.6.months,breaks = c(-1,0,1,2,3,4,12),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,12]"))
rejected$No.of.trades.opened.in.last.6.months=mapvalues(rejected$No.of.trades.opened.in.last.6.months,from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months,to=IV$Tables$No.of.trades.opened.in.last.6.months$WOE)
#MAPPING WOE VALUES FOR NO OF TRADES OPENED IN LAST 12 MONTHS
rejected$No.of.trades.opened.in.last.12.months=as.numeric(rejected$No.of.trades.opened.in.last.12.months)
rejected$No.of.trades.opened.in.last.12.months=cut(rejected$No.of.trades.opened.in.last.12.months,breaks = c(-1,0,1,2,3,5,7,9,12,28),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]", "[4,5]", "[6,7]","[8,9]","[10,12]","[13,28]"))
rejected$No.of.trades.opened.in.last.12.months=mapvalues(rejected$No.of.trades.opened.in.last.12.months,from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months,to=IV$Tables$No.of.trades.opened.in.last.12.months$WOE)
#MAPPING WOE VALUES FOR pl OF TRADES OPENED IN LAST 6 MONTHS
rejected$No.of.PL.trades.opened.in.last.6.months=as.numeric(rejected$No.of.PL.trades.opened.in.last.6.months)
rejected$No.of.PL.trades.opened.in.last.6.months=cut(rejected$No.of.PL.trades.opened.in.last.6.months,breaks = c(-1,0,1,2,4),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,6]"))
rejected$No.of.PL.trades.opened.in.last.6.months=mapvalues(rejected$No.of.PL.trades.opened.in.last.6.months,from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months,to=IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE)
#MAPPING WOE VALUES FOR pl OF TRADES OPENED IN LAST 12 MONTHS
rejected$No.of.PL.trades.opened.in.last.12.months=as.numeric(rejected$No.of.PL.trades.opened.in.last.12.months)
rejected$No.of.PL.trades.opened.in.last.12.months=cut(rejected$No.of.PL.trades.opened.in.last.12.months,breaks = c(-1,0,1,2,3,4,5,12),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]","[4,4]","[5,5]","[6,12]"))
rejected$No.of.PL.trades.opened.in.last.12.months=mapvalues(rejected$No.of.PL.trades.opened.in.last.12.months,from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months,to=IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE)
#MAPPING WOE VALUES FOR NO OF INQUIRIES IN LAST SIX MONTHS
rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=as.numeric(rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=cut(rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,breaks = c(-1,0,1,2,4,10),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,4]","[5,10]"))
rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=mapvalues(rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,to=IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE)
#MAPPING WOE VALUES FOR NO OF INQUIRIES IN LAST 12 MONTHS
rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=as.numeric(rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=cut(rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,breaks = c(-1,0,1,2,3,4,5,8,20),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]","[4,4]","[5,5]","[6,8]","[9,20]"))
rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=mapvalues(rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,to=IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE)
#MAPPING WOE VALUES FOR PRESENCE OF OPEN HOME LOAN
rejected$Presence.of.open.home.loan=addNA(rejected$Presence.of.open.home.loan)
rejected$Presence.of.open.home.loan=mapvalues(rejected$Presence.of.open.home.loan,from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan,to=IV$Tables$Presence.of.open.home.loan$WOE)
#MAPPING WOE VALUES FOR OUTSTANDING BALANCE
rejected$Outstanding.Balance=as.numeric(rejected$Outstanding.Balance)
rejected$Outstanding.Balance=cut(rejected$Outstanding.Balance,breaks = c(-1,6843,25509,386809,585402,774228,972455,1357300,2960987,3282013,5218801),labels = c("[0,6843]", "[6847,25509]", "[25522,386809]", "[386813,585402]", "[585423,774228]", "[774241,972455]", "[972456,1357300]", "[1357399,2960987]", "[2960994,3282013]", "[3282027,5218801]"))
rejected$Outstanding.Balance=addNA(rejected$Outstanding.Balance)
levels(rejected$Outstanding.Balance) <- c(levels(rejected$Outstanding.Balance), "NA")
rejected$Outstanding.Balance[is.na(rejected$Outstanding.Balance)]="NA"
rejected$Outstanding.Balance=mapvalues(rejected$Outstanding.Balance,from = IV$Tables$Outstanding.Balance$Outstanding.Balance,to=IV$Tables$Outstanding.Balance$WOE)
#MAPPING WOE VALUES FOR TOTAL NO OF TRADES
rejected$Total.No.of.Trades=as.numeric(rejected$Total.No.of.Trades)
rejected$Total.No.of.Trades=cut(rejected$Total.No.of.Trades,breaks = c(-1,1,2,3,4,5,6,8,10,19,44),labels = c("[0,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]","[7,8]","[9,10]","[11,19]","[20,44]"))
rejected$Total.No.of.Trades=mapvalues(rejected$Total.No.of.Trades,from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades,to=IV$Tables$Total.No.of.Trades$WOE)
#MAPPING WOE VALUES FOR PRESENCE OF AUTO OPEN HOME LOANS
rejected$Presence.of.open.auto.loan=mapvalues(rejected$Presence.of.open.auto.loan,from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan,to=IV$Tables$Presence.of.open.auto.loan$WOE)

###### 2. evaluating likelihood of default on rejected candidates ########

predicted= predict(fit_Final,rejected[,-29])
rejected$Performance.Tag= factor(rejected$Performance.Tag)
levels(rejected$Performance.Tag)[levels(rejected$Performance.Tag)==1]<-"yes"
mean(as.numeric(predicted)==as.numeric(rejected$Performance.Tag))

#### 86% are will be classified as defaulters.
## RESULT: SINCE THE REJECTED CANDIDATES WERE REJECTED BECAUSE OF THE RISK TO DEFAULT IN FUTURE HENCE OUR MODEL PREDICTS THAT MORE THAN 85% OF CANDIDATES WILL BE DEFAULTERS IF THEY WOULD HAVE BEEN ACCEPTED.
## There may be many other factors as well for which the candidate may be rejected other than his possibility of rejection. This has been accounted into remaining 14%.

######### COMBINING BOTH ACCEPTED AND REJECTED CANDIDATES DATASET #######

levels(testing$Performance.Tag)[levels(testing$Performance.Tag)==0]<-"no"
levels(testing$Performance.Tag)[levels(testing$Performance.Tag)==1]<-"yes"

fin_merged_data=rbind(woe_master,rejected)
fin_merged_data$Performance.Tag=as.factor(fin_merged_data$Performance.Tag)


#####BALANCING THE FINAL MERGED DATA ######
fin_balanced=fin_merged_data

####### probability of likelihood on original unbalanced data #####

predict_unb_final=predict(final_model,newdata = fin_merged_data,type = "prob")
predict_unb_final=predict_unb_final[,2]


###### PREPARING APPLICATION SCORECARD #######

Factor=20/log(2)
Offset=400-(Factor*log(10))
score=Offset+(Factor*log((1-predict_unb_final)/predict_unb_final))
fin_merged_data$score=score

###### FINDING CUTOFF BELOW WHICH ALL WILL BE REJECTED #####

rejected_score=subset(fin_merged_data,fin_merged_data$Application.ID %in% rejected$Application.ID)
selected_score=subset(fin_merged_data,fin_merged_data$Application.ID %in% selected$Application.ID)

summary(rejected_score$score)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#253.1   340.4   349.1   346.7   356.0   384.8 

summary(selected_score$score)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#250.8   355.7   378.9     Inf   416.3     Inf 

max(selected_score$score[is.finite(selected_score$score)])
#511.81

cutoff_score=seq(238.7,511.8192,1)


rejected_count2=NULL
selected_count2=NULL
for(i in seq(along=cutoff_score)){
  rejected_count2=c(rejected_count2,sum(rejected_score$score<cutoff_score[i]))
  selected_count2=c(selected_count2,sum(selected_score$score>cutoff_score[i]))
}
plot(cutoff_score,rejected_count2,type = "l")
par(new = T)
plot(cutoff_score, selected_count2,type = "l",axes=F, xlab=NA, ylab=NA)
axis(side = 4)
mtext(side = 4, line = 2, 'selected_count')
mtext(side=2,line= 2 ,'rejected_count')
mtext(side=1 ,line =2 ,"cut_off score")

locator()

# point this locator on the graph where the lines are intersecting.
#Thus the optimal cutoff score is 356.


#Financial Benifits Calculation

#                                  Actual Defaults		
#                          Good Cust (0)	Bad Cust (1)	Total
# Predicted Defaults	     Good Cust (0)	49,330 	 745 	 50,075 
#                          Bad Cust (1)	 17,578 	 2,202 	 19,780 
#                                Total	 66,908 	 2,947 	 69,855 

# Accuracy	74%
# Sensitivity	75%
# Specificity	74%

# Credit Loss - Saved	

# Credit loss no model -	4%	
# Credit loss with model	1.1% ~ 1% (745/69855)
#   Credit Loss Saved	3%	(4 - 1)

# Revenue Loss	
# Good customers identified as bad		26% (17578/66908)
# So Revenue Loss		26%

# Equation	Revenue per good customer		rev
# Credit loss per bad customer 		loss





