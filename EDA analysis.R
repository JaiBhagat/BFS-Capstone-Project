
library(dplyr)
library(tidyr)
library(ggplot2)

##################
#### Loading data

credit_bureau <- read.csv("Credit Bureau data.csv", header = T, stringsAsFactors = F, na.strings = c(""," ","NA"))
demographic_data <- read.csv("Demographic data.csv", header = T, stringsAsFactors = F, na.strings = c(""," ","NA"))


##################
## Check for duplicates
sum(duplicated(credit_bureau$Application.ID))
sum(duplicated(demographic_data$Application.ID))

length(unique(credit_bureau$Application.ID))
length(unique(demographic_data$Application.ID))

## remove duplicate application Id
credit_bureau <- credit_bureau[!duplicated(credit_bureau$Application.ID), ]
demographic_data <- demographic_data[!duplicated(demographic_data$Application.ID), ]

##################
## Check for NAs in the data

sum(is.na(demographic_data))
dd_na_count_cols <- colSums(is.na(demographic_data))
dd_na_count_cols[dd_na_count_cols>0]

## Dropping na values as the count is quite less
demographic_data <- na.omit(demographic_data)

sum(is.na(credit_bureau))
colnames(credit_bureau)[colSums(is.na(credit_bureau))>0]

cb_na_count_cols <- colSums(is.na(credit_bureau))
cb_na_count_cols[cb_na_count_cols>0]

credit_bureau <- filter(credit_bureau, !is.na(Performance.Tag))

colnames_with_na <- names(cb_na_count_cols[cb_na_count_cols>0])[-5]
sapply(colnames_with_na, function(x){ sum(is.na(credit_bureau[,x]) & credit_bureau$Performance.Tag==1) } )

sum(credit_bureau$Performance.Tag==1)

## Dropping na values in "Outstanding.Balance" and "Presence.of.open.home.loan" as the count is quite less
credit_bureau <- filter(credit_bureau, !is.na(Presence.of.open.home.loan) | !is.na(Outstanding.Balance))


####################
## Data Quality check

merged_data <- merge(credit_bureau, demographic_data, by = "Application.ID")


### Checking Age variable
boxplot(merged_data$Age, ylab = "Age", col= c("royalblue2"))
quantile(merged_data$Age, probs = seq(0,1,0.01))

sum(merged_data$Age<18)

## Dropping all rows with Age less than 18, as valid age for issuing credit card is above 18years
demographic_data <- filter(demographic_data, Age>=18)
merged_data <- filter(merged_data, Age>=18)

ggplot(merged_data[merged_data$Performance.Tag.x==1,], aes(x=Age, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 10, col = 'black')+
  ggtitle("Age wise Defaulters Count")+
  labs(x="Age", y="Count")


## Checking income variable
quantile(merged_data$Income, probs = seq(0,1,0.01))
sum(merged_data$Income<4.5)

# View(merged_data[merged_data$Income<4.5,])

## Negative and incomes less than 4.5 doesn't seem right as they have are mostly aged and having large outstandings
merged_data <- filter(merged_data, Income>4.5)

ggplot(merged_data[merged_data$Performance.Tag.x==1,], aes(x=Income, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 10)+
  ggtitle("Income wise Defaulters count")+
  labs(x="Income", y="Count")

## Outstading Balance
boxplot(merged_data$Outstanding.Balance)
ggplot(merged_data[merged_data$Performance.Tag.x==1,], aes(x=Outstanding.Balance, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 100000)+
  ggtitle("Outstanding Balance / Defaulter")+
  labs(x="Outstanding Balance", y="Count")


## Gender
ggplot(merged_data, aes(x=Gender, fill=as.factor(Performance.Tag.x)))+
  geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  ggtitle("Gender/Defaulter")+
  labs(x="Gender", y="Count")


## Total.No.of.Trades
ggplot(merged_data[merged_data$Performance.Tag.x==1,], aes(x=Total.No.of.Trades, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 10)+
  # geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "fill")+
  ggtitle("Total.No.of.Trades / Defaulter")+
  labs(x="Total.No.of.Trades", y="Count")


## Profession
ggplot(merged_data[merged_data$Performance.Tag.x==1,], aes(x=Profession, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(stat = "count")+
  ggtitle("Profession / Defaulter's Count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="Profession", y="Count")


ggplot(merged_data, aes(x=Profession, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(stat = "count", position = position_fill())+
  ggtitle("Profession/Defaulter")+
  labs(x="Profession", y="Count")

#### salaried customer seems to default more than others

## No.of.dependents
ggplot(merged_data[merged_data$Performance.Tag.x==1,], aes(x=No.of.dependents, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(stat = "count")+
  ggtitle("No.of.dependents / Defaulter's Count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="No.of.dependents", y="Count")
##### Observations - no real trend in the data w.r.t No.of.dependents


## Education
ggplot(merged_data[merged_data$Performance.Tag.x==1,], aes(x=Education, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(stat = "count")+
  ggtitle("Education / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="Education", y="Count")


## Type.of.residence
ggplot(merged_data[merged_data$Performance.Tag.x==1,], aes(x=Type.of.residence, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(stat = "count")+
  ggtitle("Type.of.residence / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="Type.of.residence", y="Count")
#### Customers with rented residence tend to default more


## Marital.Status..at.the.time.of.application.
ggplot(merged_data[merged_data$Performance.Tag.x==1,], aes(x=Marital.Status..at.the.time.of.application., fill=as.factor(Performance.Tag.x)))+
  geom_histogram(stat = "count")+
  ggtitle("Marital.Status..at.the.time.of.application. / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="Marital.Status..at.the.time.of.application.", y="Count")
## Married people tend to default more than single

ggplot(merged_data, aes(x=Presence.of.open.auto.loan, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(stat = "count")+
  ggtitle("Marital.Status..at.the.time.of.application. / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="Marital.Status..at.the.time.of.application.", y="Count")


ggplot(merged_data[merged_data$Performance.Tag.x==1,], aes(x=Avgas.CC.Utilization.in.last.12.months, fill=as.factor(Performance.Tag.x)))+
  geom_bar(binwidth = 10)+
  ggtitle("Avgas.CC.Utilization.in.last.12.months / Defaulter's count")+
  # geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="Avgas.CC.Utilization.in.last.12.months", y="Count")


ggplot(merged_data, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 1)+
  ggtitle("No.of.times.90.DPD.or.worse.in.last.6.months / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="No.of.times.90.DPD.or.worse.in.last.6.months", y="Count") + facet_wrap(~Performance.Tag.x)


ggplot(merged_data, aes(x=No.of.times.60.DPD.or.worse.in.last.6.months, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 1)+
  ggtitle("No.of.times.60.DPD.or.worse.in.last.6.months / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="No.of.times.60.DPD.or.worse.in.last.6.months", y="Count")+ facet_wrap(~Performance.Tag.x)

ggplot(merged_data, aes(x=No.of.times.30.DPD.or.worse.in.last.6.months, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 1)+
  ggtitle("No.of.times.30.DPD.or.worse.in.last.6.months / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="No.of.times.30.DPD.or.worse.in.last.6.months", y="Count")+ facet_wrap(~Performance.Tag.x)

ggplot(merged_data, aes(x=No.of.times.90.DPD.or.worse.in.last.12.months, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 1)+
  ggtitle("No.of.times.90.DPD.or.worse.in.last.12.months / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="No.of.times.90.DPD.or.worse.in.last.12.months", y="Count")+ facet_wrap(~Performance.Tag.x)

ggplot(merged_data, aes(x=No.of.times.60.DPD.or.worse.in.last.12.months, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 1)+
  ggtitle("No.of.times.60.DPD.or.worse.in.last.12.months / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="No.of.times.60.DPD.or.worse.in.last.12.months", y="Count")+ facet_wrap(~Performance.Tag.x)

ggplot(merged_data, aes(x=No.of.times.30.DPD.or.worse.in.last.12.months, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 1)+
  ggtitle("No.of.times.30.DPD.or.worse.in.last.12.months / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="No.of.times.30.DPD.or.worse.in.last.12.months", y="Count")+ facet_wrap(~Performance.Tag.x)


ggplot(merged_data, aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 5)+
  ggtitle("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. / Defaulter's count")+
  # geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.", y="Count")+ facet_wrap(~Performance.Tag.x)


ggplot(merged_data, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 5)+
  ggtitle("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. / Defaulter's count")+
  # geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.", y="Count")+ facet_wrap(~Performance.Tag.x)

ggplot(merged_data, aes(x=Total.No.of.Trades, fill=as.factor(Performance.Tag.x)))+
  geom_histogram(binwidth = 10)+
  ggtitle("Total.No.of.Trades / Defaulter's count")+
  # geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="Total.No.of.Trades", y="Count")+ facet_wrap(~Performance.Tag.x)

ggplot(merged_data, aes(x=Presence.of.open.auto.loan, fill=as.factor(Performance.Tag.x)))+
  geom_bar()+
  ggtitle("Presence.of.open.auto.loan / Defaulter's count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "stack")+
  labs(x="Presence.of.open.auto.loan", y="Count")+ facet_wrap(~Performance.Tag.x)



############
## creating woe buckets for numerical variables
library(Information)
IV <- create_infotables(data=merged_data, y="Performance.Tag.x", bins=10, parallel = TRUE)
knitr::kable(IV$Summary)

names <- IV$Summary$Variable
plots <- list()
for (i in 1:length(names)){
  print(IV$Summary[i,]$IV > 0.02)
  if(IV$Summary[i,]$IV > 0.02){
    print('adding plot')
    plots[[i]] <- plot_infotables(IV, names[i])
  }
}
# Showing the top 18 variables
length(plots)
plots

## Considering only variables with IV greater than 0.02


colnames(merged_data)

