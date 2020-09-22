#installing the package that will be necessary later in environment

#install.packages("dplyr")
library("dplyr" )
#removing any objects in the environment
rm(list=ls())
#

#
#Step 1
gender_data <- read.csv("/Users/milanastetsenko/Documents/Coding/R/R Assignments/R Competency/odp_contributionsbygender.csv", header=TRUE, na.strings=c(""," ","NA"))
#checking the dimensions of the dataset to udnerstand what I am working in and to make sure the rows are taken away with N/A values
dim(gender_data)
#
#
#Step 2
#trying to see whether there are N/A values in the dataset
is.na(gender_data)
#removing all the n/a values from the dataset
gender_data1<- na.omit(gender_data)
#checking the dimensions of the omitted n/a values dataset (5 rows omitted)
dim(gender_data1)
#
#
#Step 3
#converting the class of data column to Date
gender_data1$Last_Reporting_Date<-as.Date(gender_data1$Last_Reporting_Date, "%m-%d-%y")
#
#
#Step 4
#was the goal of 20% WOMEN SERVING IN FORMED POLICE UNITS achieved in July 2020?
head(gender_data1)
#since all of the July missions are reported on the 31 of July, I am choosing
july_2020<-subset(gender_data1, gender_data1$Last_Reporting_Date =="2020-07-31")
july_2020
#women_police_units <- gender_data1$Personnel_Type == "Formed Police Units"
women_police_units<-sum(july_2020[which(july_2020$Personnel_Type== "Formed Police Units"),7])
women_police_units
men_police_units<-sum(july_2020[which(july_2020$Personnel_Type== "Formed Police Units"),8])
men_police_units
percentage_women<-(women_police_units/(men_police_units+women_police_units))*100
percentage_women
#so women percentage make up 10.9 ~11% of the whole popultaion sent on the misssion, their goal of 20+% is not achieved,
#but it is going in that direction
#
#
#Step 5
library(ggplot2)
#creating a dataframe with necessary values
Years<-c('2017', '2018', '2019', '2020 July', '2028')
women_employed<-c(7,8,10.8,11,20)
percentage_rate<- data.frame(Years, women_employed)
percentage_rate
p<-ggplot(data = percentage_rate, aes(x = Years, y = women_employed, fill = Years)) +
  geom_bar(stat="identity", color = "lightblue") +
  geom_text(aes(label=Years), vjust=1.6, color = 'white', size=3)+
  theme_linedraw()
p+scale_fill_brewer()
#
#
#Step 6

gender_data1$ISOCode3<- trimws(gender_data1$ISOCode3,which='both', whitespace = "[ \t\r\n]")
minerva_missions<-function(ISO){
  unique_missions<- list(unique(factor((gender_data1[which(gender_data1$ISOCode3 == ISO), 5]))))
  output<- append(unique_missions, lengths(unique_missions))
  return (output)
}
minerva_missions("DEU")
#
#
#Step 7
#creating a new column in df
gender_data1$Total_personnel = gender_data1$Female_Personnel+gender_data1$Male_Personnel
#creating a subset of the df
minusma <- subset(gender_data1, Mission_Acronym == "MINUSMA", select=c(ISOCode3, Total_personnel, Last_Reporting_Date))
head(minusma)
#calculating the descriptive stats of the column Total_personnel
round(mean(minusma$Total_personnel))
median(minusma$Total_personnel)
sapply(minusma, class)
sapply(minusma, class)
quantile(minusma$Total_personnel, probs=c(25/100, 75/100))


###

output<-min(minusma$Total_personnel)
output1<-max(minusma$Total_personnel)
output1
lowest_point<-minusma %>% filter(minusma$Total_personnel == 1)
dim(lowest_point)
highest_point<-minusma %>% filter(minusma$Total_personnel == 1726)
highest_point
#the lowest points were multiple 1127 variables in different dates
#the highest point was on the 31st of January 2017

                   