library(Amelia)
library(Hmisc)
library(mice)
library(VIM)
library(dplyr)
library(ggplot2)
library(gridExtra)

#a. Create a visual for representing missing values in the dataset.
setwd("C:\\Users\\DELL\\Desktop\\Assignments\\Assignment11")

bnkData<- read.table("bank-full.csv", header = T, sep=";" )
View(bnkData)

for ( i in 1:nrow(bnkData)){
  for ( j in 1:ncol(bnkData)){
    if ( i == j | (j%/%i) == 2 ) {
      bnkData[i,j] <- NA
    }
  } 
}

aggr_plot <- aggr(bnkData, col=c('navyblue','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(bnkData), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

#b. Show a distribution of clients based on a job.
cnts<-table(bnkData$job)
per <- round(cnts/sum(cnts)*100)
lbls<-paste(names(cnts), per, "%", sep =" ")
pie(cnts, labels = lbls,  main="Distribution of clients based on a job.")

#c. Check whether is there any relation between Job and Marital Status?
cnts<-table(bnkData$marital, bnkData$job)
b<- barplot(cnts, main = "Relation between Job and Marital Status", xlab = "Profession", ylab="Counts", col = c("Red","Green", "Grey"), legend= row.names(cnts))

#d. Check whether is there any association between Job and Education?
cnts<-table(bnkData$education, bnkData$job)
b<- barplot(cnts, main = "Association between Job and Eductiona", xlab = "Profession", ylab="Counts", col = c("Red","Green", "Blue", "Grey"), legend= row.names(cnts))
© 2019 GitHub, Inc.