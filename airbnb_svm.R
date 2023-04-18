library(tm)
#install.packages("tm")
library(stringr)
library(wordcloud)
# ONCE: install.packages("Snowball")
## NOTE Snowball is not yet available for R v 3.5.x
## So I cannot use it  - yet...
##library("Snowball")
##set working directory
## ONCE: install.packages("slam")
library(slam)
library(quanteda)
## ONCE: install.packages("tidytext")
## Note - this includes SnowballC
library(SnowballC)
library(arules)
##ONCE: install.packages('proxy')
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext) # convert DTM to DF
library(plyr) ## for adply
library(ggplot2)
library(factoextra) # for fviz
library(mclust) # for Mclust EM clustering

library(naivebayes)
#Loading required packages
#install.packages('tidyverse')
library(tidyverse)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('caret')
library(caret)
#install.packages('caretEnsemble')
library(caretEnsemble)
#install.packages('psych')
library(psych)
#install.packages('Amelia')
library(Amelia)
#install.packages('mice')
library(mice)
#install.packages('GGally')
library(GGally)
library(e1071)

listingdata="/Users/anilareddy/Downloads/cleaned datasets/cleaned_listings.csv"
listing<-read.csv(listingdata, stringsAsFactors=TRUE)

blank_values <- colSums(listing == "")
blank_values
#we could see that there are a lot of blank values. We could replace price, host_responseTime and ratetype with mode.
listing[listing==""]<-NA
listing<-listing[complete.cases(listing),]
summary(listing)
#Now there are no blank spaces or na values in any column.
str(listing)



str(listing)
listing$price <- as.integer(listing$price)
#As a label, let's convert price column into price level columns
temp <- sort.int(listing$price, decreasing = FALSE)
level_1 <- temp[round(length(temp)/3, digits = 0)]
level_2 <- temp[2*round(length(temp)/3, digits = 0)]
listing$price_level[listing$price <= level_1] <- "Low"
listing$price_level[listing$price > level_1 & listing$price <= level_2] <- "Medium"
listing$price_level[listing$price > level_2] <- "High"
listing$price_level <- as.factor(listing$price_level)
#lets remove the columns that we don't need
listing <- listing %>% select(price, number_of_reviews, host_total_listings_count, bedrooms, minimum_nights, accommodates, price_level)
write.csv(listing, "gh.csv", row.names = FALSE)
(Size <- (as.integer(nrow(listing)/4)))  ## Test will be 1/4 of the data
(SAMPLE <- sample(nrow(listing), Size))

(listing_Test<-listing[SAMPLE, ])
(listing_Train<-listing[-SAMPLE, ])

head(listing_Train)
str(listing_Test$price_level)  ## Notice that the label is called "price_level" and
## is correctly set to type FACTOR. This is IMPORTANT!!
str(listing_Train$price_level)  ## GOOD! Here "price_level" is also type FACTOR
##Check balance of test dataset
table(listing_Test$price_level)

(listing_Test_Labels <- listing_Test$price_level)
## Remove the labels
listing_Test_NL<-listing_Test[ , -which(names(listing_Test) %in% c("price_level"))]
(listing_Test_NL[1:5, 1:5])
head(listing_Test_NL)
## Check size
(ncol(listing_Test_NL))
#(listing_Test_Student_NL)
## Train...--------------------------------
## Copy the Labels
(listing_Train_Labels <- listing_Train$price_level)
## Remove the labels
listing_Train_NL<-listing_Train[ , -which(names(listing_Train) %in% c("price_level"))]
(listing_Train_NL[1:5, 1:5])
head(listing_Train_NL)
## Check size
(ncol(listing_Train_NL))
#(DF_Train_Student_NL)

#polynomial
SVM_fit_P <- svm(listing_Train$price_level~., data=listing_Train_NL,
                 kernel="polynomial", cost=.1, scale=FALSE)
print(SVM_fit_P)
pred_p=predict(SVM_fit_P,listing_Test_NL, type="class" )
(Ptable <- table (pred_p, listing_Test_Labels))
plot(SVM_fit_P, data=listing_Train_NL, price ~ number_of_reviews, slice=list(host_total_listings_count=3, bedrooms=3, minimum_nights=2, accommodates=3))
## The ptable above is the confusion matrix that shows ## what was classified correctly and what was not
#* Misclassification Rate for Polynomial
(MR_P <- 1 - sum(diag (Ptable))/sum(Ptable))  
conf_mat <- table(pred_p, listing_Test_Labels)
confusionMatrix(conf_mat) 
 
#linear              
columns <- c('price', 'accommodates', 'price_level')
samplerownums<- sample (150,40)
listing_testset <-listing[samplerownums, columns]
listing_testset2 <-listing[samplerownums, columns]
## Remove and keep the labels
(listingTestLabels <- listing_testset[,c(3)])
(listing_testset<-listing_testset[,-c(3)])
(head (listing_testset))
## For the training data, we want to have/keep the class label 
listing_trainset <- listing[-samplerownums, columns]
head(listing_trainset)
## Set up the SVM again
SVM_fit2 <- svm(listing_trainset$price_level~., data=listing_trainset, kernel="linear", cost=.1)
print(SVM_fit2)
plot(SVM_fit2, listing_trainset)
(pred_2 <- predict(SVM_fit2, listing_testset, type="class")) 
(pred_2) 
(listingTestLabels)
conf_mat <- (table(pred_2, listingTestLabels))
confusionMatrix(conf_mat) 

#radial
library(e1071)
SVM_fit3<- svm(listing_trainset$price_level~., data=listing_trainset, kernel = 'radial')
print(SVM_fit3)
plot(SVM_fit3, listing_trainset)
(pred_3 <- predict(SVM_fit3, listing_testset, type="class")) 
(pred_3) 
(listingTestLabels)
conf_mat <- (table(pred_3, listingTestLabels))
confusionMatrix(conf_mat) 
