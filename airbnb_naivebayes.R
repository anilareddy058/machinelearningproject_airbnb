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

listingdata="/Users/anilareddy/Downloads/project/portland/cleaned datasets/cleaned_listings.csv"
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
listing <- listing %>% select(-review_scores_value , -review_scores_location, -review_scores_communication , -review_scores_checkin , -review_scores_cleanliness, -review_scores_accuracy, -review_scores_rating , -amenities, -bathrooms_text, -longitude, -latitude, -id, -host_id, -property_type, -maximum_nights, -availability_30, -availability_60, -availability_90, -availability_365, -number_of_reviews_ltm, -number_of_reviews )
listing <- listing %>% select(-neighbourhood_cleansed, price)
listing <- listing %>% select(-minimum_nights,calculated_host_listings_count)
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
## Check size
(ncol(listing_Train_NL))
#(DF_Train_Student_NL)


##                         NAIVE BAYES

#install.packages("e1071")
library(e1071)
(NB_e1071<-naiveBayes(listing_Train_NL, listing_Train_Labels, laplace = 1))
NB_e1071_Pred <- predict(NB_e1071, listing_Test_NL)
conf_mat <- table(NB_e1071_Pred,listing_Test_Labels)
(NB_e1071_Pred)
confusionMatrix(conf_mat) 