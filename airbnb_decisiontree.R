library(ggplot2)
library(rpart)## FOR Decision Trees
library(rpart.plot)
#install.packages("network")
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(rattle)  ## FOR Decision Tree Vis
library(rpart.plot)
library(RColorBrewer)
#library(Cairo)
library(network)
library(ggplot2)
##If you install from the source....
#Sys.setenv(NOAWT=TRUE)
## ONCE: install.packages("wordcloud")
library(wordcloud)
## ONCE: install.packages("tm")

library(slam)
library(quanteda)
## ONCE: install.packages("quanteda")
## Note - this includes SnowballC
#library(SnowballC)

library(proxy)
## ONCE: if needed:  install.packages("stringr")
library(stringr)
## ONCE: install.packages("caret")
library(textmineR)
library(igraph)
library(caret)
#library(lsa)


listingdata="/Users/anilareddy/Downloads/project/portland/cleaned datasets/cleaned_listings.csv"
listings<-read.csv(listingdata, stringsAsFactors=TRUE)
head(listings)
str(listings)
listings$price <- as.integer(listings$price)
#As a label, let's convert price column into price level columns
temp <- sort.int(listings$price, decreasing = FALSE)
level_1 <- temp[round(length(temp)/3, digits = 0)]
level_2 <- temp[2*round(length(temp)/3, digits = 0)]
listings$price_level[listings$price <= level_1] <- "Low"
listings$price_level[listings$price > level_1 & listings$price <= level_2] <- "Medium"
listings$price_level[listings$price > level_2] <- "High"
listings$price_level <- as.factor(listings$price_level)
write.csv(listings, "cc.csv", row.names = FALSE)
#lets remove the columns that we don't need
listings <- listings %>% select(-review_scores_value , -review_scores_location, -review_scores_communication , -review_scores_checkin , -review_scores_cleanliness, -review_scores_accuracy, -review_scores_rating , -amenities, -bathrooms_text, -longitude, -latitude, -id, -host_id, -property_type, -maximum_nights, -availability_30, -availability_60, -availability_90, -availability_365, -number_of_reviews_ltm, -number_of_reviews )
listings <- listings %>% select(-price)
listings <- listings %>% select(-minimum_nights,calculated_host_listings_count)
#GoPlot <- function(x) {
#  G <-ggplot(data=listings, aes(.data[[x]], y="") ) +
#    geom_bar(stat="identity", aes(fill =.data[[x]])) 
#  return(G)
#}
## Use the function in lappy
#lapply(names(listings), function(x) GoPlot(x))

write.csv(listings, "cc.csv", row.names = FALSE)


blank_values <- colSums(listings == "")
blank_values
#we could see that there are a lot of blank values. We could replace price, host_responseTime and ratetype with mode.
listings[listings==""]<-NA
listings<-listings[complete.cases(listings),]
summary(listings)
#Now there are no blank spaces or na values in any column.
str(listings)
head(listings)
(DataSize=nrow(listings)) ## how many rows?
(TrainingSet_Size<-floor(DataSize*(3/4))) ## Size for training set
(TestSet_Size <- DataSize - TrainingSet_Size) ## Size for testing set
## Random sample WITHOUT replacement (why?)
## set a seed if you want it to be the same each time you
## run the code. The number (like 1234) does not matter
set.seed(1234)
## This is the sample of row numbers
(MyTrainSample <- sample(nrow(listings),
                         TrainingSet_Size,replace=FALSE))
## Use the sample of row numbers to grab those rows only from
## the dataframe....
(MyTrainingSET <- listings[MyTrainSample,])
table(MyTrainingSET$price_level)
## Use the NOT those row numbers (called -) to get the
## other row numbers not in the training to use to create
## the test set.
## Training and Testing datasets MUST be disjoint. Why?
(MyTestSET <- listings[-MyTrainSample,])
table(MyTestSET$price_level)
##Make sure your Training and Testing datasets are BALANCED

head(MyTestSET)

###########
## NEXT - 
## REMOVE THE LABELS from the test set!!! - and keep them
################################################
(TestKnownLabels <- MyTestSET$price_level)
(MyTestSET <- MyTestSET[ , -which(names(MyTestSET) %in% c("price_level"))])




##     Decision Trees
##
##      First - train the model with your training data
##
##      Second - test the model - get predictions - compare
##               to the known labels you have.
###########################################################
head(MyTestSET)
str(MyTrainingSET)
str(MyTestSET)
## This code uses rpart to create decision tree
## Here, the ~ .  means to train using all data variables
## The MyTrainingSET#label tells it what the label is called
## In this dataset, the label is called "label".

DT <- rpart(MyTrainingSET$price_level ~ host_is_superhost + bedrooms + room_type + accommodates + instant_bookable + reviews_per_month, data = MyTrainingSET, method="class")
summary(DT)
printcp(DT)

## Let's make another tree...here we will use cp
DT2<-rpart(MyTrainingSET$price_level ~ host_is_superhost + bedrooms + room_type + accommodates + instant_bookable + reviews_per_month, data = MyTrainingSET,cp=.01, method="class")
## The small cp the larger the tree if cp is too small you have overfitting
summary(DT2)

plotcp(DT) ## This is the cp plot

## Let's make a third tree - here we use cp = 0 and 
## "information" instead of the default which is GINI
DT3<-rpart(MyTrainingSET$price_level ~ host_is_superhost + room_type + accommodates + instant_bookable + reviews_per_month, 
           data = MyTrainingSET,cp=0.01, method="class",
           parms = list(split="entropy"),minsplit=3, control = rpart.control(maxdepth = 5))
## The small cp the larger the tree if cp is too small you have overfitting
summary(DT3)


###########################################################
DT3$variable.importance  ## before re-eval to add to 100

############################################################
##
## Predict the Testset using all 4 trees - 
## Let's see what we get. 
## We will build a tree and a confusion matrix for all 4
##
###############################################################
## 
## DT---------------------------------
(DT_Prediction= predict(DT, MyTestSET, type="class"))
## Confusion Matrix
table(DT_Prediction,TestKnownLabels) ## one way to make a confu mat
## VIS..................
fancyRpartPlot(DT)

## DT2-----------------------------
### Example two with cp - a lower cp value is a bigger tree
(DT_Prediction2= predict(DT2, MyTestSET, type = "class"))
## ANother way to make a confusion matrix
caret::confusionMatrix(DT_Prediction2, TestKnownLabels, positive="true")
fancyRpartPlot(DT2)
## Example three with information gain and lower cp

##DT3---------------------------------------------------------
(DT_Prediction3= predict(DT3, MyTestSET, type = "class"))
confusionMatrix(DT_Prediction3, TestKnownLabels, positive="true")
rattle::fancyRpartPlot(DT3,main="Decision Tree", cex.main=.6)
print(rattle::fancyRpartPlot(DT3,main="Decision Tree", cex.main=.6))
confusionMatrix(DT_Prediction3, TestKnownLabels, positive="true")
#accuracy(MyTrainingSET$price_levels, DT_Prediction3)

##
listing.class <- rpart(MyTrainingSET$price_level ~room_type +reviews_per_month + instant_bookable+ host_is_superhost + bedrooms, data = MyTrainingSET, method = 'class', control = rpart.control(maxdepth = 5))
rpart.plot(listing.class)
(DT_Prediction= predict(listing.class, MyTestSET, type = "class"))
confusionMatrix(DT_Prediction, TestKnownLabels, positive="true")



