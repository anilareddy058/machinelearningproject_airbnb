#association rule mining
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)

library(arules)
library(ROAuth)
library(jsonlite)
#library(streamR)
library(rjson)
library(tokenizers)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
#install.packages("syuzhet")
## sentiment analysis
library(syuzhet)
library(stringr)
library(arulesViz)

#calendar data cleaning and preprocessing
Myfile1="/Users/anilareddy/Downloads/project/portland/cleaned datasets/airbnb_api_cleaned.csv"
df <- read.csv(Myfile1)
colnames(df)
#install.packages('tokenizers')
library(tokenizers)

## Start the file
Trans <- file(Myfile1)
df$reviews[1]
## Tokenize to words
Tokens<-tokenizers::tokenize_words(df$reviews[1], stopwords = stopwords::stopwords ("en"),
                                     lowercase=TRUE, strip_punct = TRUE, strip_numeric = TRUE, simplify = TRUE)
## write squished tokens 
cat(unlist(str_squish (Tokens)), "\n", file=Trans, sep=",")
close (Trans)
## Append remaining lists of tokens into file
## Recall - a list of tokens is the set of words from a Tweet
Trans <- file(Myfile1, open = "a")
for (i in 2:nrow (df)) {
  Tokens<-tokenize_words(df$reviews [i],stopwords = stopwords::stopwords("en"),
                                         lowercase = TRUE,
                                         strip_punct = TRUE, simplify = TRUE)
  cat (unlist (str_squish(Tokens)), "\n", file=Trans, sep=",")
}
close(Trans)

#read and inspect the transactions
reviewstrans <- read.transactions (Myfile1,
                                  rm.duplicates = FALSE,
                                  format = "basket", sep="," 
                                  ## cols =
                                  )
inspect (reviewstrans)
## See the words that occur the most
Sample_Trans <- sample(reviewstrans, 50) 
summary (reviewstrans)

#clean up

reviewsDF <- read.csv (Myfile1, header = FALSE, sep = ",")
head (reviewsDF)

#specifically remove words

reviewsDF<-reviewsDF %>%
  mutate_all(as.character)
(str(reviewsDF))
# We can now remove certain words
reviewsDF[reviewsDF == "great"] <- ""
reviewsDF[reviewsDF == "take"] <- ""
reviewsDF[reviewsDF == "gives"] <- ""
reviewsDF[reviewsDF == "that's"] <- ""
reviewsDF[reviewsDF == "don't"] <- ""
reviewsDF[reviewsDF == "didn't"] <- ""
reviewsDF[reviewsDF == "feel"] <- ""
reviewsDF[reviewsDF == "like"] <- ""
reviewsDF[reviewsDF == "emily"] <- ""
reviewsDF[reviewsDF == "recommend"] <- ""
reviewsDF[reviewsDF == "definitely"] <- ""
reviewsDF[reviewsDF == "highly"] <- ""
reviewsDF[reviewsDF == "place"] <- ""
reviewsDF[reviewsDF == "well"] <- ""
reviewsDF[reviewsDF == "home"] <- ""

MyDF<-NULL
MyDF2<-NULL
for (i in 1:ncol(reviewsDF)){
  MyList=c() 
  MyList2=c() # each list is a column of logicals ...
  MyList=c(MyList,grepl("[[:digit:]]", reviewsDF[[i]]))
  MyDF<-cbind(MyDF,MyList)  ## create a logical DF
  MyList2=c(MyList2,(nchar(reviewsDF[[i]])<4 | nchar(reviewsDF[[i]])>11))
  MyDF2<-cbind(MyDF2,MyList2) 
  ## TRUE is when a cell has a word that contains digits
}
## For all TRUE, replace with blank
reviewsDF[MyDF] <- ""
reviewsDF[MyDF2] <- ""
(head(reviewsDF,10))



# Now we save the dataframe using the write table command 
write.table(reviewsDF, file = "Updatedreviews.csv", col.names = FALSE, 
            row.names = FALSE, sep = ",")
reviewstrans <- read.transactions("Updatedreviews.csv", sep =",", 
                                format("basket"),  rm.duplicates = TRUE)
itemFrequencyPlot(reviewstrans, topN=15, type = 'relative')

reviewsTrans_rules = arules::apriori(reviewstrans, 
                                   parameter = list(support=.02, conf=0.6, minlen=3))

inspect(reviewsTrans_rules[1:10])


##  SOrt by Conf
SortedRules_conf <- sort(reviewsTrans_rules, by="confidence", decreasing=TRUE)
inspect(SortedRules_conf[1:15])
## Sort by Sup
SortedRules_sup <- sort(reviewsTrans_rules, by="support", decreasing=TRUE)
inspect(SortedRules_sup[1:15])
## Sort by Lift
SortedRules_lift <- sort(reviewsTrans_rules, by="lift", decreasing=TRUE)
inspect(SortedRules_sup[1:15])



subrules <- head(sort(reviewsTrans_rules, by="lift"),20)
plot(subrules)

#plot(subrules, method="graph", engine="interactive")
plot(subrules, method="graph", engine="htmlwidget")
