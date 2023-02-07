install.packages("gridExtra")
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)

#calendar data cleaning and preprocessing
Myfile1="/Users/anilareddy/Downloads/project/portland/airbnb_api.csv"
df <- read.csv(Myfile1)
colnames(df)
#In this csv file, there are a lot of columns that need to be removed. The availability columns for each day were given, which is huge and necessary, so i have deleted the columns.
#The remaining columns like pricing/rate/currency, primaryHost/badges/1, primaryHost/hostUrl, primaryHost/smartName, reviews/0/author/firstName, reviews/0/createdAt, reviews/1/author/firstName, url are not required for our analysis
df <- df %>% select(-`pricing.rate.amountFormatted`, -`pricing.rate.currency`, -`primaryHost.badges.1`, -`primaryHost.hostUrl`, -`primaryHost.smartName`, -`reviews.0.author.firstName`, -`reviews.0.createdAt`, -`reviews.1.author.firstName`, -`url`, -address, -primaryHost.responseRate)
summary(df)
#first let's rename the column names
df <- rename(df, c(price= pricing.rate.amount, rateType=pricing.rateType, host_about=primaryHost.about, host_badges = primaryHost.badges.0, host_name = primaryHost.firstName,host_id = primaryHost.id, host_is_superhost = primaryHost.isSuperHost ,host_responseTime = primaryHost.responseTime, reviewer_id = reviews.0.author.id, reviews = reviews.0.comments, reviewer_rating = reviews.0.rating))
summary(df)
is.na(colnames(df))
# replace NA's in reviewer_rating and star with median.
df$reviewer_rating <- ifelse(is.na(df$reviewer_rating), median(df$reviewer_rating, na.rm = TRUE), df$reviewer_rating)
df$stars <- ifelse(is.na(df$stars), median(df$stars, na.rm = TRUE), df$stars)
#We should not replace id with mean, median,or mode.This is because the ID column is usually a unique identifier for each observation and should not have duplicates. Replacing missing values in the ID column can lead to unintended consequences and negatively impact the analysis.
#so, let's impute missing values in the ID column with a sequence number.
df$reviewer_id[is.na(df$reviewer_id)] <- 1:sum(is.na(df$reviewer_id))
summary(df)
#Now let's see if any column has blank as values
blank_values <- colSums(df == "")
blank_values
#we could see that there are a lot of blank values. We could replace price, host_responseTime and ratetype with mode.
df[df==""]<-NA
df<-df[complete.cases(df),]
summary(df)
#Now there are no blank spaces or na values in any column.
str(df)

write.csv(df, "airbnb_api_cleaned.csv", row.names = FALSE)
#All the variable have the correct data formats
install.packages(c("tm", "wordcloud","SnowballC"))
install.packages("tm_map")
install.packages("corpus")
library(tm_map)
library(tmaptools)
library(wordcloud)
library(SnowballC)
library(corpus)
library(tm)


#wordclouds
df$reviews <- gsub("<.*?>", "", df$reviews)
rev <- VectorSource(df$reviews)
rev_corpus <- VCorpus(rev)
rev_corpus
rev_corpus[[10]][1]
rm<-removePunctuation(df$reviews)
removeNumbers(df$reviews)
stripWhitespace(df$reviews)
replace_symbol(df$reviews)
removeWords(df$reviews, stopwords("en"))
n_char_vec <- unlist(strsplit(rm, split = "  "))
stem_doc <- stemDocument(n_char_vec)
stem_doc
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus)
}
clean_corp <- clean_corpus(rev_corpus)
wordcloud(clean_corp
          , scale=c(5,0.5)     # Set min and max scale
          , max.words=100      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.35       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "Dark2"))



#

