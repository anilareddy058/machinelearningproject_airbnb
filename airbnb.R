install.packages("gridExtra")
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)

#calendar data cleaning, preprocessing and visualizations
Myfile1="/Users/anilareddy/Downloads/project/portland/calendar.csv"
calendar <- read.csv(Myfile1)
str(calendar)
summary(calendar)
# Check for missing values in all columns
colSums(is.na(calendar))
# Check for missing values in each column
sapply(calendar, function(x) sum(is.na(x)))
# Check for missing values and return the columns with missing values
colnames(calendar)[colSums(is.na(calendar)) > 0]
#there are no missing values in the dataset. 
#But, we need to drop columns adjusted_price as we would not need that column.
#also, we need to change the datatype of price to float and remote the '$" symbol.
calendar <- calendar %>% select(-adjusted_price)
calendar$price <- as.numeric(gsub("[$,]", "", calendar$price))
calendar$available <- ifelse(calendar$available == "t", 1, 0)
calendar$date <- as.Date(calendar$date, format =  "%m/%d/%Y")
#calendar$date <- ymd(calendar$date)
calendar$month <- month(as.Date(calendar$date))
calendar$year <- year(as.Date(calendar$date))
str(calendar)
calendar <- calendar %>% select(-adjusted_price)
calendar <- calendar %>% select(-date)
#calendar$available <- calendar$available.apply(lambda x:1 if x =='t' else 0)
summary(calendar)


#histogram
library(ggplot2)
ggplot(calendar) +
  aes(x = available) +
  geom_histogram(bins=30, fill = "#0c4c8a") 
  theme_minimal()
summary(calendar$available)


#boxplot of prices
boxplot(calendar$price,ylab = "price")
boxplot.stats(calendar$price)$out
out <- boxplot.stats(calendar$price)$out
out_ind <- which(calendar$price %in% c(out))
out_ind
calendar[out_ind, ]
#The lower range outliers start at 10$. There is a possibility that a listing could be this low. So, let's keep this outlier.
#there is no way a listing could be around 9999/night. These outliers should definitely be removed.
boxplot(calendar$price,
        ylab = "price",
        main = "Boxplot of prices", ylim = c(0, 9999)
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
calendar[calendar$price==10, ]
calendar <- subset(calendar,price < 1000 )
boxplot.stats(calendar$price)$out
out <- boxplot.stats(calendar$price)$out
out_ind <- which(calendar$price %in% c(out))
out_ind
calendar[out_ind, ]
#when we look at the visualisation of the boxplot there seems to be outliers. But, we extract the rows and take a close look at them, the price seems to be high beacause, that specific properties are huge. so, they cost more.
boxplot(calendar$price,
        ylab = "price",
        main = "Boxplot of prices", ylim = c(0, 1000)
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))



#available column 
calendar$available<-as.factor(calendar$available)
#install.packages("hrbrthemes")
library(hrbrthemes)
summary(calendar$available)



write.csv(calendar, "cleaned_calendar.csv", row.names = FALSE)
summary(calendar)


##############################################################################################################
#listings data cleaning, preprocessing and visualisations
install.packages("ggmap")
library(ggmap)
Myfile2="/Users/anilareddy/Downloads/project/portland/listings.csv"
listings <- read.csv(Myfile2)
str(listings)
summary(listings)
colSums(is.na(listings))
# This dataset has a lot of missing values and there are a lot of columns that would not be needed for analysis.
listings <- listings %>% select(-listing_url,-source, -scrape_id,-last_scraped,-name,-description, -neighborhood_overview,-picture_url,-host_url, -host_about, -host_thumbnail_url,-host_picture_url, -license, -host_name, -host_location,
                                -host_neighbourhood, -neighbourhood_group_cleansed,-host_acceptance_rate,
                                 -has_availability, -calendar_last_scraped,-host_listings_count,-host_verifications, -host_has_profile_pic, -host_identity_verified,neighbourhood, -bathrooms, -minimum_minimum_nights, -maximum_minimum_nights, -minimum_maximum_nights, -maximum_maximum_nights, -minimum_nights_avg_ntm, -maximum_nights_avg_ntm,-calendar_updated, -number_of_reviews_l30d, -calculated_host_listings_count_private_rooms, -calculated_host_listings_count_shared_rooms, -calculated_host_listings_count_entire_homes)
listings <- listings %>% select(-neighbourhood,-host_response_time, -host_response_rate, -host_since)
str(listings)
summary(listings)
cleaned_listings<-listings
# the columns reviews_per_months, beds and bathrooms have lot of null values. we could replace them by mean.
cleaned_listings$beds[is.na(cleaned_listings$beds)] <- mean(cleaned_listings$beds, na.rm = TRUE)
cleaned_listings$bedrooms[is.na(cleaned_listings$bedrooms)] <- mean(cleaned_listings$bedrooms, na.rm = TRUE)
colSums(is.na(cleaned_listings))
summary(cleaned_listings)
str(cleaned_listings)
cleaned_listings$price <- as.numeric(gsub("[$,]", "", cleaned_listings$price))
cleaned_listings$first_review <- as.Date(cleaned_listings$first_review, format =  "%m/%d/%Y")
cleaned_listings$last_review <- as.Date(cleaned_listings$last_review, format =  "%m/%d/%Y")
cleaned_listings$review_scores_value[is.na(cleaned_listings$review_scores_value)] <- mean(cleaned_listings$review_scores_value, na.rm = TRUE)
cleaned_listings$reviews_per_month[is.na(cleaned_listings$reviews_per_month)] <- mean(cleaned_listings$reviews_per_months, na.rm = TRUE)
cleaned_listings$review_scores_checkin[is.na(cleaned_listings$review_scores_checkin)] <- mean(cleaned_listings$review_scores_checkin, na.rm = TRUE)
cleaned_listings$review_scores_communication[is.na(cleaned_listings$review_scores_communication)] <- mean(cleaned_listings$review_scores_communication, na.rm = TRUE)
cleaned_listings$review_scores_location[is.na(cleaned_listings$review_scores_location)] <- mean(cleaned_listings$review_scores_location, na.rm = TRUE)
cleaned_listings$review_scores_rating[is.na(cleaned_listings$review_scores_rating)] <- mean(cleaned_listings$review_scores_rating, na.rm = TRUE)
cleaned_listings$review_scores_accuracy[is.na(cleaned_listings$review_scores_accuracy)] <- mean(cleaned_listings$review_scores_accuracy, na.rm = TRUE)
cleaned_listings$review_scores_cleanliness[is.na(cleaned_listings$review_scores_cleanliness)] <- mean(cleaned_listings$review_scores_cleanliness, na.rm = TRUE)
cleaned_listings$reviews_per_month[is.na(cleaned_listings$reviews_per_month)] <- mean(cleaned_listings$reviews_per_month, na.rm = TRUE)
summary(cleaned_listings)
cleaned_listings <- cleaned_listings %>% select(-first_review, -last_review)
write.csv(listings, "cleaned_listings.csv", row.names = FALSE)

#correlation of price with all other columns
library(ggplot2)
install.packages("reshape2")
library(reshape2)
colSums(is.na(cleaned_listings))

summary(cleaned_listings)
cleaned_listings[cleaned_listings$price==9999, ]
plot(density(cleaned_listings$price),
     main = "Density plot", #this is the title of the plot
     xlab = "distribution of price of listings") #this is the title of the x-axis
boxplot(cleaned_listings$price,ylab = "price")
boxplot.stats(cleaned_listings$price)$out
out <- boxplot.stats(cleaned_listings$price)$out
out_ind <- which(cleaned_listings$price %in% c(out))
out_ind
cleaned_listings[out_ind, ]
boxplot(cleaned_listings$price,
        ylab = "price",
        main = "Boxplot of prices", ylim = c(0, 9999)
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
cleaned_listings <- subset(cleaned_listings,price < 1000 )
boxplot(cleaned_listings$price,
        ylab = "price",
        main = "Boxplot of prices", ylim = c(0, 2000)
)

mtext(paste("Outliers: ", paste(out, collapse = ", ")))
plot(density(cleaned_listings$price),
     main = "Density plot", #this is the title of the plot
     xlab = "distribution of price of listings") #this is the title of the x-axis



#room type and property type visualizatons
room_type <- cleaned_listings %>%
  count(room_type, sort = TRUE) %>%
  mutate(room_type = reorder(room_type, n)) %>%
  ggplot(aes(room_type, n)) +
  geom_col() +
  xlab(NULL) +
  ylab("count") + ggtitle("Room type")
  coord_flip()

property_type <- cleaned_listings  %>%
  count(property_type, sort = TRUE) %>%
  filter(n > 25) %>%
  mutate(property_type = reorder(property_type, n)) %>%
  ggplot(aes(property_type, n)) +
  geom_col() +
  xlab(NULL) +
  ylab("count") + ggtitle("Property type")
  coord_flip()
grid.arrange(room_type, property_type, nrow = 1)

#histogram for host_is_superhost
library(ggplot2)
theme_set(theme_classic())
ggplot(cleaned_listings) + ggtitle("How many hosts are superhosts ?") +
  geom_bar(stat = "count", aes(x = host_is_superhost), color='red', fill="white") +
  theme_minimal()
#From this we can infer that many of the host are superhosts.
summary(cleaned_listings$host_is_superhost)
#cleaning all blank value rows
cleaned_listings <- with(cleaned_listings, cleaned_listings[!(host_is_superhost == "" | is.na(host_is_superhost)), ])
cleaned_listings %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit


#categorical plots
library(ggplot2)
theme_set(theme_classic())
summary(cleaned_listings$price)

cleaned_listings %>%
  count(neighbourhood_cleansed, sort = TRUE) %>%
  filter(n > 75) %>%
  mutate(neighbourhood_cleansed = reorder(neighbourhood_cleansed, n)) %>%
  ggplot(aes(neighbourhood_cleansed, n)) + xlab("Neighbourhood") +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#map
install.packages("leaflet")
library(leaflet)
basemap <- addTiles(leaflet())
#df_unknown <- cleaned_listings %>%
  #filter(neighbourhood_cleansed == "Other neighborhoods")
df_unknown<- cleaned_listings$neighbourhood_cleansed
map <- addCircleMarkers(setView(basemap, lng = 122.67, lat = 45.52, zoom = 12), lng = cleaned_listings$longitude, lat = cleaned_listings$latitude, radius = 1, fillOpacity = 6)
map


#categorical histogram plot
cleaned_listings %>%
  mutate(price = as.numeric(gsub("\\$", "", price))) %>%
  count(neighbourhood_cleansed, sort = TRUE) %>%
  filter(n > 150) %>%
  mutate(neighbourhood_cleansed = reorder(neighbourhood_cleansed, n)) %>%
  inner_join(cleaned_listings, by = "neighbourhood_cleansed") %>%
  ggplot() +
  geom_histogram(mapping = aes(price, fill=neighbourhood_cleansed), binwidth=50) +
  labs(fill = NULL) + ggtitle("Price variation across top neighbourhoods") +
  scale_x_continuous(breaks=c(0,250, 500,  750, 1000, 1250, 1500, 1750, 2000,3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000))


#3D Scatter plot
install.packages("plotly")
library(plotly)

x <- cleaned_listings$beds
y <- cleaned_listings$bedrooms
z <- cleaned_listings$accommodates

trace1 <- plot_ly(x = x, y = y, z = z,type = "scatter3d",  mode = "markers",
                  marker = list(size = 12, color = cleaned_listings$price, colorscale = "Viridis", 
                                opacity = 0.8),
                  text = cleaned_listings$price) %>%
  layout(scene = list(xaxis = list(title = "beds"),
                      yaxis = list(title = "bedrooms"),
                      zaxis = list(title = "accommodates")),
         margin = list(l = 0, r = 0, b = 0, t = 0))

fig <- plotly::ggplotly(trace1)

plotly::ggplotly(fig, filename = "3d-scatter-colorscale")



#correlation plot
numerical_columns <- cleaned_listings %>% select_if(is.numeric)
# Access the list of column names
numerical_columns_names <- colnames(numerical_columns)
install.packages("corrplot")
library(corrplot)
cleaned_listings.cor = cor(numerical_columns , method = c("spearman")) + ggtitle("Minimum Nights <= 32 | Violin")
corrplot(cleaned_listings.cor)


tema <- theme(
  plot.title = element_text(size = 23, hjust = .5),
  axis.text.x = element_text(size = 19, face = "bold"),
  axis.text.y = element_text(size = 19, face = "bold"),
  axis.title.x = element_text(size = 19),
  axis.title.y = element_text(size = 19),
  legend.text = element_text(size = 14, face = "bold"))

df <- data.frame(minimum_nights = cleaned_listings["minimum_nights"][cleaned_listings["minimum_nights"] <= 32])
d <- ggplot(data = df, mapping = aes(y = "", x = minimum_nights)) +
  geom_violin(size=1.1, color = "black", fill = "yellow", alpha = .7) +
  geom_vline(xintercept=median(cleaned_listings$minimum_nights), size =1.5, color = "black") +
  coord_flip() +
  ggtitle("Minimum Nights <= 32 | Violin") +
  theme_minimal() +
  xlab("Minimum nights") +
  ylab("") +
  tema
d


#reviews per month histogram
hist(cleaned_listings$reviews_per_month, main="distribution of reviews per month", xlab="reviews per month")
nrow(cleaned_listings[cleaned_listings$reviews_per_month> 10, ])






