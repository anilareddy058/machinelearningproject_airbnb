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


r_reg <- lm(data=listing, price ~ latitude + longitude + room_type + accommodates + bedrooms + minimum_nights + maximum_nights + number_of_reviews + availability_365)
summary(r_reg)


listing <- listing %>% select(price,accommodates)
write.csv(listing, "listing_reg.csv", row.names = FALSE)

(Size <- (as.integer(nrow(listing)/4)))  ## Test will be 1/4 of the data
(SAMPLE <- sample(nrow(listing), Size))

(listing_Test<-listing[SAMPLE, ])
(listing_Train<-listing[-SAMPLE, ])

lm_1 <- lm(data=listing_Train, price ~ accommodates)
summary(lm_1)
#plots
summary_model_1 <- summary(lm_1)
mse_1 <- summary_model_1$sigma^2
r_sq_1 <- summary_model_1$r.squared
adj_r_sq_1 <- summary_model_1$adj.r.squared
par(mfrow=c(2,2)) 
plot(lm_1)

prediction1<-predict(lm_1, newdata = listing_Test)
prediction1<- exp(prediction1)
mse = mean(lm_1$residuals^2)
AIC(lm_1)
BIC(lm_1)


#predict
pred <- predict(lm_1, newdata = listing_Test)

RMSE <- sqrt(mean((listing_Test$price - pred)**2 ))
SSE <- sum((listing_Test$price - pred)**2)
SSR <- sum((pred - mean(listing_Test$price)) ** 2)
SST <- SSR +SSE
R2 <- (SST - SSE) / SST

cat("SST: ", SST, "   SSE: ", SSE, "   SSR: ", SSR, "\nR2: ", R2, "   RMSE: ", RMSE)


actual <- listing_Test$price
lr_result <- data.frame(
  "Actual" = actual,
  "Predicted" = pred
)
head(lr_result, 20)



lm_line = lm(Predicted ~ Actual, data = lr_result)
plot(x = lr_result$Actual, y = lr_result$Predicted,
     main = "Actual and Predicted Price",
     xlab = "Actual Price ($)",
     ylab = "Predicted Price ($)")
abline(lm_line, col="red", lwd=3)

