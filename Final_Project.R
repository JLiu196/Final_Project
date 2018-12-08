library(ggplot2)
library(data.table)
library(dplyr)
library(bit64)
library(lubridate)
message("All library loaded")

setwd("~/Desktop/STAT 184/Final_Project/Five_US_Major_Cities")

LA_calendar <- fread("LA_calendar.csv")
LA_listings <- fread("LA_listings.csv")
LA_reviews <- fread("LA_reviews.csv")
message("All LA files read in.")

Austin_calendar <- fread("Austin_calendar.csv")
Austin_listings <- fread("Austin_listings.csv")
Austin_reviews <- fread("Austin_reviews.csv")
message("All Austin files read in.")

Chicago_calendar <- fread("Chicago_calendar.csv")
Chicago_listings <- fread("Chicago_listings.csv")
Chicago_reviews <- fread("Chicago_reviews.csv")
message("All Chicago files read in.")

NYC_calendar <- fread("NYC_calendar.csv")
NYC_listings <- fread("NYC_listings.csv")
NYC_reviews <- fread("NYC_reviews.csv")
message("All NYC files read in.")

DC_calendar <- fread("DC_calendar.csv")
DC_listings <- fread("DC_listings.csv")
DC_reviews <- fread("DC_reviews.csv")
message("All DC files read in.")

##Fully spell out state name for the state variable
LA_listings$state <- state.name[match(LA_listings$state, state.abb)]
Austin_listings$state <- state.name[match(Austin_listings$state, state.abb)]
Chicago_listings$state <- state.name[match(Chicago_listings$state, state.abb)]
NYC_listings$state <- state.name[match(NYC_listings$state, state.abb)]

major_cities_calendar <- rbind(LA_calendar, Austin_calendar)
major_cities_calendar <- rbind(major_cities_calendar, Chicago_calendar)
major_cities_calendar <- rbind(major_cities_calendar, NYC_calendar)
major_cities_calendar <- rbind(major_cities_calendar, DC_calendar)
message("major_cities_calendar created")

major_cities_listings <- rbind(LA_listings, Austin_listings)
major_cities_listings <- rbind(major_cities_listings, Chicago_listings)
major_cities_listings <- rbind(major_cities_listings, NYC_listings)
major_cities_listings <- rbind(major_cities_listings, DC_listings)
message("major_cities_listings created")

major_cities_reviews <- rbind(LA_reviews, Austin_reviews)
major_cities_reviews <- rbind(major_cities_reviews, Chicago_reviews)
major_cities_reviews <- rbind(major_cities_reviews, NYC_reviews)
major_cities_reviews <- rbind(major_cities_reviews, DC_reviews)
message("major_cities_reviews created")

setnames(major_cities_listings, "id", "listing_id")

##subset calendar to Christmas
major_cities_calendar_sub <- major_cities_calendar[date %in% "2018-12-25"]
major_cities_calendar_sub$date <- as_datetime(major_cities_calendar_sub$date)

##Create a new variable location with city, state, and country combined
major_cities_listings$location <- paste(major_cities_listings$city, major_cities_listings$state, sep = ", ")
major_cities_listings$location <- paste(major_cities_listings$location, major_cities_listings$country, sep = ", ")
message("New variable: location created")

##Does the host live within the city?
major_cities_listings$is_host_near <- major_cities_listings$host_location == major_cities_listings$location
message("new variable: is_host_near created")

##Merge
major_cities_calendar_sub$listing_id <- as.character(major_cities_calendar_sub$listing_id)
major_cities_listings$listing_id <- as.character(major_cities_listings$listing_id)
major_cities <- merge(major_cities_calendar_sub[,c('listing_id','available','price')], major_cities_listings[,c('listing_id','host_name','host_since','host_location','host_is_superhost','host_total_listings_count','host_identity_verified',
                                                                                                                'location','state','room_type','accommodates','bathrooms','bedrooms','beds','price','security_deposit','number_of_reviews',
                                                                                                                'first_review','last_review','review_scores_rating','cancellation_policy','reviews_per_month','is_host_near')], by = "listing_id", all.x = T)
message("major_cities_calendar_sub and major_cities_listings merged")

##Subset listings to these five states
five_cities <- major_cities[state %in% c("New York", "California", "DC", "Texas", "Illinois")]

##ggplot for room type distribution in each state
ggplot(five_cities, aes(x=state, fill = room_type)) + geom_bar() + ggtitle("Room Type Distribution") + xlab("State") + ylab("Number of Listings")
message("ggplot created")

##Tidy data
roomTypes <- dcast(five_cities, listing_id~room_type,length, value.var = c("room_type"))
five_cities<- merge(five_cities[,-c('room_type')], roomTypes, by = "listing_id", all.x = T)
cancellation_policy <- dcast(five_cities, listing_id~cancellation_policy,length, value.var = c("cancellation_policy"))
five_cities <- merge(five_cities[,-c('cancellation_policy')], cancellation_policy, by = "listing_id", all.x = T)
message("Tidy Table")

##Average price per person (assuming two people per bed)
five_cities$price.x <- as.numeric(gsub("\\$","",five_cities$price.x))
five_cities$price.y <- as.numeric(gsub("\\$","",five_cities$price.y))
five_cities$avg_price <- five_cities$price.y/(2*five_cities$beds)
message("New variable: avg_price created")

##Does the host rise the price for holiday
price_change <- function(x,y){
  if (is.na(x)) {
    change <- "Listing not available"
  } else if(is.na(y)){
    change <- "No listing price"
  }else if (x == y) {
    change <- "Same Price"
  } else if (x > y){
    change <- "Increased"
  } else if (x < y){
    change <- "Decreased"
  }
  change
}
message("New function: price_change")

##Use function created to compare listing price on Christmas and on a normal day.
changed <- NULL
for (i in 1:dim(five_cities)[1]){
  price_changed <- price_change(five_cities$price.x[i],five_cities$price.y[i])
  changed <- c(changed,price_changed)
}
five_cities$change_of_price <- changed
message("New variable: change_of_price created in five_cities data table")

##Merge the third table
five_cities2 <- merge(major_cities_reviews[,c('listing_id','date','reviewer_name','comments')], five_cities, by = "listing_id", all.x = T)

##Date format
five_cities$first_review <- as_datetime(five_cities$first_review)
five_cities$last_review <- as_datetime(five_cities$last_review)
five_cities2$date <- as_datetime(five_cities2$date)
five_cities2$review_year <- year(five_cities2$date)

##count the number of reviews in a year for each state
review_count <- dcast(five_cities2, review_year+state~., length, value.var = c("reviewer_name"))
review_count <- review_count[!is.na(review_count$state)]
##Trend on Airbnb review counts for each state
ggplot(review_count, aes(x=review_year, y = ., color = state)) + geom_line() + ggtitle("Review Trend for Each State") + xlab("Year") + ylab("Number of Reviews")

##How is listing price on Christmas different from a normal day
ggplot(five_cities, aes(x=change_of_price, fill = state)) + geom_bar() + ggtitle("Distribution of Price Change Type") + xlab("Price Change Categories") + ylab("Number of Listings")

#Statistical summary of listing price (i.e. price.y)
five_cities<- five_cities[price.y>0]
summary(five_cities$price.y)
message("Mean of price.y if $149.80")
ggplot(five_cities, aes(x=price.y)) + geom_histogram(binwidth = 15)+ggtitle("Listing Price Distribution") + xlab("Listing Price")+ylab("Frequency")
message("Price are heavily right skewed")
fit <- aov(price.y ~ state, data = five_cities)
##Tukey Comparison Test
TukeyHSD(fit)
plot(TukeyHSD(fit))
##The output indicates the differences in all groups are significant.

