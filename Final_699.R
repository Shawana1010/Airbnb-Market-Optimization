

roam_list <- read.csv("C:/Users/maxma/Documents/AD 699/Final_699/rome_listings.csv")
summary(roam_list)


colnames(roam_list)

neighbourhood_data <- dplyr::filter(roam_list, neighbourhood_cleansed == "XIII Aurelia")

unique(neighbourhood_data$id)

#"id"
#"listing_url"
#"scrape_id"
# "last_scraped" 
# "source"
# "name"
# "neighborhood_overview"
# "picture_url"
# "host_url"
# "host_about"
# "host_neighbourhood"
# "neighbourhood" 
# "neighbourhood_cleansed"
# "neighbourhood_group_cleansed"
# "bathrooms" 
#  "maximum_maximum_nights"
#  "minimum_minimum_nights"
# "maximum_minimum_nights" 
# "minimum_maximum_nights"
#  "calendar_last_scraped"
# "license"
#  "first_review"
#  "last_review"
#  "calculated_host_listings_count_shared_rooms" 
# "host_verifications" 
#"host_picture_url" 
# host_thumbnail_url
#"neighbourhood_group_cleansed"
#"calendar_updated"
#"description"

neighbourhood_data1 <- subset(neighbourhood_data, select = -c(id,listing_url,scrape_id,last_scraped, source, 
                                                              name, neighborhood_overview,picture_url, host_url,
                                                              host_about, host_neighbourhood,neighbourhood,neighbourhood_cleansed,
                                                              bathrooms,maximum_maximum_nights, minimum_minimum_nights, maximum_minimum_nights,
                                                              minimum_maximum_nights,calendar_last_scraped,license, first_review, 
                                                              last_review,calculated_host_listings_count_shared_rooms,
                                                              host_verifications, host_picture_url,host_thumbnail_url,
                                                              neighbourhood_group_cleansed,calendar_updated, description))

library(tidyverse)


missing_values <- neighbourhood_data1 %>%
  summarise_all(~sum(is.na(.)))

print(missing_values)


#46
column_data_type <- class(neighbourhood_data1$reviews_per_month)

summary(neighbourhood_data1$reviews_per_month)

neighbourhood_data1$reviews_per_month[is.na(neighbourhood_data1$reviews_per_month)] <- 1.709


#45

class(neighbourhood_data1$calculated_host_listings_count_private_rooms)

summary(neighbourhood_data1$calculated_host_listings_count_private_rooms)


#44

class(neighbourhood_data1$calculated_host_listings_count_entire_homes)

summary(neighbourhood_data1$calculated_host_listings_count_entire_homes)
# do we need the details of listing types counts? 

#43
class(neighbourhood_data1$calculated_host_listings_count)

summary(neighbourhood_data1$calculated_host_listings_count)

#42
class(neighbourhood_data1$calculated_host_listings_count)

summary(neighbourhood_data1$calculated_host_listings_count)

#41
class(neighbourhood_data1$instant_bookable)

summary(neighbourhood_data1$instant_bookable)

neighbourhood_data1$instant_bookable <- ifelse(neighbourhood_data1$instant_bookable == "t", 1, 0)
#we converted t f to 1 and 0

#40
class(neighbourhood_data1$review_scores_value)

summary(neighbourhood_data1$review_scores_value)

#imputed the missing values with the median value

neighbourhood_data1$review_scores_value[is.na(neighbourhood_data1$review_scores_value)] <- 4.750


#39
class(neighbourhood_data1$review_scores_location)

summary(neighbourhood_data1$review_scores_location)

neighbourhood_data1$review_scores_location[is.na(neighbourhood_data1$review_scores_location)] <- 4.730


#38
class(neighbourhood_data1$review_scores_communication)

summary(neighbourhood_data1$review_scores_communication)

neighbourhood_data1$review_scores_communication[is.na(neighbourhood_data1$review_scores_communication)] <- 4.930


#37
class(neighbourhood_data1$review_scores_checkin)

summary(neighbourhood_data1$review_scores_checkin)

neighbourhood_data1$review_scores_checkin[is.na(neighbourhood_data1$review_scores_checkin)] <- 4.910


#36

class(neighbourhood_data1$review_scores_cleanliness)

summary(neighbourhood_data1$review_scores_cleanliness)

neighbourhood_data1$review_scores_cleanliness[is.na(neighbourhood_data1$review_scores_cleanliness)] <- 4.860

#35

class(neighbourhood_data1$review_scores_accuracy)

summary(neighbourhood_data1$review_scores_accuracy)

neighbourhood_data1$review_scores_accuracy[is.na(neighbourhood_data1$review_scores_accuracy)] <- 4.870

#34
class(neighbourhood_data1$review_scores_rating)

summary(neighbourhood_data1$review_scores_rating)

neighbourhood_data1$review_scores_rating[is.na(neighbourhood_data1$review_scores_rating)] <- 4.820

#33
class(neighbourhood_data1$number_of_reviews_l30d)

summary(neighbourhood_data1$number_of_reviews_l30d)

#32

class(neighbourhood_data1$number_of_reviews_ltm)

summary(neighbourhood_data1$number_of_reviews_ltm)

#31
class(neighbourhood_data1$number_of_reviews)

summary(neighbourhood_data1$number_of_reviews)
#maybe look for outliars? 

#30
class(neighbourhood_data1$availability_365)

summary(neighbourhood_data1$availability_365)

#29
class(neighbourhood_data1$availability_90)

summary(neighbourhood_data1$availability_90)

#28
class(neighbourhood_data1$availability_60)

summary(neighbourhood_data1$availability_60)

#27
class(neighbourhood_data1$availability_30)

summary(neighbourhood_data1$availability_30)

#26
class(neighbourhood_data1$has_availability)

summary(neighbourhood_data1$has_availability)

neighbourhood_data1$has_availability <- ifelse(neighbourhood_data1$has_availability == "t", 1, 0)
#we converted t f to 1 and 0

#dropping the $ sign  
neighbourhood_data1$price <- gsub("\\$", "", neighbourhood_data1$price)

class(neighbourhood_data1$price)

summary(neighbourhood_data1$price)
neighbourhood_data1$price <- as.numeric(trimws(as.character(neighbourhood_data1$price)))

na_rows <- neighbourhood_data1[is.na(neighbourhood_data1$price), ]


#Amenities 
isolated_vector <- neighbourhood_data1$amenities

split_list <- strsplit(isolated_vector, split = ",")
split_df <- do.call(rbind, lapply(split_list, as.data.frame))


split_df <- do.call(rbind, lapply(split_list, function(x) {
  # Ensure that each row has the same number of elements
  length(x) <- max(sapply(split_list, length))
  as.data.frame(t(x))
}))


install.packages("writexl")

library(writexl)
write_xlsx(split_df,"C:/Users/maxma/Documents/AD 699/Final_699/split_df.xlsx")

wifi_rows <- neighbourhood_data1[grepl("Wifi", neighbourhood_data1$amenities), ]

neighbourhood_data1$amenities <- grepl("Wifi", neighbourhood_data1$amenities, ignore.case = TRUE)

