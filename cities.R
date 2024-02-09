#Load required libraries
library(ggplot2)  # for data visualization
library(leaflet) 

#Read the .csv file
df <- read.csv("data.csv")


head(df,5)

#we see that wellnessAddress has NA so removing the column
df <- subset(df, select = -c(wellnessCentreAddress))

#seeing the updated data df
head(df,5)

#summaries
summary(df$doctorCount)
summary(df$longitude)
summary(df$latitude)


#doctorCount vs. longitude
ggplot(df, aes(x = longitude, y = doctorCount)) +
  geom_point() +
  labs(x = "Longitude", y = "Doctor Count",
       title = "Scatterplot: Doctor Count vs. Longitude")

#doctorCount vs. latitude
ggplot(df, aes(x = latitude, y = doctorCount)) +
  geom_point() +
  labs(x = "Latitude", y = "Doctor Count",
       title = "Scatterplot: Doctor Count vs. Latitude")

##AIM= to find top cities with maximum number of doctors 
#Find the total number of doctors for each city, Sort cities on the total number of doctors, top 10 cities with the highest number of doctors
citydoc_counts <- aggregate(doctorCount ~ cityName, df, sum)
citydoc_counts <- citydoc_counts[order(-citydoc_counts$doctorCount), ]
top10_cities <- head(citydoc_counts, 10)

ggplot(top10_cities, aes(x = reorder(cityName, -doctorCount), y = doctorCount)) + 
  geom_bar(stat = "identity", fill = "turquoise") + 
  labs(title = "Top 10 Cities with the Highest Number of Doctors", x = "City Name", y = "Number of Doctors")


##Aim= to find the Doctors by Category within Top 10 Cities
# Calculate total number of doctors for each category within the top 10 cities
category_doctor_counts_top10 <- aggregate(doctorCount ~ category, df[df$cityName %in% top_10_cities$cityName, ], sum)

# Visualize distribution of doctors among different categories
ggplot(category_doctor_counts_top10, aes(x = category, y = doctorCount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Doctors by Category within Top 10 Cities", x = "Category", y = "Number of Doctors")

# Aggregate doctor counts by category
category_doctor_counts <- aggregate(doctorCount ~ category, df, sum)

# Sort the categories by total doctor count
category_doctor_counts <- category_doctor_counts[order(-category_doctor_counts$doctorCount), ]

ggplot(category_doctor_counts, aes(x = reorder(category, -doctorCount), y = doctorCount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Doctors by Wellness Center Category",
       x = "Category", y = "Number of Doctors") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


##Aim= to have locations of top 10 cities
df_top10 <- df[df$cityName %in% top_10_cities$cityName, ]

leaflet() %>%
  addTiles() %>%
  addMarkers(data = df_top10, ~longitude, ~latitude, popup = ~cityName) %>%
  fitBounds(lng1 = min(df_top10$longitude), lat1 = min(df_top10$latitude),
            lng2 = max(df_top10$longitude), lat2 = max(df_top10$latitude))
# one point is wrong


