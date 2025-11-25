# Install necessary packages if not already installed
if (!require(data.table)) install.packages("data.table")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(corrplot)) install.packages("corrplot")
if (!require(factoextra)) install.packages("factoextra")
if (!require(cluster)) install.packages("cluster")
if (!require(lubridate)) install.packages("lubridate")
if (!require(Hmisc)) install.packages("Hmisc")

# Load the packages
library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)
library(factoextra)
library(cluster)
library(lubridate)
library(Hmisc)

#Import the data set
taxi_data <- read.csv("Taxi Dataset.csv")
summary(taxi_data)
head(taxi_data)
str(taxi_data)

# Convert date-time columns from character to POSIXct
taxi_data <- taxi_data %>%
  mutate(
    tpep_pickup_datetime = ymd_hms(tpep_pickup_datetime),
    tpep_dropoff_datetime = ymd_hms(tpep_dropoff_datetime)
  )

# Create trip_duration
taxi_data <- taxi_data %>%
  mutate(trip_duration = as.numeric(difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins")))

# Verify the parsing
head(taxi_data)
str(taxi_data)

# Select relevant columns for correlation analysis
selected_columns <- taxi_data %>%
  select(trip_distance, trip_duration, fare_amount, total_amount, tip_amount, tolls_amount, passenger_count)

# Ensure selected_columns is a data frame
selected_columns <- as.data.frame(selected_columns)

# Compute the correlation matrix using the Hmisc package, which handles missing values
correlation_results <- rcorr(as.matrix(selected_columns), type = "pearson")

# Extract the correlation matrix
cor_matrix <- correlation_results$r

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle")

# Clustering Analysis
# Normalize the data for clustering
normalized_data <- scale(selected_columns)

# Determine the optimal number of clusters using the Elbow method
fviz_nbclust(normalized_data, kmeans, method = "wss")

# Apply K-means clustering with the optimal number of clusters (e.g., 3 clusters)
set.seed(123)
kmeans_result <- kmeans(normalized_data, centers = 3, nstart = 25)

# Add cluster assignments to the original data
taxi_data$cluster <- as.factor(kmeans_result$cluster)

# Add cluster assignments to the original data
taxi_data$cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters
ggplot(taxi_data, aes(x = trip_distance, y = fare_amount, color = cluster)) +
  geom_point() +
  labs(title = "Clustering of Taxi Trips based on Distance and Fare Amount")

# Pattern Analysis
# Plot trip distance vs trip duration colored by cluster
ggplot(taxi_data, aes(x = trip_distance, y = trip_duration, color = cluster)) +
  geom_point() +
  labs(title = "Pattern Analysis of Taxi Trips: Distance vs Duration")

# Boxplot to compare fare amounts across clusters
ggplot(taxi_data, aes(x = cluster, y = fare_amount, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Boxplot of Fare Amount by Cluster")

# Analyze pickup times and locations
ggplot(taxi_data, aes(x = hour(tpep_pickup_datetime), fill = cluster)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Distribution of Pickup Times by Cluster", x = "Hour of Day", y = "Count")

# Analyze pickup locations
ggplot(taxi_data, aes(x = as.factor(PULocationID), fill = cluster)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Pickup Locations by Cluster", x = "Pickup Location ID", y = "Count")

# Save the plots
ggsave("correlation_plot.png")
ggsave("cluster_plot_distance_fare.png")
ggsave("pattern_plot_distance_duration.png")
ggsave("boxplot_fare_by_cluster.png")
ggsave("pickup_times_by_cluster.png")
ggsave("pickup_locations_by_cluster.png")

# Save the plots
ggsave("correlation_plot.png")
ggsave("cluster_plot_distance_fare.png")
ggsave("pattern_plot_distance_duration.png")
ggsave("boxplot_fare_by_cluster.png")
