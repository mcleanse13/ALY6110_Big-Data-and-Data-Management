#ALY 6110 - Module 4 Lab
#Installing Spark packages
system("java -version")
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre-1.8")
install.packages("sparklyr")
install.packages("dplyr")
install.packages("sparklyr.nested")
install.packages("corrr")
install.packages("dbplot")
install.packages("rmarkdown")
packageVersion("sparklyr")
library(sparklyr)
library(dplyr)
library(readr)
library(ggplot2)
library(corrr)
library(dbplot)
spark_install()
spark_install("2.3")
spark_install(version = "3.5.1")
spark_available_versions()
spark_installed_versions()
if (!require(sparklyr)) install.packages("sparklyr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(ggplot2)) install.packages("ggplot2")

#Connect to this local cluster
sc <- spark_connect(master = "local", version = "3.5.1")
install.packages("readxl")

# Read the CSV file into R for Five Digit Dataset
zip5 <- read_csv("HPI_AT_BDL_ZIP5.csv")

# Inspect the first few rows of the data frame to ensure it's read correctly
head(zip5)

# Copy the local data frame to Spark
zip5 <- copy_to(sc, zip5, overwrite = TRUE)

# Inspect the Spark DataFrame
head(zip5)

# Read the CSV file into R for Zillow Dataset
zhvi <- read_csv("Zip_Zhvi_Summary_AllHomes.csv")

# Inspect the first few rows of the data frame to ensure it's read correctly
head(zhvi)

# Copy the local data frame to Spark
zhvi <- copy_to(sc, zhvi, overwrite = TRUE)

# Inspect the Spark DataFrame
head(zhvi)

##Five Digit Zip Code Dataset
#Summary Statistics of Dataset
zip5
summary(zip5)
View(zip5)
str(zip5)
summarize_all(zip5, mean) %>%
  show_query()

#Analysis
library(DBI)
dbGetQuery(sc, "SELECT count(*) FROM zip5")
select(zip5, HPI_with_1990_base, HPI_with_2000_base) %>%
  sample_n(100) %>%
  collect() %>%
  plot()

# Calculate the average HPI for 1990 and 2000 bases (Code provided by ChatGPT)
hpi_comparison <- zip5 %>%
  summarise(HPI_1990 = mean(HPI_with_1990_base, na.rm = TRUE),
            HPI_2000 = mean(HPI_with_2000_base, na.rm = TRUE)) %>%
  collect()

# Plot the comparison
ggplot(hpi_comparison, aes(x = "", y = HPI_1990, fill = "1990 Base")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = HPI_2000, fill = "2000 Base"), stat = "identity", position = "dodge") +
  labs(title = "Average HPI: 1990 vs 2000 Base", x = "", y = "Average HPI") +
  scale_fill_manual(values = c("1990 Base" = "blue", "2000 Base" = "red"))

#Correlation Analysis (Part of the code provided by ChatGPT)
# Convert all columns to numeric types
numeric_hpi_data <- zip5 %>%
  mutate(across(everything(), as.numeric))

# Collect the data to R for correlation analysis
numeric_hpi_data_df <- numeric_hpi_data %>%
  collect()

# Compute correlation matrix for all columns in the HPI dataset
cor_hpi <- numeric_hpi_data_df %>%
  correlate()

# View the correlation matrix
print(cor_hpi)
correlate(cor_hpi, use = "pairwise.complete.obs", method = "pearson") %>%
  shave() %>%
  rplot()

#DBPlot of the 'Year' variable
zip5 %>%
  dbplot_histogram(Year, binwidth = 3) +
  labs(title = "Housing DBPlot",
       subtitle = "Number of Houses Per Year")

##Zillow Dataset
#Summary Statistics of Dataset
zhvi
summary(zhvi)
View(zhvi)
str(zhvi)
summarize_all(zhvi, mean) %>%
  show_query()

#Analysis
library(DBI)
dbGetQuery(sc, "SELECT count(*) FROM zhvi")
model <- ml_linear_regression(zhvi, PctFallFromPeak ~ PeakZHVI)
model
model %>%
  ml_predict(copy_to(sc, data.frame(PeakZHVI = 1000000 + 190000 * 1:10))) %>%
  transmute(PeakZHVI = PeakZHVI, PctFallFromPeak = prediction) %>%
  full_join(select(zhvi, PeakZHVI, PctFallFromPeak)) %>%
  collect() %>%
  plot()

#Descriptive statistics analysis, focusing on aggregation and ranking
#What parts of the country have the highest peak values historically? (Code from ChatGPT)
peak_values <- zhvi %>%
  group_by(State) %>%
  summarise(MaxPeakZHVI = max(PeakZHVI, na.rm = TRUE)) %>%
  arrange(desc(MaxPeakZHVI)) %>%
  head(10) %>%
  collect()

# Plot the peak values
ggplot(peak_values, aes(x = reorder(State, -MaxPeakZHVI), y = MaxPeakZHVI, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 States with Highest Peak ZHVI", x = "State", y = "Peak ZHVI") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Time Series Analysis: Extract year from PeakMonth and create a new column (ChatGPT code)
zhvi_data <- zhvi %>%
  mutate(PeakYear = as.integer(substr(PeakMonth, 1, 4)))

# Calculate the average ZHVI for each state and year 
zhvi_trend <- zhvi_data %>%
  group_by(State, PeakYear) %>%
  summarise(AvgZHVI = mean(Zhvi, na.rm = TRUE)) %>%
  arrange(PeakYear) %>%
  collect()

# Create a line plot for the trends over time
ggplot(zhvi_trend, aes(x = PeakYear, y = AvgZHVI, color = State)) +
  geom_line() +
  labs(title = "Trends of Housing Prices Over Time by State",
       x = "Year",
       y = "Average ZHVI") +
  theme_minimal()
