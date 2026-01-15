library(readr)
AirbnbSydney <- read_csv("AirbnbSydney.csv")
View(AirbnbSydney)
install.packages("dplyr")
library(dplyr)
airbnb_data <- AirbnbSydney


# Create a new variable called price.per.guest (computed for each Airbnb listing).
# Report the sample minimum, maximum, mean, median and standard deviation of this new variable.


# Create the new variable price.per.guest
airbnb_data <- airbnb_data %>%
  mutate(price.per.guest = ifelse(accommodates > 0, price / accommodates, NA))

# Display the first few rows with the new variable to verify
head(airbnb_data[, c("price", "accommodates", "price.per.guest")])

# Calculate descriptive statistics
min_price_per_guest <- min(airbnb_data$price.per.guest, na.rm = TRUE)
max_price_per_guest <- max(airbnb_data$price.per.guest, na.rm = TRUE)
mean_price_per_guest <- mean(airbnb_data$price.per.guest, na.rm = TRUE)
median_price_per_guest <- median(airbnb_data$price.per.guest, na.rm = TRUE)
std_dev_price_per_guest <- sd(airbnb_data$price.per.guest, na.rm = TRUE)

# Report the results
cat("Sample Minimum of price.per.guest:", sprintf("%.2f", min_price_per_guest), "\n")
cat("Sample Maximum of price.per.guest:", sprintf("%.2f", max_price_per_guest), "\n")
cat("Sample Mean of price.per.guest:", sprintf("%.2f", mean_price_per_guest), "\n")
cat("Sample Median of price.per.guest:", sprintf("%.2f", median_price_per_guest), "\n")
cat("Sample Standard Deviation of price.per.guest:", sprintf("%.2f", std_dev_price_per_guest), "\n")

#  Produce a data visualisation which shows the relationship between the variables price.per.guest and longitude


# Create the scatter plot
plot(x = airbnb_data$longitude,
     y = airbnb_data$price.per.guest,
     main = "Price Per Guest Vs Longitude", # Plot title
     xlab = "Longitude",                # X-axis label
     ylab = "Price Per Guest (AUD)",    # Y-axis label
     col = "blue", 
     pch = 16,                          # Point character (16 is filled circle)
     cex = 0.8                          # Size of points
)

# Add the regression line to the plot
lines(lowess(airbnb_data$longitude, airbnb_data$price.per.guest),
      col = "red", # Set the line color to red
      lwd = 2)     # Make the line thicker

# Add a legend to explain the line
legend("topleft", "Trend Line", col = "red", lty = 1, lwd = 2, bty = "n")

# Calculate the Correlation
# We need to handle NA values for the correlation calculation.
correlation <- cor(airbnb_data$longitude, airbnb_data$price.per.guest, use = "complete.obs")

cat("The correlation coefficient between Longitude and Price Per Guest is:", round(correlation, 4), "\n")

#  Produce a histogram to illustrate the probability distribution of the new variable price.per.guest. 


# Clean price.per.guest for distribution fitting:
# 1. Remove NA values (results from original NAs or accommodates = 0).
# 2. Filter out infinite values (e.g., price > 0 and accommodates = 0 before NA handling).
# 3. Ensure values are strictly positive for the log transformation used in Gamma parameter estimation.
price_per_guest_clean <- na.omit(airbnb_data$price.per.guest)
price_per_guest_clean <- price_per_guest_clean[is.finite(price_per_guest_clean)]
price_per_guest_clean <- price_per_guest_clean[price_per_guest_clean > 0] # Essential for log(X)

# Estimate Gamma Distribution parameters using the provided hint estimators

# Calculate the components for the estimators
X_bar <- mean(price_per_guest_clean) # Sample mean of X (price.per.guest)
X_ln_X_bar <- mean(price_per_guest_clean * log(price_per_guest_clean)) # Sample mean of X * ln(X)
ln_X_bar <- mean(log(price_per_guest_clean)) # Sample mean of ln(X)

# Calculate beta_hat (rate parameter, denoted as beta in the hint)
beta_hat <- (X_ln_X_bar - X_bar * ln_X_bar)^(-1)

# Calculate alpha_hat (shape parameter, denoted as alpha in the hint)
alpha_hat <- X_bar * beta_hat

cat(sprintf("Fitted Gamma distribution parameters (using provided hint estimators):\n"))
cat(sprintf("  Shape (alpha_hat, α) = %.4f\n", alpha_hat))
cat(sprintf("  Rate (beta_hat, β) = %.4f\n", beta_hat))

# Produce Histogram and Overlay Fitted Gamma Density

# Create the histogram with density scale (proba = TRUE as per hint)
hist(price_per_guest_clean,
     breaks = 100, # Number of bins for the histogram
     proba = TRUE, 
     main = "Probability Distribution of Price Per Guest with Fitted Gamma Density",
     xlab = "Price Per Guest (AUD)",
     ylab = "Density",
     col = "lightblue", 
     border = "darkblue",
     xlim = c(0, quantile(price_per_guest_clean, 0.99, na.rm = TRUE))
)

# Add the fitted Gamma density curve to the existing plot
curve(dgamma(x, shape = alpha_hat, rate = beta_hat),
      col = "red", 
      lwd = 2,     
      add = TRUE) 

# Add a legend 
legend("topright",
       c("Histogram", "Fitted Gamma Density"),
       col = c("lightblue", "red"),
       lwd = c(NA, 2),
       fill = c("lightblue", NA), 
       border = c("darkblue", NA),
       bty = "n") 


# Add a variable called Review.Bracket to this dataset. This new variable should be of type factor.
# report both the mean and median of price.per.guest within each Review.Bracket.


# Create Review.Bracket variable

# 1. Handle NA values in reviews_per_month: Assume NA means 0 reviews per month.
airbnb_data$reviews_per_month[is.na(airbnb_data$reviews_per_month)] <- 0

# 2. Calculate the required quantiles of reviews_per_month
q15 <- quantile(airbnb_data$reviews_per_month, 0.15, na.rm = TRUE)
q40 <- quantile(airbnb_data$reviews_per_month, 0.40, na.rm = TRUE)
q60 <- quantile(airbnb_data$reviews_per_month, 0.60, na.rm = TRUE)
q85 <- quantile(airbnb_data$reviews_per_month, 0.85, na.rm = TRUE)

# Print the quantile values 
cat(sprintf("Reviews per month quantiles:\n"))
cat(sprintf("  15th Percentile: %.2f\n", q15))
cat(sprintf("  40th Percentile: %.2f\n", q40))
cat(sprintf("  60th Percentile: %.2f\n", q60))
cat(sprintf("  85th Percentile: %.2f\n", q85))
cat("\n")

# 3. Create the Review.Bracket variable as a factor using case_when()
airbnb_data <- airbnb_data %>%
  mutate(Review.Bracket = case_when(
    reviews_per_month < q15 ~ 'Low',
    reviews_per_month >= q15 & reviews_per_month < q40 ~ 'Medium-Low',
    reviews_per_month >= q40 & reviews_per_month < q60 ~ 'Medium',
    reviews_per_month >= q60 & reviews_per_month < q85 ~ 'Medium-High',
    reviews_per_month >= q85 ~ 'High',
    TRUE ~ NA_character_ 
  )) %>%
  
  # Convert to factor with specified order for proper analysis and display
  mutate(Review.Bracket = factor(Review.Bracket,
                                 levels = c('Low', 'Medium-Low', 'Medium', 'Medium-High', 'High')))

# Report Mean and Median of price.per.guest within each Review.Bracket
summary_by_review_bracket <- airbnb_data %>%
  group_by(Review.Bracket) %>%
  summarise(
    Mean_Price_Per_Guest = mean(price.per.guest, na.rm = TRUE),
    Median_Price_Per_Guest = median(price.per.guest, na.rm = TRUE),
    N_Listings = n(), 
    .groups = 'drop'
  ) %>%
  arrange(Review.Bracket) 

print(summary_by_review_bracket)

# Relationship between room type and pricing


# Create the box plot
boxplot(price.per.guest ~ room_type,
        data = airbnb_data_clean,
        main = "Price Per Guest Distribution by Room Type",
        xlab = "Room Type",
        ylab = "Price Per Guest (AUD)",
        col = c("skyblue", "lightgreen", "coral"),
        border = "darkblue",
        outline = FALSE, 
        ylim = c(0, 250)
)

# Calculate summary statistics for price.per.guest grouped by room_type
summary_by_room_type <- airbnb_data %>%
  group_by(room_type) %>%
  summarise(
    Mean_Price_Per_Guest = mean(price.per.guest, na.rm = TRUE),
    Median_Price_Per_Guest = median(price.per.guest, na.rm = TRUE),
    SD_Price_Per_Guest = sd(price.per.guest, na.rm = TRUE),
    N_Listings = n(), # Count the number of listings in each category
    .groups = 'drop' # Ungroup the data at the end
  )

# Print the resulting summary table
print(summary_by_room_type)



