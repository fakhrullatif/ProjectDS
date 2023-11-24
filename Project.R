
#1 - getting data set
getwd()
data = read.csv("PlasticFate.csv",header=TRUE)
#plot the variable
ggplot(data, aes(Year, Share.of.waste.recycled, colour=Entity)) + 
  geom_line() + 
  geom_point()

#plot the variable
ggplot(data, aes(Year, Share.of.littered.and.mismanaged, colour=Entity)) + 
  geom_line() + 
  geom_point()

#2 Average of waste recycled and mismanaged
# Set the desired range of years
year_range <- c(2000, 2019)

# Filter the data within the specified range
filtered_data <- data %>% filter(Year >= year_range[1] & Year <= year_range[2])

# Calculate the average for each year in the filtered data
average_WasteRecycled <- tapply(filtered_data$Share.of.waste.recycled, filtered_data$Year, mean)
average_WasteMismanaged <- tapply(filtered_data$Share.of.littered.and.mismanaged, filtered_data$Year, mean)

#3 - normalization
# Function to perform min-max normalization
min_max_normalize <- function(x) {
  # Calculate min and max
  min_val <- min(x)
  max_val <- max(x)
  
  # Apply min-max normalization
  normalized <- (x - min_val) / (max_val - min_val)
  
  return(normalized)
}

#normalization on attribute WasteRecycled
data_WasteRecycled <- c(average_WasteRecycled)
normalized_WasteRecycled <- min_max_normalize(data_WasteRecycled)

#Extract the unique years and corresponding averages
years <- as.numeric(names(average_WasteRecycled))
avg_WasteRecycled <- unname(normalized_WasteRecycled)

#normalization on attribute WasteMismanaged
data_mismanaged <- c(average_WasteMismanaged)
normalized_WasteMismanaged <- min_max_normalize(data_mismanaged)

years2 <- as.numeric(names(average_WasteMismanaged))
avg_WasteMismanaged <- unname(normalized_WasteMismanaged)

# Create a line plot
plot(years, avg_WasteRecycled, type = "l", main = "Average Waste Recycled VS Waste Mismanaged over Year", xlab = "Year", ylab = "Average Waste Recycled VS Waste Mismanaged", col = "blue")
lines(years2, avg_WasteMismanaged, col="red")
points(years, avg_WasteRecycled, col = "blue", pch = 16)
points(years2, avg_WasteMismanaged, col = "red", pch = 16)
legend("bottom", legend = c("Waste Recycled", "Waste Mismanaged"), col = c("blue", "red"), lty = 1, pch = 16, title = "Entities",cex = 0.8)


#4.1 linear regression of Waste Recycled
lm(normalized_WasteRecycled~years, data=data)

#y=-105.86028 + 0.05293x

fit = lm(normalized_WasteRecycled~years, data=data)
summary(fit)

#p-values = 2.2*10^-16 < 0.05
#x is significantly associated with target y

#5.1 Prediction Waste Recycled
yhat <- fit$coefficient[[1]] + fit$coefficient[[2]] + normalized_WasteRecycled
yhat
#performs prediction
yhat <- predict(fit, data)
yhat

#best fit lines
plot(normalized_WasteRecycled~years)
lines(years, yhat, lwd = 2)

#4.2 linear regression of Waste Mismanaged

#adjusting outliers on year 2009
normalized_WasteMismanaged <- subset(normalized_WasteMismanaged, years!= 2009)
years3 <- as.numeric(names(normalized_WasteMismanaged))

lm_model <- lm(normalized_WasteMismanaged~years3, data=data)

#y=107.28439 - 0.05313x

fit2 <- lm(normalized_WasteMismanaged~years3,  data=data)
summary(fit2)

#p-values = 2.2*10^-16 < 0.05
#x is significantly associated with target y

#5.2 Prediction Waste Mismanaged
yhat2 <- fit2$coefficient[[1]] + fit2$coefficient[[2]] + normalized_WasteMismanaged
yhat2
#performs prediction
yhat2 <- predict(fit2, data)
yhat2

#best fit lines
plot(normalized_WasteMismanaged~years3)
lines(years3, yhat2, lwd = 2)
