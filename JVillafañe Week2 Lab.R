# load the data.table, ggolot2, and dplyr libraries and the zillow_price.csv file
library(data.table)
library(ggplot2)
library(dplyr)

#reading in csv file
zillow <- read.csv("~/MSDS/MSDS660/Week2/Lab/zillow_price.csv")

zillow

# Convert the file to a data table
setDT(zillow)

# how many observations and columns are there? 
#There are 90,275 obs & 60 columns
dim(zillow)


# use str and summary to see how many missing values we have,
# and what the data looks like
summary(zillow)
str(zillow)


# columns that are numeric and don't have lots of missing values
# you can add others if you like
numeric_cols <- c('bathroomcnt',
                  'bedroomcnt',
                  'calculatedfinishedsquarefeet',
                  'roomcnt',
                  'yearbuilt',
                  'taxvaluedollarcnt',
                  'landtaxvaluedollarcnt',
                  'price',
                  'numberofstories')

# Simplify your dataset by only selecting the columns of your choosing dt[, numeric_cols, with = FALSE]
zillowtrim <- zillow[, numeric_cols, with = FALSE]

dim(zillowtrim)
#There are now 9 columns

# We want to try to correlate home price with another variable.
# Let's look to see if there are any outliers in the price column we need to remove
# Create a boxplot of the price data
boxplot(zillowtrim$price)

# Wow there are expensive homes!

# Remove the outliers. dt[!which(dt$price %in% boxplot(dt$price)$out)]
zillowprice <- zillowtrim[!which(zillow$price %in% boxplot(zillow$price)$out)]

dim(zillowprice)


# How many outliers did we drop? And lets plot a new box plot to see the column
#I'm seeing 84,180 observations, meaning 6,095 outliers were dropped since there were originally 90,275.
boxplot(zillowprice$price)


# In our case, we have too many observations.  
# Use sample() to only sample a few hundred (maybe 500) points to plot.
# plot a few of the more interesting pairs together
plot(sample(zillowprice$calculatedfinishedsquarefeet,500), sample(zillowprice$price,500))

plot(sample(zillowprice$yearbuilt,500), sample(zillowprice$landtaxvaluedollarcnt,500))

plot(sample(zillowprice$calculatedfinishedsquarefeet,500), sample(zillowprice$taxvaluedollarcnt,500))

# bonus: try to make some nice-looking scatter plots with ggplot2
ggplot(zillowprice, aes(x=calculatedfinishedsquarefeet, y=yearbuilt)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")
# Adds confidence interval fill (default value of 0.95)
ggplot(zillowprice, aes(x=taxvaluedollarcnt, y=price)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")

# create a new data.table by dropping any missing values
# look up 'complete.cases()'
# use dim() to see how many cases we dropped
zillowprice[complete.cases(zillowprice), ] # Keep only the complete rows
zillow_complete <- zillowprice[complete.cases(zillowprice), ] # Store the complete cases subset in a new data frame

dim(zillow_complete)

# get the pearson correlation between price and another variable using cor()
#...there are other types of correlations
# try ?cor to see options, and try another correlation 
cor(zillow_complete$price, zillow_complete$calculatedfinishedsquarefeet)
cor(zillow_complete$price, zillow_complete$taxvaluedollarcnt) #highest cor value
cor(zillow_complete$price, zillow_complete$roomcnt)
cor(zillow_complete$price, zillow_complete$landtaxvaluedollarcnt)

cor(zillow_complete$price, zillow_complete$taxvaluedollarcnt, method = "spearman")
cor(zillow_complete$price, zillow_complete$landtaxvaluedollarcnt, method = "spearman")

cor(zillow_complete$price, zillow_complete$taxvaluedollarcnt, method = "kendall")
cor(zillow_complete$price, zillow_complete$landtaxvaluedollarcnt, method = "kendall")

# use the lm() command to fit a linear model of price to the 
# one variable you think is most correlated or predictive of price
# lm stands for 'linear model'
fit <- lm(zillow_complete$price ~ zillow_complete$taxvaluedollarcnt)


# view the model summary 
summary(fit)


# plot a scatter plot of the price and the variable you chose
# Adds confidence interval fill (default value of 0.95)
ggplot(zillow_complete, aes(x=taxvaluedollarcnt, y=price)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")


# add the regression line to the current plot using abline()...done above


# R makes it very easy to plot the diagnostics of a fit
# here's a decent resources explaining the plots: 
# http://data.library.virginia.edu/diagnostic-plots/
# plot the fit diagnostics here
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(fit)

par(mfrow=c(1,1)) # Change back to 1 x 1

# How does your model with the price outliers removed compare to a model with the outliers still in the data?
ggplot(zillowtrim, aes(x=taxvaluedollarcnt, y=price)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")

fitoutliers <- lm(zillowtrim$price ~ zillowtrim$taxvaluedollarcnt)

summary(fitoutliers)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(fitoutliers)


# Comment on the model you created. # Should you keep the 'outliers'?

#Based on the fact that the outlier-inclusive dataset has a higher R-squared score (~0.9)
#than the outlier-exclusive dataset (~0.85) and the plot for the outlier-inclusive dataset
#doesn't seem to be terribly different from the outlier-exclusive one I'd say yes, the outliers should be kept.



# Which model do you think is more accurate? Is your model reliable? What does the R^2 and RSE 
# tell you about the accuracy of the model? Look at the diagnostics.  Are there any outliers?

#Based on what I'm seeing it seems as if the model for the dataset with the outliers is more accurate.
#Despite this, the model for the dataset without the outliers seems to be more reliable-I say this
#because the residuals vs fitted plot for the dataset without the outliers is much more along 0
#on the residuals axis, whereas the residuals vs fitted plot for the dataset with the outliers trends
#above 0 as the fitted values increase. Both R2 & RSE indicate higher accuracy for the dataset with
#the outliers' model based on the aforementioned R2 values for both-the model for the dataset with
#the outliers is basically saying the model accounts for ~90% of the dataset's variability.
#As for outliers within the diagnostics, the diagnostics for the dataset with the outliers also contains
#more outliers within the diagnostic plots compared to the diagnostic plots for the dataset without the outliers.



# Is there any evidence that the data is not linear or normally distributed? 

#Due to the fact that the tails seem to curve off in the QQPlots for both models, which tells us
#there are more extreme values than there would be within a normal distribution of data.