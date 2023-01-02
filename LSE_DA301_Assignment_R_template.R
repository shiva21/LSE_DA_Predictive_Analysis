## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Set Working directory to source file location, as it contains the Data files.
current_path = rstudioapi::getActiveDocumentContext()$path
dirname(current_path)
setwd(dirname(current_path))

# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
sales_df <- read.csv('turtle_sales.csv', header=TRUE)
# sales_df <- read.csv(file.choose(), header=TRUE)

# Print the data frame.
View(sales_df)

# First part of data frame.
head(sales_df)

# Last part of data frame.
tail(sales_df)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_df2 <- select(sales_df, -c(Ranking, Year, Genre, Publisher))

# View the data frame.
View(sales_df2)

# View the descriptive statistics.
glimpse(sales_df2)

summary(sales_df2)

sales_df3 <- mutate(sales_df2, Product = as.factor(Product), Platform= as.factor(Platform))

summary(sales_df3)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

qplot(NA_Sales, Platform, data=sales_df3)

qplot(EU_Sales, Platform, data=sales_df3)

qplot(Global_Sales, Platform, data=sales_df3)


## 2b) Histograms
# Create histograms.

qplot(Platform, data=sales_df3)


## 2c) Boxplots
# Create boxplots.
qplot(NA_Sales, Platform, data=sales_df3, geom='boxplot')

qplot(EU_Sales, Platform, data=sales_df3, geom='boxplot')

qplot(Global_Sales, Platform, data=sales_df3, geom='boxplot')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
#Some outliers were seen using scatter plot and box plots, 
#some of the products has higher mean some don't as there are less sales for them.



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# Import libraries 
# Create insightful `sum`maries of the data set.
library(skimr)
# Create insightful reports on the data set.
library(DataExplorer)

# View data frame created in Week 4.
View(sales_df3)

as_tibble(sales_df3)

# Determine the sum of missing values 
sum(is.na(sales_df3))

# DF with only Sales information.
sales_df4 <- sales_df3[c('NA_Sales', 'EU_Sales', 'Global_Sales')]

# Check output: Determine the min, max, and mean values.
min_sales <- sapply(sales_df4, min)
min_sales

max_sales <- sapply(sales_df4, max)
max_sales

mean_sales <- sapply(sales_df4, mean)
mean_sales


summary(select(sales_df3, -c(Product, Platform)))

# View the descriptive statistics.
glimpse(sales_df3)

summary(sales_df3)


# Create a data profile report.
DataExplorer::create_report(sales_df3)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

dim(sales_df3)

# No of Products using aggregate
agg_df <- aggregate(sales_df3$Product, by=list(sales_df3$Product), FUN=length)
agg_df

# No of Products using group by & summarise
sales_df3 %>% group_by(Product) %>% summarise(count = n())

# View the data frame.
View(sales_df3)
str(sales_df3)

# Explore the data frame.
summary(sales_df3)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
# NA_Sales
ggplot (data = sales_df3, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x = Platform, y = NA_Sales )) +
  
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             # Set the alpha transparency to 0.5.
             alpha = 0.5,  
             # Set the point size to 1.5.
             size = 1.5) +
  # Add argument/title (y).
  scale_y_continuous(breaks = seq(0, 350, 50),
                     "NA Sales in millions (£)") + 
  # Add theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  # Add labels for title
  labs(title = "Relationship between Platform and NA Sales") 

# EU_Sales
ggplot (data = sales_df3, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x = Platform, y = EU_Sales )) +
  
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             # Set the alpha transparency to 0.5.
             alpha = 0.5,  
             # Set the point size to 1.5.
             size = 1.5) +
  # Add argument/title (y).
  scale_y_continuous(breaks = seq(0, 350, 50),
                     "EU Sales in millions (£)") + 
  # Add theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  # Add labels for title
  labs(title = "Relationship between Platform and EU Sales")

# Global_Sales
ggplot (data = sales_df3, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x = Platform, y = Global_Sales )) +
  
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             # Set the alpha transparency to 0.5.
             alpha = 0.5,  
             # Set the point size to 1.5.
             size = 1.5) +
  # Add argument/title (y).
  scale_y_continuous(breaks = seq(0, 350, 50),
                     "Global Sales in millions (£)") + 
  # Add theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  # Add labels for title
  labs(title = "Relationship between Platform and Global Sales")

# Create histograms.

hist(sales_df3$NA_Sales)
hist(sales_df3$EU_Sales)
hist(sales_df3$Global_Sales)

# Create boxplots.
boxplot(sales_df3$NA_Sales)
boxplot(sales_df3$EU_Sales)
boxplot(sales_df3$Global_Sales)

###############################################################################


# 3. Determine the normality of the data set.
# import the moments package and library.
library(moments)

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

# NA_Sales
qqnorm(sales_df3$NA_Sales)
# Add a reference line:
qqline(sales_df3$NA_Sales, col='red')

# EU_Sales
qqnorm(sales_df3$EU_Sales)
# Add a reference line:
qqline(sales_df3$EU_Sales, col='red')

# Global_Sales
qqnorm(sales_df3$Global_Sales)
# Add a reference line:
qqline(sales_df3$Global_Sales, col='red')

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.

# NA_Sales
shapiro.test((sales_df3$NA_Sales))
# Output : W = 0.6293, p-value < 2.2e-16
# Our p-value is <0.05, and we can conclude that the sample data is not 
# normally distribution.

# EU_Sales
shapiro.test((sales_df3$EU_Sales))
# Output : W = 0.64687, p-value < 2.2e-16
# Our p-value is < 0.05, and we can conclude that the sample data is not 
# normally distribution.

# NA_Sales
shapiro.test((sales_df3$Global_Sales))
# Output : W = 0.6818, p-value < 2.2e-16
# Our p-value is <0.05, and we can conclude that the sample data is not 
# normally distribution.


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

# NA_Sales
skewness(sales_df3$NA_Sales)
# Our output suggests a positive skewness. 4.30921

kurtosis(sales_df3$NA_Sales)
# heavy tail distribution with value 31.36852

# EU_Sales
skewness(sales_df3$EU_Sales)
# Our output suggests a positive skewness. 4.818688

kurtosis(sales_df3$EU_Sales)
# heavy tail distribution with value 44.68924

# Global_Sales
skewness(sales_df3$Global_Sales)
# Our output suggests a positive skewness. 4.045582

kurtosis(sales_df3$Global_Sales)
# heavy tail distribution with value 32.63966


## 3d) Determine correlation
# Determine correlation.

# Correlation between NA Sales & EU Sales
cor(sales_df4$NA_Sales, sales_df4$EU_Sales)
# Output : 0.7055236
# correlation is positive between NA Sales & EU Sales

# Correlation between  EU  & Global Sales
cor(sales_df4$EU_Sales, sales_df4$Global_Sales)
# Output : 0.8775575
# correlation is positive between EU  & Global Sales

# Determine the correlation for the whole data frame.
round (cor(sales_df4),
       digits=2)
# correlation is closer to 1 which means is a strong positive correlation


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

library(dplyr)

glimpse(sales_df3)

colnames(sales_df3)

NA_Sales <- select(sales_df3, -c(EU_Sales, Global_Sales)) %>% 
            rename(Sales = NA_Sales)  %>%
            add_column(Region = "NA")

EU_Sales <- select(sales_df3, -c(NA_Sales, Global_Sales)) %>% 
  rename(Sales = EU_Sales)  %>%
  add_column(Region = "EU")

Global_Sales <- select(sales_df3, -c(EU_Sales, NA_Sales)) %>% 
  rename(Sales = Global_Sales)  %>%
  add_column(Region = "Global")

sales_df5 <- rbind(NA_Sales, EU_Sales, Global_Sales)

sales_df5$Region <- as.factor(sales_df5$Region)

glimpse(sales_df5)

total_sales_df <- sales_df5 %>% group_by(Platform, Region) %>% summarize(total = sum(Sales), .groups='drop')
total_sales_df

# Grouped Chart
ggplot(total_sales_df, aes(fill=Region, y=Platform, x=total)) + 
  geom_bar(position="dodge", stat="identity")

# Stacked Bar Chart
ggplot(total_sales_df, aes(fill=Region, y=Platform, x=total)) + 
  geom_bar(position="stack", stat="identity")

# ScatterPlot
ggplot(data = total_sales_df,
       mapping = aes(x = total, y = Platform, color = Region)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  
  # Add argument/title (x).
  scale_x_continuous(breaks = seq(0, 400, 25),
                     "Sales in millions (£)") + 
  
  # Add layer (colours).
  scale_color_manual(values = c('red', 'blue','green')) + 
  # Add theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  # Add labels for title
  labs(title = "Relationship between Platform vs Sales vs Region") 

  

# Box Plot
ggplot(total_sales_df, aes(x = total, y = Platform)) +
  # Specify the geom_boxplot function.
  geom_boxplot()  +
  labs(title = "Relationship between Platform vs Sales", y="Sales in millions (£)") 

###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
View(sales_df3)

dim(sales_df3)


# Determine a summary of the data frame.
summary(sales_df3)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
cor(sales_df3$NA_Sales, sales_df3$EU_Sales)
# Output: 0.7055236

cor(sales_df3$NA_Sales + sales_df3$EU_Sales, sales_df3$Global_Sales)
# Output:  0.983796

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

glimpse(sales_df3)

plot(sales_df3$NA_Sales, sales_df3$Global_Sales)

slm_na <- lm(sales_df3$NA_Sales ~ sales_df3$Global_Sales, data = sales_df3)

summary(slm_na)

plot(slm_na$residuals)
# Plots lie are random doesn't form a pattern

abline(coefficients(slm_na))
 
na_model_1 = mutate(sales_df3, logIndex = log(NA_Sales))
slm_na_1 <- lm(na_model_1$logIndex ~ na_model_1$Global_Sales, data = na_model_1)
summary(slm_na_1)

plot(na_model_1$Global_Sales, na_model_1$logIndex)
abline(coefficients(slm_na_1))

predict_df_slm = data.frame(NA_Sales=c(34.02, 3.93, 2.73, 2.26, 22.08))
                        
predict_df_slm$Global_Sales <- predict(slm_na_1, new_data=predict_df_slm)

predict_df_slm
predict_df_slm <- mutate(predict_df_slm, Global_Sales=exp(Global_Sales))
###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
glimpse(sales_df4)

install.packages('psych')
library(psych)

corPlot(sales_df4)

# Multiple linear regression model.

mlrm = lm(Global_Sales~NA_Sales+EU_Sales, data = sales_df4)
summary(mlrm)
# both variables are significant explanatory, looks like multicollinearity

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
predict_df = data.frame(NA_Sales=c(34.02, 3.93, 2.73, 2.26, 22.08), 
                        EU_Sales= c(23.80, 1.56, 0.65, 0.97, 0.52))

glimpse(predict_df)

predict_global_sales = predict(mlrm, newdata=predict_df, interval='confidence')
predict_global_sales
# Actual Global Sales
# 67.85, 6.04, 4,32, 3.53, 23.21
# Predicted  Global Sales
# 71.46, 6.85, 4.24, 4.13, 26.43

###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################




