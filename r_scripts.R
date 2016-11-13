# TLST Evaluation Project
##########################

# Note: Before running the following r script, please install
# the following R packages
# forecast: R's forecasting functions for time series and linear models
#install.packages("forecast")
#install.packages("ggplot2")

# load necessary libraries
library(zoo)
library(ggplot2)
library(forecast)

# R scripts 
# Set the working directory 
# Note: change this to project's home directory
setwd("/media/d2/r-proj/tlst_proj")

# Load jobs by month data from file
jobs_by_month <- read.csv("./data/jobs_by_month.csv", header=T, sep=",")
# Add Month-Year column and change its type to yearmon
jobs_by_month$month_year <- format(seq(as.Date("2013/1/1"), by = "month", length.out = 36), "%b-%Y")
jobs_by_month$month_year <- as.yearmon(jobs_by_month$month_year, "%b-%Y")


# Create a barplot showing jobs by montth
barplot(c(jobs_by_month$jobs), names.arg=c(jobs_by_month$month_year),
        main = "Jobs 2013 - 2015", xlab = "Month - Year", ylab = "Jobs Count")

# Show the histogram of frequency of job count
hist(jobs_by_month$jobs, xlab="Job Count", main = "Histogram of jobs by month")

# Scatterplot of the relationship between jobs by month, year and job count
plot(jobs_by_month$month_year, jobs_by_month$jobs, 
     main = "Relationship between Jobs by month and Job count",
     xlab = "Jobs by Month - Year", ylab = 'Job count')

# Time series analysis
job_by_month_ts <- ts(jobs_by_month$jobs, start=c(2013, 1), frequency=12)

# plot the time series
plot(job_by_month_ts, xlab="Year", ylab="Jobs", type="o", pch=19,
     main="Jobs by month and year")

# plot corellation matrix
pairs(jobs_by_month)

# Prepare and plot simple moving averages
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(job_by_month_ts), max(job_by_month_ts))
plot(job_by_month_ts, main="Raw time series")
plot(ma(job_by_month_ts, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(job_by_month_ts, 5), main="Simple Moving Averages (k=5)", ylim=ylim)
plot(ma(job_by_month_ts, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
par(opar)


# Linear regression analysis
# Note: Dataset for this example is the 2011 US Census  PUMS (Public Use Microdata Sample) data.
# The dataset is the sample of US housesolds and records variables such as age (AGEP),
# sex (SEX), class of worker (COW) and level of education (SCHL). The output variable is
# personal income (PINCP)
# Dataset URL: https://github.com/WinVector/zmPDSwR/blob/master/PUMS/psub.RData
# This following regression analysis example is adapted from book "Practical Datascience with R"
# by Nina Zumel and John Mount, Manning Publications Co, 2014 

load("./data/psub.RData")

# Split job resources into training and test into roughly equal sizes
psub$is_training = rbinom(dim(psub)[1], size=1, prob = 0.5)
psub_train_ds <- subset(psub, psub$is_training == 1)
psub_test_ds <- subset(psub, psub$is_training == 0)

# Perform the analysis
psub_model <- lm(log(PINCP,base=10) ~ AGEP + SEX + COW + SCHL, data=psub_train_ds)

# Make predictions
psub_train_ds$predLogPINCP <- predict(psub_model, newdata=psub_train_ds)
psub_test_ds$predLogPINCP <- predict(psub_model, newdata=psub_test_ds)

# Plot the log income as a function of the predicted log income
ggplot(data=psub_test_ds,aes(x=log(PINCP,base=10), y=predLogPINCP)) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(aes(x=predLogPINCP,
                  y=log(PINCP,base=10)),color="black") +
  geom_line(aes(x=log(PINCP,base=10),
                y=log(PINCP,base=10)),color="blue",linetype=2) +
  scale_x_continuous(limits=c(4,5)) +
  scale_y_continuous(limits=c(3.5,5.5))


# Plot the residual income as a function of the predicted log income
ggplot(data=psub_test_ds,aes(x=predLogPINCP,
                      y=predLogPINCP-log(PINCP,base=10))) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(aes(x=predLogPINCP,
                  y=predLogPINCP-log(PINCP,base=10)),
              color="black")

# Read model summary
summary(psub_model)
