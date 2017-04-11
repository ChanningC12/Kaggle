library(dplyr)
library(ggplot2)

# Read in data
amazon = read.csv("../../../sample_data.csv")
# Check the dimension of the data
dim(amazon)
# Check the structure of the data
str(amazon)
# Check the missing values
colSums(is.na(amazon))

# Model 1 and Model 2, 20,000 record respectively
table(amazon$model_name)

# Convert the week to date format for further analysis
amazon$target_week = as.Date(amazon$target_week, format = "%m/%d/%Y")
amazon$forecast_creation_date = as.Date(amazon$forecast_creation_date, format = "%m/%d/%Y")

# Add indicators to indicate whether the demand is within the forecast - Overall
amazon$p50_acc = ifelse(amazon$p50_fcst<amazon$demand,1,0)
tapply(amazon$p50_acc,amazon$model_name,mean)

amazon$p60_acc = ifelse(amazon$p60_fcst<amazon$demand,1,0)
tapply(amazon$p60_acc,amazon$model_name,mean)

amazon$p80_acc = ifelse(amazon$p80_fcst<amazon$demand,1,0)
tapply(amazon$p80_acc,amazon$model_name,mean)

amazon$p90_acc = ifelse(amazon$p90_fcst<amazon$demand,1,0)
tapply(amazon$p90_acc,amazon$model_name,mean)

# Indicators for total # missing
amazon$totl_miss = amazon$p50_acc+amazon$p60_acc+amazon$p80_acc+amazon$p90_acc
tapply(amazon$totl_miss,amazon$model_name,mean)

# summary by week and model
amazon_summary <- amazon %>%
  group_by(target_week,model_name)%>%
  dplyr::summarise(
    p50 = mean(p50_acc),
    p60 = mean(p60_acc),
    p80 = mean(p80_acc),
    p90 = mean(p90_acc))

amazon_summary = as.data.frame(amazon_summary)

# plot the trend by model
p <- ggplot(amazon_summary, aes(x=target_week, y=p50, group=model_name))
p + geom_line(aes(colour = model_name))

p <- ggplot(amazon_summary, aes(x=target_week, y=p60, group=model_name))
p + geom_line(aes(colour = model_name))

p <- ggplot(amazon_summary, aes(x=target_week, y=p80, group=model_name))
p + geom_line(aes(colour = model_name))

p <- ggplot(amazon_summary, aes(x=target_week, y=p90, group=model_name))
p + geom_line(aes(colour = model_name))




