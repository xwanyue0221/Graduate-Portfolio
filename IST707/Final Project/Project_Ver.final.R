
# IST 707 Final Project
# Team 6: Wanyue Xiao, Yingxue Gao, Yiyuan Cheng

# ----------------------------------- Import all the needed packages ------------------------------
# install.packages("arules")
library(arules)
# install.packages("arulesViz")
library(arulesViz)
# install.packages("caret")
library(caret)
# install.packages("caTools")
library(caTools)
# install.packages("class")
library(class)
# install.packages("corrplot")
library(corrplot)
# install.packages("dplyr")
library(dplyr)
# install.packages("dummies")
library(dummies)
# install.packages("e1071")
library(e1071)
# install.packages("factoextra")
library(factoextra)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("gplots")
library(gplots)
# install.packages("ggpubr")
library(ggpubr)
# install.packages("ggthemes")
library(ggthemes)
# install.packages("arules")
library(knitr)
# install.packages("klaR")
library(klaR)
# install.packages("lubridate")
library(lubridate)
# install.packages("naivebayes")
library(naivebayes)
# install.packages("naniar")
library(naniar)
# install.packages("pROC")
library(pROC)
# install.packages("plyr")
library(plyr)
# install.packages("rpart")
library(rpart)
library(rpart.plot)
# install.packages("RANN")
library(RANN)
# install.packages("RColorBrewer")
library(RColorBrewer)
# install.packages("tidyr")
library(tidyr)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("viridis")
library(viridis)
# install.packages("xgboost")
library(xgboost)
# install.packages("zoo")
library(zoo)


################################################# Section 1: Data preparation #########################################

# ------------------------ 1.1 Import dataset ----------------------
df <- read.csv("usa.csv", header = T, stringsAsFactors = F)
str(df)
df <- df[,-45] # Remove turning_loop, which only has 1 value
df <- df[,-c(1,9,10,13,25,31)] # Remove columns: ID, End_Lat, End_Lng, Number, Wind_Chill.F., Precipitation.in.
colnames(df)[c(2,21)] <- c("TMC_Code", "Humidity.%.") # Change the column names


# ------------------------ 1.2 Missing data ---------------------
# ---------- 1.2.1 Visualize missing data ----------
vis_miss(df, warn_large_data = FALSE)
gg_miss_var(df)

# ---------- 1.2.2 Manage missing Data ----------
# Empty categorical data
categorical_columns <- colnames(df[sapply(df, is.factor),])
df <- df[-which(df$City == ""), ]
df <- df[-which(df$Zipcode == ""), ]
df <- df[-which(df$Timezone == ""), ]
df <- df[-which(df$Airport_Code == ""), ]
df <- df[-which(df$Weather_Timestamp == ""), ]
df <- df[-which(df$Wind_Direction == ""), ]
df <- df[-which(df$Weather_Condition == ""), ]

# for (i in categorical_columns){
#   print(unique(df[i]))
# }

# Missing numeric value
preProcValues <- caret::preProcess(df, method = c("knnImpute", "center", "scale"))
df_manipulated <- predict(preProcValues, df)

procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)
for(i in procNames$col){
  df_manipulated[i] <- df_manipulated[i]*preProcValues$std[i]+preProcValues$mean[i]
}

sapply(df_manipulated, function(x) sum(is.na(x)))
sapply(df_manipulated, function(x) sum(x == ""))
# write.csv(df_manipulated, "df_knn.csv")
# df_manipulated <- read.csv("df_knn.csv", header = T, stringsAsFactors = F, row.names = 1)

df_manipulated$Severity[which(df_manipulated$Severity == 1)] <- "Low"
df_manipulated$Severity[which(df_manipulated$Severity == 2)] <- "Low"
df_manipulated$Severity[which(df_manipulated$Severity == 3)] <- "High"
df_manipulated$Severity[which(df_manipulated$Severity == 4)] <- "High"

table(df_manipulated$Severity)

  # Check the missing data again:
data.frame(colnames(df_manipulated),colSums(is.na(df_manipulated)))
str(df_manipulated)


# -------------------------- 1.3 Create Prediction data ----------------------
set.seed(66)
# sample_index <- sample(1:nrow(df_manipulated), size = 10000, replace = F)
# predicting <- df_manipulated[sample_index,]
# preprocess <- df_manipulated[-sample_index,]
# 
# write.csv(predicting_data, "predicting.csv")


# -------------------------- 1.4 Dealing with duplicates ------------------------
preprocess <- df_manipulated
ifelse(nrow(preprocess) == nrow(preprocess[!duplicated(preprocess),]), 'The dataset does not have duplicate', 'The dataset has duplicate')
preprocess <- unique(preprocess)


# -------------------------- 1.5 Checking for outliers -------------------------
preprocess %>% keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(y = value, fill = "orange")) +
  facet_wrap(~key, scales = "free") +
  geom_boxplot() +
  labs(title = "Boxplots of Numeric Columns") +
  theme_minimal()

preprocess %>% keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(x = value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(bins = 15, fill = "lightblue", color = "white") +
  labs(title = "Show Distribution of Each Numeric Column")


# --------------------------- 1.6 Dealing with outliers -------------------------
# distnace, pressure, Temperature, TMC_CODE, Visibility, Wind_Speed

# Traffic Message Channel (TMC) is a technology for delivering traffic and travel information to motor vehicle drivers. 
# It is digitally coded using the ALERT C or TPEG protocol into RDS Type 8A groups carried via conventional FM radio broadcasts.
summary(preprocess$TMC_Code)

# Distance: The length of the road extent affected by the accident.
summary(preprocess$Distance.mi.)
quantile(preprocess$Distance.mi., 0.9975)
preprocess$Distance.mi.[preprocess$Distance.mi. > quantile(preprocess$Distance.mi., 0.9975)] <- round(mean(preprocess$Distance.mi., na.rm = T),3)

# pressure
summary(preprocess$Pressure.in.)
preprocess$Pressure.in.[preprocess$Pressure.in. <= quantile(preprocess$Pressure.in., 0.0025)] <- round(median(preprocess$Pressure.in., na.rm = T),3)
preprocess$Pressure.in.[preprocess$Pressure.in. >= quantile(preprocess$Pressure.in., 0.9975)] <- round(median(preprocess$Pressure.in., na.rm = T),3)

# temperature
summary(preprocess$Temperature.F.)
quantile(preprocess$Temperature.F., 0.9975)
quantile(preprocess$Temperature.F., 0.0025)
preprocess$Pressure.in.[preprocess$Pressure.in. <= quantile(preprocess$Pressure.in., 0.0025)] <- round(median(preprocess$Pressure.in., na.rm = T),3)
preprocess$Pressure.in.[preprocess$Pressure.in. >= quantile(preprocess$Pressure.in., 0.9975)] <- round(median(preprocess$Pressure.in., na.rm = T),3)

# Visibility 
summary(preprocess$Visibility.mi.) # not going to handle since those outliers are reasonable to exist
quantile(preprocess$Visibility.mi., 0.9975)
quantile(preprocess$Visibility.mi., 0.0025)

# Wind_Speed: normally wind speed ranges from 1 (light air) - over 73 (hurrican). 47 is "Fresh gale" which means Twigs broken off trees.
summary(preprocess$Wind_Speed.mph.)
quantile(preprocess$Wind_Speed.mph., 0.9975)
quantile(preprocess$Wind_Speed.mph., 0.0025)


# ----------------------------- 1.7 Convert the data type -------------------------
str(preprocess)
preprocess$Severity <- as.factor(preprocess$Severity)
# change date type
preprocess$Start_Time <- as.POSIXct(preprocess$Start_Time, origin="1970-01-01")
preprocess$End_Time <- as.POSIXct(preprocess$End_Time, origin="1970-01-01")
preprocess$Weather_Timestamp <- as.POSIXct(preprocess$Weather_Timestamp, origin="1970-01-01")

preprocess_same <- preprocess %>% filter(strftime(preprocess$Start_Time, "%Y-%m-%d") == strftime(preprocess$End_Time, "%Y-%m-%d"))
preprocess_same$Start <- strftime(preprocess_same$Start_Time, "%H:%M")
preprocess_same$End <- strftime(preprocess_same$End_Time, "%H:%M")
preprocess_same$Start <- hm(preprocess_same$Start)
preprocess_same$End <- hm(preprocess_same$End)
elapsed.time <- preprocess_same$End - preprocess_same$Start
preprocess_same$Duration <- as.numeric(as.duration(elapsed.time), "minutes")
preprocess_same <- preprocess_same[,-c(43,44)]

preprocess_notsame <- preprocess %>% filter(strftime(preprocess$Start_Time, "%Y-%m-%d") != strftime(preprocess$End_Time, "%Y-%m-%d"))
preprocess_notsame$Start <- strftime(preprocess_notsame$Start_Time, "%H:%M")
preprocess_notsame$End <- strftime(preprocess_notsame$End_Time, "%H:%M")
preprocess_notsame$Start <- hm(preprocess_notsame$Start)
preprocess_notsame$End <- hm(preprocess_notsame$End)
preprocess_notsame$today <- hm(c("24:00"))
preprocess_notsame$nextday <- hm(c("00:00"))
elapsed.time <- preprocess_notsame$today - preprocess_notsame$Start
preprocess_notsame$Duration1 <- as.numeric(as.duration(elapsed.time), "minutes")
elapsed.time <- preprocess_notsame$End - preprocess_notsame$nextday
preprocess_notsame$Duration2 <- as.numeric(as.duration(elapsed.time), "minutes")
preprocess_notsame$Duration <- preprocess_notsame$Duration1 + preprocess_notsame$Duration2
preprocess_notsame <- preprocess_notsame[,-c(43:48)]

preprocess <- as.data.frame(bind_rows(as.data.frame(preprocess_same), as.data.frame(preprocess_notsame)))
preprocess$Start_Hour <- strftime(preprocess$Start_Time, "%H")
preprocess$End_Hour <- strftime(preprocess$End_Time, "%H")
preprocess$Start_Date <- strftime(preprocess$Start_Time, "%Y-%m-%d")
preprocess$End_Date <- strftime(preprocess$End_Time, "%Y-%m-%d")
preprocess <- preprocess[,-c(4,5,19)] # remove the Start_Time, End_Time, and Weather_Stampe
# write.csv(preprocess, "df_knn_have_description.csv")

# preprocess <- read.csv("df_knn_have_description.csv", stringsAsFactors = F, row.names = 1)

table(preprocess$Source)
table(preprocess$Severity)
table(preprocess$Side)
table(preprocess$City) # has too many factors
table(preprocess$County) # has two many factors
table(preprocess$State)
table(preprocess$Zipcode) # has two many factors
table(preprocess$Country) # has only one factor - remove this later
table(preprocess$Timezone)
table(preprocess$Airport_Code) # has two many factors
table(preprocess$Weather_Condition)

# Combine weather condition category
# Clear: Clear
# Fair: Fair | Fair / Windy | Scattered Clouds
# Heavy Rain: Heavy Rain | Heavy Rain / Windy | Heavy Rain Showers 
# Rain: Drizzle | Heavy Drizzle | Light Drizzle | Light Freezing Drizzle | Light Freezing Rain | Light Rain |
#       Light Rain / Windy | Light Rain Shower | Light Rain Showers | Light Rain with Thunder | Light Sleet |
#       N/A Precipitation | Rain | Rain / Windy | Showers in the Vicinity
# Snow / Ice: Heavy Snow | Ice Pellets | Light Ice Pellets | Light Snow | Light Snow / Windy | Light Snow with Thunder |
#             Snow | Snow / Windy | Snow Showers
# Overcast: Funnel Cloud | Overcast | Squalls
# Cloudy: Cloudy | Cloudy / Windy |  Mostly Cloudy | Mostly Cloudy / Windy | Partly Cloudy | Partly Cloudy / Windy 
#         
# Fog / Dust: Fog | Fog / Windy | Haze | Haze / Windy | Light Freezing Fog | Blowing Dust / Windy | Mist |
#             Patches of Fog | Sand | Sand / Dust Whirlwinds | Shallow Fog | Smoke | Smoke / Windy | Volcanic Ash |
#             Widespread Dust | Wintry Mix | Wintry Mix / Windy
# Thunderstorm: Heavy T-Storm |  Heavy T-Storm / Windy | Heavy Thunderstorms and Rain | Light Thunderstorm | 
#                Light Thunderstorms and Rain | T-Storm | T-Storm / Windy | Thunder | Thunder / Windy | 
#                Thunder in the Vicinity | Thunderstorm | Thunderstorms and Rain 

preprocess$Weather_Condition[grep("Fair", as.character(preprocess$Weather_Condition))] <- "Fair"
preprocess$Weather_Condition[grep("Scattered Clouds", as.character(preprocess$Weather_Condition))] <- "Fair"
preprocess$Weather_Condition[grep("Heavy Rain", as.character(preprocess$Weather_Condition))] <- "Heavy Rain"
preprocess$Weather_Condition[grep("Drizzle", as.character(preprocess$Weather_Condition))] <- "Rain"
preprocess$Weather_Condition[grep("Light Rain", as.character(preprocess$Weather_Condition))] <- "Rain"
preprocess$Weather_Condition[grep("Rain / Windy", as.character(preprocess$Weather_Condition))] <- "Rain"
preprocess$Weather_Condition[grep("Precipitation", as.character(preprocess$Weather_Condition))] <- "Rain"
preprocess$Weather_Condition[grep("Sleet", as.character(preprocess$Weather_Condition))] <- "Rain"
preprocess$Weather_Condition[grep("Showers in the Vicinity", as.character(preprocess$Weather_Condition))] <- "Rain"
preprocess$Weather_Condition[grep("Light Freezing Rain", as.character(preprocess$Weather_Condition))] <- "Rain"

preprocess$Weather_Condition[grep("Snow", as.character(preprocess$Weather_Condition))] <- "Snow.Ice"
preprocess$Weather_Condition[grep("Ice", as.character(preprocess$Weather_Condition))] <- "Snow.Ice"

preprocess$Weather_Condition[grep("Funnel", as.character(preprocess$Weather_Condition))] <- "Overcast"
preprocess$Weather_Condition[grep("Squalls", as.character(preprocess$Weather_Condition))] <- "Overcast"
preprocess$Weather_Condition[grep("Cloudy", as.character(preprocess$Weather_Condition))] <- "Cloudy"
preprocess$Weather_Condition[grep("Fog", as.character(preprocess$Weather_Condition))] <- "Fog.Dust"
preprocess$Weather_Condition[grep("Haze", as.character(preprocess$Weather_Condition))] <- "Fog.Dust"
preprocess$Weather_Condition[grep("Dust", as.character(preprocess$Weather_Condition))] <- "Fog.Dust"
preprocess$Weather_Condition[grep("Mist", as.character(preprocess$Weather_Condition))] <- "Fog.Dust"
preprocess$Weather_Condition[grep("Sand", as.character(preprocess$Weather_Condition))] <- "Fog.Dust"
preprocess$Weather_Condition[grep("Smoke", as.character(preprocess$Weather_Condition))] <- "Fog.Dust"
preprocess$Weather_Condition[grep("Ash", as.character(preprocess$Weather_Condition))] <- "Fog.Dust"
preprocess$Weather_Condition[grep("Mix", as.character(preprocess$Weather_Condition))] <- "Fog.Dust"
preprocess$Weather_Condition[grep("Thunder", as.character(preprocess$Weather_Condition))] <- "Thunderstorm"
preprocess$Weather_Condition[grep("T-Storm", as.character(preprocess$Weather_Condition))] <- "Thunderstorm"

# rename columns
colnames(preprocess)[c(6, 17:20, 22:23)] <- c("Distance.mile", "Temperature.F", "Humidity", "Pressure.in", "Visibility.mile", 
                                              "WindSpeed.mph", "Weather")

# write.csv(preprocess, "df_train.csv")
# preprocess <- read.csv("df_train.csv", header = T, stringsAsFactors = F, row.names = 1)



df_high <- preprocess[preprocess$Severity == 'High',]
df_low <- preprocess[preprocess$Severity == 'Low',]
set.seed(66)

sample_index <- sample(1:nrow(df_high), size = 10000, replace = F)
df_high_10000 <- df_high[sample_index,]
df_high <- df_high[-sample_index,]
sample_index <- sample(1:nrow(df_high), size = 2500, replace = F)
df_high_2500 <- df_high[sample_index,]
df_high <- df_high[-sample_index,]

sample_index <- sample(1:nrow(df_low), size = 10000, replace = F)
df_low_10000 <- df_low[sample_index,]
df_low <- df_low[-sample_index,]
sample_index <- sample(1:nrow(df_low), size = 2500, replace = F)
df_low_2500 <- df_low[sample_index,]
df_low <- df_low[-sample_index,]

df_train <- rbind(df_high_10000, df_low_10000)
df_test <- rbind(df_high_2500, df_low_2500)

# remove TMC, address information, wind direction, extremely biased columns
df_train <- df_train[, -c(4, 5, 7:16, 21, 24:25, 27, 29:31, 33:34, 43:44)]
df_test <- df_test[, -c(4, 5, 7:16, 21, 24:25, 27, 29:31, 33:34, 43:44)]

df_train$TMC_Code <- round(df_train$TMC_Code, 0)
df_test$TMC_Code <- round(df_test$TMC_Code, 0)

# write.csv(df_train, "df_train.csv")
# write.csv(df_test, "df_test.csv")
df_train <- read.csv("df_train.csv", header = T, stringsAsFactors = F, row.names = 1)
df_test <- read.csv("df_test.csv", header = T, stringsAsFactors = F, row.names = 1)

# ---------------- 1.7.1 Categorical dataset -----------------
# using the original dataset
df_cat <- df_train
df_cat <- df_cat[, -c(4, 5, 7:16, 21, 24:25, 27, 29:31, 33:34, 43:44)]
df_cat$TMC_Code <- round(df_cat$TMC_Code, 0)

str(df_cat)

# Categotization
summary(df_cat$Distance.mile>1)
df_cat$TMC_Code <- as.factor(df_cat$TMC_Code)
df_cat$Distance.mile <- cut(df_cat$Distance.mile, breaks=c(-Inf, 0, 1, Inf), labels=c("No extenstion", "<1", ">1"))
df_cat$Temperaturhe.F <- cut(df_cat$Temperature.F, breaks=c(-Inf, 50, 65, 75, Inf), labels=c("<50", "50-58", "68-75", ">75"))
df_cat$Humidity <- cut(df_cat$Humidity, breaks=c(-Inf, 50, 68, 85, Inf), labels=c("<50", "50-68", "68-85", ">85"))
df_cat$Pressure.in <- cut(df_cat$Pressure.in, breaks=c(-Inf, 29.5, 30.2, Inf), labels=c("25-29.5", "29.5-30.2", ">30.2"))
# Generally, low pressure (<29.2) - stormy, high pressure (>30.2) - sunny
df_cat$Visibility.mile <- cut(df_cat$Visibility.mile, breaks=c(-Inf, 5, 9.99, Inf), labels=c("<5", "5-10", ">=10"))
# 10-mile-visibility means that a person should be able "to see and identify" in the daytime" (Wikipedia)
df_cat$WindSpeed.mph <- cut(df_cat$WindSpeed.mph, breaks=c(-Inf, 7, 18, Inf), labels=c("0-2", "3-4", ">5"))
# According to the Beaufort Force level
df_cat$Duration <- cut(df_cat$Duration, breaks=c(-Inf, 30, 45, 75, Inf), labels=c("<30", "30-45", "45-75", ">75"))
table(df_cat$Start_Hour)
df_cat$Start_Hour <- cut(as.numeric(df_cat$Start_Hour), breaks=c(-Inf, 4, 11, 15, 18, 21, Inf), 
                         labels=c("Night", "Morning", "Noon", "Afternoon", "Evening", "Night"))
df_cat$End_Hour <- cut(as.numeric(df_cat$End_Hour), breaks=c(-Inf, 4, 11, 15, 18, 21, Inf), 
                       labels=c("Night", "Morning", "Noon", "Afternoon", "Evening", "Night"))

df_cat <- lapply(df_cat, as.factor)
df_cat <- as.data.frame(df_cat)
str(df_cat) 

# ------------------ 1.7.2 Numeric dataset -------------------
df_num <- df_train
df_num_test <- df_test

# Convert binary vairables
df_num[,c(11:14)] <- ifelse(df_num[,c(11:14)] == "True", 1, 0)   # True/False
df_num[,c(15:18)] <- ifelse(df_num[,c(15:18)] == "Day", 1, 0)    # Day/Night

df_num_test[,c(11:14)] <- ifelse(df_num_test[,c(11:14)] == "True", 1, 0)   # True/False
df_num_test[,c(15:18)] <- ifelse(df_num_test[,c(15:18)] == "Day", 1, 0)    # Day/Night

str(df_num)

# Convert categorical vairables
df_num <- dummy.data.frame(df_num, names = c("Source", "Weather"), sep = ".", all = T)
df_num <- df_num[, -c(3, 12)]   # drop Source.MapQuest-Bing, Weather.Clear

df_num_test <- dummy.data.frame(df_num_test, names = c("Source", "Weather"), sep = ".", all = T)
df_num_test <- df_num_test[, -c(3, 12)]   # drop Source.MapQuest-Bing, Weather.Clear

df_num[,c(1:3, 5:29)] <- lapply(df_num[,c(1:3, 5:29)], as.numeric)
colnames(df_num) <- c("SourceBing", "SourceMapQuest", "TMC", "Severity", "Distance", "Temperature", "Humidity", "Pressure", "Visibility", "WindSpeed",
                      "WeatherCloudy", "WeatherFair", "WeatherFogDust", "WeatherHeavyRain", "WeatherOvercast", "WeatherRain", "WeatherSnowIce",
                      "WeatherThunderstorm", "Crossing", "Junction", "Station", "TrafficSignal", "SunriseSunset", "CivilTwilight", "NauticalTwilight",
                      "AstronomicalTwilight", "Duration", "StartHour", "EndHour")

df_num_test[,c(1:3, 5:29)] <- lapply(df_num_test[,c(1:3, 5:29)], as.numeric)
colnames(df_num_test) <- c("SourceBing", "SourceMapQuest", "TMC", "Severity", "Distance", "Temperature", "Humidity", "Pressure", "Visibility", "WindSpeed",
                      "WeatherCloudy", "WeatherFair", "WeatherFogDust", "WeatherHeavyRain", "WeatherOvercast", "WeatherRain", "WeatherSnowIce",
                      "WeatherThunderstorm", "Crossing", "Junction", "Station", "TrafficSignal", "SunriseSunset", "CivilTwilight", "NauticalTwilight",
                      "AstronomicalTwilight", "Duration", "StartHour", "EndHour")
str(df_num_test)

# Data normalization and standardization
pre_process  <- preProcess(df_num, method = c("center", "scale"))
df_scale <- predict(pre_process , newdata = df_num)

pre_process  <- preProcess(df_num_test, method = c("center", "scale"))
df_scale_test <- predict(pre_process , newdata = df_num_test)


# ------------------------- 1.8 Dataset Summary ------------------------
# df_cat for association rules mining
# df_num for decision tree model
# df_scale for support vector machine

# predicting data 


############################################## Section 2: Data visualization ##########################################

colors <- c("#D3DDC9","#D15F32","#F7E174")

# ------------------------- 2.1 Weather Bar Plot -------------------------

weather.agg <- preprocess %>% group_by(Weather, Severity) %>% dplyr::summarize(count = n())
colnames(weather.agg)[3] <- "Counts"
ggplot(weather.agg, aes(x=Weather, y=Counts, fill = Severity)) + 
  geom_bar(position = 'dodge', stat='identity') +
  scale_fill_manual(values=alpha(c("#00AFBB", "#E7B800"), 0.5)) +
  geom_text(aes(label=Counts), position=position_dodge(width=0.9), vjust=-0.35) +
  xlab("Weather Condition") +
  ylab("Number of Accidents") + 
  ylim(0, 10000) +
  theme_minimal() +
  ggtitle("Stacked Bar Plot")


# -------------------------- 2.2 State Plot -------------------------------

# state.bar <- preprocess %>% group_by(State) %>% dplyr::summarize(Counts = n()) %>% filter(Counts >= mean(Counts))
# ggplot(state.bar, aes(x=State, y=Counts, fill = Severity)) + 
#   geom_bar(position = 'dodge', stat='identity') +
#   scale_fill_manual(values=colors[2:3]) +
#   geom_text(aes(label=Counts), position=position_dodge(width=0.9), vjust=-0.25)

# Scatter Plot
state.agg <- preprocess %>% dplyr::select(State, Severity, Start_Date)
state.agg$Start_Date <- as.Date(as.yearmon(state.agg$Start_Date))
state.agg <- state.agg %>% group_by(State, Severity, Start_Date) %>% dplyr::summarize(Counts = n())
state.agg <- state.agg[order(state.agg$Counts, decreasing = TRUE),]
get <- state.agg %>% group_by(State, Severity, Start_Date) %>% dplyr::summarize(max = max(Counts)) %>% arrange(desc(max))

state.agg <- as.data.frame(state.agg)[c(1:100),]
state.agg <- state.agg %>% 
  mutate(annotation = case_when(
    State == "CA" & Severity == "Low" & Start_Date == '2019-12-01' ~ "yes",
    State == "CA" & Severity == "Low" & Start_Date == '2019-10-01' ~ "yes",
    State == "CA" & Severity == "Low" & Start_Date == '2019-09-01' ~ "yes",
    State == "CA" & Severity == "Low" & Start_Date == '2019-11-01' ~ "yes",
    State == "CA" & Severity == "Low" & Start_Date == '2016-10-01' ~ "yes"))
state.agg$annotation[is.na(state.agg$annotation)] <- 'no'

colors_top <- c("#fad4d4", "#f5b5b5", "#fc9d9d","#ea728c", "#bd4482", "#963366", "#5e2040","#fad4d4", "#f5b5b5", "#fc9d9d","#ea728c", "#bd4482", "#963366")
state.agg$Rank <- c(1:dim(state.agg)[1])
state.agg %>%
  arrange(desc(Counts)) %>%
  ggplot(aes(x = Rank, y=Start_Date, size = Counts, fill = State)) +
  geom_point(alpha=0.6, shape=21, color = "lightgrey") +
  scale_size(range = c(5, 21), name="Counts of Accident") +
  theme(legend.position="bottom") +
  labs(x = "Rank", y = "Year", title = "Counts of Accident") +
  # scale_fill_manual(values=colors_top) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title=element_text(face = "bold", size = 10),
        plot.title = element_text(hjust = 1)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  ggrepel::geom_text_repel(data=state.agg %>% filter(annotation=="yes"), aes(label=State, color = State), size=3,
                           box.padding = unit(1, "lines"),
                           point.padding = unit(.5, "lines"))

# Map Plot By State
state.agg <- preprocess %>% group_by(State, Severity) %>% dplyr::summarize(count = n())
colnames(preprocess)
states <- map_data("state") %>% as_tibble() %>% dplyr::select(long, lat, group, region)
states_abb <- preprocess %>%
  mutate(State = tolower(State)) %>%
  dplyr::select(State, Zipcode) %>%
  dplyr::rename("State_full" = State)
address <- c("Country", "City", "County", "Street", "Zipcode")
df_TMC %>% dplyr::select(all_of(address)) %>% head(5)


# df_TMC?????????



df_add <- df_TMC %>% select(-all_of(address))
df_add <- df_add %>% 
  mutate(TMC = as.character(TMC), Severity = as.character(Severity)) %>% 
  mutate_if(is.logical, as.character)
accident_count <- preprocess %>%
  count(State) %>%
  left_join(states_abb, by = c("State" = "Code"))

states <- preprocess %>% left_join(accident_count, by = c("region" = "State_full"))
# top 10 states
top_10 <- accident_count %>%
  arrange(desc(n)) %>%
  head(10)
top_10 <- top_10$State %>% unlist()

top_10_map <- states %>%
  filter(State %in% top_10)
top_10_label <- top_10_map %>%
  group_by(region, State) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(states, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = n), color = "#636363", size = 0.1) +
  geom_polygon(data = top_10_map, color = "red", fill = NA, size = 0.8) +
  scale_fill_gradient(low = "#fee5d9", high = "#de2d26",
                      name = "Accident Count", labels = unit_format(unit = "K", scale = 1e-03)) +
  ggrepel::geom_label_repel(mapping = aes(label = State, group = 1), data = top_10_label) +
  theme_minimal() +
  coord_quickmap() +
  labs(title = "Accident distribution in the U.S.",
       x = "Longitude",
       y = "Latitude")


# ----------------------- 2.3 Time Series Plot ------------------------
# Time Series -> Month
time.series <- preprocess %>% dplyr::select(Severity, Start_Date)
time.series$Start_Date <- as.Date(as.yearmon(time.series$Start_Date))
time.series <- time.series %>% group_by(Severity, Start_Date) %>% dplyr::summarize(Counts = n())
time.series <- as.data.frame(time.series)
ggplot(time.series, aes(x = Start_Date, y = Counts)) + 
  geom_area(aes(color = Severity, fill = Severity), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal() +
  xlab("Date") +
  ylab("Number of Accidents") + 
  ylim(0, 1500) +
  ggtitle("Time Series Plot by Month")

# Time Series -> Hour
time.series <- preprocess %>% dplyr::select(Severity, Start_Hour)
time.series <- time.series %>% group_by(Severity, Start_Hour) %>% dplyr::summarize(Counts = n())
time.series <- as.data.frame(time.series)
ggplot(time.series, aes(x = Start_Hour, y = Counts, color = Severity, fill = Severity, group = 1)) +
  geom_point(size = 2) +
  geom_line(size = 1.1) +
  facet_grid(Severity ~ ., scales="free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(c(0,3800)) +
  geom_text(aes(label = Counts), vjust = -1.5) +
  theme_minimal() +
  xlab("Hour") +
  ylab("Number of Accidents") + 
  ggtitle("Time Series Plot by Hour")


# --------------------------- 2.4 Road Condition Plots -----------------------

colnames(preprocess)
binary.bump <- preprocess %>% dplyr::select(Bump, Severity)
binary.bump <- binary.bump %>% group_by(Severity, Bump) %>% dplyr::summarize(Counts = n())
ggplot(binary.bump, aes(x = Bump, y = Counts, fill = Severity, label = Counts)) +
  geom_bar(stat = "identity") +
  geom_text(size = 5, position = position_stack(vjust = 1)) +
  theme_minimal() 

binary.Junction <- preprocess %>% dplyr::select(Junction, Severity)
binary.Junction <- binary.Junction %>% group_by(Severity, Junction) %>% dplyr::summarize(Counts = n())
ggplot(binary.Junction, aes(x = Junction, y = Counts, fill = Severity, label = Counts)) +
  geom_bar(stat = "identity") +
  geom_text(size = 5, position = position_stack(vjust = 1)) +
  theme_minimal()  # keep this

binary.traffic_signal <- preprocess %>% dplyr::select(Traffic_Signal, Severity)
binary.traffic_signal <- binary.traffic_signal %>% group_by(Severity, Traffic_Signal) %>% dplyr::summarize(Counts = n())
ggplot(binary.traffic_signal, aes(x = Traffic_Signal, y = Counts, fill = Severity, label = Counts)) +
  geom_bar(stat = "identity") +
  geom_text(size = 5, position = position_stack(vjust = 0.8)) +
  theme_minimal()  # keep this

table(preprocess$Severity, preprocess$Amenity)
table(preprocess$Severity, preprocess$Crossing) # keep this
table(preprocess$Severity, preprocess$Give_Way)
table(preprocess$Severity, preprocess$No_Exit)
table(preprocess$Severity, preprocess$Railway)
table(preprocess$Severity, preprocess$Roundabout)
table(preprocess$Severity, preprocess$Station) # keep this
table(preprocess$Severity, preprocess$Stop)
table(preprocess$Severity, preprocess$Traffic_Calming)

barplot(table(preprocess$Severity, preprocess$Sunrise_Sunset), label = TRUE, legend = TRUE, main = "Sunrise_Sunset")
barplot(table(preprocess$Severity, preprocess$Civil_Twilight), label = TRUE, legend = TRUE, main = "Civil_Twilight")
barplot(table(preprocess$Severity, preprocess$Nautical_Twilight), label = TRUE, legend = TRUE, main = "Nautical_Twilight")
barplot(table(preprocess$Severity, preprocess$Astronomical_Twilight), label = TRUE, legend = TRUE, main = "Astronomical_Twilight")


# -------------------------- 2.5 Correlation & PCA ------------------------

# matrix of the p-value of the correlation
cor_matrix <- cor(cor_df[complete.cases(cor_df), sapply(cor_df, is.numeric)], method = "pearson")
corrplot(cor_matrix, type = "upper")



# col_df?????????



# pca
vis_pca <- function(preprocess){
  res.pca <- prcomp(preprocess, scale = TRUE)
  fviz_pca_var(res.pca,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )
}
vis_pca(cor_df)


########################## Section 3: Build, tune and evaluate various machine learning algorithms ####################

# -------------------------- 3.1 Association rule mining -----------------------
# Baseline model:
ruleset <- apriori(df_cat, parameter = list(support=0.1, confidence=0.8))
inspect(head(ruleset, 10))

# # Tune the model:
# inspect(head(apriori(df_cat, parameter=list(support=0.1, confidence=0.8), appearance = list(rhs = c("Severity=Low", "Severity=High"))),10))
# inspect(head(apriori(df_cat, parameter=list(support=0.1, confidence=0.8, minlen=3), appearance = list(rhs = c("Severity=Low", "Severity=High"))),10))
# inspect(head(apriori(df_cat, parameter=list(support=0.15, confidence=0.8, minlen=3), appearance = list(rhs = c("Severity=Low", "Severity=High"))),10))
# inspect(head(apriori(df_cat, parameter=list(support=0.15, confidence=0.3, minlen=3), appearance = list(rhs = c("Severity=Low", "Severity=High"))),10))
# inspect(head(apriori(df_cat, parameter=list(support=0.15, confidence=0.5, minlen=3), appearance = list(rhs = c("Severity=Low", "Severity=High"))),10))
# inspect(head(apriori(df_cat, parameter=list(support=0.15, confidence=0.7, minlen=3), appearance = list(rhs = c("Severity=Low", "Severity=High"))),10))
# inspect(head(apriori(df_cat, parameter=list(support=0.15, confidence=0.85, minlen=3), appearance = list(rhs = c("Severity=Low", "Severity=High"))),10))
# inspect(head(apriori(df_cat, parameter=list(support=0.13, confidence=0.8, minlen=3), appearance = list(rhs = c("Severity=Low", "Severity=High"))),10))
# 
# inspect(head(sort(apriori(df_cat, parameter=list(support=0.15, confidence=0.41, minlen=3), appearance = list(rhs = c("Severity=High"))), by = "lift",
#                   descreasing = T),10))

# Best model:
ruleset_best <- apriori(df_cat, parameter=list(support=0.13, confidence=0.8, minlen=3), appearance = list(rhs = c("Severity=Low", "Severity=High")))
inspect(head(sort(ruleset_best, by = "lift", descreasing = T),10))

plot(ruleset_best, measure = c("support", "lift"), shading = "confidence", jitter = 0, main = "ruleset_best (rhs: Severity=Low, Severity=High)")


# --------------------------- 3.2 Decision tree -------------------------
# Separate the training_num dataset into train_num and val_num:
sample_num <- sample.split(df_scale, SplitRatio = 0.7)

train_num <- subset(df_num, sample_num == TRUE)
val_num <- subset(df_num, sample_num == FALSE)

train_scale <- subset(df_scale, sample_num == TRUE)
val_scale <- subset(df_scale, sample_num == FALSE)

# # Baseline model:
# dt <- train(Severity ~ ., data = train_scale, metric = "Accuracy", method = "rpart")
# dt
# 
# # Tune the model:
# dt_tune <- train(Severity ~ ., data = train_scale, metric = "Accuracy", method = "rpart",
#                  tuneGrid = expand.grid(cp = seq(0, 0.1, 0.01)))
# plot(dt_tune, main="DT Model Tuning Result")
# 
# Best model:
# best cp = 0.01
dt_best <- train(Severity ~ ., 
                 data = df_scale, 
                 # metric = "Accuracy", 
                 method = "rpart",
                 minsplit = 30, 
                 maxdepth = 7, 
                 trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
                 tuneGrid = expand.grid(cp = seq(0,0.10,0.01)))
plot(dt_best)
resampleHist(dt_best)
ggplot(varImp(dt_best), main = "Variable Importance with DT")
# Model performance:
perform_dt <- predict(dt_best, newdata = df_scale_test[,-c(4)])
confusionMatrix(perform_dt, df_scale_test$Severity)
# Accuracy : 0.6418; Sensitivity : 0.7724; Specificity : 0.5112

# ---------------------------- 3.3 Random Forest ---------------------------
# Baseline model:
# rf <- train(Severity ~., data = df_scale, method = "rf")
# rf
# 
# # Tune the model:
# rf_tune <- train(Severity ~., data = train_scale, method = "rf",
#                  trControl = trainControl(method = "cv", number = 3),
#                  tuneGrid = data.frame(mtry = seq(1, 25)))
# rf_tune
# plot(rf_tune, main="RF Model Tuning Result")

# Best model:
rf_best <- train(Severity ~., data = df_scale, 
                 method = "rf",
                 # ntree = 3000,
                 trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
                 tuneGrid = expand.grid(mtry = c(6,8,10,12,14)))
plot(rf_best)
resampleHist(rf_best)
ggplot(varImp(rf_best), main = "Variable Importance with RF")
# Model performance:
perform_rf <- predict(rf_best, newdata = df_scale_test[,-c(4)], type = "raw")
confusionMatrix(perform_rf, df_scale_test$Severity)

# Accuracy : 0.6895  mtry = 14
# Accuracy : 0.6865  mtry = 10

# --------------------------- 3.4 K Nearest Neighbor --------------------------
# Baseline model:
# knn <- train(Severity ~., data = train_scale, method = "knn")
# knn
# 
# # Tune the model:
# set.seed(242)
# knn_tune <- train(Severity ~., data = train_scale, method = "knn",
#                  trControl = trainControl(method = "cv", number = 3),
#                  tuneGrid = data.frame(k = seq(1, 25)))
# knn_tune
# plot(knn_tune, main="KNN Model Tuning Result")

# Best model:
knn_best <- train(Severity ~., 
                  data = df_scale, 
                  method = "knn",
                  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
                  tuneGrid = data.frame(k = seq(20:80)))
plot(knn_best)
resampleHist(knn_best)
ggplot(varImp(knn_best), main = "Variable Importance with RF")
# Model performance:
perform_knn <- predict(knn_best, newdata = df_scale_test[,-c(4)], type = "raw")
confusionMatrix(perform_knn, df_scale_test$Severity)

# --------------------------- 3.5 XGBoost --------------------------
# Tune the model:
XGBOOST_best <- train(Severity ~., 
                   data = df_scale,
                   method = "xgbTree",
                   trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
                   tuneGrid = expand.grid(nrounds = c(25,50,100,150,200,250,300),
                                          max_depth = c(2,3,5,7,9),
                                          colsample_bytree = seq(0.1, 1, length.out = 5),
                                          eta = 0.1,
                                          gamma=0,
                                          min_child_weight = 1,
                                          subsample = 1))
plot(XGBOOST_best)
resampleHist(XGBOOST_best)
ggplot(varImp(XGBOOST_best), main = "Variable Importance with XGBoost")
# Model performance:
perform_XGB <- predict(XGBOOST_best, newdata = df_scale_test[,-c(4)], type = "raw")
confusionMatrix(perform_XGB, as.factor(df_scale_test$Severity))

# ---------------------------- 3.6 Support vector machines ---------------------------
# ------------- 3.6.1 SVM with linear kernel functions -------------
model_svm_linear <- train(Severity ~ .,
                          data = df_scale,
                          method = "svmLinear",
                          trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
                          tuneGrid = expand.grid(C = seq(0, 1, 0.1)))
plot(model_svm_linear)
resampleHist(model_svm_linear)
ggplot(varImp(model_svm_linear), main = "Variable Importance with XGBoost")

perform_SVM_linear <- predict(model_svm_linear, newdata = df_scale_test[,-c(4)], type = "raw")
confusionMatrix(perform_SVM_linear, as.factor(df_scale_test$Severity))
# Accuracy = 0.6759, C = 0.1
# Accuracy = 0.6786, C = 0.7 drop station & source

varImp(model_svm, data = train_scale)


# ------------- 3.6.2 SVM with non-linear kernel functions ---------------
model_svm_rbf <- train(Severity ~., data = df_scale, method = "svmRadial",
                       tuneGrid = expand.grid(sigma = seq(0.1, 0.3, 0.1), C = seq(0.1, 0.5, 0.1)),
                       trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))
plot(model_svm_rbf)
resampleHist(model_svm_rbf)
ggplot(varImp(model_svm_rbf), main = "Variable Importance with XGBoost")

perform_SVM_rbf <- predict(model_svm_rbf, newdata = df_scale_test[,-c(4)], type = "raw")
confusionMatrix(perform_SVM_rbf, df_scale_test$Severity)
# Accuracy = 0.6725, C = 0.1, Sigma = 0.1
# Accuracy = 0.6912, C = 0.5, Sigma = 0.1

# ----------------------------- 3.6 Logistic regression ----------------------------
model_lm <- train(Severity ~ ., data = df_scale, method = "glm", family = "binomial",
                  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))




################################################# Section 4: Summary ###############################################
model_comparison <- resamples(list(DT = dt_best,
                                   RF = rf_best,
                                   KNN = knn_best,
                                   XGBoost = XGBOOST_best, 
                                   SVM_Linear = model_svm_linear, 
                                   GLM = model_lm))
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(model_comparison, scales = scales)


perform_dt <- predict(dt_best, newdata = df_scale_test[,-c(4)], type = "raw")
Matrix_dt <- confusionMatrix(perform_dt, as.factor(df_scale_test$Severity))

perform_rf <- predict(rf_best, newdata = df_scale_test[,-c(4)], type = "raw")
Matrix_rf <- confusionMatrix(perform_rf, as.factor(df_scale_test$Severity))

perform_knn <- predict(knn_best, newdata = df_scale_test[,-c(4)], type = "raw")
Matrix_knn  <- confusionMatrix(perform_knn, as.factor(df_scale_test$Severity))

perform_SVM_linear <- predict(model_svm_linear, newdata = df_scale_test[,-c(4)], type = "raw")
Matrix_svm  <- confusionMatrix(perform_SVM_linear, as.factor(df_scale_test$Severity))

perform_XGB <- predict(XGBOOST_best, newdata = df_scale_test[,-c(4)], type = "raw")
Matrix_XGB <- confusionMatrix(perform_XGB, as.factor(df_scale_test$Severity))

perform_glm <- predict(model_lm, newdata = df_scale_test[,-c(4)], type = "raw")
Matrix_glm <- confusionMatrix(perform_glm, as.factor(df_scale_test$Severity))



Per_names <- as.data.frame(c("Accuracy", "Sensitivity", "Specificity", "Precision", "Recall", "AUC"))
rownames(Per_names) <- c("Accuracy", "Sensitivity", "Specificity", "Precision", "Recall", "AUC")
colnames(Per_names) <- "DT"


Per_names$DT <- c(as.numeric(Matrix_dt$overall[1]), 
                       as.numeric(Matrix_dt$byClass[1]), 
                       as.numeric(Matrix_dt$byClass[2]), 
                       as.numeric(Matrix_dt$byClass[5]), 
                       as.numeric(Matrix_dt$byClass[6]),
                       as.numeric(roc_dt$auc))

Per_names$RF <- c(as.numeric(Matrix_rf$overall[1]), 
                  as.numeric(Matrix_rf$byClass[1]), 
                  as.numeric(Matrix_rf$byClass[2]), 
                  as.numeric(Matrix_rf$byClass[5]), 
                  as.numeric(Matrix_rf$byClass[6]),
                  as.numeric(roc_rf$auc))

Per_names$KNN <- c(as.numeric(Matrix_knn$overall[1]), 
                  as.numeric(Matrix_knn$byClass[1]), 
                  as.numeric(Matrix_knn$byClass[2]), 
                  as.numeric(Matrix_knn$byClass[5]), 
                  as.numeric(Matrix_knn$byClass[6]),
                  as.numeric(roc_knn$auc))

Per_names$SVM <- c(as.numeric(Matrix_svm$overall[1]), 
                   as.numeric(Matrix_svm$byClass[1]), 
                   as.numeric(Matrix_svm$byClass[2]), 
                   as.numeric(Matrix_svm$byClass[5]), 
                   as.numeric(Matrix_svm$byClass[6]),
                   NA)

Per_names$XGB <- c(as.numeric(Matrix_XGB$overall[1]), 
                   as.numeric(Matrix_XGB$byClass[1]), 
                   as.numeric(Matrix_XGB$byClass[2]), 
                   as.numeric(Matrix_XGB$byClass[5]), 
                   as.numeric(Matrix_XGB$byClass[6]),
                   as.numeric(roc_xgb$auc))

Per_names$GLM <- c(as.numeric(Matrix_glm$overall[1]), 
                   as.numeric(Matrix_glm$byClass[1]), 
                   as.numeric(Matrix_glm$byClass[2]), 
                   as.numeric(Matrix_glm$byClass[5]), 
                   as.numeric(Matrix_glm$byClass[6]),
                   as.numeric(roc_glm$auc))

Per_names[,order(Per_names[1,] ,decreasing = T)]



perform_dt <- predict(dt_best, newdata = df_scale_test[,-c(4)], type = "prob")
roc_dt <- roc(df_scale_test$Severity, perform_dt$High, levels = c("High", "Low"), direction = ">")

perform_knn <- predict(knn_best, newdata = df_scale_test[,-c(4)], type = "prob")
roc_knn <- roc(df_scale_test$Severity, perform_knn$High, levels = c("High", "Low"), direction = ">")

perform_rf <- predict(rf_best, newdata = df_scale_test[,-c(4)], type = "prob")
roc_rf <- roc(df_scale_test$Severity, perform_rf$High, levels = c("High", "Low"), direction = ">")

perform_glm <- predict(model_lm, newdata = df_scale_test[,-c(4)], type = "prob")
roc_glm <- roc(df_scale_test$Severity, perform_glm$High, levels = c("High", "Low"), direction = ">")

perform_xgb <- predict(XGBOOST_best, newdata = df_scale_test[,-c(4)], type = "prob")
roc_xgb <- roc(df_scale_test$Severity, perform_xgb$High, levels = c("High", "Low"), direction = ">")

plot(roc_dt, col = "#e76f51")
plot(roc_knn, col = "#98c1d9", add = TRUE)
plot(roc_rf, col = "#e9c46a", add = TRUE)
plot(roc_glm, col = "#2a9d8f", add = TRUE)
plot(roc_xgb, col = "#3d5a80", add = TRUE)
legend(x = 0.25, y = 0.4, lwd = 2, border = NA,
       legend = c("DT", "KNN", "RF", "GLM", "XGB"),
       col = c("#e76f51", "#98c1d9", "#e9c46a", "#2a9d8f", "#3d5a80"))






