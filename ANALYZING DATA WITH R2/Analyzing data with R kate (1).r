# Install tidymodels if you haven't done so
install.packages("rlang")
install.packages("tidymodels")

# Library for modeling
library(tidymodels)

# Load tidyverse
library(tidyverse)


url <- "https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz"
download.file(url, destfile = "noaa-weather-sample-data.tar.gz")

untar("noaa-weather-sample-data.tar.gz", tar = "internal")


NOAAdf <-read.csv("noaa-weather-sample-data/jfk_weather_sample.csv")

head(NOAAdf)

glimpse(NOAAdf)

subsetNOAAdf <- select(NOAAdf, c("HOURLYRelativeHumidity",
"HOURLYDRYBULBTEMPF",
"HOURLYPrecip",
"HOURLYWindSpeed",
"HOURLYStationPressure"))


head(subsetNOAAdf,10)

unique(subsetNOAAdf$HOURLYPrecip)

subset2NOAAdf <- subsetNOAAdf %>%
 mutate(HOURLYPrecip = str_replace_all(HOURLYPrecip, "T", "0.0"))

subset3NOAAdf <- subset2NOAAdf %>%
 mutate(HOURLYPrecip = str_remove(HOURLYPrecip, pattern = "s$"))

unique(subset3NOAAdf$HOURLYPrecip)


glimpse(subset3NOAAdf)

subset3NOAAdf$HOURLYPrecip <- (as.numeric(subset3NOAAdf$HOURLYPrecip))
glimpse(subset3NOAAdf)



glimpse(subset3NOAAdf)


subset4NOAAdf <- subset3NOAAdf %>% 
rename('relative_humidity' = 'HOURLYRelativeHumidity',
'dry_bulb_temp_f' = 'HOURLYDRYBULBTEMPF',
'precip' = 'HOURLYPrecip',
'wind_speed' = 'HOURLYWindSpeed',
'station_pressure' = 'HOURLYStationPressure')

glimpse(subset4NOAAdf)


set.seed(1234)
train_NOAA <- initial_split(subset4NOAAdf, prop = 0.8)
train_set <- training(train_NOAA)
test_set <- testing(train_NOAA)

 
 
ggplot(train_set, aes(x = relative_humidity))+
geom_histogram(color = " darkgreen", fill = "lightgreen")



ggplot(train_set, aes(x = dry_bulb_temp_f))+
geom_histogram(color = " darkgreen", fill = "lightgreen")



ggplot(train_set, aes(x = precip))+
geom_histogram(color = " darkgreen", fill = "lightgreen")



ggplot(train_set, aes(x = wind_speed))+
geom_histogram(color = " darkgreen", fill = "lightgreen")


ggplot(train_set, aes(x = station_pressure))+
geom_histogram(color = " darkgreen", fill = "lightgreen")



# Simple linear regression 
lm_a_train <- lm(precip ~ relative_humidity, data = train_set)
lm_b_train <- lm(precip ~ dry_bulb_temp_f, data = train_set)
lm_c_train <- lm(precip ~ wind_speed, data = train_set)
lm_d_train <- lm(precip ~ station_pressure, data = train_set)

 

train_set %>%
ggplot(aes(relative_humidity, precip))+
  geom_point()
 



train_set %>%
ggplot(aes(dry_bulb_temp_f, precip))+
  geom_point()
 



train_set %>%
ggplot(aes(wind_speed, precip))+
  geom_point()
 



train_set %>%
ggplot(aes(station_pressure, precip))+
  geom_point()
 

# Multiple linear regression
mlr_e_train <- lm(precip ~ relative_humidity + wind_speed, data = train_set)
mlr_f_train <- lm(precip ~ relative_humidity + wind_speed + station_pressure, data = train_set)


lm_a_test <- lm(precip ~ relative_humidity, data = test_set)
lm_b_test <- lm(precip ~ dry_bulb_temp_f, data = test_set)
lm_c_test <- lm(precip ~ wind_speed, data = test_set)
lm_d_test <- lm(precip ~ station_pressure, data = test_set)



mlr_e_test <- lm(precip ~ relative_humidity + wind_speed, data = test_set)
mlr_f_test <- lm(precip ~ relative_humidity + wind_speed + station_pressure, data = test_set)

#nova verzija, radi razlike u pred truth uzimala mean squared error po nizem principu (yt video)
#IDEJA JE kao da svaki put jedna varijabla utjecaja i jedan x i predikcija y-ona po njoj, pa pogreska predikcije po fitu po njoj, , jer zadatak
#da napravimo 4 SIMPLE LINEAR MODELA, IAKO OVO ZAPRAVO MULTIPLE LINEAR REGRESSION,  ONDA U IMPROVED DIJELU SAM RADILA KAO MULTIPLE LINEAR REG.
# A NAJBOLJI OD OVIH JE ONAJ KOJI IMA NAJMANJI MEAN SQUARED ERROR RECIMO, lm_a_train, b ,  c, d, e, f ;

lm_a_predict <- linear_reg() %>%
  set_engine(engine = "lm")

train_fit <- lm_a_predict %>% 
    fit(precip ~ relative_humidity , data = train_set)
#fit na train dijelu podataka

lm_a_results <- train_fit %>%
  predict(new_data = test_set) %>%
  mutate(truth = test_set$precip)
#primjena predict train fita na test dijelu podataka i umetanje nove kolone predikt podataka i kolone stvarnih podataka precip da se vidi razlika predikcije i stvarnih podataka test seta
#dataframe with new predicted data and real truth data for test set
#varijabla po varijabla,parametar po parametar ovdje

head(lm_a_results)

mse_lm_a_train <- mean(lm_a_train$residuals^2)
#razlika izmedju 2 stupca gore je pogrska i ko ima najmanju pogresku pobjednik od ovih predikcija

print(mse_lm_a_train)

#mogla sam i (https://www.youtube.com/watch?v=5MvyuMcqpFg)
#mse_lm_a_train <-((train_set$precip - predict(lm_a_train))^2)

lm_b_predict <- linear_reg() %>%
  set_engine(engine = "lm")

train_fit <- lm_b_predict %>% 
    fit(precip ~ dry_bulb_temp_f , data = train_set)

lm_b_results <- train_fit %>%
  predict(new_data = test_set) %>%
  mutate(truth = test_set$precip)

head(lm_b_results)

lm_c_predict <- linear_reg() %>%
  set_engine(engine = "lm")

train_fit <- lm_c_predict %>% 
    fit(precip ~ wind_speed , data = train_set)

lm_c_results <- train_fit %>%
  predict(new_data = test_set) %>%
  mutate(truth = test_set$precip)

head(lm_c_results)

lm_d_predict <- linear_reg() %>%
  set_engine(engine = "lm")

train_fit <- lm_c_predict %>% 
    fit(precip ~ station_pressure , data = train_set)

lm_d_results <- train_fit %>%
  predict(new_data = test_set) %>%
  mutate(truth = test_set$precip)

head(lm_d_results)

lm_e_predict <- linear_reg() %>%
  set_engine(engine = "lm")

train_fit <- lm_e_predict %>% 
    fit(precip ~ relative_humidity + wind_speed , data = train_set)

lm_e_results <- train_fit %>%
  predict(new_data = test_set) %>%
  mutate(truth = test_set$precip)

head(lm_e_results)

lm_f_predict <- linear_reg() %>%
  set_engine(engine = "lm")

train_fit <- lm_f_predict %>% 
    fit(precip ~ relative_humidity + wind_speed + station_pressure , data = train_set)

lm_f_results <- train_fit %>%
  predict(new_data = test_set) %>%
  mutate(truth = test_set$precip)

head(lm_f_results)

#od nize prva verzija



mse_lm_a_train <- mean(lm_a_train$residuals^2)
rmse_lm_a_train <- sqrt(mse_lm_a_train)
rmse_lm_a_train

summary(lm_a_train)$r.squared

mse_lm_b_train <- mean(lm_b_train$residuals^2)
rmse_lm_b_train <- sqrt(mse_lm_b_train)
rmse_lm_b_train

summary(lm_b_train)$r.squared

mse_lm_c_train <- mean(lm_c_train$residuals^2)
rmse_lm_c_train <- sqrt(mse_lm_c_train)
rmse_lm_c_train

summary(lm_c_train)$r.squared

mse_lm_d_train <- mean(lm_d_train$residuals^2)
rmse_lm_d_train <- sqrt(mse_lm_d_train)
rmse_lm_d_train

summary(lm_d_train)$r.squared

mse_mlr_e_train <- mean(mlr_e_train$residuals^2)
rmse_mlr_e_train <- sqrt(mse_mlr_e_train)
rmse_mlr_e_train

summary(mlr_e_train)$r.squared

mse_mlr_f_train <- mean(mlr_f_train$residuals^2)
rmse_mlr_f_train <- sqrt(mse_mlr_f_train)
rmse_mlr_f_train

summary(mlr_f_train)$r.squared

mse_lm_a_test <- mean(lm_a_test$residuals^2)
rmse_lm_a_test <- sqrt(mse_lm_a_test)
rmse_lm_a_test

summary(lm_a_test)$r.squared

mse_lm_b_test <- mean(lm_b_test$residuals^2)
rmse_lm_b_test <- sqrt(mse_lm_b_test)
rmse_lm_b_test

summary(lm_b_test)$r.squared

mse_lm_c_test <- mean(lm_c_test$residuals^2)
rmse_lm_c_test <- sqrt(mse_lm_c_test)
rmse_lm_c_test

summary(lm_c_test)$r.squared

mse_lm_d_test <- mean(lm_d_test$residuals^2)
rmse_lm_d_test <- sqrt(mse_lm_d_test)
rmse_lm_d_test

summary(lm_d_test)$r.squared

mse_mlr_e_test <- mean(mlr_e_test$residuals^2)
rmse_mlr_e_test <- sqrt(mse_mlr_e_test)
rmse_mlr_e_test

summary(mlr_e_test)$r.squared

mse_mlr_f_test <- mean(mlr_f_test$residuals^2)
rmse_mlr_f_test <- sqrt(mse_mlr_f_test)
rmse_mlr_f_test

summary(mlr_f_test)$r.squared

 



 

model_names <- c("lm_a", "lm_b", "lm_c", "lm_d", "mlr_e", "mlr_f")
train_rmse <- c("0.042633884986004", "0.043392385033562", "0.0433315518426125", "0.0430772440922356", "0.0424013869180329", "0.0423731277100182")
test_rmse <- c("0.03322053062065", "0.0342407570726119", "0.0336190623854605", "0.0336635239626001", "0.0321504176207227", "0.0321171813867534")
train_rsquared <- c("0.0348125959119728", "0.00016376925087888", "0.00296520979252207", "0.01557426038156", "0.0453109273640681", "0.047492986038494")
test_rsquared <- c("0.0587280705157021", "0.0000260319245104704", "0.0360085732827907", "0.0334571088498333", "0.118392624992331", "0.120214446648217")
comparison_df <- data.frame(model_names, train_rmse, test_rmse, train_rsquared, test_rsquared)

comparison_df %>%
#Comparison data frame by descending R2 values and ascending root mean squared error RMSE values
arrange(desc(train_rsquared), train_rmse)
#mlr_f is the best model because of the highest R2 and lowest RMSE


