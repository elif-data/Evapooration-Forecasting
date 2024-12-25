install.packages("readxl")
install.packages("tidyverse")
install.packages("ggrepel")

library(readxl)
library(tidyverse)
library(ggrepel)
library(infer)
library(lubridate)

# Set working directory
setwd("C:/Users/hande/OneDrive/Downloads/datacamp/projects/15 - Evapooration Forecasting Linear Regression")
# Get working directory
getwd()
# Show folders in working directory
dir() 
# Getting sheet names and numbers
excel_sheets("Evaporation Linear Regression.xlsx")


if(FALSE){ # OTHER CHECKS
  
# Removing empty rows
"Monthly Avg Temperature.xlsx" %>% 
  read_xlsx(sheet = "Monthly Avg Temperature(C)") %>%
  mutate(YEAR = as.numeric(`YEAR`)) %>% 
  is.na() %>% 
  as.data.frame() %>% 
  mutate(sum = rowSums(.)) %>% 
  rowid_to_column() %>% 
  filter(sum == 14) # Now we know from rows 56 to 90 are empty rows

  
df <- "Monthly Avg Temperature.xlsx" %>% 
  read_xlsx() %>%
  mutate(YEAR = as.numeric(`YEAR`)) 

df %>%
  slice(-c(55:90)) %>%  # this removes rows 55 to 90
  mutate(AVG_CHECK = rowMeans(.[, 2:13], na.rm = TRUE)) %>% 
  # filter(is.na(AVG)) %>% 
  # summarise(sum(AVG_CHECK)) # Total of AVG_CHECK when AVG is NA which returns 26.9
  summarise(diff = sum(AVG, na.rm = TRUE) - sum(AVG_CHECK, na.rm = TRUE)) # This also is 26.9 so data is ok

} # CLEANINIG DATA & DOING CHECKS



###### STEP 1: Reading Siverek Station temperature data, closest station to construciton site

temperature_siverek <- "Monthly Avg Temperature.xlsx" %>% 
  read_xlsx() %>%
  mutate(YEAR = as.numeric(`YEAR`)) %>%
  rename(YEARLY_AVG_TEMP = AVG) %>% 
  slice(-c(55:90)) 

colnames(temperature_siverek) <- tolower(colnames(temperature_siverek))

View(temperature_siverek)

temperature_siverek %>% # Looking at overall trend between 1970-2010
  ggplot(aes(x = year, y = yearly_avg_temp)) + geom_point() + geom_smooth(method = "lm")

# Pivot longer

temperature_longer <- temperature_siverek %>% 
  select(1:13) %>% 
  pivot_longer(!year, names_to = "month", values_to = "celcius") %>% 
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" )))

ggplot(temperature_longer, aes(month, celcius)) + geom_boxplot() 

# Get monthly average temperature over the years with pivot_longer

monthly_mean_temp_station <- temperature_longer %>% 
  group_by(month) %>% 
  summarise(mean_temp = mean(celcius, na.rm = TRUE)) # Max temperature: Jul with a mean of 30
                                                     # Min temperature: Jan with a mean of 3.5

ggplot(monthly_mean_temp_station, aes(month, mean_temp)) + geom_point() #+ geom_smooth(method = "lm") 

# STEP 2: Reading Siverek Station evaporation data

evaporation <- "Monthly Avg Evaporation.xlsx" %>% 
  read_xlsx() %>%
  select(-SUM) %>% 
  mutate(JAN = as.numeric(`JAN`),
         FEB = as.numeric(`FEB`),
         YEARLY_AVG_EVAP = rowMeans(.[, 2:13], na.rm = TRUE))

colnames(evaporation) <- tolower(colnames(evaporation))

View(evaporation)

# Pivot longer

evaporation_longer <- evaporation %>% 
  select(1:13) %>% 
  pivot_longer(!year, names_to = "month", values_to = "mm") %>% 
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" ))) 

ggplot(evaporation_longer, aes(month, mm)) + geom_boxplot() # Max evaporation: Jul with a mean of 400
                                                            # Min evaporation: Dec with a mean of 45

ggplot(evaporation_longer, aes(year, mm)) + geom_boxplot() # Max evaporation: Jul with a mean of 400
                                                            # Min evaporation: Dec with a mean of 45

evaporation_longer %>% 
  group_by(month) %>% 
  summarise(avg_monthly_mm = mean(mm, na.rm = TRUE)) %>% 
  summarise(total_yearly_mm = sum(avg_monthly_mm, na.rm = TRUE)) # Total yearly evaporation is 2145mm

# Get monthly average evaporation over the years with pivot_longer

monthly_mean_evap_station <- evaporation_longer %>% 
  group_by(month) %>% 
  summarise(mean_evap = mean(mm, na.rm = TRUE)) 


# Correlation between temperature and evaporation between 1972-2010

tem_evap_merged <- temperature_longer %>% 
  inner_join(evaporation_longer, by = c("month" = "month", "year" = "year")) 

ggplot(tem_evap_merged, aes(x = celcius , y = mm)) + geom_jitter(alpha = 0.75) + geom_smooth()


# Linear model

ggplot(tem_evap_merged, aes(x = celcius , y = mm)) + geom_jitter(alpha = 0.75) + geom_smooth(method = "lm", se = FALSE)

linear_mdl <- lm(mm ~ celcius, data = tem_evap_merged)

summary(linear_mdl) # For all the details

summary(linear_mdl)$r.squared # R2 = 0.8481728 

install.packages(moderndive)
library(moderndive)
get_regression_points(linear_mdl)%>%  summarize(r_squared =1- var(residual)/var(mm))

# Linear model (but with power of 2)

ggplot(tem_evap_merged, aes(celcius ^ 2, mm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

linear_mdl2 <- lm(mm ~ I(celcius^2), data = tem_evap_merged)

summary(linear_mdl2)$r.squared # R2 = 0.8543076. This fits better.

# Linear model (but with power of 3)

ggplot(tem_evap_merged, aes(celcius ^ 3, mm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # You see it does not fit as good as lm with power of 2

linear_mdl3 <- lm(mm ~ I(celcius^3), data = tem_evap_merged)

summary(linear_mdl3)$r.squared # R2 = 0.8286202. This is worse.

############################ SAME CALCULATIONS WITH MEAN VALUES ############################

merged_mean <- monthly_mean_temp_station %>% 
  inner_join(monthly_mean_evap_station, by = "month") 


# Linear 

merged_mean %>% 
  ggplot(aes(x = mean_temp , y = mean_evap)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

linear_mdl_mean <- lm(mean_evap ~ mean_temp , data = merged_mean)

summary(linear_mdl_mean) # R2 = 0.9641 


# Linear (2nd degree)

merged_mean %>% 
  ggplot(aes(x = (mean_temp)^2 , y = mean_evap)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 

linear_mdl_mean <- lm(mean_evap ~ I(mean_temp^2) , data = merged_mean)

summary(linear_mdl_mean) # R2 = 0.979, better fit.

# Polynomial (2nd degree)

poly_mdl_mean <- lm(mean_evap ~ poly(mean_temp,2), data = merged_mean) 


left_join(
merged_mean %>% 
  mutate(
    name = as.character(
      row_number()
    )
  ),
poly_mdl_mean %>% 
  .$fitted.values %>% 
  enframe()
) %>% 
  ggplot(aes(x = (mean_temp)^2 , y = mean_evap)) + geom_point() +
  geom_line(aes(y = value))

poly_mdl_mean %>% 
  summary() # R2 = 0.9812, much better fit.


if(FALSE){
confidence <- confint(poly_mdl_mean, level = 0.95) # Confidence interval check

predicted.intervals <- predict(poly_mdl_mean, merged_mean, interval = 'confidence',
                               level=0.99)

plot(fitted(poly_mdl_mean), residuals(poly_mdl_mean)) + lines(mean_temp$mean_temp, predicted.intervals[,1], col='green',lwd=3)  # ?????

# Polynomial (3rd degree)

lm(mean_evap ~  mean_temp + I(mean_temp^2) + I(mean_temp^3) , data = merged_mean) %>% 
  summary() # R2 = 0.9821, much better fit.

} # Tried to create a graph with predicted line.



# FINAL STEPS

# 1. PREDICTING CONSTRUCTION SITE TEMPERATURE USING SIVEREK TEMPERATURE, LATTITUDE & LONGITUDE

lat_siverek <- 37+30.15/60

lat_construction <- 37+46/60

long_siverek <- 1070

long_construction <- 801

change_in_temp <- (long_construction-long_siverek)*.5/100 + lat_construction - lat_siverek

monthly_mean_temp_construction <- monthly_mean_temp_station %>% 
  mutate(mean_temp = mean_temp + change_in_temp)
  
  


