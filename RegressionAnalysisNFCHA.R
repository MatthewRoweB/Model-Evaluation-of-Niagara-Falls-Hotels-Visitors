## Final Project Code STAT 3P82 
## Matthew Rowe (7953326) and Dejan Jandric (6950042)

# How do milder winters and cooler summers impact the number 
# of total and same day visitors, in Niagara Falls, Ontario? We will 
# take into account temperature and humidity during the peak and off-season to see how these
# factors impact the numbers. Any change to the number of visitors will have a direct implication
# on hotel occupancy rates, pricing strategies, and staffing requirements. Due to the vast seasonal
# fluctuations in tourism, the NFCHA will forecast visitor numbers precisely in an attempt to
# allocate resources appropriately and better target marketing initiatives, to hopefully minimize
# any decline in tourism brought on by adverse weather. 


# Would impact tourism. Through the analysis of our data, we will figure out how each year's
# seasons were in terms of climate and uncover if it had a substantial impact on the number of
# visits and same-day visitors. We will do hypothesis testing at a specific degree of confidence
# such as 95%, using this example we will test the hypothesis that any of the weather factors
# (temperature, humidity) have a positive correlation to the number of same-day visitors and
# visitors as a whole. Likewise, we will be testing two various hypotheses where we formally will
# test if milder winters or cooler summers are statistically associated with tourism trends. We plan
# on utilizing a multiple linear regression model having the tourism numbers per season/month
# being our dependent variable, meanwhile, our independent variables would be the average
# temperature for each season/month. Then through the analysis and computations of this data, we
# can see if there is a relation to assist the NFCHA with future forecasting. Moreover, computing
# confidence intervals for our model parameters could help us better comprehend each variable's
# impact on tourism, specifically if it impacts people's decision to visit Niagara Falls for the day or
# to spend the night. Through this statistical model, we will better acknowledge if weather



# Linear Regression Model 


# Pulled from the source https://www.timeanddate.com/weather/canada/niagara-falls/historic?month=1&year=2018
# Select month and a chart will appeared below listing avg temp and hum for a specific month

temp2018 <- c(-4, 0, 1, 4, 18, 21, 24, 22, 19, 10, 2, 0)                 # Avg. Temperature by month 2018
hum2018 <- c(.70, .71, .65, .66, .62, .62, .64, .78, .79, .79, .79, .80) # Avg. Humidity by month 2018

temp2019 <- c(-6,-3, -1, 6, 12, 18, 23, 21, 18, 11, 1, 0)                # Avg. Temperature by month 2019
hum2019 <- c(.76, .76, .70, .75, .78, .72, .71, .71, .78, .78, .77, .82) # Avg. Humidity by month 2019

temp2022 <- c(-6, -3, 2, 8, 17, 20, 23, 23, 18, 11, 6, 1)                # Avg. Temperature by month 2022
hum2022 <- c(.72, .71, .70, .67, .64, .65, .67, .71, .76, .70, .74, .77) # Avg. Humidity by month 2022
  
temp2023 <- c(1, 0, 2, 10, 14, 20, 23, 21, 18, 13, 5, 5)                 # Avg. Temperature by month 2023
hum2023 <- c(.83, .72, .75, .67, .61, .69, .74, .74, .74, .72, .67, .78) # Avg. Humidity by month 2023

temp <- c(-4, 0, 1, 4, 18, 21, 24, 22, 19, 10, 2, 0,-6,-3, -1, 6, 12, 18, 23, 21, 18, 11, 1, 0,-6, -3, 2, 8, 17, 20, 23, 23, 18, 11, 6, 1,
              1, 0, 2, 10, 14, 20, 23, 21, 18, 13, 5, 5)     # Monthly Temperature (2018, 2019, 2022, 2023)
hum <- c(.70, .71, .65, .66, .62, .62, .64, .78, .79, .79, .79, .80, .76, .76, .70, .75, .78, .72, .71, .71, .78, .78, .77, .82, 
             .72, .71, .70, .67, .64, .65, .67, .71, .76, .70, .74, .77, .83, .72, .75, .67, .61, .69, .74, .74, .74, .72, .67, .78) 
           # Monthly Humidity (2018, 2019, 2022, 2023)

y <- c(4643, 5477, 6070, 5962, 6754, 7394, 7819, 8387, 6530, 6952, 5327, 6680,
       5591, 5046, 5585, 5096, 7115, 7472, 8073, 8525, 7682, 6398, 5856, 6896,
       4015, 3893, 3921, 5126, 6589, 5844, 9526, 7936, 6703, 6696, 5291, 5940,
       5717, 5357, 6091, 4443, 6979, 7368, 8275, 6905, 6924, 7352, 5527, 7805) # Number of Same-Day visitors per month 



ggp <- ggplot(data, aes(temp, y)) +            
  geom_point() 
ggp 
ggp +                                      
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") 

data1 <- data.frame(SameDayVisitors = y, temp=temp, hum=hum)
model1 <- lm(y~temp+hum, data=data1)
plot()
model2 <- lm(y~temp:hum, data=data1)


data2 <- data.frame(SameDayVisitors = y, temp=temp)
arimamodel <- arima(data2$temp, order=c(1,0,0))
arimamodel$coef
acf(data2$SameDayVisitors)    # Are the # Same Day Visits by month correlated? 

library(olsrr)
ols_plot_cooksd_chart(model, type = 1)
ols_plot_cooksd_chart(model1, type = 1)
ols_plot_cooksd_chart(model2,type = 1)

vif_values1 <- vif(model1)
plot(model, which=1, main="Model Fit") 

par(mfrow=c(2,2)) # Compare Transformations of model1 (SameDayVisitors ~ temp + hUm)

library(car)
ehat <- model1$residuals
qqnorm(ehat)
qqline(ehat)

model1_improved <- lm(log(y)~temp+hum, data=data1)
ehat1 <- model1_improved$residuals
qqnorm(ehat1, main="Log Transform")
qqline(ehat1)

model1_improved2 <- lm(sqrt(y)~temp+hum, data=data1)
ehat2 <- model1_improved2$residuals
qqnorm(ehat2, main="Square Root Transform")
qqline(ehat2)








