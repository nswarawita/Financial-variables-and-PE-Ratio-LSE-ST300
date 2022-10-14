# Mlr to predict the PE Ratio 

# Change the working directory
setwd("~/Desktop/ST300 Project")

# Data Pre-processing -----------------------------------------------------

data_1 <- read.csv("Market_data2020.csv", header=T) 
head(data_1)

## Look at the summary of data_1
names(data_1)
dim(data_1)
summary(data_1)
str(data_1) 

## ROE, EPS_Growth, Cost_of_Equity, CEO_holding and Institutional_holding are wrongly recognised as factors
## Convert these factors to numbers(i.e. as.numeric)
## Strip out % and convert factor to numeric overwriting existing variable

data_1$ROE <-  as.numeric(gsub("[\\%,]", "", data_1$ROE))
data_1$EPS_Growth <-  as.numeric(gsub("[\\%,]", "", data_1$EPS_Growth))
data_1$Cost_of_Equity <-  as.numeric(gsub("[\\%,]", "", data_1$Cost_of_Equity))
data_1$CEO_holding <-  as.numeric(gsub("[\\%,]", "", data_1$CEO_holding))
data_1$Institutional_holding <-  as.numeric(gsub("[\\%,]", "", data_1$Institutional_holding))

str(data_1)

## After the changes, save the  changes to a new data file
write.csv(data_1,'Market_data_clean.csv')



# Install packages and formatting the dataset --------------------------------------------------------

library(ggplot2)
library(dplyr)
library(gridExtra)
library(car)
library(leaps)

data_2 <- read.csv("Market_data_clean.csv", header=T) #data_2 is the new datset

## Look at the summary of data_2
head(data_2)
names(data_2)
dim(data_2)
str(data_2) 
summary(data_2) ## There is a N/A entry in PBV 

data_2[69,] ## The N/A entry is in row 69

## Remove the row with the N/A entry and create data_3
data_3 <- data_2[-69,] # Removing this datapoint makes point 70 correspond to datapoint 69

## Look at the summary of data_3
head(data_3)
names(data_3)
dim(data_3)
str(data_3) 
summary(data_3) ## The column X is unnecessary

## Create data_4 by removing the variable X 
data_3[,1]
data_4 <- data_3[,-1] 

## Look at the summary of data_4
head(data_4)
names(data_4)
dim(data_4) ## 281 rows cos 1 was deleted
str(data_4) 
summary(data_4) ## There are no missing values in the data



# Exploratory Data Analysis (EDA) ----------------------------------------------------

# 1. PE vs log_PE ----------------------------------------------------

summary(data_4$PE)
Histogram_PE <- ggplot(data=data_4)+
   geom_histogram(aes(x=PE,y=..density..), fill= "plum", colour = 'black') 
Histogram_PE

round(cor(data_4[,-c(1,2)]), digits=4) 
## Check the correlation among the indipendant variables vs PE/log_PE.

## Create a log version of PE
data_4$log_PE <- log(data_4$PE)
summary(data_4$log_PE)
Histogram_log_PE <- ggplot(data=data_4)+
  geom_histogram(aes(x=log_PE,y=..density..))+ labs(title="Histogram of log(PE)")
Histogram_log_PE
options(max.print=1000000)
data_4[order(data_4$log_PE),] 
# Datapoints 263 (262 after removing point 69),28 and 81 (80 after removing point 69) lie away from the others.

# Histograms of PE and a log version of PE
grid.arrange(Histogram_PE, Histogram_log_PE, ncol=2)



# 2. Observe the nature of the continuous predictors by plotting histograms  ----------------------------------------------------

Bar_chart_1 <- ggplot(data=data_4)+
  geom_bar(aes(x=Region))+
  labs(title = "Region", x = "Region", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_1
RegionTab<-table(data_4$Region)
RegionTab

nlevels(data_4$Industry) ## 94  
Bar_chart_2 <- ggplot(data=data_4)+
  geom_bar(aes(x=Industry))+
  labs(title = "Industry", x = "Industry", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_2
IndustryTab<-table(data_4$Industry)
IndustryTab

Histogram_1 <- ggplot(data=data_4)+
  geom_histogram(aes(x=Number_of_firms,y=..density..))
data_4[order(data_4$Number_of_firms),]

Histogram_2 <- ggplot(data=data_4)+
  geom_histogram(aes(x=ROE,y=..density..))
data_4[order(data_4$ROE),] ## 263 (262 after removing 69) was less than the bulk of the data

Histogram_3 <- ggplot(data=data_4)+
  geom_histogram(aes(x=EPS_Growth,y=..density..))
data_4[order(data_4$EPS_Growth),] ## 263 (262 after removing 69) was less than the bulk of the data

Histogram_4 <- ggplot(data=data_4)+
  geom_histogram(aes(x=PBV,y=..density..)) ## 71 and 89
data_4[order(data_4$PBV),] ## 83(82 after removing 69), 71(70 after removing 69) and 89(88 after removing 69) were further than the rest of the data

Histogram_5 <- ggplot(data=data_4)+
  geom_histogram(aes(x=PS,y=..density..))
data_4[order(data_4$PS),] ## No datapoint was significantly away from the rest of the data

Histogram_6 <- ggplot(data=data_4)+
  geom_histogram(aes(x=Beta,y=..density..))
data_4[order(data_4$Beta),] ## 91 (90 after removing 69) was further than the rest of the data

Histogram_7 <- ggplot(data=data_4)+
  geom_histogram(aes(x=Cost_of_Equity,y=..density..))
data_4[order(data_4$Cost_of_Equity),] ## No datapoint was significantly away from the rest of the data

Histogram_8 <- ggplot(data=data_4)+
  geom_histogram(aes(x=CEO_holding,y=..density..)) ## No datapoint was significantly away from the rest of the data

Histogram_9 <- ggplot(data=data_4)+
  geom_histogram(aes(x=Institutional_holding,y=..density..))
data_4[order(data_4$Institutional_holding),] ## No datapoint was significantly away from the rest of the data

grid.arrange(Histogram_1, Histogram_2, Histogram_3, Histogram_4, Histogram_5, Histogram_6 ,Histogram_7,
             Histogram_8, Histogram_9,nrow=3, ncol=3)
## Number_of_firms, PBV, PS, CEO_holding and Institutional_holding are positively skewed
## ROE, EPS_Growth, Beta, Cost_of_Equity are not skewed



# 3. Looking for relationships between the independant variables  ----------------------------------------------------

## Produce a correlation matrix
round(cor(data_4[,-c(1,2)]), digits=4)

## Produce a scatterplot matrix
pairs(data_4[,-c(1,2)])
## From the correlation matrix and the scatterplot matrix, strong correlationss are observed between Beta vs Cost_of_Equity and ROE vs EPS_Growth, 

# Beta vs Cost_of_Equity
cor(data_4$Cost_of_Equity, data_4$Beta) ## 0.8538 so clear positive correlation
## The points lie on 3 clear lines

dat_1 <- data_4[1:93,] 
dat_2 <- data_4[94:187,]
dat_3 <- data_4[188:281,]

ggplot(mapping = aes(x = Cost_of_Equity, y = Beta)) +
  geom_point(data = dat_1, color = "steelblue1") + geom_line(data = dat_1, color = "black",lwd = 0.25) +
  geom_point(data = dat_2, color = "turquoise") + geom_line(data = dat_2, color = "black",lwd = 0.25) +
  geom_point(data = dat_3, color = "green") + geom_line(data = dat_3, color = "black",lwd = 0.25)

p1 <- ggplot(mapping = aes(x = Beta, y = Cost_of_Equity)) +
  geom_point(data = dat_1, color = "steelblue1") + geom_line(data = dat_1, color = "black",lwd = 0.25) +
  geom_point(data = dat_2, color = "turquoise") + geom_line(data = dat_2, color = "black",lwd = 0.25) +
  geom_point(data = dat_3, color = "green") + geom_line(data = dat_3, color = "black",lwd = 0.25)


cor(data_4[1:93,c(9,10)]) ## 0.9999651
cor(data_4[94:187,c(9,10)]) ## 0.9999446
cor(data_4[188:281,c(9,10)]) ## 0.9999534

library(grid)
library(gridExtra)
grid.arrange( p1, p2, ncol=2, nrow=1) 
              top = textGrob(expression(bold("Figure 2 - Relationship between independent variables"))))



## ROE vs EPS_Growth
## They have a similar range as well
p2<- ggplot(data_4,aes(x=ROE, y=EPS_Growth))+
  geom_point(color = "greenyellow")+
  geom_smooth(method=lm,se=F, color = "black", lwd = 0.5)
cor(data_4$ROE, data_4$EPS_Growth) ## 0.9588 so almost perfect positive correlation 

# 4. Relationship between the independent and log_PE  ----------------------------------------------------

Boxplot_Region <- ggplot(data=data_4,aes(x=Region, y=log_PE), colour = Region) +
  geom_boxplot()
Boxplot_Region ## EMG, EUR and US have similar medians
tapply(data_4$log_PE,data_4$Region,median, na.rm=T)

Boxplot_Industry <- ggplot(data=data_4)+
  geom_boxplot(aes(x=Industry, y=log_PE)) 
Boxplot_Industry ## Medians greatly differ across industries
tapply(data_4$log_PE,data_4$Industry,median, na.rm=T)
summary(data_4$Industry)

Scatterplot_Number_of_firms <- ggplot(data_4,aes(x=Number_of_firms, y=log_PE))+
  geom_point()+
  theme_bw()+
  geom_smooth(method=lm,se=F) 
Scatterplot_Number_of_firms
m.Number_of_firms <- lm(log_PE~Number_of_firms, data_4)
summary(m.Number_of_firms) ## The slope coefficient is highly significant (t=3.966, p<0.01).
par(mfrow=c(2,2))
plot(m.Number_of_firms) ## Residuals vs fitted has a funnel shape
summary(data_4$Number_of_firms)
# Use a log transform the assumption of constant variance is violated and the values are greater than 0
data_4$log_Number_of_firms <- log(data_4$Number_of_firms)
m.log_Number_of_firms <- lm(log_PE~log_Number_of_firms, data_4)
summary(m.log_Number_of_firms) ## The slope coefficient is highly significant (t=5.647, p<0.01).
par(mfrow=c(2,2))
plot(m.log_Number_of_firms) ## Residuals vs fitted did not have a funnel shape
plot(m.log_Number_of_firms, which = 4) ## No points with a Cook's distance greater than 1
car::avPlots(m.log_Number_of_firms) ## 81 and 28 are candidates for influential points

par(mfrow=c(1,2))
Histogram_1 <- ggplot(mapping = aes(x = Number_of_firms, y=..density..))+
  geom_histogram(data = data_4, colour = "black",fill = "yellow")

Histogram_1
p4 <- plot(m.Number_of_firms,which = 1)
grid.arrange(Histogram_1, p4, ncol=2, nrow=1, 
              top = textGrob(expression(bold("Figure 2 - Relationship between independent variables"))))
data_4[order(data_4$Number_of_firms),]




Scatterplot_ROE <- ggplot(data_4,aes(x=ROE, y=log_PE))+
  geom_point()+
  theme_bw()+
  geom_smooth(method=lm,se=F) # Most of the points are -25 to 50 but there are a few points outside this range
Scatterplot_ROE
m.ROE <- lm(log_PE~ROE, data_4)
summary(m.ROE) ## The slope coefficient is highly significant (t=-3.782, p<0.01).
par(mfrow=c(2,2))
plot(m.ROE)
plot(m.ROE, which = 4) ## No points with a Cook's distance greater than 1
car::avPlots(m.ROE) ## 28,71,81 and 263 are candidates for influential plots

Scatterplot_EPS_Growth <- ggplot(data_4,aes(x=EPS_Growth, y=log_PE))+
  geom_point()+
  theme_bw()+
  geom_smooth(method=lm,se=F) # Most of the points are -25 to 50 but there are a few points outside this range
Scatterplot_EPS_Growth
m.EPS_Growth <- lm(log_PE~EPS_Growth, data_4)
summary(m.EPS_Growth) # The slope coefficient is highly significant (t=-4.494, p<0.01).
par(mfrow=c(2,2))
plot(m.EPS_Growth)
plot(m.EPS_Growth, which = 4) ## No points with a Cook's distance greater than 1
car::avPlots(m.EPS_Growth) ## 28,81 and 263 are candidates for influential plots

par(mfrow=c(1,2))
Scatterplot_ROE
Scatterplot_EPS_Growth
grid.arrange( Scatterplot_ROE, Scatterplot_EPS_Growth, ncol=2, nrow=1)
              top = textGrob(expression(bold("Figure 2 - Relationship between independent variables"))))


Scatterplot_PBV <- ggplot(data_4,aes(x=PBV, y=log_PE))+
  geom_point()+
  theme_bw()+
  geom_smooth(method=lm,se=F) # Most of the points are 0 to 10 but there are a few points outside this range
Scatterplot_PBV
m.PBV <- lm(log_PE~PBV, data_4)
summary(m.PBV) # The slope coefficient is highly significant (t=3.346, p<0.01).
par(mfrow=c(2,2))
plot(m.PBV)
plot(m.PBV, which = 4) ## 89 has a Cook's distance greater than 1
car::avPlots(m.PBV) 
m.PBV.without.88 <- lm(log_PE~PBV, data_4[-88,])
summary(m.PBV.without.88)

data_4 <- data_4[-88,]

Scatterplot_PS <- ggplot(data_4,aes(x=PS, y=log_PE))+
  geom_point()+
  theme_bw()+
  geom_smooth(method=lm,se=F) 
Scatterplot_PS
m.PS <- lm(log_PE~PS, data_4)
summary(m.PS) ## The slope coefficient is highly significant (t=5.079, p<0.01).
par(mfrow=c(2,2))
plot(m.PS) ## Residuals vs fitted has a funnel shape
summary(data_4$PS)
# Use a log transform the assumption of constant variance is violated and the values are greater than 0
data_4$log_PS <- log(data_4$PS)
m.log_PS <- lm(log_PE~log_PS, data_4)
summary(m.log_PS) ## The slope coefficient is highly significant (t=5.636, p<0.01).
par(mfrow=c(2,2))
plot(m.log_PS) ## Residuals vs fitted did not have a funnel shape
plot(m.log_PS, which = 4) ## No points with a Cook's distance greater than 1
car::avPlots(m.log_PS) ## 81 and 28 are candidates for influential points

Scatterplot_Beta <- ggplot(data_4,aes(x=Beta, y=log_PE))+
  geom_point()+
  theme_bw()+
  geom_smooth(method=lm,se=F)
Scatterplot_Beta
m.Beta <- lm(log_PE~Beta, data_4)
summary(m.Beta) # The slope coefficient is highly significant (t=2.957, p<0.01).
par(mfrow=c(2,2))
plot(m.Beta)
plot(m.Beta, which = 4) # No points with a Cook's distance greater than 1
car::avPlots(m.Beta) ## 28 and 81 are candidates for influential points

Scatterplot_Cost_of_Equity <- ggplot(data_4,aes(x=Cost_of_Equity, y=log_PE))+
  geom_point()+
  theme_bw()+
  geom_smooth(method=lm,se=F)
Scatterplot_Cost_of_Equity
m.Cost_of_Equity <- lm(log_PE~Cost_of_Equity, data_4)
summary(m.Cost_of_Equity) # The slope coefficient is highly significant (t=3.514, p<0.01).
plot(m.Cost_of_Equity)
plot(m.Cost_of_Equity, which = 4) # No points with a Cook's distance greater than 1
car::avPlots(m.Cost_of_Equity) ## 28 and 81 are candidates for influential points

Scatterplot_CEO_holding <- ggplot(data_4,aes(x=CEO_holding, y=log_PE))+
  geom_point()+
  theme_bw()+
  geom_smooth(method=lm,se=F)
Scatterplot_CEO_holding
m.CEO_holding <- lm(log_PE~CEO_holding, data_4)
summary(m.CEO_holding) # The slope coefficient is highly significant (t=3.764, p<0.01).
par(mfrow=c(2,2))
plot(m.CEO_holding)
plot(m.CEO_holding, which = 4) # No points with a Cook's distance greater than 1
car::avPlots(m.CEO_holding)

Scatterplot_Institutional_holding <- ggplot(data_4,aes(x=Institutional_holding, y=log_PE))+
  geom_point()+
  theme_bw()+
  geom_smooth(method=lm,se=F)
Scatterplot_Institutional_holding
m.Institutional_holding <- lm(log_PE~Institutional_holding, data_4)
summary(m.Institutional_holding) # The slope coefficient is highly significant (t=-2.831, p<0.01).
plot(m.Institutional_holding)
plot(m.Institutional_holding, which = 4) # No points with a Cook's distance greater than 1
car::avPlots(m.Institutional_holding) ## 28 and 81 are candidates for influential points



# Create Mlrs to predict log_PE ----------------------------------------------------

data_5 <- data_4[,-c(6,3,8)]
str(data_5)
dim(data_5)

## Base plot
log_PE.lm <- lm(log_PE~.-Industry,data=data_5)
summary(log_PE.lm) ## Adjusted R-squared:  0.3313

par(mfrow=c(2,2))
plot(log_PE.lm) ## Seems fine

plot(log_PE.lm, which=4) 
car::avPlots(log_PE.lm)# # 81 seems to be influential points

## Rerun the regression without 80
log_PE.lm.without.80 <- lm(log_PE~.-Industry,data=data_5[-80,])
summary(log_PE.lm.without.80) ## Adjusted R-squared:  0.3379

AIC(log_PE.lm)
AIC(log_PE.lm.without.80)

## Remove datapoint 80 
data_5 <- data_5[-80,]


## Model 1 - Using stepwise regression based on AIC ( Forward Selection )

## Initial model 
null<-lm(log_PE~1, data=data_5)
full<-lm(log_PE~.,data=data_5)
log_PE.lm.1<-step(null, scope=list(lower=null, upper=full),direction="forward", trace=0)
summary(log_PE.lm.3)

## Final model
log_PE.lm.1.Final <- lm(formula = log_PE ~ log_Number_of_firms + PBV + ROE + Beta + 
                          log_PS + CEO_holding + Region + Institutional_holding, data = data_5)
summary(log_PE.lm.1.Final)


## Model 2 - Using stepwise regression based on AIC (Backward elimination)

## Initial model 
log_PE.lm<-lm(log_PE~.,data = data_5)
log_PE.lm.2 <-step(log_PE.lm)
summary(log_PE.lm.2) ## Step selects a formula-based model by AIC

## Final model
log_PE.lm.2.Final <- lm(formula = log_PE ~ Region + EPS_Growth + PBV + Cost_of_Equity + 
                          Institutional_holding + log_Number_of_firms + log_PS, data = data_5)
summary(log_PE.lm.2.Final)


## Model 3 - Using Best subsets

## Initial model 
log_PE.lm.3<-leaps::regsubsets(log_PE~.-Industry, nvmax=10,really.big = TRUE,data=data_5)
plot(log_PE.lm.3, scale="bic") ## by default uses BIC; may use "adjr2"

## Final model
log_PE.lm.3.Final <-lm(log_PE~ ROE + PBV + Beta + log_Number_of_firms, data = data_5)
summary(log_PE.lm.3.Final)

## Model 3 is the final model


# Check the model assumptions for model 3----------------------------------------------------

## Residual vs fitted plot and Normal Q-Q Plot
par(mfrow=c(1,2))
plot(log_PE.lm.3.Final, which = c(1,2))


#  Outlier analysis ----------------------------------------------------

## Cook's Distance
par(mfrow=c(1,1))
plot(log_PE.lm.3.Final, which=4) ## 28 and 83 (28 and 81 after removing 69,88 and 80) are candidates for influential points

## AVP Plot
car::avPlots(log_PE.lm.3.Final) 

log_PE.lm.3.Final.without <-lm(log_PE~ ROE + PBV + Beta + log_Number_of_firms, data = data_5[-c(28,81),])
summary(log_PE.lm.3.Final.without) ## No significant difference. Therefore, stick to log_PE.lm.3.Final

#  Checking for multicollinearity ----------------------------------------------------

vif(log_PE.lm.3.Final)
# No multi-collinearity in the model




plot(m.Number_of_firms,which = 1)
grid.arrange( p1, p2, ncol=2, nrow=1, 
              top = textGrob(expression(bold("Figure 2 - Relationship between independent variables"))))









