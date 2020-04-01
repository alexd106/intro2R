## ----Q1, results = 'asis'---------------------------------------------------------------------------------------------------------
prawns <- read.table('workshop/data/prawnGR.CSV', sep = ",", header = TRUE)

# or

prawns <- read.csv("workshop/data/prawnGR.CSV")

# take a look at the data
str(prawns)

# 'data.frame':   60 obs. of  2 variables:
#  $ GRate: num  9.77 10.29 10.05 10.08 9.31 ...
#  $ diet : Factor w/ 2 levels "Artificial","Natural":...

summary(prawns)

#      GRate                diet   
#  Min.   : 7.395   Artificial:30  
#  1st Qu.: 9.550   Natural   :30  
#  Median : 9.943                  
#  Mean   : 9.920                  
#  3rd Qu.:10.344                  
#  Max.   :11.632         

# how many replicates for each level of diet

table(prawns$diet)

# Artificial    Natural 
#         30         30 

# or use xtabs

xtabs(~ diet, data = prawns)

# produce a boxplot

boxplot(GRate ~ diet, data = prawns, xlab = "Diet", ylab = "Growth Rate")


## ----Q2, results = 'asis'---------------------------------------------------------------------------------------------------------
# test normality assumption

# Do not perform test on all data together, i.e.

shapiro.test(prawns$GRate) # this is wrong!!

# Need to test for departures from normality for each group 
# separately. Remember your indexing [ ]

shapiro.test(prawns$GRate[prawns$diet == "Artificial"])
 
#         Shapiro-Wilk normality test
# 
# data:  prawns$GRate[prawns$diet == "Artificial"] 
# W = 0.9486, p-value = 0.1553

shapiro.test(prawns$GRate[prawns$diet == "Natural"])

#         Shapiro-Wilk normality test
# 
# data:  prawns$GRate[prawns$diet == "Natural"] 
# W = 0.9598, p-value = 0.307

# Therefore no evidence to reject the Null hypothesis and data are normally distributed

# However much better assessment of normality is to use a quantile - quantile plot
# looking for points to lie along the line for normality

qqnorm(prawns$GRate[prawns$diet == "Artificial"])
qqline(prawns$GRate[prawns$diet == "Artificial"])

qqnorm(prawns$GRate[prawns$diet == "Natural"])
qqline(prawns$GRate[prawns$diet == "Natural"])

# test for equal variance
# if normal
# Null hypothesis Ho: variances are equal

var.test(prawns$GRate ~ prawns$diet)

#         F test to compare two variances
# data:  prawns$GRate by prawns$diet 
# F = 1.9629, num df = 29, denom df = 29, p-value = 0.07445
# alternative hypothesis: true ratio of variances is not equal to 1 
# 95 percent confidence interval:
#  0.9342621 4.1240043 
# sample estimates:
# ratio of variances 
#           1.962881 

# No evidence to reject null hypothesis (P=0.07) therefore no 
# difference in variance


## ----Q3 , results = 'asis'--------------------------------------------------------------------------------------------------------
# conduct t-test assuming equal variances
# Null hypothesis Ho: no difference in growth rate 
# between prawns fed on artificial diet or Natural diet

t.test(GRate ~ diet, var.equal = TRUE, data = prawn)

#         Two Sample t-test
# 
# data:  prawns$GRate by prawns$diet 
# t = -1.3259, df = 58, p-value = 0.1901
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -0.6319362  0.1283495 
# sample estimates:
# mean in group Artificial    mean in group Natural 
#                 9.794133                10.045927 
# 
                
# No evidence to reject the Null hypothesis, therefore no 
# difference in growth rate of prawns fed on either artificial 
# or natural diet (t = -1.33, df = 58, p = 0.19).


## ----Q4, results = 'asis'---------------------------------------------------------------------------------------------------------
# fit the model

growth.lm <- lm(GRate ~ diet, data = prawns)


## ----Q5, results = 'asis'---------------------------------------------------------------------------------------------------------
# produce the ANOVA table

anova(growth.lm)

# Analysis of Variance Table
# 
# Response: GRate
#           Df Sum Sq Mean Sq F value Pr(>F)
# diet       1  0.951 0.95100  1.7579 0.1901
# Residuals 58 31.377 0.54098  

# notice the p value is the same as for the t-test
# also if you square the t statistic from the t-test
# you get the F statistic from the linear model.
# They're the same test


## ----Q6, results = 'asis'---------------------------------------------------------------------------------------------------------
# plot the residuals to assess normality and equal variance
# divide the plotting device into 2 rows and 2 columns to get all
# the graphs on one device

par(mfrow = c(2,2))
plot(growth.lm)  



## ----Q7, tidy = TRUE--------------------------------------------------------------------------------------------------------------
gigartina <- read.table('workshop/data/Gigartina.CSV', header = TRUE, sep = ",")

# or

gigartina <- read.csv('workshop/data/Gigartina.CSV')

str(gigartina)
# 'data.frame':	40 obs. of  2 variables:
#  $ diameter    : int  110 115 110 108 109 101 101 98 120  ...
#  $ diatom.treat: Factor w/ 4 levels "ASGM","Sdecl",..: 1 1...

table(gigartina$diatom.treat)

#  ASGM Sdecl Sexpo Sstat 
#    10    10    10    10 

# or use xtabs

xtabs(~ diatom.treat, data = gigartina)

# diatom.treat
#  ASGM Sdecl Sexpo Sstat 
#    10    10    10    10 

# plot these data

boxplot(diameter ~ diatom.treat, data = gigartina, xlab = "diatom treatment", ylab = "diameter (um)")

#or if you want to do the fancy um symbol correctly

boxplot(diameter ~ diatom.treat, data = gigartina, xlab = "diatom treatment", ylab = expression(paste("diameter", " (",mu,"m)")))



## ---- Q8, results = 'asis'--------------------------------------------------------------------------------------------------------
# The null hypothesis Ho: there is no difference in mean diameter 
# of the spores between the different treatment groups



## ----Q9, results = 'asis'---------------------------------------------------------------------------------------------------------

gigartina.lm <- lm(diameter ~ diatom.treat, data = gigartina)



## ----Q10--------------------------------------------------------------------------------------------------------------------------
anova(gigartina.lm)

# Analysis of Variance Table
# 
# Response: diameter
#              Df Sum Sq Mean Sq F value    Pr(>F)    
# diatom.treat  3 1880.3  626.76  22.775 1.929e-08 ***
# Residuals    36  990.7   27.52                      
# ---

# reject the null hypothesis, therefore there is a significant 
# difference in the diameter between the treatment groups
# (F_3,36 = 22.78, p < 0.001)


## ----Q11--------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2,2))
plot(gigartina.lm)

# residual plots look ok to me!



## ----Q13, results = 'asis'--------------------------------------------------------------------------------------------------------
# what group mean is different from what? Post-hoc comparisons.
# we will use Tukey's Honest significant difference method 
# to compare group means.

gigartina.av <- aov(gigartina.lm)

# compare the group means using TukeysHSD method

TukeyHSD(gigartina.av)
 
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = diameter ~ diatom.treat, data = gigartina)
# 
# $diatom.treat
#              diff         lwr       upr     p adj
# Sdecl-ASGM  -14.3 -20.6184102  -7.98159 0.0000030
# Sexpo-ASGM   -8.9 -15.2184102  -2.58159 0.0029489
# Sstat-ASGM  -18.3 -24.6184102 -11.98159 0.0000000
# Sexpo-Sdecl   5.4  -0.9184102  11.71841 0.1165421
# Sstat-Sdecl  -4.0 -10.3184102   2.31841 0.3360087
# Sstat-Sexpo  -9.4 -15.7184102  -3.08159 0.0016145

# the null hypothesis for each comparison is
# drp1 - grp2 = 0 (i.e. no difference)

# Sdecl-ASGM, Sexpo-ASGM, Sstat-ASGM and Sstat-Sexpo
# are significantly different



## ----Q14--------------------------------------------------------------------------------------------------------------------------
plot(TukeyHSD(gigartina.av), cex.axis = 0.5, las = 2)


## ----Q15, tidy = TRUE-------------------------------------------------------------------------------------------------------------
temora <- read.table('workshop/data/TemoraBR.CSV', header = TRUE, sep = ",")

# or

temora <- read.csv('workshop/data/TemoraBR.CSV')

str(temora)

# 'data.frame':	45 obs. of  3 variables:
#  $ temp                : num  5 6 7 10 11 12 13 15 16 17 ...
#  $ beat_rate           : num  3.76 5.4 8 9.4 16.6 18.5 19...
#  $ acclimitisation_temp: int  5 5 5 5 5 5 5 5 5 5 ...


## ----Q16, tidy = TRUE-------------------------------------------------------------------------------------------------------------
temora$Facclimitisation_temp <- factor(temora$acclimitisation_temp)

# boxplot of beat rate and acclimitisation temp

boxplot(beat_rate ~ Facclimitisation_temp, data = temora, xlab = "acclimitisation temp", ylab = "beat rate")

# scatter plot using the with function

with(temora, plot(beat_rate ~ temp, xlab = "temperature", ylab = "beat rate"))

# using a coplot

coplot(beat_rate ~ temp | Facclimitisation_temp, data = temora)

# scatter plot with different symbols and colours

with(temora, plot(beat_rate ~ temp, xlab = "temperature", ylab = "beat rate", col = as.numeric(Facclimitisation_temp), pch =   as.numeric(Facclimitisation_temp)))

legend("topleft", legend = c("5", "10", "20"), pch = unique(as.numeric(temora$Facclimitisation_temp)), col =   unique(as.numeric(temora$Facclimitisation_temp)))

									
# or more flexibly

plot(beat_rate ~ temp, xlab = "temperature", ylab = "beat rate", type = "n", data = temora)
with(temora, points(beat_rate[Facclimitisation_temp == "5"] ~ temp[Facclimitisation_temp == "5"], pch = 1, col = "black"))
with(temora, points(beat_rate[Facclimitisation_temp == "10"] ~ temp[Facclimitisation_temp == "10"], pch = 2, col = "red"))
with(temora, points(beat_rate[Facclimitisation_temp == "20"] ~ temp[Facclimitisation_temp == "20"], pch = 3, col = "blue"))

legend("topleft", legend = c("5", "10", "20"), col = c("black", "red","blue"), pch = c(1,2,3))



## ----Q17--------------------------------------------------------------------------------------------------------------------------
# the slope of the relationship between beat rate and temp
# look different for each acclimitisation temp


## ----Q18, tidy = TRUE-------------------------------------------------------------------------------------------------------------
temora.lm <- lm(beat_rate ~ temp + Facclimitisation_temp + temp:Facclimitisation_temp, data = temora)

# or equivalently

temora.lm <- lm(beat_rate ~ temp * Facclimitisation_temp, data = temora)


## ----Q19, results = 'asis'--------------------------------------------------------------------------------------------------------
anova(temora.lm)

# Analysis of Variance Table

# Response: beat_rate
#                            Df Sum Sq Mean Sq F value    Pr(>F)    
# temp                        1 4293.7  4293.7 835.866 < 2.2e-16 ***
# Facclimitisation_temp       2 1197.7   598.8 116.576 < 2.2e-16 ***
# temp:Facclimitisation_temp  2  284.1   142.0  27.651 3.331e-08 ***
# Residuals                  39  200.3     5.1   

# there is a significant interaction between temp and 
# Facclimitisation_temp therefore there is a significant 
# relationship between beat_rate and temp, and this relationship 
# is different depending on the level of Facclimitisation_temp.
# Therefore we should not interpret the main effect of temp
# or Facclimitisation_temp


## ----Q20--------------------------------------------------------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(temora.lm)

# there is a hint of heterogeneity of variance (non equal variance)
# as the variance increases with the fitted values. This is typical 
# of count data.



## ----Q22, results = 'asis'--------------------------------------------------------------------------------------------------------
# we could try square root transforming the variable 
# beat_rate to stabilise the variance

# square root transform beat_rate and store in the dataframe

temora$SQRT_beatrate <- sqrt(temora$beat_rate)

# refit the model using the square root transformed data

temora.lm2 <- lm(SQRT_beatrate ~ temp * Facclimitisation_temp, data = temora)

par(mfrow = c(2,2))
plot(temora.lm2)

# Residuals look a bit better
# now lets look at the ANOVA table for our new model

anova(temora.lm2)

# Analysis of Variance Table
# 
# Response: SQRT_beatrate
#                            Df Sum Sq Mean Sq  F value    Pr(>F)    
# temp                        1 67.916  67.916 712.1746 < 2.2e-16 ***
# Facclimitisation_temp       2 17.600   8.800  92.2782 1.632e-15 ***
# temp:Facclimitisation_temp  2  1.151   0.576   6.0353  0.005205 ** 
# Residuals                  39  3.719   0.095                       

# model has the same interpretation but the p value for the 
# interaction term is a bit larger.


