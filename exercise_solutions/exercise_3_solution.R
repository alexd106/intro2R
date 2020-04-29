## ----Q5---------------------------------------------------------------------------------------------------------------------------------------
whale <- read.table('workshop/data/whaledata.txt', header = TRUE)


## ----Q6, results = 'asis'---------------------------------------------------------------------------------------------------------------------
head(whale)         # display the first 5 rows 
names(whale)        # display the variable names
str(whale)          # display the structure of the dataframe whale

# 'data.frame':	100 obs. of  8 variables:
#  $ month          : Factor w/ 2 levels "May","October": 1 1 1 1 ...
#  $ time.at.station: int  1344 1633 743 1050 1764 580 459 ...
#  $ water.noise    : Factor w/ 3 levels "high","low","medium": 2 3 3 3 ...
#  $ number.whales  : int  7 13 12 10 12 10 5 8 11 12 ...
#  $ latitude       : num  60.4 60.4 60.5 60.3 60.4 ...
#  $ longitude      : num  -4.18 -4.19 -4.62 -4.35 -5.2 ...
#  $ depth          : int  520 559 1006 540 1000 1000 993 988 ...
#  $ gradient       : int  415 405 88 409 97 173 162 162 245 161 ..

# the dataframe whale has 100 observations
# the dataframe whale has 8 variables
# the variables month and water.noise are factors


## ----Q7, results = 'asis'---------------------------------------------------------------------------------------------------------------------
summary(whale)

# NOTE: I have removed the last 3 column information to save space!

 #    month    time.at.station  water.noise number.whales      latitude        
 # May    :50   Min.   :  60.0   high  :15   Min.   : 0.00   Min.   :60.29   
 # October:50   1st Qu.: 693.8   low   :28   1st Qu.: 9.00   1st Qu.:60.69   
 #              Median :1077.5   medium:57   Median :11.00   Median :61.29  
 #              Mean   :1064.7               Mean   :11.56   Mean   :61.16  
 #              3rd Qu.:1349.2               3rd Qu.:14.00   3rd Qu.:61.59     
 #              Max.   :2158.0               Max.   :28.00   Max.   :62.10     
 #                                           NA's   :1    

# the variable number.whales has one missing value (NA)


## ----Q8, results = 'asis'---------------------------------------------------------------------------------------------------------------------
# first 10 rows and first 4 columns
whale.sub <- whale[1:10, 1:4]                                      

# all rows and columns 1, 3 and 6
whale.num <- whale[, c(1, 3, 4)] 
# alternative way of indexing columns with named indexes
whale.num <- whale[, c("month", "water.noise", "number.whales")]    

# first 50 rows and all columns
whale.may <- whale[1:50, ]  

# excluding first 10 rows and last column using negative indexing
whale.last <- whale[-c(1:10), -8]  
# more general way if you have lots of columns
whale.last <- whale[-c(1:10), -c(ncol(whale))] 
# NOTE: this doesn't work for named columns
whale.last <- whale[-c(1:10), -c("gradient")]   


## ----Q9, tidy = TRUE--------------------------------------------------------------------------------------------------------------------------
whale.1200 <- whale[whale$depth > 1200, ]

whale.200 <- whale[whale$gradient > 200, ]

whale.low <- whale[whale$water.noise == 'low', ]

whale.h.may <- whale[whale$water.noise == 'high' & whale$month == 'May', ]

whale.subset <- whale[whale$month == 'October' & whale$water.noise == 'low' & whale$gradient > 132, ]

whale.lat.long <- whale[whale$latitude > 60 & whale$latitude < 61 & whale$longitude > -6 & whale$longitude < -4, ]

whale.nomed <- whale[whale$water.noise != 'medium', ]


## ----Q10, tidy = TRUE-------------------------------------------------------------------------------------------------------------------------
whale.subset <- whale[whale$month == 'October' & whale$water.noise == 'low' & whale$gradient > median(whale$gradient), ]


## ----Q11, tidy = TRUE-------------------------------------------------------------------------------------------------------------------------
# results in a dataframe filled with NAs. 
whale.new <- whale[whale$depth > 1500 & whale$number.whales > mean(whale$number.whales), ]

# the variable number.whales contains 1 NA value. By default the mean function will return an NA.
# use the na.rm argument to ignore NAs
whale.new <- whale[whale$depth > 1500 & whale$number.whales > mean(whale$number.whales, na.rm = TRUE), ]  



## ----Q12, tidy = TRUE-------------------------------------------------------------------------------------------------------------------------
subset(whale, month == 'May' & time.at.station < 1000 & depth > 1000)

subset(whale, month == "October" & latitude > 61, select = c("month", "latitude", "longitude", "number.whales"))


## ----Q13--------------------------------------------------------------------------------------------------------------------------------------
whale.depth.sort <- whale[order(whale$depth), ]


## ----Q14, results = 'asis'--------------------------------------------------------------------------------------------------------------------
# notice how the variable water.noise has been ordered - why?
whale.sorted <- whale[order(whale$water.noise, whale$depth), ]        

# use '-' to reverse the order of depth
whale.rev.sorted <- whale[order(whale$water.noise, -whale$depth), ]   


## ----Q15a-------------------------------------------------------------------------------------------------------------------------------------
mean(whale$time.at.station)     # mean time at station
median(whale$depth)             # median depth
length(whale$number.whales)     # number of observations


## ----Q15b, results = 'asis'-------------------------------------------------------------------------------------------------------------------
tapply(whale$number.whales, whale$water.noise, mean)      # notice the NA?

# use the na.rm argument again
tapply(whale$number.whales, whale$water.noise, mean, na.rm = TRUE)    

# alternative method using the with() function. see ?with
with(whale, tapply(number.whales, water.noise, mean, na.rm = TRUE))   

# when using multiple factors these need to be supplied as a list
tapply(whale$number.whales, list(whale$water.noise, whale$month), median, na.rm = TRUE)    



## ----Q16, tidy = TRUE-------------------------------------------------------------------------------------------------------------------------
aggregate(whale[, c(2, 4, 7, 8)], by = list(water.noise = whale$water.noise), mean, na.rm = TRUE)

aggregate(whale[, c(2, 4, 7, 8)], by = list(water.noise = whale$water.noise, month = whale$month), mean, na.rm = TRUE)

# optional question. Need to specify a function 'on the fly' using function(x){}
aggregate(whale[, c(2, 4, 7, 8)], by = list(water.noise = whale$water.noise, month = whale$month), function(x){round(mean(x, na.rm = TRUE), digits = 2)})


## ----Q17, results = 'asis'--------------------------------------------------------------------------------------------------------------------
# using table
table(whale$water.noise)
table(whale$water.noise, whale$month)

# using xtabs
xtabs(~ water.noise, data = whale)
xtabs(~ month + water.noise, data = whale)


## ----Q18, tidy = TRUE-------------------------------------------------------------------------------------------------------------------------
write.table(whale.num, "workshop/data/whale_num.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

