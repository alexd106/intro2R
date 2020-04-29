## ----Q4, results = 'asis'---------------------------------------------------------------------------------------------------------------------
squid <- read.table('workshop/data/squid1.txt', header =TRUE)

str(squid)
# 'data.frame':	519 obs. of  13 variables:
#  $ sample.no        : int  105128901 105128901 105128901 105128901 ...
#  $ specimen         : int  1002 1003 1005 1007 1008 1009 1011 1013 ...
#  $ year             : int  1989 1989 1989 1989 1989 1989 1989 1989 ...
#  $ month            : int  12 12 12 12 12 12 12 12 12 12 ...
#  $ weight           : num  152 106 138 141 126 ...
#  $ sex              : int  2 2 2 2 2 2 2 2 2 2 ...
#  $ maturity.stage   : int  3 1 2 2 3 1 2 3 3 4 ...
#  $ DML              : int  174 153 169 175 169 116 135 192 170 205 ...
#  $ eviscerate.weight: num  87.5 62.6 79.4 83.1 72.2 ...
#  $ dig.weight       : num  4.648 3.138 0.307 4.123 3.605 ...
#  $ nid.length       : num  39.4 24.1 39 41.4 39.8 20 14 55 44 53 ...
#  $ nid.weight       : num  2.46 0.319 1.169 1.631 2.03 ...
#  $ ovary.weight     : num  1.68 0.103 0.289 0.252 0.86 ...

summary(squid)

# convert variables to factors
squid$Fmaturity <- factor(squid$maturity.stage)
squid$Fmonth <- factor(squid$month) 
squid$Fyear <- factor(squid$year)

str(squid)
# 'data.frame':	519 obs. of  16 variables:
#  $ sample.no        : int  105128901 105128901 105128901 ...
#  $ specimen         : int  1002 1003 1005 1007 1008 1009 ...
#  $ year             : int  1989 1989 1989 1989 1989 1989 ...
#  $ month            : int  12 12 12 12 12 12 12 12 12 12 ...
#  $ weight           : num  152 106 138 141 126 ...
#  $ sex              : int  2 2 2 2 2 2 2 2 2 2 ...
#  $ maturity.stage   : int  3 1 2 2 3 1 2 3 3 4 ...
#  $ DML              : int  174 153 169 175 169 116 135 ...
#  $ eviscerate.weight: num  87.5 62.6 79.4 83.1 72.2 ...
#  $ dig.weight       : num  4.648 3.138 0.307 4.123 3.605 ...
#  $ nid.length       : num  39.4 24.1 39 41.4 39.8 20 14 ...
#  $ nid.weight       : num  2.46 0.319 1.169 1.631 2.03 ...
#  $ ovary.weight     : num  1.68 0.103 0.289 0.252 0.86 ...
#  $ Fmaturity        : Factor w/ 5 levels "1","2","3","4" "5"...
#  $ Fmonth           : Factor w/ 12 levels "1","2","3","4" ...
#  $ Fyear            : Factor w/ 3 levels "1989","1990",..1 ...
 


## ----Q5---------------------------------------------------------------------------------------------------------------------------------------
table(squid$Fmonth, squid$Fyear)

  #   1989 1990 1991
  # 1     0    3   37
  # 2     0    0   30
  # 3     0   40   29
  # 4     0   10   33
  # 5     0    1   30
  # 6     0    0   14
  # 7     0   42    1
  # 8     0   29    0
  # 9     0   82    0
  # 10    0   19    0
  # 11    0   76    0
  # 12   12   31    0
  
ftable(xtabs(~ Fyear + Fmaturity + Fmonth, data = squid))

#                 Fmonth  1  2  3  4  5  6  7  8  9 10 11 12
# Fyear Fmaturity                                           
# 1989  1                 0  0  0  0  0  0  0  0  0  0  0  2
#       2                 0  0  0  0  0  0  0  0  0  0  0  3
#       3                 0  0  0  0  0  0  0  0  0  0  0  5
#       4                 0  0  0  0  0  0  0  0  0  0  0  2
#       5                 0  0  0  0  0  0  0  0  0  0  0  0
# 1990  1                 0  0  0  0  0  0  8  0  1  1  1  2
#       2                 0  0  0  0  0  0 22 21 76 17 31  4
#       3                 0  0  0  0  0  0  0  5  5  1 31  6
#       4                 2  0 15  7  0  0  4  3  0  0 10 13
#       5                 1  0 25  3  1  0  8  0  0  0  3  6
# 1991  1                 0  0  0  2  0  4  0  0  0  0  0  0
#       2                 1  1  0  1  0  6  0  0  0  0  0  0
#       3                 2  0  0  1  1  0  0  0  0  0  0  0
#       4                16  8  6 13  6  1  1  0  0  0  0  0
#       5                18 21 23 16 23  3  0  0  0  0  0  0
      



## ----Q6---------------------------------------------------------------------------------------------------------------------------------------
pdf('workshop/figures/ex4_dotplots.pdf')
par(mfrow = c(2, 2))
dotchart(squid$DML, main = "DML")
dotchart(squid$weight, main = "weight")
dotchart(squid$nid.length, main = "nid length")
dotchart(squid$ovary.weight, main = "ovary weight")
dev.off()

# alternative code using dotplot function from lattice package
library(lattice)
dotplot(as.matrix(squid[,c("DML", "weight", "nid.length", "ovary.weight")]),
      groups=FALSE,
      strip = strip.custom(bg = 'white',
            par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col=1, cex  =0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")


## ----Q7---------------------------------------------------------------------------------------------------------------------------------------
which(squid$nid.length > 400)
# [1] 11

squid$nid.length[11]
# [1] 430.2

squid$nid.length[11] <- 43.2
squid$nid.length[11]
# [1] 43.2

dotchart(squid$nid.length, main = "nid length")


## ----Q8---------------------------------------------------------------------------------------------------------------------------------------
pdf('workshop/figures/ex4_hist.pdf')
par(mfrow = c(2,2))
hist(squid$DML, main="", xlab = "DML")
hist(squid$weight, main="", xlab = "weight")
hist(squid$eviscerate.weight, main="", xlab = "eviscerate weight")
hist(squid$ovary.weight, main="", xlab = "ovary weight")
dev.off()

# need to get the min and max values for DML
# to work out the limits for the breaks

summary(squid$DML)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   88     187     217     215     240     323 
 
# experimenting with different breaks
par(mfrow = c(2,2))
brk1 <- seq(from = 80, to = 340, by = 20)   
hist(squid$DML, xlab = "DML", breaks = brk1, main = "brk: 20")

brk2 <- seq(from = 80, to = 340, by = 10)   
hist(squid$DML, xlab = "DML", breaks = brk2, main = "brk: 10")

brk3 <- seq(from = 80, to = 340, by = 5)   
hist(squid$DML, xlab = "DML", breaks = brk3, main = "brk: 5")

brk4 <- seq(from = 80, to = 340, by = 2)   
hist(squid$DML, xlab = "DML", breaks = brk4, main = "brk: 2")


## ----Q9---------------------------------------------------------------------------------------------------------------------------------------
# clearly not linear
plot(squid$DML, squid$weight)

# natural log and sqrt tranform weight
squid$weight.sqrt <- sqrt(squid$weight)
squid$weight.log <- log(squid$weight)

par(mfrow = c(1,2))
plot(squid$DML, squid$weight.sqrt)
plot(squid$DML, squid$weight.log)

# the square root transformation looks
# most appropriate
jpeg('workshop/figures/ex4_transf_plot.jpeg')
plot(squid$DML, squid$weight.sqrt)
dev.off()

png('workshop/figures/ex4_transf_plot.png')
plot(squid$DML, squid$weight.sqrt)
dev.off()


## ----Q10, tidy = TRUE-------------------------------------------------------------------------------------------------------------------------
# note: Fmaturity is the recoded maturity.stage variable cerated in Q4
boxplot(DML ~ Fmaturity, data = squid, xlab = "maturity stage", ylab = "DML")

# violin plot
library(vioplot)
vioplot(DML ~ Fmaturity, data = squid, xlab = "maturity stage", ylab = "DML" , col = "lightblue")


## ----Q11--------------------------------------------------------------------------------------------------------------------------------------
coplot(DML ~ weight.sqrt | Fmaturity, data = squid)

# using xyplot from the lattice package
xyplot(DML ~ weight.sqrt | Fmaturity, data = squid)



## ----Q12, tidy = TRUE-------------------------------------------------------------------------------------------------------------------------
# vanilla pairs plot
pairs(squid[,c(5, 8, 9, 11, 12, 13)])

# customise the plot. You will need to define the
# panel.hist and panel.cor functions first. see the
# course manual or ?pairs help file

pairs(squid[,c(5, 8, 9, 11, 12, 13)], diag.panel = panel.hist, upper.panel = panel.cor,
			lower.panel = panel.smooth)


## ----Q13a, tidy = TRUE------------------------------------------------------------------------------------------------------------------------
# quick and dirty way
# need to transform ovary.weight first
squid$ovary.weight.sqrt <- sqrt(squid$ovary.weight)

# create the plot
with(squid, plot(DML, ovary.weight.sqrt, xlab = "DML (mm)", ylab = "square root ovary weight (g)", col = as.numeric(Fmaturity), xlim =     c(60, 350), ylim = c(0, 8.5)))

# create the legend
labs <- c("stage 1", "stage 2", "stage 3", "stage 4","stage 5")
cols <- as.numeric(levels(squid$Fmaturity))
legend("topleft", labs,col = cols, pch = 1)


## ----Q13b, tidy = TRUE------------------------------------------------------------------------------------------------------------------------
# longer but more control

# need to scales package to set transparency of points
# will the alpha function
library(scales)

#setup the axes but dont plot the points
with(squid, plot(DML, ovary.weight.sqrt, xlab = "DML (mm)", ylab = "square root ovary weight (g)",
					type = "n", xlim = c(60, 350), ylim = c(0, 8.5)))

# plot the points with custom colours
with(squid, points(DML[Fmaturity == "1"], ovary.weight.sqrt[Fmaturity == "1"], col = alpha("deepskyblue3", 0.7), pch = 16))
with(squid, points(DML[Fmaturity == "2"], ovary.weight.sqrt[Fmaturity == "2"], col = alpha("darkolivegreen3", 0.7), pch = 16))
with(squid, points(DML[Fmaturity == "3"], ovary.weight.sqrt[Fmaturity == "3"], col = alpha("coral3", 0.7), pch = 16))
with(squid, points(DML[Fmaturity == "4"], ovary.weight.sqrt[Fmaturity == "4"], col = alpha("lemonchiffon3", 0.7), pch = 16))
with(squid, points(DML[Fmaturity == "5"], ovary.weight.sqrt[Fmaturity == "5"], col = alpha("darkorchid3", 0.7), pch = 16))

# include the legend
labs <- c("stage 1", "stage 2", "stage 3", "stage 4","stage 5")
cols <- c("deepskyblue3", "darkolivegreen3", "coral3", "lemonchiffon3", "darkorchid3")
legend(55, 8.2, labs,col = alpha(cols, 0.7), pch = 16, bty = "n")

