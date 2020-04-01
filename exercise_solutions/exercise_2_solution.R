## ----Q2, results = 'asis'---------------------------------------------------------------------------------------------------------
log(12.43)              # natural log
log10(12.43)            # log to base 10
log2(12.43)             # log to base 2
log(12.43, base = 2)    # alternative log to base 2
sqrt(12.43)             # square root
exp(12.43)              # exponent


## ----Q3, , results = 'asis'-------------------------------------------------------------------------------------------------------
area.circle <- pi * (20/2)^2


## ----Q4---------------------------------------------------------------------------------------------------------------------------
(14 * 0.51)^(1/3)


## ----Q5---------------------------------------------------------------------------------------------------------------------------
weight <- c(69, 62, 57, 59, 59, 64, 56, 66, 67, 66)


## ----Q6, results = 'asis'---------------------------------------------------------------------------------------------------------
mean(weight)                                # calculate mean 
var(weight)                                 # calculate variance
sd(weight)                                  # calculate standard deviation
range(weight)                               # range of weight values
length(weight)                              # number of observations

first.five <- weights[1:5]                  # extract first 5 weight values
first.five <- weights[c(1, 2, 3, 4, 5)]     # alternative method


## ----Q7, results = 'asis'---------------------------------------------------------------------------------------------------------
height <- c(112, 102, 83, 84, 99, 90, 77, 112, 133, 112)

summary(height)   # summary statistics of height variable

some.child <- height[c(2, 3, 9, 10)]      # extract the 2nd, 3rd, 9th, 10th height

shorter.child <- height[height <= 99]     # extract all heights less than or equal to 99


## ----Q8---------------------------------------------------------------------------------------------------------------------------
bmi <- weight/(height/100)^2    # don't forget to convert height to meters


## ----Q9---------------------------------------------------------------------------------------------------------------------------
seq1 <- seq(from = 0, to = 1, by = 0.1)


## ----Q10--------------------------------------------------------------------------------------------------------------------------
seq2 <- rev(seq(from = 1, to = 10, by = 0.5))


## ----Q11--------------------------------------------------------------------------------------------------------------------------
rep(1:3, times = 3)
rep(c("a", "c", "e", "g"), each = 3)
rep(c("a", "c", "e", "g"), times = 3)
rep(1:3, each = 3, times = 2)
rep(1:5, times = 5:1)
rep(c(7, 2, 8, 1), times = c(4, 3, 1, 5))


## ----Q12--------------------------------------------------------------------------------------------------------------------------
height.sorted <- sort(height)

height.rev <- rev(sort(height))



## ----Q13--------------------------------------------------------------------------------------------------------------------------
child.names <- c("Alfred", "Barbara", "James", "Jane", "John", "Judy", "Louise", "Mary", "Ronald", "William")


## ----Q14--------------------------------------------------------------------------------------------------------------------------
height.ord <- order(height)   # get the indexes of the heights, smallest to tallest
names.sort <- child.names[height.ord]     # Louise is shortest, Ronald is tallest


## ----Q15--------------------------------------------------------------------------------------------------------------------------
weight.ord <- rev(order(weight))
weight.rev <- child.names[weight.ord]     # Alfred is heaviest, Louise is lightest


## ----Q16--------------------------------------------------------------------------------------------------------------------------
mydata <- c(2, 4, 1, 6, 8, 5, NA, 4, 7)
mean(mydata)    # returns NA!

mean(mydata, na.rm = TRUE)    # returns 4.625


## ----Q17--------------------------------------------------------------------------------------------------------------------------
ls()          # list all variables in workspace
rm(seq1)      # remove variable seq1 from the workspace
ls()          # check seq1 has been removed

