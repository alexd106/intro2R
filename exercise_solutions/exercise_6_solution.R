## ----Q1, results = 'asis'------------------------------------------------------------------------------------------------------------------------

# area of a circle
# the equation to calculate the area of a circle is pi * radius^2

circle.area <- function(d){ 
	pi * (d/2)^2
}

# to use your new function

circle.area(10)
# [1] 78.53982

# to test on a vector of diameters
# first create a vector with diameters ranging from 0 to 50 in steps of 10

cir.diam <- seq(from = 0, to = 50, by = 10)

# test your function

circle.area(cir.diam)
# [1]    0.00000   78.53982  314.15927  706.85835 1256.63706 1963.49541


## ----Q2, tidy = TRUE-----------------------------------------------------------------------------------------------------------------------------

far.cent <- function(a){
	val <- (a-32)*5/9
	print(paste("Fahrenheit: ", round(a, digits = 3), "oF",sep = " "), quote = FALSE)# round 3dp
	print(paste("Centigrade: ", round(val, digits = 3), "oC", sep = " "), quote = FALSE)# round 3dp
}

# alternative Fahrenheit to centigrade using cat function

far.cent2 <- function(a){
	val <- (a - 32) * 5/9	#calculation
	cat("Fahrenheit: ", round(a, digits = 3), "oF", "\n")   # use cat function
	cat("Centigrade: ", round(val, digits = 3), "oC", "\n")   
}



## ----Q3------------------------------------------------------------------------------------------------------------------------------------------

# Create a vector of normally distributed data
# length 100, mean 35 and standard deviation of 29

vals <- rnorm(100, 35, 15)	# create some norm dist data mean 35, sd = 15

summary.fun <- function(dat){
	mymean <- round(mean(dat), digits = 4)      	# calc mean
	mymedian <- round(median(dat), digits = 4)    # calc median
	mymin <- round(min(dat), digits = 4)          # calc min
	mymax <- round(max(dat), digits = 4)          # calc max
	print(paste("mean:", mymean, sep = " "), quote = FALSE)     # print mean
	print(paste("median:", mymedian, sep = " "), quote = FALSE)     # print median
	print(paste("range:", "from:", mymin, "to", mymax, sep = " "), quote = FALSE) 
	dens <- density(dat)                          # estimate density curve
	hist(dat, main = "",type = "l",freq = FALSE)  # plot histogram
	lines(dens, lty = 1, col = "red")             # plot density curve
}

# use the function
summary.fun(vals)


## ----Q4------------------------------------------------------------------------------------------------------------------------------------------

# calculate a median

ourmedian <- function(x){
	n <- length(x)
	if (n %% 2 == 1)      # odd numbers
	  sort(x)[(n + 1)/2]  # find the middle number by adding 1 to length and div 2
	else {                # even numbers
	  middletwo <- sort(x)[(n/2) + 0:1]   #find the two middle numbers
	  mean(middletwo)
	  }
}

# use the function
mydat <- sample(1:50, size = 10, replace = TRUE )

# our function
ourmedian(mydat)

# R median function
median(mydat)


## ----Q5, tidy = TRUE-----------------------------------------------------------------------------------------------------------------------------

# function to simulate Ricker model

Ricker.model <- function(nzero, r, time, K=1){     # sets initial parameters
  N <- numeric(time + 1)    # creates a real vector of length time+1 to store values of Nt+1
  N[1] <- nzero             # sets initial population size in first element of N
  for (i in 1:time) {				# loops over time
      N[i+1] <- N[i]*exp(r*(1 - N[i]/K))    
  }
  Time <- 0:time   					 # creates vector for x axis
  plot(Time, N, type = "o", xlim = c(0, time), xlab = "Time", ylab = "Population size (N)", main = paste("r =", r, sep=" "))   # plots     output
}

# To run
# play around with the different parameters, especially r
Ricker.model(nzero=0.1,r=1,time=100)


