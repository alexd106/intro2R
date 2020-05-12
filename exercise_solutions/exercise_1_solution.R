## ----Q4--------------------------------------------------------------------------------------------------
help(mean)          # different methods of using help
?mean
help("mean")


## ----Q5--------------------------------------------------------------------------------------------------
plot(1:10)    #dont worry about what 1:10 does just yet


## ----Q6--------------------------------------------------------------------------------------------------
first_num <- 42    # create variable first_num and assign the value 42
first_char <- "my first character"


## ----Q7--------------------------------------------------------------------------------------------------
rm(first_num)
ls()          # list all variables in the workspace 


## ----Q8--------------------------------------------------------------------------------------------------
first_char <- "my second variable"
first_char    # display the value 


## ----Q11-------------------------------------------------------------------------------------------------
apropos("plot")
help('plot.design')


## ----Q12-------------------------------------------------------------------------------------------------
help.search("plot")
??plot     # shortcut for help.search dunction

help.search("plot", package = "nlme")


## ----Q13-------------------------------------------------------------------------------------------------
getwd()    # displays the current working directory 


## ----Q14-------------------------------------------------------------------------------------------------
dir.create(path = 'data')
dir.create(path = 'data')
list.files(include.dirs = TRUE)

