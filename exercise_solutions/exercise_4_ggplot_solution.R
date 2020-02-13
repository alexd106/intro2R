## ----packages------------------------------------------------------------------------------------------------------------------------------------

# make ggplot2 and gridExtra packages available
# Note: you may need to install these packages first
# Use : install.packages('ggplot2', dep = TRUE)
# Use : install.packages('gridExtra', dep = TRUE)

library(ggplot2)
library(gridExtra)


## ----Q4, results = 'asis'------------------------------------------------------------------------------------------------------------------------
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
 


## ----Q5------------------------------------------------------------------------------------------------------------------------------------------
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
      



## ----Q6------------------------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(data = squid) + 
  geom_point(aes(x = DML, y = as.numeric(rownames(squid)))) +
  ylab("")
p1

p2 <- ggplot(data = squid) +
  geom_point(aes(x = weight, y = as.numeric(rownames(squid)))) +
  ylab("") 
p2

p3 <- ggplot(data = squid) +
  geom_point(aes(x = nid.length, y = as.numeric(rownames(squid)))) +
  ylab("")
p3

p4 <- ggplot(data = squid) +
  geom_point(aes(x = ovary.weight, y = as.numeric(rownames(squid)))) +
  ylab("")
p4

# arrange all 4 plots in a single
# graphics device

pall <- grid.arrange(p1, p2, p3, p4, nrow = 2)
pall
# save the plot 

ggsave('workshop/figures/gg_dotplot.pdf', plot = pall, device = 'pdf')


## ----Q7------------------------------------------------------------------------------------------------------------------------------------------
which(squid$nid.length > 400)
# [1] 11

squid$nid.length[11]
# [1] 430.2

squid$nid.length[11] <- 43.2
squid$nid.length[11]
# [1] 43.2

ggplot(data = squid) +
  geom_point(aes(x = nid.length, y = as.numeric(rownames(squid)))) +
  ylab("")


## ----Q8------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = squid) +
  geom_histogram(aes(DML))

ggplot(data = squid) +
 geom_histogram(aes(weight))
  
ggplot(data = squid) +
 geom_histogram(aes(eviscerate.weight))

ggplot(data = squid) +
  geom_histogram(aes(ovary.weight)) 

# experimenting with different breaks
bp1 <- ggplot(data = squid) +
          geom_histogram(aes(DML), bins = 20)
bp1

bp2 <- ggplot(data = squid) +
          geom_histogram(aes(DML), bins = 10) 
bp2

bp3 <- ggplot(data = squid) +
          geom_histogram(aes(DML), bins = 5)
bp3

bp4 <- ggplot(data = squid) +
          geom_histogram(aes(DML), bins = 2)
bp4

bpall <- grid.arrange(bp1, bp2, bp3, bp4, nrow = 2)

# save the plot to file
ggsave('workshop/figures/gg_hist.pdf', plot = bpall, device = 'pdf')



## ----Q9------------------------------------------------------------------------------------------------------------------------------------------
# clearly not linear
# also note use of the classic theme
ggplot(data = squid) +
  geom_point(aes(x = DML, y = weight)) +
  theme_classic()

# natural log and sqrt tranform weight
squid$weight.sqrt <- sqrt(squid$weight)
squid$weight.log <- log(squid$weight)

ggplot(data = squid) +
  geom_point(aes(x = DML, y = weight.log)) +
  theme_classic()

ggplot(data = squid) +
  geom_point(aes(x = DML, y = weight.sqrt)) +
  theme_classic()

# the square root transformation looks
# most appropriate

# save plot as jpeg
ggsave('workshop/figures/gg_xy_weight.jpeg', device = 'jpeg')

# save plot as png
ggsave('workshop/figures/gg_xy_weight.png', device = 'png')


## ----Q10, tidy = TRUE----------------------------------------------------------------------------------------------------------------------------
# note: Fmaturity is the recoded maturity.stage variable cerated in Q4
ggplot(data = squid) +
  geom_boxplot(aes(x = Fmaturity, y = DML)) +
  theme_classic() +
  xlab("Maturity stage") +
  ylab("Dorsal mantel length")

# violin plot
ggplot(data = squid) +
  geom_violin(aes(x = Fmaturity, y = DML))


## ----Q11-----------------------------------------------------------------------------------------------------------------------------------------
# use facet_wrap to produce a 
# panel plot
ggplot(data = squid) +
  geom_point(aes(x = DML, y = weight.sqrt)) +
  facet_wrap(~Fmaturity)



## ----Q12, tidy = TRUE----------------------------------------------------------------------------------------------------------------------------
library(GGally)

ggpairs(squid[, c(5, 8, 9, 11, 12, 13)])


## ----Q13a, tidy = TRUE---------------------------------------------------------------------------------------------------------------------------
# square root transform ovary weight
squid$ovary.weight.sqrt <- sqrt(squid$ovary.weight)
ggplot(data = squid) +
  geom_point(aes(x = DML, y = ovary.weight.sqrt, colour = Fmaturity), alpha = 0.8, size = 2) +
  scale_colour_manual(values = c("deepskyblue3", "darkolivegreen3", "coral3", "lemonchiffon3", "darkorchid3"),
                  labels = c("stage 1", "stage 2", "stage 3", "stage 4", "stage 5")) +
 theme_classic(base_size = 12) +
  labs(colour = "", x = "DML (mm)", y = "square root ovary weight (g)")

# OR
ggplot(data = squid) +
  geom_point(aes(x = DML, y = ovary.weight.sqrt, colour = Fmaturity), alpha = 0.8, size = 2) +
  theme_classic() +
  labs(colour = "", x = "DML (mm)", y = "square root ovary weight (g)")


browseURL("https://giphy.com/gifs/RyXVu4ZW454IM/fullscreen", browser = getOption("browser"), encodeIfNeeded = FALSE)

