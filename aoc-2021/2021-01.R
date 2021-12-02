#######################
##                   ##
##   AOC 2021 - 01   ##
##    Linh Nguyen    ##
##                   ##
#######################

# read in data
dat01 <- read.table(file = "2021-01.txt")

#### ---- Part 1 ---- ####

# How many measurements are larger than the previous measurement?
increases <- 0
for (i in 2:nrow(dat01)) {
  if (dat01[i,] > dat01[i-1,]) {
    increases <- increases + 1
  }
}
increases

#### ---- Part 2 ---- ####

# Consider sums of a three-measurement sliding window. 
#    How many sums are larger than the previous sum?

increases_sum3 <- 0
for (i in 2:(nrow(dat01) - 2)){
  if (dat01[i+2,] > dat01[i-1,]) {
    increases_sum3 <- increases_sum3 + 1
  }
}
increases_sum3