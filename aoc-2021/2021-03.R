#######################
##                   ##
##   AOC 2021 - 03   ##
##    Linh Nguyen    ##
##                   ##
#######################

# read in data
dat03 <- read.table(file = "2021-03.txt", colClasses = "character")$V1

#### ---- Part 1 ---- ####
# Use the binary numbers in your diagnostic report to calculate 
#     the gamma rate and epsilon rate, then multiply them together. 
#     What is the power consumption of the submarine? 
#     (Be sure to represent your answer in decimal, not binary.)

# gamma and epsilon
# most and least common
gammas <- epsilons <- numeric(length = nchar(dat03[1]))
for(i in 1:nchar(dat03[1])) {
  num_list <- c()
  for(k in 1:length(dat03)) {
    num_list <- append(num_list, as.numeric(substr(dat03[k], i, i)))
  }
  gammas[i] <- as.numeric(mean(num_list) > 0.5)
  epsilons[i] <- as.numeric(gammas[i] == 0)
}
gamma <- paste(gammas, collapse = '')
gamma <- strtoi(gamma, base = 2)

epsilon <- paste(epsilons, collapse = '')
epsilon <- strtoi(epsilon, base = 2)

# answer
gamma * epsilon

#### ---- Part 2 ---- #### 
# Use the binary numbers in your diagnostic report 
#     to calculate the oxygen generator rating and CO2 scrubber rating, 
#     then multiply them together. 
#     What is the life support rating of the submarine? 
#     (Be sure to represent your answer in decimal, not binary.)

# initialize data frame
o2dat <- co2dat <- dat03

# find most common values
for(i in 1:nchar(o2dat[1])) {
  num_list <- c()
  for(k in 1:length(o2dat)) {
    num_list <- append(num_list, as.numeric(substr(o2dat[k], i, i)))
  }
  o2 <- as.numeric(mean(num_list) >= 0.5)
  
  # update data
  selected_idx <- which(num_list == o2)
  o2dat <- o2dat[selected_idx]
  
  # check for stopping condition
  if (length(selected_idx) == 1) {
    break
  }
} 

# find least common values
for(i in 1:nchar(co2dat[1])) {
  num_list <- c()
  for(k in 1:length(co2dat)) {
    num_list <- append(num_list, as.numeric(substr(co2dat[k], i, i)))
  }
  co2 <- as.numeric(mean(num_list) < 0.5)
  
  # update data
  selected_idx <- which(num_list == co2)
  co2dat <- co2dat[selected_idx]
  
  # check for stopping condition
  if (length(selected_idx) == 1) {
    break
  }
} 

# answer 
strtoi(o2dat, base = 2) * strtoi(co2dat, base = 2)