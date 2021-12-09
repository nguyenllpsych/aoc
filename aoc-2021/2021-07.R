#######################
##                   ##
##   AOC 2021 - 07   ##
##    Linh Nguyen    ##
##                   ##
#######################

# read in data
dat07 <- unlist(lapply(strsplit(read.table(file = "2021-07.txt")$V1, ","),
                FUN = function(x) as.numeric(x)))

#### ---- Part 1 ---- ####

# median of data
median <- median(dat07)

# distance to mean
sum(abs(dat07 - median))

#### ---- Part 2 ---- ####

sum_list <- vector(mode = "numeric", length = max(dat07) - min(dat07) + 1)
idx <- 0

# loop through all possible entries
for (i in min(dat07):max(dat07)){
  
  idx <- idx + 1
  dist <- vector(mode = "numeric", length = length(dat07))
  
  # loop through all actual entries
  for (j in seq(length = length(dat07))){
    
    # distance between each point and proposed destination
    dist[j] <- sum(seq(from = 1, to = abs(dat07[j]-i))
                   * rep(1, times = abs(dat07[j]-i))) 
  }
  
  # sum_list for this proposed destination
  sum_list[idx] <- sum(dist)
}

# minimum distance
min(sum_list)
