#######################
##                   ##
##   AOC 2021 - 06   ##
##    Linh Nguyen    ##
##                   ##
#######################

# read in data 
dat05 <- read.table(file = "2021-06.txt")
dat05 <- unlist(lapply(strsplit(dat05$V1, ","),
                       FUN = function(x) as.numeric(x)))

#### ---- Part 1 ---- ####
# Find a way to simulate lanternfish. 
#     How many lanternfish would there be after 80 days?

# loop through 80 days
for (i in 1:80) {
  dat05 <- dat05 - 1
  baby <- length(dat05[which(dat05 == -1)])
  dat05 <- append(dat05, rep(8, times = baby))
  dat05[which(dat05 == -1)] <- 6
}

# count final number of fish
length(dat05)

#### ---- Part 2 ---- ####
# How many lanternfish would there be after 256 days?

# count for each age
count_0 <- count_1 <- count_2 <- count_3 <- count_4 <- count_5 <- count_6 <- count_7 <- count_8 <- numeric(length = 257)
count_0[1] <- length(dat05[which(dat05 == 0)])
count_1[1] <- length(dat05[which(dat05 == 1)])
count_2[1] <- length(dat05[which(dat05 == 2)])
count_3[1] <- length(dat05[which(dat05 == 3)])
count_4[1] <- length(dat05[which(dat05 == 4)])
count_5[1] <- length(dat05[which(dat05 == 5)])
count_6[1] <- length(dat05[which(dat05 == 6)])
count_7[1] <- length(dat05[which(dat05 == 7)])
count_8[1] <- length(dat05[which(dat05 == 8)])

# increase each day
for (i in 1:256){
  count_0[i+1] <- count_1[i]
  count_1[i+1] <- count_2[i]
  count_2[i+1] <- count_3[i]
  count_3[i+1] <- count_4[i]
  count_4[i+1] <- count_5[i]
  count_5[i+1] <- count_6[i]
  count_6[i+1] <- count_7[i] + count_0[i]
  count_7[i+1] <- count_8[i]
  count_8[i+1] <- count_0[i]
}

# sum after 256th day
sum(count_0[257], count_1[257], count_2[257], count_3[257], count_4[257], 
    count_5[257], count_6[257], count_7[257], count_8[257])
