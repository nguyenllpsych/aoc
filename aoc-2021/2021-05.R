#######################
##                   ##
##   AOC 2021 - 05   ##
##    Linh Nguyen    ##
##                   ##
#######################

# read in data
dat05 <- read.table(file = "2021-05.txt")
x1 <- unlist(lapply(strsplit(dat05$V1, ","), FUN = function(x){as.numeric(x[1])}))
y1 <- unlist(lapply(strsplit(dat05$V1, ","), FUN = function(x){as.numeric(x[2])}))
x2 <- unlist(lapply(strsplit(dat05$V3, ","), FUN = function(x){as.numeric(x[1])}))
y2 <- unlist(lapply(strsplit(dat05$V3, ","), FUN = function(x){as.numeric(x[2])}))

#### ---- Part 1 ---- ####
# Consider only horizontal and vertical lines. 
#     At how many points do at least two lines overlap?

# list to store points
points <- c()

# loop through 500 lines and log down which points are covered
for (i in 1:nrow(dat05)) {
  
  # move through y if x1=x2
  if (x1[i] == x2[i]) {
    points <- append(points,
                     paste0(x1[i], ",", seq(from = y1[i], to = y2[i])))
  } else if (y1[i] == y2[i]) {
    # move through x if y1=y2
    points <- append(points, 
                     paste0(seq(from = x1[i], to = x2[i]), ",", y1[i]))
  }
}

# count points with freq >= 2
freq <- as.data.frame(table(points))
length(freq[which(freq$Freq >= 2), ]$points)

#### ---- Part 2 ---- ####
# Consider all of the lines (horizontal, vertical, 45-degree diagonal)
#     At how many points do at least two lines overlap?

# list to store points
points <- c()

# loop through 500 lines and log down which points are covered
for (i in 1:nrow(dat05)) {
  
  # move through y if x1=x2
  if (x1[i] == x2[i]) {
    points <- append(points,
                     paste0(x1[i], ",", seq(from = y1[i], to = y2[i])))
  } else if (y1[i] == y2[i]) {
    # move through x if y1=y2
    points <- append(points, 
                     paste0(seq(from = x1[i], to = x2[i]), ",", y1[i]))
  } else if (abs(x2[i] - x1[i]) == abs(y2[i] - y1[i])) {
    # move diagonally
    points <- append(points,
                     paste0(seq(from = x1[i], to = x2[i]), ",",
                            seq(from = y1[i], to = y2[i])))
  }
}

# count points with freq >= 2
freq <- as.data.frame(table(points))
length(freq[which(freq$Freq >= 2), ]$points)
