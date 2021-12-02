#######################
##                   ##
##   AOC 2021 - 02   ##
##    Linh Nguyen    ##
##                   ##
#######################

# read in data
dat02 <- read.table(file = "2021-02.txt")

#### ---- Part 1 ---- ####

# Calculate the horizontal position and depth you would have 
#     after following the planned course. 
#     What do you get if you multiply your final horizontal position 
#     by your final depth?

pos <- depth <- 0
for (i in 1:nrow(dat02)) {
  if (dat02[i, 1] == "forward") {
    pos <- pos + dat02[i, 2]
  } else if (dat02[i, 1] == "down") {
    depth <- depth + dat02[i, 2]
  } else {
    depth <- depth - dat02[i, 2]
  }
}
pos * depth

#### ---- Part 2 ---- ####

# Using this new interpretation of the commands, 
#     calculate the horizontal position and depth you would have 
#     after following the planned course. 
#     What do you get if you multiply your final horizontal position 
#     by your final depth?

pos <- depth <- aim <- 0
for (i in 1:nrow(dat02)) {
  if (dat02[i, 1] == "down") {
    aim <- aim + dat02[i, 2]
  } else if (dat02[i, 1] == "up") {
    aim <- aim - dat02[i, 2]
  } else {
    pos <- pos + dat02[i, 2]
    depth <- depth + aim * dat02[i, 2]
  }
}
pos * depth