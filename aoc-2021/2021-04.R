#######################
##                   ##
##   AOC 2021 - 04   ##
##    Linh Nguyen    ##
##                   ##
#######################

# read in data
dat04_board <- read.table(file = "2021-04.txt", skip = 1)
dat04_call  <- t(read.table(file = "2021-04.txt", nrows = 1, sep = ","))

#### ---- Part 1 ---- ####
# To guarantee victory against the giant squid, 
#     figure out which board will win first. 
#     What will your final score be if you choose that board?

# initialize search conditions
searching <- TRUE
call <- 0 # loop through called numbers in dat04_call
winning_row <- winning_col <- 0

while (searching) {
  
  # call counter
  call <- call + 1
  
  # change marked numbers to NA
  dat04_board[dat04_board == dat04_call[call]] <- NA
  
  # check winning conditions row-wise
  for (r in 1:nrow(dat04_board)) {
    if (sum(is.na(dat04_board[r, ])) == 5) {
      
      # stop search
      searching <- FALSE
      
      # output winning row
      winning_row <- r
    }
  } # END for r LOOP
  
  # check winning conditions col-wise
  for (r in seq(from = 1, to = nrow(dat04_board), by = 5)) {
    for (c in 1:ncol(dat04_board)) {
      if(sum(is.na(dat04_board[c(seq(from = r, to = r + 4)), c])) == 5) {
        
        # stop search
        searching <- FALSE
        
        # output winning row (start of board)
        winning_col <- c
        winning_row <- r
      }
    } # END for c LOOP
  } # END for r LOOP
} # END while searching LOOP

# winning row is 114 row-wise
# this belongs to board number 23
ceiling(winning_row/5)

# which is 4 rows after the beginning of the board
winning_row %% 5

# winning board
winning_board <- dat04_board[c(seq(from = winning_row - 3, to = winning_row + 1)), ]

# final score = sum of unmarked numbers in this board * last call
sum(winning_board, na.rm = T) * dat04_call[call]

#### ---- Part 2 ---- ####
# Figure out which board will win last. 
#     Once it wins, what would its final score be?

# modified searching condition above to stop once 10 rows are left
# remove each board after winning
call <- 0 # loop through called numbers in dat04_call
current_board <- dat04_board # modify board after each win

while (nrow(current_board) > 10) {

  winning_row <- c()

  # call counter
  call <- call + 1
  
  # change marked numbers to NA
  current_board[current_board == dat04_call[call]] <- NA
  
  # check winning conditions row-wise
  for (r in 1:nrow(current_board)) {
    if (sum(is.na(current_board[r, ])) == 5) {
      
      # append rows from winning board
      if (r %% 5 == 0) {
        winning_row <- append(winning_row, seq(from = r - 4,
                                               to = r))
      } else {
        winning_row <- append(winning_row, seq(from = r - (r %% 5 - 1),
                                               to = r - (r %% 5 - 1) + 4))
      }
    }
  } # END for r LOOP
  
  # check winning conditions col-wise
  for (r in seq(from = 1, to = nrow(current_board), by = 5)) {
    for (c in 1:ncol(current_board)) {
      if(sum(is.na(current_board[c(seq(from = r, to = r + 4)), c])) == 5) {
        
        # append rows from winning board
        winning_row <- append(winning_row, seq(from = r,
                                               to = r + 4))
      }
    } # END for c LOOP
  } # END for r LOOP

  # remove winning board from game if there is a winning row
  if (length(winning_row) > 0) {
    current_board <- current_board[-winning_row, ]
  }
} # END while nrow LOOP

# last two boards
current_board

# last board has not won yet
# continue with count
last_call <- call
searching <- TRUE
while (searching) {

  # call counter
  last_call <- last_call + 1
  
  # change marked numbers to NA
  current_board[current_board == dat04_call[last_call]] <- NA

  # check winning conditions row-wise
  for (r in 1:nrow(current_board)) {
    if (sum(is.na(current_board[r, ])) == 5) {
      
      # stop search
      searching <- FALSE      
      
      # rows for winning board
      if (r %% 5 == 0) {
        last_row <- seq(from = r - 4,
                        to = r)
      } else {
        last_row <- seq(from = r - (r %% 5 - 1),
                        to = r - (r %% 5 - 1) + 4)
      }
    }
  } # END for r LOOP
  
  # check winning conditions col-wise
  for (r in seq(from = 1, to = nrow(current_board), by = 5)) {
    for (c in 1:ncol(current_board)) {
      if(sum(is.na(current_board[c(seq(from = r, to = r + 4)), c])) == 5) {
        
        # stop search
        searching <- FALSE      
        
        # rows for winning board
        last_row <- seq(from = r, to = r + 4)
      }
    } # END for c LOOP
  } # END for r LOOP
} # END while searching LOOP

# final board to win 
current_board[last_row, ]

# final score = sum of unmarked numbers in this board * last call
sum(current_board[last_row, ], na.rm = T) * dat04_call[last_call]
