# read the input into a table and take the first column
input <- read.table("~/Code/adventofcode2020/day2/input", sep=" ")

# question 1: the two numbers mean the min and max
# of the number of times the character in col2
# can appear in the third column
check_password <- function(col1, col2, col3) {
  # get min and max
  min.max <- unlist(strsplit(col1, "-"))
  min <- as.integer(min.max[1])
  max <- as.integer(min.max[2])
  
  # which character are we looking for
  char <- substr(col2,1,1)
  
  # how many times does it occur
  count <- sum(unlist(strsplit(col3,"")) == char)
  
  (min <= count) & (count <= max)
}

# question 2: the two numbers mean the positions
# where the character in col2 
# should and should not appear in the string col3
check_password2 <- function(col1, col2, col3) {
  # get a and b
  a.b <- unlist(strsplit(col1, "-"))
  a <- as.integer(a.b[1])
  b <- as.integer(a.b[2])
  
  # which character are we looking for
  char <- substr(col2,1,1)
  
  # how many times does it occur
  chars <- unlist(strsplit(col3, ""))
  
  # either it matches first and not second position
  # or second and not first position
  ( (chars[a] == char) & (chars[b] != char) | 
    (chars[a] != char) & (chars[b] == char) )
}

# Question1: How many valid passwords based on one criterion?
question1 <- sum(mapply(check_password, 
                        unlist(input[1]), 
                        unlist(input[2]), 
                        unlist(input[3])))

# Question2: How many valid passwords based on another criterion?
question2 <- sum(mapply(check_password2, 
                        unlist(input[1]), 
                        unlist(input[2]), 
                        unlist(input[3])))


