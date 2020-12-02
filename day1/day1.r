# read the input into a table and take the first column
input <- read.table("~/Code/adventofcode2020/day1/input")
numbers <- input[,1]

# first part: which two numbers add to an arbitary sum in a given vector?
solution1 <- function(target.vector, target.sum) {
  # which elements, summed with an arbitrary integer, equal a target sum
  present <- function(x) any(target.vector + x == target.sum)
  
  # check the vector using each element
  indices <- lapply(target.vector, present)
  
  # recover the rows
  target.vector[indices == TRUE]
}

answer1 <- solution1(numbers, 2020)
print(paste("Answers:",  paste(answer1, collapse=" ")))
print(paste("Product: ", answer1[1] * answer1[2]))

# second part: which three numbers add to 2020?
# we divide this into 200 subproblems: which two numbers add to 2020 - X
subproblems <- lapply(numbers, function(x) solution1(numbers, 2020-x))

# which ones have solutions?
indices2 <- which(lapply(subproblems,length) > 0)

# recover the numbers
answer2 <- numbers[indices2]

print(paste("Answers:",  paste(answer2, collapse=" ")))
print(paste("Product: ", answer2[1] * answer2[2] * answer2[3]))



