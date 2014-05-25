## A function that makes a list of functions
## which: set a matrix, prints the matrix, solves the matrix,
## and prints the solved matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Create a subfunction that sets the argument as 
    ## a matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Create a subfunction that prints the matrix
    get <- function() x
    ## Create a subfunction that solves the matrix 
    ## and store that result as a variable in the parent environment
    solveInverse <- function(solve) m <<- solve
    ## Create a subfunction that prints the solved matrix
    getInverse <- function() m
    ## Create a list of the previously defined functions
    list(set = set, get = get,
         solveInverse = solveInverse,
         getInverse = getInverse)
}


## A function that takes the information stored by
## the previous function which checks if the stored matrix
## alreay has a solution, then prints the solution if it 
## already exists, and solves it if it doesn't

cacheSolve <- function(x, ...) {
  ## Create a variable that stores the solved matrix, 
  ## if it exists
  m <- x$getInverse()
  ## Check to see if the variable has the solution
  if(!is.null(m)) {
    ## Print the solved matrix, if it already exists
    message("getting cached data")
    return(m)
  }
  ## Solve the matrix and print the solution 
  ## if it doesn't already exist
  data <- x$get()
  m <- solve(data, ...)
  x$solveInverse(m)
  m
}
