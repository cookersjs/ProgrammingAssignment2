## Coursera R Programming -- Assignment 2
## By: Justin Cook

## makeCacheMatrix accepts a (preferably) invertible matrix object.
## When first initialized, it assigns the matrix (x) to the function's environment.
## 'set' -> Re-assign x to any value (even if it's not a matrix)
## 'get' -> Return the current value of x
## 'setInverse' -> Assign the incoming inverse matrix to m
## 'getInverse' -> Return the current value of m

makeCacheMatrix <- function(x = matrix()) {

  rowCount = nrow(x)
  colCount = ncol(x)
  # Won't assign matrix unless it's invertible
  if (rowCount / colCount != 1) {
    print("Please submit a matrix with same number of rows and columns")
    return()
  }
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve accepts a 'makeCacheMatrix' environment/function as it's argument.
## First, it attempts to get the value of 'getInverse' in 'makeCacheMatrix'.
## If this value is not NULL, it has already been created and stored in the cache,
## and returns this.
## Otherwise, it will get the matrix stored in 'makeCacheMatrix', invert it (using solve),
## and then store it in the cache using the 'setInverse' function.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
  if(!is.null(m)) {
    message("Data in cache -- Retrieving!")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
