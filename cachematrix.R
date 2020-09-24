## Below are two functions that are used to create a special object that 
## stores a numeric matrix and cache's its inverse

## The function makeCacheMatrix creates a special "matrix", which is really 
## a data frame containing a function to: set the value of the matrix, 
## get the value of the matrix, set the value of the inverse of the matrix, 
## get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    }

  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## matrix has already been inversed. If so, it gets the inverse matrix 
## from the cache and skips the computation. Otherwise, it inverses the 
## matrix and sets the value of the inversed matrix in the cache 
## via the setmatrix function

cacheSolve <- function(x, ...) {
  m <- getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- get()
  m <- solve (data, ...)
  setmatrix(m)
  m
}