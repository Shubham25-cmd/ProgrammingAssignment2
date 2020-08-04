## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Setting the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Getting the matrix
  get <- function() x
  ## Setting the inverse
  setinverse <- function(inverse) m <<- inverse
  ## Getting the inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse
  ## Getting the inverse matrix if already calculated
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  ## Obtaining the matrix 
  data <- x$get()
  ## Matrix inverse calculation
  m <- solve(data) %*% data
  ## Setting the inverse of the matrix
  x$setinverse(m)
  ## Returning the matrix
  m
}
