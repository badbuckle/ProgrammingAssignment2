## This functions calculates and stores the inverse of a matrix
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #before the first calculation the matrix m is empty
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    
  m <- x$getInverse()
  
  ## checks if the inverse has been alreadz calculated 
  ## if yes, then it returns the stored result
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #gets the data
  data <- x$get()
  #calcualtes the inverse matrix
  # udner the assumption that the matrix is inversible
  # solve calculates the inverse of the matrix
  m <- solve(data, ...)
  
  #stores the inverse matrix using the setter
  x$setInverse(m)
  
  #delivers the result
  m
  
  
}
