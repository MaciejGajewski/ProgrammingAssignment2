## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a functions to
## set the value of the vector
## get the value of the vector
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function calculates the inverse matrix 
## of the special "vector" created with the above function. 
# However, it first checks to see if the inverse matrix
## has already been calculated. If so, it gets the value 
## from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data 
## and sets the value of the inverse matrix in the cache 
## via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix  
}
