##  write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## set the value of x with default of empty matrix
  inv <- NULL                               ## set value of inv as NULL
  set <- function(y) {                      ## create the set function as mutator for matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                       ## create the get function as an accessor for the matrix 
  setinverse <- function(invert) inv <<- solve  ## mutator for inverted matrix
  getinverse <- function() inv              ## accessor for inverted matrix
  list(set = set, get = get,                ## pass accessors and mutators out of function
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()                     ## get current value of inverse matrix
  if(!is.null(inv)) {                       ## check to see if it exists 
    message("getting cached data")          ## notify user that inverse exists
    inv                                     ## return inverse
  }
  data <- x$get()                           ## inv does not exist so get the matrix
  inv <- solve(data, ...)                   ## calculate the inverse
  x$setinverse(inv)                         ## store inverse
  inv                                       ## return inverse
}
