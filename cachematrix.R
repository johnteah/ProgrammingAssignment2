## This function allows for the creation and calling of a cache of 
## the inverse of a matrix.

## Creates a set of four functions to set and call a matric and set and call 
## inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) I <<- inv
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function will check for a cached invese of a given matrix and return it
## if no cached value is stored, then the inverse is calculated, called  and
## set to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I  
}
