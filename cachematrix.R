## Functions to cache matrix inversion.
##
## These functions provide a convenient interface
## to access and cache the inverse of a matrix.
##
## Example usage:
##
##   # Create an object to store the matrix and cache its inverse
##   cacheableMatrix = makeCacheMatrix(matrix(rnorm(160000),400,400))
##
##   # The first call to cacheSolve will calculate the inverse,
##   inv = cacheSolve(cacheableMatrix)
##
##   # Further calls to cacheSolve will be significantly faster, since
##   # they return the value from cache caculated during the previous call.



## makeCacheMatrix takes in a matrix as parameter 
## and return a list of 4 functions: 
## set: sets the matrix
## get: returns the matrix
## setinverse: sets the inverse matrix
## getinverse: gets the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize inverse matrix to NULL
  i <- NULL
  
  # Function to set the matrix value, and reinitialize its inverse as the matrix changed.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Function to get the stored x matrix
  get <- function() x
  
  # Function to set the cached inverse matrix
  setinverse <- function(solve) i <<- solve
  
  # Function to get the cached inverse matrix
  getinverse <- function() i
  
  # Return a list of the 4 functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cachesolve will return the inverse of the matrix x passed as parameter.
## If the inverse of the matrix x has already been calculated it will be returned from cache,
## otherwise it will be calculated and stored in the cache for a potential later reuse.
cacheSolve <- function(x, ...) {
  
  # Get the matrix inverse from cache
  i <- x$getinverse()
  
  #If the matrix inverse returned from cache is not null,
  #it has already been calculated, so return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #the matrix inverse from cache is NULL, 
  #we need to calculate it, store it back in cache and return it
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setinverse(i)
  
  i
}
