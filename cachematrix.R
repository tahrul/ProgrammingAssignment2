## My below functions calculate inverse matrix from a given matrix. As calculation is time consuming,
## the makeCacheMatrix will utilize a feature to cache the computed value in a different environment
## For the first time, inverse will be calculated, next time the cached value will be returned to save time

## This function will cache the computed value of inverse in a different environment and will return a list

makeCacheMatrix <- function(x = matrix()) {
  ## returns a list including function to
  ## 1. set the value of the matrix
  ## 2. get the value of the matrix
  ## 3. set the inverse of the matrix
  ## 4. get the inverse of the matrix
  ## this list is the input parameter to cacheSolve() function
  
  inv = NULL
  set = function(y) {
    x <<- y         # assign value to x in env different from current env
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# This function take the list as input from makeCacheMatrix and will calculate inverse for first time only
cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get from  cache to bypass re-calculation 
    message("getting cached data")
    return(inv)
  }
  
  #else calculate the inverse (first time)
  matrix.data = x$get()
  inv = solve(matrix.data, ...)
  
  x$setinv(inv) # sets the value of the inverse in the cache
  
  return(inv)
}