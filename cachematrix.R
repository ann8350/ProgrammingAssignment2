
# makeCacheMatrix creates the list of functions related to our matrix x
makeCacheMatrix <- function(x = matrix()) {
  
  # matrix inverse becomes null because we are (re)defining the matrix
  mi <- NULL
  
  # set function changes the matrix to a new one, also resets the inverse to null
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  
  # get function retrieves the matrix
  get <- function() x
  
  # setinv function allows us to manually set the inverse matrix to anything
  setinv <- function(matinv) mi <<- matinv
  
  # getinv retrieves the stored inverse matrix
  getinv <- function() mi
  
  # stored list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve gets the inverse of matrix x if it has been calculated already
# and calculates it otherwise
# either way it will return the inverse of x
cacheSolve <- function(x, ...) {
  
  # use makeCacheMatrix to try to retrieve the matrix inverse
  mi <- x$getinv()
  
  # shows the value of the matrix inverse if it already has been calculated, ends function
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  
  # otherwise use makeCacheMatrix to retrieve the matrix
  data <- x$get()
  
  # calculate the matrix inverse
  mi <- solve(data, ...)
  
  # again use makeCacheMatrix to set the matrix inverse so it can be retrieved later
  x$setinv(mi)
  
  # display the matrix inverse
  mi
}