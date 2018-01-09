## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # to initialize the inverse
  ivs <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    ivs <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() x
  
  ##Meyhod to set the inverse of the matrix
  setinvs <- function(invs) ivs <<- invs
  getinvs <- function() ivs
  
  ## return the list of methods
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs = getinvs)
}

## This function calculates the inverse of the special "matrix" returned by makeCacheMatrix above. 
## and check if the inverse has already been calculated (and the matrix has not changed),
#then cacheSolve whould retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    ivs <- x$getinvs()
    
    ## return the inverse of the it initial set
    if(!is.null(ivs)) {
      message("getting cached data")
      return(ivs)
    }
    ##to get the inverse from our object
    data <- x$get()
    
    ## to calculate the inverse
    ivs <- solve(data, ...)
    x$setinvs(ivs)
    
    ##Return the matrix
    ivs
  }
}
