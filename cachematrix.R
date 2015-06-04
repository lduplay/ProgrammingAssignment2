## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  
  ## Gets the stored matrix
  get <- function() x
  
  ## Sets a new matrix to the makeCacheMatrix environment
  ## The <<- operator avoids setting the new matrix only in the set function environment
  set <- function(y) {
    x <<- y
    xInv <<- NULL ## Since we've set a new matrix, the old inverted matrix isn't needed
  }
  
  ## Sets or gets a number for the mean of vector x (but not neccessarily correct)
  setInv <- function(Inverted) xInv <<- Inverted
  getInv <- function() xInv
  
  ## Create list of callable functions for the object assigned the makeCacheMatrix function
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The input of cacheSolve is the object assigned the makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Check if Inverse has already been calculated
  xInv <- x$getInv()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  
  ## If it hasn't, calculate and store a new one
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInv(xInv)
  xInv
}
