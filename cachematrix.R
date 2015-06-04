## This pair of functions calculate the inverse of a matrix in a cost- and time-effective manner
## It does so by caching the result, and recalling the result if the input hasn't changed instead of recalculating.
## How to use: cacheSolve(matrix)

## makeCacheMatrix: This function creates an object with functions to set and get both a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  
  ## Gets the stored matrix
  get <- function() x
  
  ## Sets a new matrix
  set <- function(y) {
    x <<- y
    xInv <<- NULL ## Since we've set a new matrix, the old inverted matrix isn't needed
  }
  
  ## Sets or gets the inverse of a matrix
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
