## This pair of functions calculate the inverse of a matrix in a cost- and time-effective manner
## It does so by caching the result, and recalling the result if the input hasn't changed instead of recalculating.
## How to use: cacheSolve(matrix)

## makeCacheMatrix: This function creates an object with functions to set and get both a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  
  ## Gets or sets a stored matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    xInv <<- NULL ## Since we've set a new matrix, the old inverted matrix isn't needed
  }
  
  ## Gets or sets the inverse of a stored matrix
  getInv <- function() xInv
  setInv <- function(Inverted) xInv <<- Inverted
  
  ## Create list of callable functions for the object assigned the makeCacheMatrix function
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve: This function computes the inverse of the matrix returned by the get() in makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
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
