
## The funktions makeCacheMatrix and cacheSolve are used together to calcualte an inverse matrix.


## This function creates a special "matrix" object that can cache its inverse.
## In case the matrix inverse has already been calculated, 
## it will use the cached object to avoid unnecessary cumputations.
makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInverse<- function(inverse) invX <<-inverse
  getInverse <- function() invX
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invX <- x$getInverse()
  if (!is.null(invX)) {
    return(invX)
  } else {
    invX <- solve(x$get())
    x$setInverse(invX)
    return(invX)
  }
}
