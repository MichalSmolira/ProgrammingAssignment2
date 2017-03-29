## Functions computing the inverse of a square matrix and cache the scoore. In case when 
## the inverse has been calculated already it returns value from cache otherwise it calculate
## inverse of a matrix using SOLVE function.
## The assumption is that the matrix supplied is always invertible.

## Function return the list of function 
## set - creating new object
## get - return the supplied matrix 
## setInvertMatrix - cache the scoore of
## getInvertMatrix - return a cache scoore

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvertMatrix <- function(solve) m <<- solve
  getInvertMatrix <- function() m
  list(set = set, get = get,
       setInvertMatrix = setInvertMatrix,
       getInvertMatrix = getInvertMatrix)
}


## Function check if the calculation of the invert matrix has been done already. IF true then
## return a cache value otherwise calculate inverse of a matrix using SOLVE function and return it
## The argument x is a list creted by funcion makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInvertMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvertMatrix(m)
  m
}
