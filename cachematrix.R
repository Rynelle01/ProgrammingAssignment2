## This R file allows the user to cache the inverse of a special "matrix".

## This makeCacheMatrix function creates a special matrix object that cache its inverse.
makeCacheMatrix <- function(y = matrix()) {
  inverseMatrix <- NULL
  set <- function(x) {
    y <<- x
    inverseMatrix <<- NULL
  }
  get <- function() y
  setinverseMatrix <- function(inverse) inverseMatrix <<- inverse
  getinverseMatrix <- function() inverseMatrix
  list(set = set, get = get, setinverseMatrix = setinverseMatrix, getinverseMatrix = getinverseMatrix)
}


## This cacheSolve function deals with th computation of inverse returned from the function above.
cacheSolve <- function(y, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- y$getinverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached result")
    return(inverseMatrix)
  }
  data <- y$get()
  inverseMatrix <- solve(data, ...)
  y$setinverseMatrix(inverseMatrix)
  inverseMatrix
}

