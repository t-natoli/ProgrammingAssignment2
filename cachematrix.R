## These functions take a square matrix as input, and returns an object that can
## create the inverse of the matrix, and allow that object to be cached.
## Assumes a square matrix, and doesn't include error handling if the
## inverse can't be solved.

## Takes a matrix as input, and returns a matrix object that can create and cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverseMatrix <- function(solve) m <<- solve
     getInverseMatrix <- function() m
     list(set = set, get = get,
          setInverseMatrix = setInverseMatrix,
          getInverseMatrix = getInverseMatrix)
}


## Checks to see if the inverse matrix is already cached; if not, it calculates the inverse.
## If so, it returns the cahed value. 

cacheSolve <- function(x, ...) {
     m <- x$getInverseMatrix()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverseMatrix(m)
     m
}
