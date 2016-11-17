## Coursera R programming Assignment 2:Lexical Scoping

## This function creates a special "matrix" object just like the special "vector"
## in the example.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## the following function computes the inverse of the matrix and if the 
## inverse has been calculated before, retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver
}
