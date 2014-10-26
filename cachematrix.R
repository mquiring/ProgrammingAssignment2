## Put comments here that give an overall description of what your
## functions do

## takes a square invertible matrix.
## set() can be used to assign a new matrix to the list
## (y instead of x, i gets reset)
## get() returns the matrix from the list
## setinverse() assigns an inverse matrix
## getinverse() returns it

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



## takes a list as made by makeCacheMatrix(),
## checks if the inverse is computed already (x$getinverse()).
## If yes, returns inverse - if not computes and sets it by
## (get(), solve(), setinverse()).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
