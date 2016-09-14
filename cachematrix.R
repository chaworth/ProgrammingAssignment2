## I really had no way of testing if this is correct
## so this is my best guess of what we were supposed to do
# please let me know if this is on target or way off!

## This function creates a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function()x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function calculates the inverse of the special matrix created above
## after first checking to see if the mean has already been calculated from the cache.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
        ## Return a matrix that is the inverse of 'x'
}
