
## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.


makeCacheMatrix <- function(x=matrix()) {
  ## Creates a list of functions that
  ## can cache the inverse of a matrix.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

cacheSolve <- function(x, ...) {
  ## Computes the inverse of the matrix returned
  ## by makeCacheMatrix(), unless the inverse has
  ## already been calculated, in which case
  ## it retrieves it from the cache.
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}


## Test cases 
> a <- diag(5,3)
> a
     [,1] [,2] [,3]
[1,]    5    0    0
[2,]    0    5    0
[3,]    0    0    5
> b <- diag(6,4)
> b
     [,1] [,2] [,3] [,4]
[1,]    6    0    0    0
[2,]    0    6    0    0
[3,]    0    0    6    0
[4,]    0    0    0    6
CachedMarix <- makeCacheMatrix(b)
> cacheSolve(CachedMarix)
          [,1]      [,2]      [,3]      [,4]
[1,] 0.1666667 0.0000000 0.0000000 0.0000000
[2,] 0.0000000 0.1666667 0.0000000 0.0000000
[3,] 0.0000000 0.0000000 0.1666667 0.0000000
[4,] 0.0000000 0.0000000 0.0000000 0.1666667
> cacheSolve(CachedMarix)   #getting cached data
getting cached data
          [,1]      [,2]      [,3]      [,4]
[1,] 0.1666667 0.0000000 0.0000000 0.0000000
[2,] 0.0000000 0.1666667 0.0000000 0.0000000
[3,] 0.0000000 0.0000000 0.1666667 0.0000000
[4,] 0.0000000 0.0000000 0.0000000 0.1666667
> 

