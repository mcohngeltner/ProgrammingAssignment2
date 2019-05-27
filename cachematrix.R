

## Inverts the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinversion<- function(inverse) i <<- inverse
    getinversion <- function() i
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
  }
  
## Inverts the matrix, unless the answer is already in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinversion()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversion(i)
  i
}

