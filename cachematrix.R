## Create a cacheable matrix and calculate its inverse

## Create a cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
        }
    get <- function() { x }
    setinverse <- function(inverse) { i <<- inverse }
    getinverse <- function() { i }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculate the inverse of a cacheable matrix
## Does not verify if the matrix is invertible
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i  
}
