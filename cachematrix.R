## The two functions below are used to demonstrate the principle of caching
## expensive calculations witin an R object that also contains the target data.

## makeCacheMatrix creates a special matrix that is capable of storing/caching
## its own inverse matrix (the matrix x is assumned to be square & invertable)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse matrix of x. If it has already been calculated
## then the cached matrix is returned, otherwise the solve() function is used
## to calculate the inverse matrix (and also store it ready for the next call).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
