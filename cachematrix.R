## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The function utilizes the superassignment operator to modify global variables within
## the local function environment. The function return an object of type "list" consisting of the
## matrix to be solved "x", its inverse "m", and 2 helper functions the caches the inverse in m (setinv),
## and retreives "m" frm cache (getinv). 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
get <- function()
  x
setinv <- function(solve)
  m <<- solve
getinv <- function()
  m
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() #here we get the matrix passed into the function from the makeCacheMatrix get function
  m <- solve(data, ...)
  x$setinv(m)
  m
}