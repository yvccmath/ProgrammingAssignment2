makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y        ## set the cached x to the passed numeric matrix
    inv <<- NULL   ## reset the value inv to NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve  ## a function that sets the cached matrix inverse
  getsolve <- function() inv  ## a function that returns the cached matrix inverse
  list(set = set, get = get,  ## set matrix in parent environment and get matix in evaluating environment
       setsolve = setsolve,   ## set inverse in parent environment
       getsolve = getsolve)   ## get inverse in evaluating environment
}

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()  ## sets local inverse to makeCacheMatrix inverse
  if(!is.null(inv)) {  ## if 'inv' is not null, then return the cached inverse
    message("getting cached data")
    return(inv)
  }
  data <- x$get()   ## if 'inv' is null, then inverse will be calculated
  inv <- solve(data, ...)  ## assign 'inv' to the calculated inverse
  x$setsolve(inv)
  inv  ## Return a matrix that is the inverse of 'x'
}
