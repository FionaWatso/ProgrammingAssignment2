## These two functions used together work out the inverse of a matrix and then store this in a cache,
## so that if the function is used repeatedly on a matrix X, it doesn't re-calculate the inverse from stratch
## but instead, uses a cached result from a previous calculation, unless x has changed since this result was stored.


## makeCacheMatrix creates a list of functions:
##    set - which can be used to change the matrix x
##    get - which returns the matrix x
##    setSolve - which will be used to store the inverted matrix of x (and cache this matrix)
##    getSolve - which returns the inverted matrix of x


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function()  x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve looks for whether the vector created by makeCacheMatrix already has a 
## vector stored as the inverse of X (called s)
##    If there is a matrix stored as "s", it will return a message "getting cached data" and return s.
##    If s is null (i.e. no inverse of x has been calculated), it will calculate the inverse using solve() and return this.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  Inv2 <- x$get()
  s <- solve(Inv2, ...)
  x$setSolve(s)
  s
}
