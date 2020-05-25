## makeCacheMatrix - accepts vector, places INVERSE into RAM, provides set and get to access
## cacheSolve - calls makeCacheMatris and checks if INV is in ram or needs to be recalculated.
##            - by calling get or set as needed

makeCacheMatrix <- function(x = matrix()) {
  #function accepts matrix and returnss inverse
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve   #set the invers
  getinv <- function() m                  #allows retrival of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinvn = getinv)
}


cacheSolve <- function(x, ...) {
  #functions uses accepts the call to makeCacheMatrix and check if inverse allready in memory.
  #m is the value returned.
  #set and get are used depending on presence in memory - clever
  m <- x$getinv()
  if(!is.null(m)) {                 #here is the check for presence of m
    message("getting cached data")
    return(m)
  }
  data <- x$get()                   #other wise inverses is calculated
  m <- solve(data, ...)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
