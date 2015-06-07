## Put comments here that give an overall description of what your
## functions do

## Function that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<-solve 
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function to calculate de inverse or return the cached vlue

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

mimatriz<-makeCacheMatrix()
mimatriz$set(matrix(1:4,2,2))
cacheSolve(mimatriz)
z<-cacheSolve(mimatriz)
mimatriz$get()%*%z