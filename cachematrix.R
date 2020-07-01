#This program creates a special object for matrix and inverse which calculates
#and stores the values

## This function receive a matrix and create a special object which have 4
## functions, 2 set and get for matrix value and 2 set and get for inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function receive a special object and calculates the inverse of the
## matrix value, if it has already been calculated, the function retrieve it 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

A <- matrix( c(2, 8, 4,
               2, 4, 4,
               2, 4, 2), nrow=3, byrow=TRUE)

m <- makeCacheMatrix(A)
x <- cacheSolve(m)
m$getinverse()
