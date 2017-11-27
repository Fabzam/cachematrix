
## The below functions will cache the inverse of a matrix.
## First we make a list to set & get the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}

## Next we see if we have inverted the Matrix
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## testing function

funs <- makeCacheMatrix()
funs$set(matrix(1:4, 2))
funs$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
cacheSolve(funs)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5


