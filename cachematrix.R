## The following functions will cache the inverse of a matrix.


##The first function, makeCacheMatrix will produce a list containing a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of the matrix
##4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## The following function calculates the matrix of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse of the matirx has already been calculated. 
## If so, it gets the inverse value from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(i)
  i
}
