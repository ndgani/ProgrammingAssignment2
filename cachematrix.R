makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
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

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("hit cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
 # print(i)
  i
}
# Run test() for a unit test generating data and printing inverse with cache check
test <- function(){
  B = matrix( c(2, 4, 1, 5), nrow=2, ncol=2)
  x = makeCacheMatrix(B)
  cacheSolve(x)
}