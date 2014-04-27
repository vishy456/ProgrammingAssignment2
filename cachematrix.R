## To demonstrate ability to cache results of data intensive operations

## makeCacheMatrix provides a list of interfaces to store value of matrix and its computed result

makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  set <- function(y) {
    x <<- y
    InvMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) InvMatrix <<- Inverse
  getInverse <- function() InvMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function tries to retrieve the already stored inverse matrix, if any. If not, computes and 
## store the result for furture resference

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  InvMatrix <- x$getInverse()
  if(!is.null(InvMatrix)) {
    message("getting cached data")
    return(InvMatrix)
  }
  data <- x$get()
  InvMatrix <- solve(data, ...)
  x$setInverse(InvMatrix)
  InvMatrix        
}
