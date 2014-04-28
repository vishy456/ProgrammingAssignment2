## To demonstrate ability to cache results of data intensive operations

## makeCacheMatrix() function provides a list of interfaces - 
## set() - To store the contents of a matrix, to be retrieved across different environment
## get() - To retrieve the contents of matrix
## setInverse() - To store the Inverse matrix, to be retrieved across different environment
## getInverse() - To retrieve the inverse matrix 

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

## cacheSolve() function tries to retrieve the already stored inverse matrix, if any. If not, computes and 
## store the result for future reference and return the just computed inverse

cacheSolve <- function(x, ...) {
  ## getInverse will return valid inverse matrix if already stored inverse matrix is available
  InvMatrix <- x$getInverse()
  if(!is.null(InvMatrix)) {
    message("getting cached data")
    return(InvMatrix)
  }
  ## If the cachekSolve is called for first time for a matrix, it will be computed,stored and returned to caller as below
  data <- x$get()
  InvMatrix <- solve(data, ...)
  x$setInverse(InvMatrix)
  InvMatrix        
}
