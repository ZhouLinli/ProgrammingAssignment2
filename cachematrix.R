## Put comments here that give an overall description of what your
## functions do

## The following functions are used to create a special object that stores a matrix and caches its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {#set the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x#get the value of the matrix
  setinverse <- function(inverse) i <<- inverse#set the value of the inverse
  getinverse <- function() i#get the value of the inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()#get inverse
  if (!is.null(i)) {#If the inverse has already been calculated (and the matrix has not changed)
    message("getting cached data")
    return(i)
  }
  data <- x$get()#cacheSolve should retrieve the inverse from the cache
  i <- solve(data, ...)
  x$setinverse(i)
  i
}