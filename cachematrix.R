#to avoid calculating the inverse many times the second function 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse of the matrix
  # 4. get the inverse of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix"  
  ## returned by makeCacheMatrix above. If the inverse has already 
  ## been calculated (and the matrix has not changed), then the 
  ## cachesolve will retrieve the inverse from the cache.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("cached matrix available")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

#end