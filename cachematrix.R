makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initializing inverse as NULL
  set <- function(y) {
    x <<- y #Assigning new matrix
    inv <<- NULL #reset the inverse cache
  }
  
  get <- function() x #retrieving the matrix
  setinverse <- function(inverse) inv <<- inverse #cache the inverse
  getinverse <- function() inv #retrieving the cached inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() #checking if the inverse is already cached
  
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get() #getting the matrix  
  inv <- solve(mat, ...) #computing the inverse
  x$setinverse(inv) #cache the result
  inv
}
