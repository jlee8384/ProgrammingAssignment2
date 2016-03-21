
# makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # sets the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # gets the value of the matrix
  get <- function() x
  
  # sets the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # gets the value of the inverse of the matrix
  getinverse <- function() inv
  
  # returns full list
  list(set = set, get = get, setinverse = setinverse, getinvere = getinverse)

    
}


# cacheSolve is a function that computes the inverse of the above function.
# It initially checks to see if the inverse has already been calculated.
# If it has, then it gets the value without addional computation.

cacheSolve <- function(x, ...) {
  
  # gets inverse of x
  inv <- x$getinverse()
  
  # checks if inverse exists and is not null
  # if in cache then display message and return cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not, solve, set in cache and return computed value
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}