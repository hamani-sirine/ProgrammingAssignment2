

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                            
  set <- function(y) {                 #setting matrix  
    x <<- y                            
    inv <<- NULL                       
  }
  get <- function() x                    #getting matrix
  
  setinverse <- function(inverse) inv <<- inverse  #setting inverse of the matrix
  getinverse <- function() inv                   #getting inverse of the matrix  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}



cacheSolve <- function(x, ...) {
  inv <- x$getinverse()     #getting the inverse
  if(!is.null(inv)) {
    message("getting cached data")   #if inv!=null gets inverse from cached data and returns it
    return(inv)
  }
  data <- x$get()                  #calculating inverse and setting it with setinverse
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
