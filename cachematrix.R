## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function (y) {
    
    x <<- y 
    
    inv <<- NULL
    
  }
  

  get <- function (x) {
    
    #creating setters and getters
    
    setInverse <- function (inverse)  inv <<- inverse
    
    getInverse <- function () inv
    
    #creating a consturctor 
    
    list (set = set, 
          get = get, 
          setInverse = setInverse,
          getInverse = getInverse)
    
    
  }
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse ()       #getting the current value of inv from x and assigning it to inv
    
  if(!is.null(inv)) {          #checking if inv is already calculated
    
    message("getting cached data")        
    
    # If already calculated it will be returned without further computation
    
    return(inv)                     
    
  }
  
  #if the value of inv is null we need to compute the inverse
  
  mat <- x$get()              
  
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  
  inv
}
