## Together, these two functions allow user to specify a square matrix,
## cache this matrix, retrieve this matrix, calculate its inverse, cache
## its inverse and finally retrieve the inverse from memory.  In this way,
## unnecessary and repetitive expensive matrix inverse computations 
## can be avoided.


## This function creates a list object that contains 4 important functions.
## The 'set' function allows user to create a sqaure matrix
## The 'get' function allows user to retreive previously created matrix
## The 'setinverse' function caches the calculated inverse matrix
## The 'getinverse' function retreives the calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # Sets inverse matrix to null
  inv <- NULL  

  # Create a square matrix
  set <- function(y = matrix()) {
    # Caches the square matrix into object 'x' that exist outside this function
    x <<- y
    inv <<- NULL
  }
  
  # Get created matrix
  get <- function(){
    x
  }
  
  # Caches the calculated inverse matrix to object 'inv'
  setinverse <- function(inverse){ 
    inv <<- inverse
  }
  
  # Get caculated inverse matrix
  getinverse <- function(){
    inv
  }
  
  # Create a list object containing all the 4 above functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix that is created using
## the makeCacheMatrix function. First, it checks if the inverse has 
## already been calculated. If so, it will simply output the inverse 
## from memory.  If not, the inverse is calculated and cached into
## memory.

cacheSolve <- function(x, ...) {
  ## Retrieve the inverse matrix
  inv <- x$getinverse()
  
  ## Checks if the inverse matrix has already been calculated
  if(!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  
  ## The followings are executed only when the inverse matrix has not
  ## calculated.
  ## Retrieivng the sqaure matrix
  data <- x$get()
  
  ## Solving for the inverse
  inv <- solve(data, ...)
  
  ## Caches the calculated inverse matrix 
  x$setinverse(inv)
  
  ## Returns the inverse matrix
  inv
  
}
