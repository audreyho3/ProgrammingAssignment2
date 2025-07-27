## This function identifies a matrix x, finds the inverse, stores it, and changes that inverse to reflect changes in matrix x. 

makeCacheMatrix <- function(x = matrix()) { # names the matrix function makeCacheMatrix
  inv <- NULL  # holds the cached inverse
  
  set <- function(y) { # updates matrix 
    x <<- y # assigns new matrix y to x
    inv <<- NULL  # clears the cached inverse if matrix changes
  }
  
  get <- function() x # gets current value of matrix x
  
  setInverse <- function(inverse) inv <<- inverse # stores inverse of matrix x
  
  getInverse <- function() inv # returns cached inverse of matrix x
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse) # changes matrix, shows it, saves the result, and shows that result 
}


## This function takes the cached matrix, makes sure the inverse is already cached, if not, it calculates, stores, and prints it, if yes it prints it.

cacheSolve <- function(x, ...) { # names function of x cacheSolve
  inv <- x$getInverse() # gets the cached inverse 
  
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv) # prints existing cached inverse 
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # computes the inverse
  x$setInverse(inv)       # caches the inverse matrix 
  inv # prints the new inverse 
}