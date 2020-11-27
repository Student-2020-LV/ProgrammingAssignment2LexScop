#The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#It also creates a list containing a function:

# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse matrix
# 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL   #the initialization of two objects, x and m
  
  set <- function(y) {           #provides four basic behaviors: set, get, setsolve, getsolve
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,  #Create a new object by returning a list
       setsolve = setsolve,
       getsolve = getsolve)
}


#The following function returns a matrix that is the inverse of of the special "matrix" 
#returned by makeCacheMatrix above.
#However, it first checks to see if the inverse matrix has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse 
#in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m     
}
