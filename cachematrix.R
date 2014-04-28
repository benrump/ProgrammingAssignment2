
# The following functions can be used to solve for and
# cache the value of the inverse of a matrix.  For a
# large matrix for which the inverse may be needed multiple
# times, caching the inverse may preferable to repeatedly
# computing the inverse.


# Function to create a list of functions that set the value
# of a matrix, get the value of that matrix, set the value of
# the inverse of that matrix, and get the value of the inverse
# of that matrix.
makeCacheMatrix <- function(x = matrix()) {

  # x - a matrix for which to define the functions listed above
  
  #initialize inverse at NULL
  i <- NULL
  
  #local function to set matrix x equal to matrix y
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #local function to return matrix x
  get <- function() x
  
  #local function to set the inverse of matrix x to i
  setinverse <- function(inverse) i <<- inverse
  
  #local function to return the inverse of matrix x
  getinverse <- function() i
  
  #return list of local functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}

# Function to solve for or retrieve the cached value of
# the inverse of a matrix.  To be used in conjunction with
# makeCacheMatrix().
cacheSolve <- function(x, ...) {
  
  # x - the list of functions returned by makeCacheMatrix()

  #retrieve inverse stored in x
  i <- x$getinverse()
  
  #if inverse is cached, return cached value, else compute
  if(!is.null(i)) {
    
    message("retrieving cached inverse")
    return(i)
    
  } else {
  
    data <- x$get()    
    i <- solve(data)    
    x$setinverse(i)    
    return(i)
  }
  
}

# ## Example ##
# 
# # create matrix
# MM = matrix(runif(10*10),nrow=10,ncol=10)
# 
# # solve for inverse
# XX = makeCacheMatrix(MM)
# YY = cacheSolve(XX)
# 
# # show that YY is inverse of MM
# MM %*% YY
# 
# # retrieve cached inverse
# YY2 = cacheSolve(XX)
# 
# # show that YY2 is inverse of MM
# MM %*% YY
