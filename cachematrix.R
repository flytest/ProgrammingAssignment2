## Cache the result of the inverse of a matrix calculation 
## allow retrieving from cache when needed to save computation time

## make a matrix that serves as the cached inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # get original matrix
  get <- function() x 
  
  # setting inverse
  setInv <- function(inv) m <<- inv 
  
  # getting inverse
  getInv <- function() m 
  
  # list of functions
  list(set = set, get = get,
                setInv = setInv,
                getInv = getInv)
  
}


## first checks if the inverse is already calculated
## if so, return inverse from cache
## otherwise, calculate and store in cache
cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
 
  # if data is cached, get cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # calculate and store
  data <- x$get()
  m <- solve(data,...)
  x$setInv(m)
  m
    
}
