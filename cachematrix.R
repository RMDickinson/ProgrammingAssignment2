##makeCacheMatrix creates a list of functions stored with values super assigned
##accepts argument x which must be an invertible matrix 'x'; assume x is always invertible
##Let blob<-makeCacheMatrix(x)
makeCacheMatrix <- function(x = numeric()) {
## value of the inverse of the matrix is initialized to NULL
  inv <- NULL
  
  ##calling blob$set(y) allows you to store a new value to x and reinitializes the inverse to NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ##calling blob$get retrieves the stored value of x
  get <- function() x
  
  ##calling blob$setinv sets the inv to a value and super assigns it 
  
  setinv <- function(inverse) inv <<- inverse
  
  ##calling blob$getinv retrieves the last value of inv.  This is tested for null.  inv
  ##must also be set to null if the set function is called.  New inv
  getinv <- function() inv
  
  ##the 'set=set' names the columns of the list after the funtion name
  ##this way they can be called using the dollar operator
  ##e.g. if you make a vector "blob" you can run any of the functions
  ##in the list using blob$'function'
  ## all the calls are made in the cachemean function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##cacheSolve accepts the list of functions created by makeCacheMatrix  
##it looks to see if the inverse has already been cached, if so it returns
##the stored value, if not it calculates and pushes it into the list created by the first function
cacheSolve <- function(x, ...) {
  #look for a cached inv, if it exists return it
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##if it doesn't exist, get the data
  data <- x$get()
  ##calculate the inv
  inv <- solve(data)
  ##cache the inv
  x$setinv(inv)
  ##return the inv
  inv
}
