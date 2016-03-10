## Put comments here that give an overall description of what your
## functions do
# 2 functions, 1 calculating the inverse, the other to cache the result 
## Write a short comment describing this function
# 
makeCacheMatrix <- function(x = matrix()) {
# initialize matrix
  mtr <- NULL
# create Matrix
  set <- function(y) {                       
    x <-- y
    mtr <<- NULL
  }
# get the matrix  
  get <- function() x  
# invert matrix
  setmtr <- function(inv) mtr <<- inv  
# get inverted matrix
  getinv <- function() mtr
  list(set = set, get = get, setmtr = setmtr, getinv = getinv)
}


## Write a short comment describing this function
# check if cache is available, if not fill

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtr <- x$getinv()
  if(!is.null(mtr)) {
    print("yes, cache is available")
    return(mtr)
  }
  data <- x$get()
  mtr <- solve(data, ...)
  x$setmtr(mtr)
  return(mtr)
}
