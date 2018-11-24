## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ## define the argument with default mode of "matrix"
  inv <- NULL ## inv as NULL to hold value of matrix inverse 
  set <- function(y){ 
    x <<- y 
    inv <<- NULL 
  }
  get <- function() x   ## define the get function to return value of the matrix argument
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv ## returns value of inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){ ## if inv has been calculated, it will return the above inv from cache 
    message("getting cached inverse matrix")
    return(inv)
  } ## if null then 
  mat <- x$get() ## get original matrix 
  inv <- solve(mat) ## use solve() to inverse it 
  x$setInverse(inv) 
  inv ## Return a matrix that is the inverse of 'x'
}