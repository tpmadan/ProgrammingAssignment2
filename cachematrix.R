## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function consists of set , get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL                    ## initialising j (inv) as NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function() {x}          ## function to get matrix x
  setInverse <- function(inverse) {j <<- inverse}
  getInverse <- function() {j}
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## Write a short comment describing this function
## this function is used to get cache data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){            ## checking whether inverse j is NULL
    message("getting cached data")
    return(j)                 ## returns inverse value
  }
  mat <- x$get()
  j <- solve(mat, ...)
  x$setInverse(j)
  j
}
