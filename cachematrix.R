## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function creates a special matrix
#your input matrix has to be : 1-Squared 2-Has a valid inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get    <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m

  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv
       )

}


## Write a short comment describing this function
# Creating a cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
 
   if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }else {
    message("inverse needs computation")
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
  
}

#Computing the inverse of a square matrix can be done with the solve function in R.
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## Testing the code
  # Testing the commands
  # a: is a square matrix that has an inverse
  # b: is created based on a

  a <- matrix(c(2,3,3,3),2,2) #Test matrix
  b <- makeCacheMatrix(a)
  # run the command below twice, and notice the printed messages, which indicates if the inverse value was computed
  #from inverse function or obtained from cache
  c <- cacheSolve(b)
  c <- cacheSolve(b)

