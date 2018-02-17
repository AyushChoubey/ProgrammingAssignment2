
##Here we are finding the inverse of a non-singular matrix .In this program we have 
##two functions " makeCacheMatrix": This function creates a special "matrix" object
##that can cache its inverse and cacheSolve: This function computes the inverse of 
##the special "matrix" returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.


##"IF THE MATRIX IS SINGULAR MATRIX MEANS IT,S DETERMINAT IS ZERO WE WILL GET AN ERROR "



##This function Function makeCacheMatrix gets a matrix as an input, set the value of the
##matrix,get the value of the matrix, set the inverse Matrix and get the inverse Matrix. 



makeCacheMatrix <- function(x = matrix()) {
  Matinv <- NULL
  
  set <- function(y) {
                      x <<- y
                      Matinv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) Matinv<<- solve
  getinv <- function() Matinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




##The function cacheSolve takes the output of the previous matrix make CacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# If the  inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data 
# and set the invertible  matrix by using the solve function.
# If the  inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "getting cached invertible Matrix" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Matinv<- x$getinv()
  if(!is.null(Matinv)) {
    message("getting cached invertible matrix")
    return(Matinv)
  }
  data <- x$get()
  Matinv <- solve(data, ...)
  x$setinv(Matinv)
  return(Matinv)
}
