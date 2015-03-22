## Function, that 
## 1. Set the values of the matrix
## 2. Get the values of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  invMat <- NULL 
  set <- function(y) { 
         x <<- y 
        invMat <<- NULL 
      } 
 
  
  get <- function() x 
  setinverse <- function(inverse) invMat <<- inverse 
  getinverse <- function() invMat 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
  
}

## Function, that firstly checks if inversion has already been done for the given matrix.
## If so, it gets the inverse values from the cache and skips the computation.
## Otherwise, it makes the matrix inversion and set the value of the inverse matrix in the cache via setinverse function.

cacheSolve <- function(x, ...) {
  invMat <- x$getinverse()
  if(!is.null(invMat)) {
    message("Getting cached data.")
    return(invMat)
  }
  matrix <- x$get()
  invMat <- solve(matrix)
  x$setinverse(invMat)
  invMat
}
