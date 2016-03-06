# Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than computing it repeatedly. 
# The pair of functions below cache the inverse of a matrix.

# The first function, "makeCacheMatrix", creates a special "matrix" object that can 
# cache its inverse. "makeCacheMatrix" creates a list containing functions to:
  # 1. set the value of the matrix
  # 2. get the value of the matrix
  # 3. set the value of inverse of the matrix
  # 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inv <<- inverse
  getinverse <- function() Inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The second function, "cacheSolve", computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  Inv <- x$getinverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data)
  x$setinverse(Inv)
  Inv
}

# Test run:

# > x<-rbind(c(4,1,2,3),c(3,2,1,4),c(2,1,3,4),c(4,3,1,2))
#>  m = makeCacheMatrix(x)
#>  m$get()
# [,1] [,2] [,3] [,4]
# [1,]    4    1    2    3
# [2,]    3    2    1    4
# [3,]    2    1    3    4
# [4,]    4    3    1    2

# run "cacheSolve" the first time, there is no cache data, sovling for inverse of matrix

# > cacheSolve(m)
# [,1]  [,2]   [,3]   [,4]
# [1,]  0.45 -0.05 -0.275 -0.025
# [2,] -0.55 -0.05  0.225  0.475
# [3,] -0.05 -0.55  0.475  0.225
# [4,] -0.05  0.45 -0.025 -0.275

# run "cacheSolve" the second time, inverse of matrix is retrieved from cache, there is no
# actual calculation

# > cacheSolve(m)
# [,1]  [,2]   [,3]   [,4]
# [1,]  0.45 -0.05 -0.275 -0.025
# [2,] -0.55 -0.05  0.225  0.475
# [3,] -0.05 -0.55  0.475  0.225
# [4,] -0.05  0.45 -0.025 -0.275