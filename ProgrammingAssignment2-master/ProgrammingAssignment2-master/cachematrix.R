# The makeVector creates a special "vector", which is 
# really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  print(environment())
  evn <- environment()
  print(parent.env(evn))
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  getevn<- function() environment()
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean,
       getevn = getevn)
}

# This function calculates the mean of the data 
# and sets the value of the mean in the cache 
# via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

# Matrix inversion is usually a costly computation 
# and their may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly. The
# following functions cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special 
# "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Example compute inverse of a square matrix. 

x = rbind(c(1, -1/2), c(-1/2, 1))
m = makeCacheMatrix(x)
m$get()

[,1] [,2]
[1,]  1.0 -0.5
[2,] -0.5  1.0

# No cache retrieved in the first pull

cacheSolve(m)

[,1]      [,2]
[1,] 1.3333333 0.6666667
[2,] 0.6666667 1.3333333

# Retrieving cache during second pull

cacheSolve(m)

getting cached data.
[,1]      [,2]
[1,] 1.3333333 0.6666667
[2,] 0.6666667 1.3333333