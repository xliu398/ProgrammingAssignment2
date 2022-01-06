#This function makes the inverse matrix of matrix "x".
#The function to create inverse matrix is solve()
#The function "makeCacheMatrix" return a list that contents different elements.

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve
  getinverse <- function() m 
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function "cacheSolve" return us the inverse matrix of "x".
#If the inverse matrix has been created before, this function get the matrix and
#return the message "getting cached data" and show us the solution. Another case,
#if the inverse matrix had not been created, cacheSolve function should calculate
#and show the matrix, not caching that in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}

