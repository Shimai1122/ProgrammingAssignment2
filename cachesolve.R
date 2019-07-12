#solve the inverse of a matrix using "cache"

#Return a special list, containing function to set the value of matrix; get the value of matrix; 
#set the inverse of matrix; and get the inverse of matrix, respectively
makeMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Solve the inverse of a matrix created above. If the inverse has already been calculated, get it.
#If it is a new matrix, calculate the inverse and save the result into the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}