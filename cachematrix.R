makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
      x
  }
  setinv <- function() {
      inv <<- solve(x)
  }
  getinv <- function() {
      inv
  }
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheMatrix <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

# Output:
# > func <- makeMatrix()
# > func$set(matrix(1:4, 2))
# > func$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > func$setinv()
# > func$getinv()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheMatrix(func)
# getting cached data.
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
