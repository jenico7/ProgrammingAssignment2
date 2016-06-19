## The two functions are used to compute the inverse of a square matrix,
## store the inverse in cache & retrieve it when needed.

## This function is used to set the value for the matrix, get the value of the 
## matrix, set the value for inverse of the matrix & get the value for inverse
## of the matrix. It then puts the 4 values in a list.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This list obtained from the makeCacheMatrix function can be passed to this 
## function to store the inverse in cache. If the inverse of another matrix is to
## be determined, then the matrix is passed to the 1st function & the list is 
## passed to be 2nd function. If the 2nd matrix = 1st matrix, the inverse is 
## retrieved from memory; else the inverse of 2nd matric is computed.

cacheSolve <- function(x, ...) {
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

##Output 1:
##> a
##     [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    1    4
##[3,]    5    6    0
##> cacheSolve(cc)
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
##> cacheSolve(cc)
##getting cached data.
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1

##Output 2:
##> a
##     [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    1    4
##[3,]    5    6    0
##> cacheSolve(cc)
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
##> b
##     [,1] [,2] [,3]
##[1,]    3    5    4
##[2,]    5    7    1
##[3,]    3    6    0
##> cc <- makeCacheMatrix(b)
##> cacheSolve(cc)
##            [,1]        [,2]       [,3]
##[1,] -0.18181818  0.72727273 -0.6969697
##[2,]  0.09090909 -0.36363636  0.5151515
##[3,]  0.27272727 -0.09090909 -0.1212121