# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

#makeCacheMatrix: This function creates a special "matrix" 
#                 object that can cache its inverse.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# cacheSolve function returns the inverse of the matrix by first checking whether
# the inverse has already been computed. If so, it retrieves the inverse from the
# cache and skips the computation. If not, it computes the inverse, sets the value
# in the cache via setinverse function. The matrix is assumed to be invertible.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

# The input matrix is stored in variable 'x' and the resulting matrix is returned
# by the variable 'm'.



# Sample Run for the above program:
# > x = rbind(c(1,2), c(5,6))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    5    6
# > cacheSolve(m)
# [,1]  [,2]
# [1,] -1.50  0.50
# [2,]  1.25 -0.25
# > cacheSolve(m)
# getting cached data
# [,1]  [,2]
# [1,] -1.50  0.50
# [2,]  1.25 -0.25