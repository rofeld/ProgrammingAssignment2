## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##For this assignment, assume that the matrix supplied is always invertible.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
    {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x #get value of matrix provided as argument
        getInverse <- function() m #get value of inversed matrix
        setInverse <- function(inverse_matrix) m <<-inverse_matrix #set value of matrix to inverse value
        
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)  
    

    }


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
    {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if ( ! is.null(m)) {
            print("getting cached data")
            return(m)
        }
        m <- solve(x$get()) #Computing the inverse of a square matrix can be done with the solve function in R. 
        #For example, if X is a square invertible matrix, then solve(X) returns its inverse.
        x$setInverse(m) #update value of m in makeCacheMatrix function
        m
    }
