## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function contains 4 subfunctions that either set, get, set the inverse, or get the inverse of a matrix that is being passed as argument to the function below.
## It caches an inverse of a matrix

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(y){
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setInverse <- function(calcMatrix) inv <<- calcMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## This function checks whether the matrix it is being passed as argument has already been inverted once and if so it will retrieve the cached inverse matrix instead of _
## calculating it again. If no match exists, the inverse matrix is being calculated 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("Retrieving cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
