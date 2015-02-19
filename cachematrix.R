## This file contains two function described as blow:
## makeCacheMatrix: return a list of functions that can operate on the input matrix
## cacheSolve: get a matrix using the get() function of the input function list, then
##             compute the inverse of the matrix, use setInverse() to cache the result
##             and also return the result

## Function makeCacheMatrix
## Input: x, an inevitable square matrix
## Output: a list of functions:
##         set(): Set the cache variable x to be the input matrix
##         get(): Return the cache variable x
##         setInverse(): Cache the inverse of input matrix to inv
##         getInverse(): Return the cache variable inv
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse<- function(inverse) inv <<- inverse 
        getInverse<- function() inv
        list(set = set, get = get,
              setInverse = setInverse,
               getInverse = getInverse)

}



## Function cacheSolve
## Input: a list of functions returned by makeCacheMatrix
## Output: the computed inverse of the matrix cached in makeCacheMatrix
## Note:  I do check for invertiblity of input matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
		if ( (nrow(data)==(ncol(data))) && det(data)!=0 ){
            inv <- solve(data)
		} else {
			warning('The input matrix is not invertible, return NA')
			inv <- NA
		}
        x$setInverse(inv)
        inv        
}
