## This function creates a matrix caching wrapper.
## It is used in conjunction with cacheSolve to prevent duplicate computations when solving a matrix.
## 
## example usage:
##    m <- matrix()
##    wrapper <- makeCacheMatrix(m)
##    cacheSolve(wrapper) 
##    result <- wrapper$cachedInverseValue()
## 
makeCacheMatrix <- function(originalMatrix = matrix()) {
    cachedInverseValue <- NULL
  
    # as this object is a 'wrapper' around matrix value, we can reuse it and wrap a different matrix
    # in that case cachedInverseValue is to be wiped
    set <- function(newOriginalMatrix) {
        originalMatrix <<- newOriginalMatrix
        cachedInverseValue <<- NULL
    }
  
    # returns matrix which is wrapped by the caching object
    get <- function() originalMatrix
  
    # store an inverse matrix value computed for the matrix wrapped
    setCachedInverseValue <- function(valueToSave) cachedInverseValue <<- valueToSave
  
    # return cached inverse matrix value
    getCachedInverseValue <- function() cachedInverseValue
    
    list(set = set, get = get,
         setCachedInverseValue = setCachedInverseValue,
         getCachedInverseValue = getCachedInverseValue)
}


## Solves (finds inverse matrix) for the wrapper
## Duplicate computations are prevented. 
## See makeCacheMatrix function for the example usage
cacheSolve <- function(cachingWrapper, ...) {
    cachedValue <- cachingWrapper$getCachedInverseValue()
    if (!is.null(cachedValue)) {
        return(cachedValue)
    }
  
    originalMatrix <- cachingWrapper$get()
    inverse <- solve(originalMatrix)
    cachingWrapper$setCachedInverseValue(inverse)
    return(inverse)
}
