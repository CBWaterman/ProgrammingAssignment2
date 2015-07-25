## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    ## clean out the local variable before starting.
    i <- NULL
    set_matrix <- function (y) {
        x <<- y 
        i <<- null
    }
    
    ## this returns the matrix in the main function.
    get_matrix <- function() x
    
    ## these next two lines set the matrix passed (the inverse, theoretically)
    ## into the cache for set_inversecache, while get_inversecache grabs and returns it.
    set_inversecache <- function(inv) i <<- inv
    get_inversecache <- function() i
    
    ## the list() function builds the list of functions in this function.
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inversecache = set_inversecache,
         get_inversecache = get_inversecache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## This function gets the cached inverse matrix; 
    ## if it exists it returns it;
    ## if it is null then it calculates the inverse matrix and caches it,
    ## before returning the fresh inverse.
    
    i <- x$get_inversecache()
    
    ## test to see if the returned matrix is null, if not return it.
    if(!is.null(i)) {
        message("getting cached data")
        ## note that this next line kicks us out of the function, so if 
        ## there was a value in the cache we're all done.
        return(i)
    }
    
    ## if inv was null we go here next. Now we get the matrix, 
    ## calculate the inverse and then store it in the cache.
    data <- x$get_matrix()
    inv <- solve(data, ...)
    x$set_inversecache(i)
    inv
    
}
