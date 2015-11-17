####################################################
##makeCacheMatrix
####################################################
##
## Returns a list of functions
## that will allow the user interact with the cached
## matrix inverse, m, and the original matrix, x
##
####################################################

####################################################
##solveCache
####################################################
##
## checks to see if the given object x
## already has the inverse matrix cached. If so then
## the cached inverse is returned. If not then the
## inverse is calculated using solve(x$get())
## and the resulting matrix is stored in the cache
## and then returned
##
####################################################


## makeCacheMatrix
## input: x (matrix) is the matrix that we are caching the inverse of
##
## output: list of functions(set, get, setInverse, getInverse) used to interact 
## with the original matrix (x) and the cached matrix inverse (m)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #initialize cache to NULL
    
    ## set function to set the original matrix x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get function will return the original matrix x
    get <- function(){
        x
    }
    
    ## setInverse will set the cached inverse matrix m
    setInverse <- function(inverseM) {
        m <<- inverseM
    }
    
    ## getInverse will return cached inverse matrix m
    getInverse <- function(){
        m
    }
    
    ## returns a named list of the functions created
    ## in makeCacheMatrix (get, set, getInverse, setInverse)
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve
## input: parameter x(expected list with functions to 
## interact with cache returned from makeCacheMatrix)
##
## output: matrix inverse of the original matrix x$get()

cacheSolve <- function(x, ...) {
    m <- x$getInverse() #get cached inverse
    if(!is.null(m)){ 
        
        #if cached inverse is not null, return it
        return(m) 
    }
    
    #cached inverse is null
    data <- x$get() #get the data
    m <- solve(data) #solve for the matrix inverse
    x$setInverse(m) #set the cached inverse
    m #return the inverse
}
