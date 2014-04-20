## Put comments here that give an overall description of what your
## functions do
##############################################################################
# Given a matrix, makeCacheMatrix 'wraps' that matrix, a variable to store the 
# inverse of that matrix, and two pairs of get/set accessor functions (one pair 
# each for the matrix and its inverse), storing them in its 'defining 
# environment'. Returning a list containing the functions
#
# Given a list of functions returned from makeCacheMatrix, cacheSolve uses the 
# accessor functions to either retrieve the cached value of the inverse of
# the matrix, or, if this is the first time the inverse has been accessed, 
# it will retrieve the matrix from the defining environment using the get 
# accessor, calculate its inverse using solve(), write the inverse to the cache
# variable in the defining enviroment using the setInverse accessor and then 
# return the calculated value of the inverse.
##############################################################################



## Write a short comment describing this function
##############################################################################
# makeCacheMatrix takes a matrix and creates a variable to hold the inverse of
# that matrix. It also creates four accessor functions - a get/set pair for 
# the original matrix and its inverse. It returns these accessor functions in 
# a list object.
#
# The matrix, its inverse variable, and the four functions are stored in the 
# environment of the makeCacheMatrix function when it is executed. The 'set'
# functions must use the <<- assignment to ensure that the matrix and inverse 
# variables that are written to are the variables in the defining environment
# and *not* to the variables local to the 'set' functions
##############################################################################

makeCacheMatrix <- function(x = matrix()) {

    #initialise the cached inverse to be NULL
    cachedInverse <- NULL
    
    
    setFn <- function(newMatrix = matrix()) {
        
        #replace the old matrix with the new one
        x <<- newMatrix
        
        #erase the cached inverse of the old matrix (re-initialise)
        cachedInverse <<- NULL
    }
    
    
    getFn <- function() {
        #just returns the matrix
        x
    }
    
    
    setInverseFn <- function(calculatedInverse = matrix()){
        # writes the inverse into cache 
        #################################################################
        # Note: this function will be called by the cacheSolve function
        # the value of calculatedInverse will be provided by cacheSolve
        
        cachedInverse <<- calculatedInverse
    }
    
    
    getInverseFn <- function(){
        #just returns the value of the cached inverse (which may be NULL!)
        cachedInverse
    }
    
    
    #Return the functions we defined above, in a list, with names.
    ##############################################################
    # Note - the 'cacheMatrix object' isn't really a matrix, but a only group
    # of functions that get and set the matrix and its inverse.
    list(
         set = setFn, 
         get = getFn,
         setInverse = setInverseFn,
         getInverse = getInverseFn
        )
    
}




## Write a short comment describing this function
##############################################################################
# cacheSolve takes a list object (containing functions) created by the 
# makeCacheMatrix function and returns the inverse of the matrix stored in the 
# defining environment of those functions.
#
# cacheSolve will retrieve the cached value of the matrix using the getInverse
# accessor function - if this value is not NULL it will return it.  If the 
# value is NULL, this indicates that the value of the inverse has not yet been
# cached - in this case, cacheSolve will retrieve the matrix from the defining
# environment using the get accessor, calculate its inverse using solve(), 
# write the inverse to the cache variable in the defining enviroment using the
# setInverse accessor and then return the calculated value of the inverse.
##############################################################################

cacheSolve <- function(x, ...) {
    
    # get the cached value of the inverse of the matrix from the defining
    # environment of the getInverse() function
    Inv <- x$getInverse()
    
    #Test the returned value of the inverse
    if(!is.null(Inv)) {
        # if the inverse was already cached, it will not be NULL
        # therefore we now have it and can return it immediately
        
        message("retrieved cached inverse")
        return(Inv)
        
    } else {
        # the returned inverse was not already cached, it was NULL:
        # so we now have to fetch the matrix, calculate the inverse,
        # then cache it, and then return the inverse to whatever 
        # called this function.
        
        message("calculating and caching inverse")
        
        # fetch the matrix
        data <- x$get()
        
        # calculate the inverse
        Inv <- solve(data)
        
        # cache the calculated inverse
        x$setInverse(Inv)
        
        #return the inverse to our caller
        return(Inv)
    }
}
