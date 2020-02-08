## Put comments here that give an overall description of what your
## functions do
#
## These functions are used to invert a matrix in an efficient way.
## Inverting matrices can be time consuming for computers. This function
## is more efficient because it first checks to see if the inversion has
## already been calculated and stored in cache. If so, it does not
## recalculate; it simply retrieves it from the cache.
#
## This function uses the ginv() function from the MASS package

## Write a short comment describing this function
#
## The first function creates a special object. The special object contains a matrix,
## its inversion, and four functions related to the matrix and its inversion.
## The four functions are used to 'get' and 'set' the matrix and its inversion.

makeCacheMatrix <- function(our_matrix = matrix()) {        ## initialize object 'our_matrix' as empty matrix
  our_inversion <- NULL                                     ## initialize object 'our_inversion' as NULL
  set_our_matrix <- function(y) {                           ## create function 'set_our_matrix'
    our_matrix <<- y                                        ## set object 'our_matrix' to be 'y'
    our_inversion <<- NULL                                  ## reset object 'our_inversion' to be NULL
  }
  get_our_matrix <- function() our_matrix                   ## returns our matrix
  set_our_inversion <- function(inv) our_inversion <<- inv  ## sets the inversion of the matrix
  get_our_inversion <- function() our_inversion             ## returns the inversion
  list(set_our_matrix = set_our_matrix,                     ## creates a list that gives names to our above functions
       get_our_matrix = get_our_matrix,                     ## the list allows use of $ to extract functions
       set_our_inversion = set_our_inversion,
       get_our_inversion = get_our_inversion)
}


## Write a short comment describing this function
#
## The second function returns the inverted matrix from the special object
## that we create with makeCacheMatrix(). First it checks to see if the
## special object already contains the inverted matrix. If so, it does not
## recalculate the inversion. It just displays it. If the special object
## does not contain the inversion, then the function calculates it, sets it
## within the special object, and returns it.

cacheSolve <- function(our_special_matrix, ...) {
        ## Return a matrix that is the inverse of our special matrix object
  our_inversion <- our_special_matrix$get_our_inversion()        ## retrieve inversion value from our special matrix object
  if(!is.null(our_inversion)) {                                  ## checks if the inversion is already cached or not
    message("getting cached data")                               ## notifies it's getting cached data (if cached)
    return(our_inversion)                                        ## returns the value from cache (if cached)
  }                                                              ## if not cached, then...
  data <- our_special_matrix$get_our_matrix()                    ## gets the actual matrix from our special matrix
  our_inversion <- ginv(data, ...)                               ## calculate the inversion of our matrix (requires MASS package to be loaded)
  our_special_matrix$set_our_inversion(our_inversion)            ## sets inversion in in our special matrix object so it's not NULL
  our_inversion                                                  ## outputs the actual inversion
}
