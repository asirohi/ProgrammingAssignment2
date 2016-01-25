## The first function makes a list with methods that set and get a matrix and its inverse into an environment variable
## The second function is passed the list from the first and attempts to calculate and set its inverse.  
## If the inverse is already set, it uses the cached value.


## makeCacheMatrix creates a matrix x and provides three methods to set and get x and its inverse

makeCacheMatrix <- function(x = matrix()) {
        cachedInv <- NULL ## initialize inverse
        
        ## set x in parent env with the desired value; if inverse is already set, unset it.
        set <- function(userValue = matrix()) {
                x <<- userValue 
                cachedInv <<- NULL
        }
        
        get <- function() x
        
        ##set inverse variable in parent env to desired value and return the value
        setInverse <- function(invVal) {
                cachedInv <<- invVal 
                return(cachedInv)
        }
        
        getInverse  <- function() cachedInv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Given the list variable from the first function, this function will check if cached inverse exists and if so return the value
## Else compute its inverse and return the value

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { ##special matrix provided or create a test 2x2 matrix
        
        ## Check to see if inverse exists
        calculatedInverse <- x$getInverse() 
        
        ##check for cached value AND if it is a matrix
        if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
                message("We found cached data and saved valuable cpus!!!")
                return(calculatedInverse)
        }
        
        ## otherwise get the matrix
        matrixToSolve <- x$get()  
        
        ## try to solve the matrix and catch errors and warnings
        calculatedInverse <- tryCatch({ 
                solve(matrixToSolve)
        }, warning=function(w) {
                message("This may not be the result you're looking for")
                message(w)
        }, error=function(e) {
                message("Something went wrong solving your matrix")
                message(e)
                message("\n")
        })
        
        ## whatever the case, set the value of the inverse (NULL if something went wrong)
        message("Setting the value of inverse to:") 
        x$setInverse(calculatedInverse)
}
