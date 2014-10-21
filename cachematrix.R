## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The following function returns a list of 2 items - 
## item 1 is a function to store a matrix and its inverse values 
## item 2 is a function to get the inverse from cache if available

makeCacheMatrix <- function(x = matrix()) {
    
    ## I am setting 2 cache variables - a matrix and its inverse to NULL    
    matrix_remote_env <- NULL
    inverse_remote_env <- NULL
    
    ## The following function accepts a matrix in its input. If the value matches with previously existing value, then ignore
    ## If it detects that the new matrix passed through the argument is different to cached matrix, then it recalculates 
    ## inverse of the matrix and stores both values in cache
    
    store_in_cache <- function(transmatx) 
    { 
        if(is.null(inverse_remote_env)||is.null(matrix_remote_env)) { 
            ## store matrix and inverse in cache if existing value is NULL
            matrix_remote_env <<- transmatx
            inverse_remote_env <<- solve(transmatx) 
            
            print('cache values are reset as cache is NULL for the matrix')
        }
        ## if matrix value has changed from last save, then save the new value in cache
        else if(!identical(transmatx,matrix_remote_env)) 
        {inverse_remote_env <<- solve(transmatx) 
         matrix_remote_env <<- transmatx
         print('cache values are re-calculated as matrix values have changed') 
        }
        else {print("cache values are unchanged and hence no update made in store cache function ")}
    }
    
    getinverse <- function(transmatx) { 
        ## if the matrix passed through argument is different from cache, then reset the values in cache 
        if(!identical(transmatx,matrix_remote_env)) {
            matrix_remote_env <<- transmatx
            inverse_remote_env <<- solve(transmatx)
            print('Values Reset and inverse re-caculated')
            return(inverse_remote_env)
        }
        ## if for any reason the inverse is null, then set the cache value for inverse 
        if (is.null(inverse_remote_env)) {inverse_remote_env <<- solve(transmatx)}
        else {print('using Cache')}
        return(inverse_remote_env)
        
    }
    
    list(store_in_cache = store_in_cache, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve  <- function(x=matrix(c(1,1),1,1), y=matrix(c(1,1),1,1), ...) {
    ## I have tweaked the function arguments to accept 2 matrix as input instead of 1, so that I could play around 
    ## with the arguments to see if the correct cache value is retrieved when the function detects a change in input matrix    
    
    
    ## call the cachematrix function and assign the value to input_matrix (which is a list of functions)
    input_matrix <- makeCacheMatrix(x)
    
    
    print('Print of matrix 1 inverse')
    ## This should not retrieve from cache as this is done first time 
    print(input_matrix$getinverse(x))
    
    print('re-print of matrix 1 inverse')
    ## cache value should be retrieved and comment 'cache' should appear in the console     
    print(input_matrix$getinverse(x))
    
    ## At this point cache refers to matrix 1. Let us try to fool this by asking the inverse of Matrix 2 
    ## without reseting the values
    print('Print of matrix 2 inverse ')
    print(input_matrix$getinverse(y))
    
    
    ## At this point cache refers to matrix 2. Let us try to fool this by asking the inverse of Matrix 1 without reseting the values
    print('Print of matrix 1 inverse')
    print(input_matrix$getinverse(x))
    
    
    ## Let us manipulate the matrix value in cache by passing second matrix. 
    ## This should reset the matrix and let us know  
    input_matrix$store_in_cache(y)
    
    ### Let us retrieve the inverse of y. This should retrieve cache as the value is already set in above function
    print('re-print of matrix 2 inverse')
    print(input_matrix$getinverse(y))
    
    ## Swap again - This should rec-calculate matrix 1 inverse
    print('Print of matrix 1')
    print(input_matrix$getinverse(x))
    
}
