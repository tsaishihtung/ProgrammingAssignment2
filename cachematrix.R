## For a invertible matrix, if the contents are not changing, we use
## this programming to cache the inverse matrix rather than recomputed.
## Function makeCacheMatrix creates a special object that stores 
## a invertible matrix and can cache its inverse matrix.
## Function cacheSolve computes the inverse of the special matrix
## returned by makeCacheMatrix, if the inverse has been calculated
## and the matrix has not changed, then the cacheSolve retrieve the inverse
## from the cache. 

## makeCacheMatrix creats a special object matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {   #inut x should be a invertible matrix
   inverse<-NULL                              #inverse will be the inverse of the matrix x and reset to NULL
   set<-function(y){                             
      x<<-y
      inverse<<-NULL
}
   get<-function() x                            #this function returns the values of the original matrix 

   setinverse<-function(solve) inverse<<-solve  #it's called by cacheSolve() during the first time
                                                # access and it will store inverse using superassignment 

   getinverse<-function() inverse               #it will return the cached inverse matrix to cacheSolve()   
   list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) #makeCacheMatrix is called, and make 
                                                                     #a list of the internal functions
}



## cacheSolve computes the inverse of the special matrix or retrieve the 
## inverse if the inverse has been calculated and the matrix has not changed

cacheSolve <- function(x, ...) {        #return a matrix that is the inverse of 'x' created by makeCacheMatrix
        
inverse<-x$getinverse()                 #access the object 'x' and gets the values of the inverse matrix 
   if(!is.null(inverse)){               #if inverse matrix was already cached
      message("getting cached data")    #send this message to the console
      return(inverse)                   #return the inverse matrix, ends cacheSolve() 
   }
   data<-x$get()                        #execute this code only if x$getinverse() returned NULL
   inverse<-solve(data,...)             #if inverse was NULL, then calculates the inverse
   x$setinverse(inverse)                #store the calculated inverse matrix in x
   inverse                              #return the inverse matrix to the code that called this function
}
