## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL                                     ##Initialize inv to NULL which holds value of inverse matrix
  set<-function(y){                             ## Define set function to assign newvalue of matrix in parent environment
    x<<-y
    inv<<-NULL                                  ## Is there a new matrix, return inv to NULL
  }
  get<-function(){x}                            ## get fucntion - returns value of the matrix argument
  setInverse<-function(inverse){inv<<-inverse}  ## assigns value of inv in parent environment
  getInverse<-function(){inv}                   ## gets the value of inv where called
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse  ## you need this in order to refer 
                                                                       ## to the functions with the $ operator
  )
  
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cche data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
}
