#The functions here are designed to cache the inverse of a matrix,


##makeCacheMatrix makes a list of functions, and return the list
#to the parent environment, and also has the "stored" object and 
#a matrix x

makeCacheMatrix <- function(x = matrix()) {
      ##Initialize a null object to store inverse  
      stored<-NULL
      set<-function(z){
           x<<-z
           stored<<-NULL
  }  #create the set() function, where we
                              #assign the values to the parent env.
      get<-function()
       x
      set_invertible<-function(solve) ##assigns the input to the stored
       stored<<-solve                 ##argument in parent env.
      
      get_invertible<-function() ##finds the correct "stored"
       stored                     #to retrieve its value
      
      #Finally, we get the list with the functions returned
      ##to the parent environment
       
      list(set=set,get=get, set_invertible=set_invertible,get_invertible=get_invertible)
  
    

}




#Here we retrieve or compute the inverse of the objects in 
#makeCacheMatrix

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
        stored<-x$get_invertible() # we check the stored matrix
        if(is.null(stored)==FALSE){
          message("returning stored invertible matrix")
          return(stored)} #if "stored" is not null, 
                                        #we get the stored inverse   
        
        matrix<-x$get()                 # if its null, the inverse is
        invertible<-solve(matrix)       ##  computed and "set" in the function
        x$set_invertible(invertible,...)  
        invertible                       #return the inverse
        
  }
