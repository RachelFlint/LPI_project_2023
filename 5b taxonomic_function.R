#Etard 2020 function - edited: made subset of function 'FunctionToApply_RL'

###########################################################
#HER function code (I added key = apikey where appropriate)
############################################################
#Redlist_synonyms <- function(SpeciesDF) {
  
  FunctionToApply_RL = function(x) {  #can isolate this function
    
    
    print(paste("Processing", x))  
    
    # Several attempts in case the connection is not established straightaway
    
    FuzzyMatch <- FALSE
    
    X <- NULL
    
    while(is.null(X)) {  
      
      try(X <- rl_synonyms(name=x, key = apikey))
      
    }
    
    ## 1. No synonym found OR no Red List record
    
    if (X$count==0) {
      
      # Check if there is no synonym OR if the species does not appear in the RedList database, by querying the IUCN website
      # several attempts in case the connection is not established straightaway
      
      Search <- NULL
      attempt <- 1
      while( is.null(Search) && attempt <= 3 ) {
        
        try(Search <- rl_search(name = x, key = apikey)$result)
        attempt <- attempt + 1
        
      } 
      
      id <- Search$taxonid
      
      # 1.1. If id is null: no record in the Red List -- 
      
      if (is.null(id)) {
        
        InRedList <- FALSE
        IsAccepted <- NA
        IsSynonym <- NA
        Accepted <- NA
        Synonyms <- NA
        
        Family <- NA
        Order <- NA
      }
      
      
      # 1.2. If id is not null: the species does not have recorded synonyms in the Red List
      
      else {
        
        InRedList <- TRUE
        IsAccepted <- TRUE
        IsSynonym <- FALSE
        Accepted <- X$name
        Synonyms <- "No recorded synonym in the Red List."
        
        if (is.null(Search$family)) { Family <- NA }
        else {Family <- unique(Search$family)}
        
        if (is.null(Search$order)) { Order <- NA }
        else {Order <- unique(Search$order) }
      }
      
      
    } 
    
    
    ## 2. When synonyms are found
    
    else {
      
      Name <- X$name
      
      ## Typo in the red list
      # if (Name=="aotus lemurinus") {Name=="Aotus lemurinus"}
      
      InRedList <- TRUE
      
      # Find out status of the name (accepted or synonym)
      
      # 2.1. If the name is the accepted scientific name
      
      # with "any" because in some cases, names differ sightly (with Acomys cahirinus for example)
      if (any(grepl(tolower(Name), tolower(X$result$accepted_name)))) {
        
        IsAccepted <- TRUE
        IsSynonym <- FALSE
        
        Accepted <- Name
        
        Synonyms <- X$result$synonym[X$result$synonym != Name]
        Synonyms <- paste(Synonyms, collapse = " | ")
        
      }
      
      
      # 2.2 If the name is a synonym that is not accepted  
      else { 
        
        IsAccepted <- FALSE
        IsSynonym <- TRUE
        
        # Look for the accepted name corresponding to that synonym -- a few special cases treated separatly
        
        ## Special cases -- when there are several accepted names returned: Find the best match between the name and the accepted names
        
        ToLook <- X$result$accepted_name %>% unique
        
        if (length(ToLook)!=1) {
          
          ToLook <- word(ToLook, 1,2) %>% unique # when genus, species + subspecies 
          
          if (length(ToLook)!=1) { # when different species: find the best match by computing the distances between the character string
            
            MinStringDist <- stringdist(Name, ToLook) %>% which.min()
            ToLook <- X$result$accepted_name[MinStringDist]
            
            FuzzyMatch <- TRUE
            
          }
          
        }
        
        ## Special cases
        # if (Name=="Bunopithecus hoolock") {
        #   ToLook <- X$result$accepted_name[2] # Hoolock hoolock
        # } 
        # 
        # if (Name=="Callicebus cupreus") {
        #   ToLook <- X$result$accepted_name[4] # Plecturocebus cupreus
        # } 
        # 
        # if (Name=="Callicebus torquatus") {
        #   ToLook <- X$result$accepted_name[4] # Cheracebus torquatus
        # }
        
        ## end of special cases
        
        else {
          
          ToLook <- X$result$accepted_name %>% unique
        }
        
        Y <- try(rl_synonyms(name=ToLook, key = apikey))
        
        if (Y$count==0) {
          
          Accepted <- Y$`name`
          Synonyms <- X$`name`
          
        } 
        
        else {
          
          # if there are more than one accepted names appearing because of subspecies: choose ToLook as the accepted name
          
          if (length(unique(Y$result$accepted_name))!=1) {
            Accepted <- ToLook
            Synonyms <- Y$result$synonym[Y$result$synonym != ToLook]
            Synonyms <- paste(Synonyms, collapse = " | ")
          }
          
          else {
            Accepted <- unique(Y$result$accepted_name)
            Synonyms <- paste(Y$result$synonym, collapse = " | ")
          }
          
        }
        
      }
      
      # Family and order information for the accepted name
      
      Search2 <- NULL
      attempt <- 1
      
      while( is.null(Search2) && attempt <= 3 ) {
        
        try(Search2 <- rl_search(name = Accepted, key = apikey)$result)
        attempt <- attempt + 1
      }
      
      if (is.null(Search2$family)) { Family <- NA }
      else {Family <- unique(Search2$family)}
      
      if (is.null(Search2$order)) { Order <- NA }
      else {Order <- unique(Search2$order)}
      
    } 
    
    return(list(InRedList=InRedList, 
                IsAccepted=IsAccepted, 
                IsSynonym=IsSynonym, 
                Accepted=Accepted, 
                Synonyms=Synonyms, 
                Order=Order, 
                Family=Family, 
                FuzzyMatch=FuzzyMatch))
    
  } # End of function to apply
  
  
  #List <- pblapply(SpeciesDF$Etard_2020, FunctionToApply_RL)
  
  
 # SpeciesDF$InRedList <- unlist(List)[attr(unlist(List),"names")=="InRedList"]
  #SpeciesDF$IsAccepted <- unlist(List)[attr(unlist(List),"names")=="IsAccepted"]
  #SpeciesDF$IsSynonym <- unlist(List)[attr(unlist(List),"names")=="IsSynonym"]
  #SpeciesDF$Accepted <- unlist(List)[attr(unlist(List),"names")=="Accepted"]
  #SpeciesDF$Synonyms <- unlist(List)[attr(unlist(List),"names")=="Synonyms"]
  #SpeciesDF$Order <- unlist(List)[attr(unlist(List),"names")=="Order"]
  #SpeciesDF$Family <- unlist(List)[attr(unlist(List),"names")=="Family"]
  #SpeciesDF$FuzzyMatch <- unlist(List)[attr(unlist(List),"names")=="FuzzyMatch"]
  
  #.firstup <- function(x) {
   # substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    #x
  #}
  
  #if (!is.na(SpeciesDF$Accepted)) {
   # SpeciesDF$Accepted <- .firstup(SpeciesDF$Accepted)
  #}
  #return(SpeciesDF)
  
#}
