## Sript

##-------LOAD SUCESSFUL COMBINATIONS-------##
library(here)
Data<-read.table(here("combinations.txt"), sep=";")
Data <- Data[,c("Item1","Item2","Item3","Item4","Number","Code","Remaining1","Remaining2")] 
Data$Code <- as.character(Data$Code)
head(Data)

##-------PARAMETERS SIMULATIONS-------##
Items <- c(1,2,3,4,5,6) # Ressources
nb_Slot <- 12 # Number of Stock slots

Nb_Attempts <- 188 # n attemps per individual 
Replicates <- 1000# n replicats

Memory <- 'True'

##
GroupSize <- 1
Results_SIL <- list(NULL) 
Results_SSL <- list(list(NULL),list(NULL)) # length = GroupSize , here GroupSize = 2

if(GroupSize == 1){Results_Simul <- Results_SIL}else{Results_Simul <- Results_SSL}

##--------------------------- Start Replicats ------------------------------##
for (j in 1 : Replicates){
  
  ##------- (Re)Initialization -------##
  Stock_list <- matrix(0,nb_Slot, GroupSize) # List of items agent can work with
  Innov_list <- matrix(0, nrow(Data), GroupSize) # Agent's innovations record
  WorkShop_list <- matrix(0,Nb_Attempts, GroupSize) # Agent's combinations record (for memory)
  Sample_list <- list() # List of items agent is working with (Stock_list + Items)
  
  Group_Stock <- NULL 
  ##--------------------------------------##
  
  for (i in 1 : Nb_Attempts){
    for (l in 1 : GroupSize){ 
      Sample_list[[l]] <- c(Items, Stock_list[which(Stock_list[,l]>0),l]) # List of usable items 
      ##--------------------------- New workshop --------------------------------------------##
      repeat{
        Nb_Items <- sample(c(1:4),1) # Sample workshop length
        Workshop <- sample(Sample_list[[l]], Nb_Items, replace = T) ## Sample items
        if (Memory == 'True'){ # Check if combination has been produced before (Warning: order matters = dumb agents)
          WorkshopString <- paste(Workshop[1],Workshop[2],Workshop[3],Workshop[4], sep ='_') # Create the string
          if (!WorkshopString%in%WorkShop_list[[l]]){
            WorkShop_list[i,l] <- WorkshopString  
            break}
        }else {break} ## Exit the loop if no memory
      }
      
      # Transform workshop to match .xls file (Data$Code) --> add O when shorter than 4 then sort
      if (length(Workshop) < 4 ){Workshop <- c(Workshop, rep(0, 4 - length(Workshop)))} # add 0
      Workshop <- sort(Workshop) # Sort
      Workshop <- paste(Workshop[1],Workshop[2],Workshop[3],Workshop[4], sep ='_') # Convert to String
      
      ##--------------------------- Test new combination -----------------------------------------##
      if (Workshop%in%Data$Code){ # Check if successful combination
        Result <- Data$Number[which(Data$Code == Workshop)] # Get new item
        if (!Result%in%Innov_list[,l]){ ## if not in Innov_list
          Innov_list[which(Innov_list[,l]==0)[1],l] <- Result # Add to innov (not clean because of potential redundancy but should not matter --> 186 slots)
          if (length(which(Stock_list[,l] == 0)) >= 1){ ## Check if empty slot in stock
            Stock_list[which(Stock_list[,l] == 0)[1],l] <- Result # if yes, put into empty slot
          }else{ 
            Stock_list[sample(c(1:nrow(Stock_list)),1),l] <- Result # if not, put into a randomly selected slot
          }
        }
      }
    }
    
    ## SOCIAL LEARNING ##
    # Share info --> Group_Stock
    for (l in 1 : GroupSize){ 
      Group_Stock <- c(Group_Stock, Stock_list[-which(Stock_list[,l] == 0),l]) # Add each player' stock to group stock
    }
    Group_Stock <- unique(Group_Stock) # Remove redundancy 
    
    # Learn from Group_Stock
    if(length(Group_Stock) > 0){ # if innov got produced ...
      for (l in 1 : GroupSize){ # add to each player's stock 
        if (length(Group_Stock) > nb_Slot){ # sample Group_Stock if more innov than slots 
          Stock_list[,l] <- sample(Group_Stock,nb_Slot, replace = F)} 
        else{ # Save as it is otherwise
          Stock_list[,l] <- c(Group_Stock, rep(0,(nrow(Stock_list)-length(Group_Stock)))) 
        } 
      }
    }
  }
  
  # Save data
  for (l in 1 : GroupSize){
    Results_Simul[[l]][[j]] <- as.numeric(names(table(Innov_list))[-which(names(table(Innov_list)) == '0')])}
}


##------- RESULTS -------##

# n innov per simul
n.innov <- unlist(lapply(Results_Simul[[1]], length)) # idem if group size = or > 1 because toolkit are correlated
mean(n.innov)
sd(n.innov)

## function that looks at the number of agents that produced logs
Log_Request <- function(x){ 
  Record <- NULL
  count <- 0
  for (i in 1 : length(x[[1]])){
    if (19%in%unlist(x[[1]][i])||36%in%unlist(x[[1]][i])){ # 19 (small log), 36 (big log)
      Record <- c(i, Record)
      count <- count + 1}
  }
  z <- list("Occurence" = count, "Slot" =Record)
  return(z)}

# find logs
Log_Request(Results_Simul)