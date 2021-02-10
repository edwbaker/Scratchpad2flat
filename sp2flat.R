sp2flat  <- function(input="data.csv") {
  input <- read.csv(input)
  
  ranks_used <- unique(as.character(input$Rank))
  ranks_used <- ranks_used[ranks_used != ""]
  col_names <- c("id","taxon","valid", "parent_id", "Rank", ranks_used)
  num_taxa <- nrow(input)
  output <- data.frame(matrix(NA, nrow=num_taxa, ncol=length(col_names)))
  colnames(output) <- col_names
  output$taxon <- input$Term.name
  output$valid <- input$Usage
  output$valid <- replace(output$valid, output$valid=="valid", "accepted")
  output$valid <- replace(output$valid, output$valid=="invalid", "not accepted")
  output$id <- input$GUID
  output$parent_id <- input$Parent.GUID
  output$Rank <- input$Rank
  for (i in 1:num_taxa) {
    rank <- as.character(input[i,"Rank"])
    if (rank != "") {
      output[i,rank] <- as.character(input[i, "Term.name"])
    }
    parent_id <- as.character(input[i, "Parent.GUID"])
    while (length(parent_id) > 0) {
      parent_rank <- as.character(output[output$id==parent_id, "Rank"])
      if (!identical(parent_rank, character(0))){
        parent_name <- as.character(output[output$id==parent_id, "taxon"])
        output[i,parent_rank] <- parent_name
      }
      parent_id <- as.character(output[output$id==parent_id, "parent_id"])
    }
  }
  return(as.data.frame(output))
}

sp2flat() -> flat

#discard nonspecies
flat <- flat[!is.na(flat$Species),]

flat <- flat[with(flat, order(Superorder, Order, Suborder, Cohort, Subcohort, Superfamily, Family, Genus, Species, Subspecies, Variety, valid)),]
write.csv(flat, "out.csv")