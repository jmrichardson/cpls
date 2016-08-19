if (!exists('stats') | !exists('ph')) { 
    
  # Load helper functions
  source('scripts/funcs.R')
  
  # Load model and stats data
  load('data/stats.rda')
  load('data/phMin.rda')
  
  # Model only complete notes
  stats = subset(stats,(loan_status=='Fully Paid' | loan_status=='Charged Off'))
  stats$loan_status <- droplevels(stats$loan_status)
  
  # Filter payment history by stats id
  ph <- merge(x = ph, y = stats[,.(id)], by = "id")

  # Total fully paid and charged off notes
  totalNotes <- nrow(stats)
}
