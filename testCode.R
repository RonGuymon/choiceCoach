oPoints <- data.frame(o1 = c(50, 30, 40)
                      , o2 = c(70, 50, 40)
                      , o3 = c(60, 60, 50))
oRatios <- oPoints/(100-oPoints)

insertedObj <- paste0('objective_', 1:5)
input <- list(
  'objective_1' = 'Weight'
  , 'objective_2' = 'Height'
  , 'objective_3' = 'Color'
)
