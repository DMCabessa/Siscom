function results = init(iter,steps)

results.Lowerbound.slotsUsed = zeros(iter,steps) ;
results.Lowerbound.slotsConflict = zeros(iter,steps) ;
results.Lowerbound.slotsEmpty = zeros(iter,steps) ;
results.Lowerbound.MAE = zeros(iter,steps) ;

results.Schoute.slotsUsed = zeros(iter,steps) ;
results.Schoute.slotsConflict = zeros(iter,steps) ;
results.Schoute.slotsEmpty = zeros(iter,steps) ;
results.Schoute.MAE = zeros(iter,steps) ;

results.Eom_Lee.slotsUsed = zeros(iter,steps) ;
results.Eom_Lee.slotsConflict = zeros(iter,steps) ;
results.Eom_Lee.slotsEmpty = zeros(iter,steps) ;
results.Eom_Lee.MAE = zeros(iter,steps) ;