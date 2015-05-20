% Recieves a filled frame and generates a new empty one
function frame = schoute(frame)

frame.slots = zeros(1,ceil(frame.conflicts*2.39)) ;

frame.conflicts = 0 ;
frame.success = 0 ;
frame.empty = length(frame) ;