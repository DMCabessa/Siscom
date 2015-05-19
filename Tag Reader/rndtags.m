% Recieves an empty frame and fills it with values:
% 0 = empty slot
% 1 = sucess slot
% 2 = conflict slot
function frame = rndtags(frame, tags)

rnds = randi([1,length(frame.slots)],1,tags.unidentified) ;

for i = 1:length(rnds)
	switch frame.slots(rnds(i))
		case 0
			frame.slots(rnds(i)) = 1 ;
		case 1
			frame.slots(rnds(i)) = 2 ;
	end % switch frame
end % for i

frame.conflicts = sum(frame.slots == 2) ;
frame.sucess = sum(frame.slots == 1) ;
frame.empty = sum(frame.slots == 0) ;