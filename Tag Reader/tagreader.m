iter = 2000 ;
tags.max = 1000 ;
tags.step = 100 ;
steps = tags.max/tags.step ;

results.slotsUsed = zeros(iter,steps) ;
results.slotsConflict = zeros(iter,steps) ;
results.slotsEmpty = zeros(iter,steps) ;

% Scanner loop
% ----------------------------------------------------------------------------------------------
for i = 1:iter
	fprintf('\nIteration #%d.',i)

	% Variable definition
	tags.total = 0 ;
	frame.size = 64 ;
	frame.slots = zeros(1,frame.size) ;
	frame.conflicts = 0 ;
	frame.sucess = 0 ;
	frame.empty = frame.size ;

	while tags.total < tags.max
		tags.total = tags.total + tags.step ;
		tags.unidentified = tags.total ;
		localstep = tags.total/tags.step ;
		while tags.unidentified > 0
			frame = rndtags(frame,tags) ;
			% For printing results
			% ----------------------------------------------------------------------------------
			results.slotsConflict(i,localstep) = results.slotsConflict(i,localstep) + frame.conflicts ;
			results.slotsEmpty(i,localstep) = results.slotsEmpty(i,localstep) + frame.empty ;
			results.slotsUsed(i,localstep) = results.slotsUsed(i,localstep) + length(frame.slots) ;
			% ----------------------------------------------------------------------------------
			tags.unidentified = tags.unidentified - frame.sucess ;
			if tags.unidentified > 0
				frame = lowerbound(frame) ;
			end % if tags
		end % while tags
	end % while
end % for i
% ----------------------------------------------------------------------------------------------

% Printing results
% ----------------------------------------------------------------------------------------------
for i = 1:steps
	postResult.slotsUsed(i) = mean(results.slotsUsed(:,i)) ;
	postResult.slotsConflict(i) = mean(results.slotsConflict(:,i)) ;
	postResult.slotsEmpty(i) = mean(results.slotsEmpty(:,i)) ;
end % for i
% ----------------------------------------------------------------------------------------------

% Ploting results
% Need to learn how to make good graphs :(
% ----------------------------------------------------------------------------------------------
xs = linspace(tags.step,tags.max,steps) ;
figure 
ys = postResult.slotsUsed ;
plot(xs,ys)

figure 
ys = postResult.slotsEmpty ;
plot(xs,ys)

figure 
ys = postResult.slotsConflict ;
plot(xs,ys)
% ----------------------------------------------------------------------------------------------