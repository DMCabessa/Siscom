function [results,postResult] = scanner(strategy, iter, tags, steps, results, postResult)

if isequal(strategy,'LOWERBOUND')
	props = results.Lowerbound ;
	fcn = @lowerbound ;
elseif isequal(strategy,'SCHOUTE')
	props = results.Schoute ;
	fcn = @schoute ;
elseif isequal(strategy,'EOM-LEE')
	props = results.Eom_Lee ;
	fcn = @eom_lee ;
elseif isequal(strategy,'CHEN')
	props = results.Chen ;
	fcn = @chen ;
end % if isequal

% Scanner loop
% ----------------------------------------------------------------------------------------------
for i = 1:iter
	fprintf('\nIteration #%d. [%s]',i,strategy)

	% Variable definition
	tags.total = 0 ;
	frame.size = 64 ;
	frame.conflicts = 0 ;
	frame.success = 0 ;
	frame.empty = frame.size ;

	while tags.total < tags.max
		turns = 0 ;
		frame.slots = zeros(1,frame.size) ;
		tags.total = tags.total + tags.step ;
		tags.unidentified = tags.total ;
		localstep = tags.total/tags.step ;
		while tags.unidentified > 0
			turns = turns + 1 ;
			frame = rndtags(frame,tags) ;
			% For printing results
			% ----------------------------------------------------------------------------------
			props.slotsConflict(i,localstep) = props.slotsConflict(i,localstep) + frame.conflicts ;
			props.slotsEmpty(i,localstep) = props.slotsEmpty(i,localstep) + frame.empty ;
			props.slotsUsed(i,localstep) = props.slotsUsed(i,localstep) + length(frame.slots) ;
			% ----------------------------------------------------------------------------------
			tags.unidentified = tags.unidentified - frame.success ;
			if tags.unidentified > 0
				frame = fcn(frame) ;
				props.MAE(i,localstep) = props.MAE(i,localstep) + abs(tags.unidentified-length(frame.slots)) ;
			end % if tags
		end % while tags
		props.MAE(i,localstep) = props.MAE(i,localstep) / turns ;
	end % while
end % for i
% ----------------------------------------------------------------------------------------------

% Storing results
% ----------------------------------------------------------------------------------------------
for i = 1:steps
	postProps.slotsUsed(i) = mean(props.slotsUsed(:,i)) ;
	postProps.slotsConflict(i) = mean(props.slotsConflict(:,i)) ;
	postProps.slotsEmpty(i) = mean(props.slotsEmpty(:,i)) ;
	postProps.MAE(i) = mean(props.MAE(:,i)) ;
end % for i
% ----------------------------------------------------------------------------------------------

if isequal(strategy,'LOWERBOUND')
	results.Lowerbound = props ;
	postResult.Lowerbound = postProps ;
elseif isequal(strategy,'SCHOUTE')
	results.Schoute = props ;
	postResult.Schoute = postProps ;
elseif isequal(strategy,'EOM-LEE')
	results.Eom_Lee = props ;
	postResult.Eom_Lee = postProps ;
elseif isequal(strategy,'CHEN')
	results.Chen = props ;
	postResult.Chen = postProps ;
end % if isequal