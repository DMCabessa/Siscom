iter = 2000 ;
tags.max = 1000 ;
tags.step = 100 ;
steps = tags.max/tags.step ;

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

% Scanner loop
% ----------------------------------------------------------------------------------------------
for i = 1:iter
	fprintf('\nIteration #%d.',i)

	% Variable definition
	tags.total = 0 ;
	frame.size = 64 ;
	frame.slots = zeros(1,frame.size) ;
	frame.conflicts = 0 ;
	frame.success = 0 ;
	frame.empty = frame.size ;

	while tags.total < tags.max
		tags.total = tags.total + tags.step ;
		tags.unidentified = tags.total ;
		localstep = tags.total/tags.step ;
		while tags.unidentified > 0
			frame = rndtags(frame,tags) ;
			% For printing results
			% ----------------------------------------------------------------------------------
			results.Lowerbound.slotsConflict(i,localstep) = results.Lowerbound.slotsConflict(i,localstep) + frame.conflicts ;
			results.Lowerbound.slotsEmpty(i,localstep) = results.Lowerbound.slotsEmpty(i,localstep) + frame.empty ;
			results.Lowerbound.slotsUsed(i,localstep) = results.Lowerbound.slotsUsed(i,localstep) + length(frame.slots) ;
			results.Lowerbound.MAE(i,localstep) = results.Lowerbound.MAE(i,localstep) + abs(tags.unidentified - length(frame.slots)) ;
			% ----------------------------------------------------------------------------------
			tags.unidentified = tags.unidentified - frame.success ;
			if tags.unidentified > 0
				frame = lowerbound(frame) ;
				%frame = schoute(frame) ;
				%frame = eom_lee(frame) ;
			end % if tags
		end % while tags
	end % while
end % for i
% ----------------------------------------------------------------------------------------------

% Scanner loop
% ----------------------------------------------------------------------------------------------
for i = 1:iter
	fprintf('\nIteration #%d.',i)

	% Variable definition
	tags.total = 0 ;
	frame.size = 64 ;
	frame.slots = zeros(1,frame.size) ;
	frame.conflicts = 0 ;
	frame.success = 0 ;
	frame.empty = frame.size ;

	while tags.total < tags.max
		tags.total = tags.total + tags.step ;
		tags.unidentified = tags.total ;
		localstep = tags.total/tags.step ;
		while tags.unidentified > 0
			frame = rndtags(frame,tags) ;
			% For printing results
			% ----------------------------------------------------------------------------------
			results.Schoute.slotsConflict(i,localstep) = results.Schoute.slotsConflict(i,localstep) + frame.conflicts ;
			results.Schoute.slotsEmpty(i,localstep) = results.Schoute.slotsEmpty(i,localstep) + frame.empty ;
			results.Schoute.slotsUsed(i,localstep) = results.Schoute.slotsUsed(i,localstep) + length(frame.slots) ;
			results.Schoute.MAE(i,localstep) = results.Schoute.MAE(i,localstep) + abs(tags.unidentified - length(frame.slots)) ;
			% ----------------------------------------------------------------------------------
			tags.unidentified = tags.unidentified - frame.success ;
			if tags.unidentified > 0
				%frame = lowerbound(frame) ;
				frame = schoute(frame) ;
				%frame = eom_lee(frame) ;
			end % if tags
		end % while tags
	end % while
end % for i
% ----------------------------------------------------------------------------------------------

% Scanner loop
% ----------------------------------------------------------------------------------------------
for i = 1:iter
	fprintf('\nIteration #%d.',i)

	% Variable definition
	tags.total = 0 ;
	frame.size = 64 ;
	frame.slots = zeros(1,frame.size) ;
	frame.conflicts = 0 ;
	frame.success = 0 ;
	frame.empty = frame.size ;

	while tags.total < tags.max
		tags.total = tags.total + tags.step ;
		tags.unidentified = tags.total ;
		localstep = tags.total/tags.step ;
		while tags.unidentified > 0
			frame = rndtags(frame,tags) ;
			% For printing results
			% ----------------------------------------------------------------------------------
			results.Eom_Lee.slotsConflict(i,localstep) = results.Eom_Lee.slotsConflict(i,localstep) + frame.conflicts ;
			results.Eom_Lee.slotsEmpty(i,localstep) = results.Eom_Lee.slotsEmpty(i,localstep) + frame.empty ;
			results.Eom_Lee.slotsUsed(i,localstep) = results.Eom_Lee.slotsUsed(i,localstep) + length(frame.slots) ;
			results.Eom_Lee.MAE(i,localstep) = results.Eom_Lee.MAE(i,localstep) + abs(tags.unidentified - length(frame.slots)) ;
			% ----------------------------------------------------------------------------------
			tags.unidentified = tags.unidentified - frame.success ;
			if tags.unidentified > 0
				%frame = lowerbound(frame) ;
				%frame = schoute(frame) ;
				frame = eom_lee(frame) ;
			end % if tags
		end % while tags
	end % while
end % for i
% ----------------------------------------------------------------------------------------------

% Printing results
% ----------------------------------------------------------------------------------------------
for i = 1:steps
	postResult.Lowerbound.slotsUsed(i) = mean(results.Lowerbound.slotsUsed(:,i)) ;
	postResult.Lowerbound.slotsConflict(i) = mean(results.Lowerbound.slotsConflict(:,i)) ;
	postResult.Lowerbound.slotsEmpty(i) = mean(results.Lowerbound.slotsEmpty(:,i)) ;
	postResult.Lowerbound.MAE(i) = mean(results.Lowerbound.MAE(:,i)) ;
end % for i
% ----------------------------------------------------------------------------------------------

% Printing results
% ----------------------------------------------------------------------------------------------
for i = 1:steps
	postResult.Schoute.slotsUsed(i) = mean(results.Schoute.slotsUsed(:,i)) ;
	postResult.Schoute.slotsConflict(i) = mean(results.Schoute.slotsConflict(:,i)) ;
	postResult.Schoute.slotsEmpty(i) = mean(results.Schoute.slotsEmpty(:,i)) ;
	postResult.Schoute.MAE(i) = mean(results.Schoute.MAE(:,i)) ;
end % for i
% ----------------------------------------------------------------------------------------------

% Printing results
% ----------------------------------------------------------------------------------------------
for i = 1:steps
	postResult.Eom_Lee.slotsUsed(i) = mean(results.Eom_Lee.slotsUsed(:,i)) ;
	postResult.Eom_Lee.slotsConflict(i) = mean(results.Eom_Lee.slotsConflict(:,i)) ;
	postResult.Eom_Lee.slotsEmpty(i) = mean(results.Eom_Lee.slotsEmpty(:,i)) ;
	postResult.Eom_Lee.MAE(i) = mean(results.Eom_Lee.MAE(:,i)) ;
end % for i
% ----------------------------------------------------------------------------------------------

% Ploting results
% Need to learn how to make good graphs :(
% ----------------------------------------------------------------------------------------------
xs = linspace(tags.step,tags.max,steps) ;
figure 
ysl = postResult.Lowerbound.slotsUsed ;
yss = postResult.Schoute.slotsUsed ;
yse = postResult.Eom_Lee.slotsUsed ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr')
ylabel('Numero de Slots')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Location','NorthEastOutside')

figure 
ysl = postResult.Lowerbound.slotsEmpty ;
yss = postResult.Schoute.slotsEmpty ;
yse = postResult.Eom_Lee.slotsEmpty ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr')
ylabel('Numero de Slots Vazios')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Location','NorthEastOutside')

figure 
ysl = postResult.Lowerbound.slotsConflict ;
yss = postResult.Schoute.slotsConflict ;
yse = postResult.Eom_Lee.slotsConflict ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr')
ylabel('Numero de Slots em Colisao')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Location','NorthEastOutside')

figure 
ysl = postResult.Lowerbound.MAE ;
yss = postResult.Schoute.MAE ;
yse = postResult.Eom_Lee.MAE ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr')
ylabel('Erro Medio Absoluto')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Location','NorthEastOutside')
% ----------------------------------------------------------------------------------------------