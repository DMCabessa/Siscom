%% qt: basic query tree snippet
function [total,conflicts,idles] = qt(prefix, tags)

if length(prefix) == 97
	error('Error: File corrupted!')
end % if length

result = tagManager(prefix,tags) ;

if result == 0
	total = 1 ;
	conflicts = 0 ;
	idles = 1 ;
	%fprintf('\nNo answer')
	%pause
elseif result == 1
	total = 1 ;
	conflicts = 0 ;
	idles = 0 ;
	%fprintf('\nSucess')
	%pause
elseif result >= 2
	%fprintf('\nConflict, calling next prefix...')
	%pause
	[totalL,conflictsL,idlesL] = qt(strcat(prefix,'0'),tags) ;
	[totalR,conflictsR,idlesR] = qt(strcat(prefix,'1'),tags) ;
	total = totalL + totalR + 1 ;
	conflicts = conflictsL + conflictsR + 1 ;
	idles = idlesL + idlesR ;
end % if result