%% tagManager: function description
function result = tagManager(prefix,tags)

%fprintf('\nCalling tagManager with prefix %s',prefix)

result = 0 ;
for i = 1:size(tags,1)
	if result >= 2
		break
	end % if result
	%fprintf('\nTag EPC::%s',tags(i,:))
	if strncmpi(tags(i,:),prefix,length(prefix))
		%fprintf('\nTag answered!')
		%pause
		result = result + 1 ;
	end % if strncmpi
end % for i