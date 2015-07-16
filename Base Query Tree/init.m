%% init: initialize properties and read data from file to start tags
function tags = init(address)

filename = ['tags/',address.folder,'/',address.tagnum,'/',address.iter,'.txt']
fid = fopen(filename,'r');
tline = fgets(fid) ;
i = 1 ;
while ischar(tline)
	tags(i,:) = tline(1:96) ;
	tline = fgets(fid) ;
	i = i+1 ;
end % while ischar
fclose(fid) ;