% Recieves a filled frame and generates a new empty one
function frame = eom_lee(frame)

L = length(frame.slots) ;

e_threshold = 0.001 ;
k = 1 ;
b(k) = inf ;
g(k) = 2 ;

% Calculating k = 2
k = k + 1 ;
b(k) = L / (g(k-1)*frame.conflicts + frame.success) ;
g(k) = (1 - exp(-1/b(k))) / (b(k)*(1 - (1 + (1/b(k))) * exp(-1/b(k)))) ;

% Main iteration loop
% ---------------------------------------------------------------------------
while abs(g(k-1) - g(k)) < e_threshold
	k = k + 1 ;
	b(k) = L / (g(k-1)*frame.conflicts + frame.success) ;
	g(k) = (1 - exp(-1/b(k))) / (b(k)*(1 - (1 + (1/b(k))) * exp(-1/b(k)))) ;
end % while abs
% ---------------------------------------------------------------------------

f = ceil(g(k) * frame.conflicts) ;

frame.slots = zeros(1,f) ;

frame.conflicts = 0 ;
frame.success = 0 ;
frame.empty = length(frame) ;