% Recieves a filled frame and generates a new empty one
function frame = chen(frame)

E = frame.empty ;
S = frame.success ;
C = frame.conflicts ;

L = C + E + S ;
n = S + 2*C ;
next = 0 ;
previous = -1 ;
while previous < next
	pe = (1-(1/L))^n ;
	ps = (n/L)*(1-(1/L))^(n-1) ;
	pc = 1 - pe - ps ;
	previous = next ;
	next = dofat(L,E,S,C)*(pe^E)*(ps^S)*(pc^C) ;
	n = n + 1 ;
end % while previous

frame.slots = zeros(1,n-2-S) ;

frame.conflicts = 0 ;
frame.success = 0 ;
frame.empty = length(frame) ;