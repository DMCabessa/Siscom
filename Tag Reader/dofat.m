function out = dofat(a,b,c,d)
result = 1 ;
while a > 1
	result = result*a ;
	a = a - 1 ;
	if b > 1
		result = result/b ;
		b = b - 1 ;
	end % of b
	if c > 1
		result = result/c ;
		c = c - 1 ;
	end % of c
	if d > 1
		result = result/d ;
		d = d - 1 ;
	end % if d
end % whie a

out = result ;