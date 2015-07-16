for i = 1:3
	switch i
		case 1
			address.folder = 'all-random' ;
		case 2
			address.folder = 'first-60-fixed' ;
		case 3
			address.folder = 'last-60-fixed' ;
	end
	for j = 1:10
		address.tagnum = num2str(j*100) ;
		count.total = 0 ;
		count.conflicts = 0 ;
		count.idles = 0 ;
		for k = 1:20
			address.iter = num2str(k) ;

			tags = init(address) ;

			[temp1.total,temp1.conflicts,temp1.idles] = qt('',tags) ;
			count.total = count.total + temp1.total;
			count.conflicts = count.conflicts + temp1.conflicts;
			count.idles = count.idles + temp1.idles;
		end % for k
		total(i,j) = count.total/k ;
		conflicts(i,j) = count.conflicts/k ;
		idles(i,j) = count.idles/k ;
	end % for j
end % for i

% Ploting results
% ----------------------------------------------------------------------------------------------
tags.max = 1000 ;
tags.step = 100 ;
steps = tags.max/tags.step ;
xs = linspace(tags.step,tags.max,steps) ;

figure 
ysa = total(1,:) ;
ysf = total(2,:) ;
ysl = total(3,:) ;
plot(xs,ysa,'-+b',xs,ysf,'-og',xs,ysl,'-sr','markers',12)
grid on
ylabel('Ciclos totais')
xlabel('Numero de Etiquetas')
legend('96 bits variaveis','Primeiros 60 bits fixos','Ultimos 60 bits fixos','Location','NorthWest')

figure 
ysa = conflicts(1,:) ;
ysf = conflicts(2,:) ;
ysl = conflicts(3,:) ;
plot(xs,ysa,'-+b',xs,ysf,'-og',xs,ysl,'-sr','markers',12)
grid on
ylabel('Ciclos de conflito')
xlabel('Numero de Etiquetas')
legend('96 bits variaveis','Primeiros 60 bits fixos','Ultimos 60 bits fixos','Location','NorthWest')

figure 
ysa = idles(1,:) ;
ysf = idles(2,:) ;
ysl = idles(3,:) ;
plot(xs,ysa,'-+b',xs,ysf,'-og',xs,ysl,'-sr','markers',12)
grid on
ylabel('Ciclos ociosos')
xlabel('Numero de Etiquetas')
legend('96 bits variaveis','Primeiros 60 bits fixos','Ultimos 60 bits fixos','Location','NorthWest')