iter = 2000 ;
tags.max = 1000 ;
tags.step = 100 ;
steps = tags.max/tags.step ;

results = init(iter, steps) ;
postResult = [] ;

% Main function
% ----------------------------------------------------------------------------------------------
[results,postResult] = scanner('LOWERBOUND',iter,tags,steps,results, postResult) ;

[results,postResult] = scanner('SCHOUTE',iter,tags,steps,results, postResult) ;

[results,postResult] = scanner('EOM-LEE',iter,tags,steps,results, postResult) ;

[results,postResult] = scanner('CHEN',iter,tags,steps,results, postResult) ;
% ----------------------------------------------------------------------------------------------


% Ploting results
% ----------------------------------------------------------------------------------------------
xs = linspace(tags.step,tags.max,steps) ;
figure 
ysl = postResult.Lowerbound.slotsUsed ;
yss = postResult.Schoute.slotsUsed ;
yse = postResult.Eom_Lee.slotsUsed ;
ysc = postResult.Chen.slotsUsed ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr',xs,ysc,'-^m','markers',12)
grid on
ylabel('Numero de Slots')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Chen','Location','NorthWest')

figure 
ysl = postResult.Lowerbound.slotsEmpty ;
yss = postResult.Schoute.slotsEmpty ;
yse = postResult.Eom_Lee.slotsEmpty ;
ysc = postResult.Chen.slotsEmpty ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr',xs,ysc,'-^m','markers',12)
grid on
ylabel('Numero de Slots Vazios')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Chen','Location','NorthWest')

figure 
ysl = postResult.Lowerbound.slotsConflict ;
yss = postResult.Schoute.slotsConflict ;
yse = postResult.Eom_Lee.slotsConflict ;
ysc = postResult.Chen.slotsConflict ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr',xs,ysc,'-^m','markers',12)
grid on
ylabel('Numero de Slots em Colisao')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Chen','Location','NorthWest')

figure 
ysl = postResult.Lowerbound.MAE ;
yss = postResult.Schoute.MAE ;
yse = postResult.Eom_Lee.MAE ;
ysc = postResult.Chen.MAE ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr',xs,ysc,'-^m','markers',12)
grid on
ylabel('Erro Abs. Medio')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Chen','Location','NorthWest')
% ----------------------------------------------------------------------------------------------