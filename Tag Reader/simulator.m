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
% ----------------------------------------------------------------------------------------------


% Ploting results
% ----------------------------------------------------------------------------------------------
xs = linspace(tags.step,tags.max,steps) ;
figure 
ysl = postResult.Lowerbound.slotsUsed ;
yss = postResult.Schoute.slotsUsed ;
yse = postResult.Eom_Lee.slotsUsed ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr','markers',12)
grid on
ylabel('Numero de Slots')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Location','NorthWest')

figure 
ysl = postResult.Lowerbound.slotsEmpty ;
yss = postResult.Schoute.slotsEmpty ;
yse = postResult.Eom_Lee.slotsEmpty ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr','markers',12)
grid on
ylabel('Numero de Slots Vazios')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Location','NorthWest')

figure 
ysl = postResult.Lowerbound.slotsConflict ;
yss = postResult.Schoute.slotsConflict ;
yse = postResult.Eom_Lee.slotsConflict ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr','markers',12)
grid on
ylabel('Numero de Slots em Colisao')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Location','NorthWest')

figure 
ysl = postResult.Lowerbound.MAE ;
yss = postResult.Schoute.MAE ;
yse = postResult.Eom_Lee.MAE ;
plot(xs,ysl,'-+b',xs,yss,'-og',xs,yse,'-sr','markers',12)
grid on
ylabel('Acuracia')
xlabel('Numero de Etiquetas')
legend('Lowerbound','Schoute','Eom Lee','Location','NorthWest')
% ----------------------------------------------------------------------------------------------