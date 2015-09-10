% The skeleton function of time_LU. Add your code to it...
clear all  % this is a script, so there may be variables floating around in the workspace
rng(1)

ns = [10 20];

for i=1:length(ns)
    n = ns(i)    
    A = 1; b = 2;
    
    % Time solving Ax=b. 
    tic; x_ml = inv(A)*b; time_ml(i) = toc;        
end

loglog(ns, time_ml, 'ro--')
hold on % for other plots
loglog(ns, ns, 'k-')
legend('scalar solve', 'O(n)', 'Location', 'NW')
xlabel('n'); ylabel('time (sec)')
