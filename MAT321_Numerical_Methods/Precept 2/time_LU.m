clear all
rng(1)
ns = floor(logspace(log10(100), log10(5000), 20));

nb_samples = 4;
for i=1:length(ns)
    n = ns(i)    
    A = rand(n,n); b = rand(n,1);
    
    % Time solving Ax=b. We take the minimum of nb_samples runs for the 
    % same A and b in order to mitigate random timing fluctuations on a PC.
    time_ml(i) = Inf;
    time_plu(i) = Inf;
    for k=1:nb_samples
        
        tic; x_ml = A \ b; time_ml(i) = min(toc,time_ml(i));    
        
        tic; [L,U,P] = lu(A); time_plu(i) = min(toc,time_plu(i));    
    end        
end

loglog(ns, time_ml, 'ro')
hold on
loglog(ns, time_plu, 'mo')
loglog(ns, ns.^3 / ns(end)^3*time_ml(end), 'k-') % theoretical scaling
legend('mldivide','PLU', 'O(n^3)', 'Location', 'NW')
xlabel('n'); ylabel('time (sec)')
