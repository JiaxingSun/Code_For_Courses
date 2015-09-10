clear all
rng(1)
ns = floor(logspace(log10(100), log10(5000), 20));

nb_samples = 4;
for i=1:length(ns)
    n = ns(i)
    A = rand(n,n); b = rand(n,1);
    
    time_ml(i) = Inf;
    time_plu(i) = Inf;
    time_plu(i) = Inf;
    
    for k=1:nb_samples
        [L,U] = lu(A); t=tic;  x_lu = U\(L\b); time_lu(i) = toc(t);

        [L,U,P] = lu(A); t=tic; x_plu = U\(L\(P*b)); time_plu(i) = toc(t);

        % notice how to use the permutation p
        [L,U,p] = lu(A,'vector'); t=tic; x_plu2 = U\(L\(b(p))); time_plu2(i) = toc(t);
    end
end

loglog(ns, time_lu, 'bo')
hold on
loglog(ns, time_plu, 'mo')
loglog(ns, time_plu2, 'ko')
loglog(ns, ns.^2 / ns(end)^2*time_lu(end), 'k-')
legend('LU', 'PLU', 'PLU as vector', 'O(n^2)', 'Location', 'NW')
xlabel('n'); ylabel('time (sec)')