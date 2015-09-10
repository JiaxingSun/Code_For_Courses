f = @(x)((5-x).*exp(x)-5);
df = @(x)(-exp(x) + (5-x).*exp(x));

figure(1)
[root,its,errors] = bisect_debug(f, 4, 5, 1e-9,x_exact); 
semilogy(errors,'ko'); hold on

[root_n, its_n, xs, errors_n] = newton(f, df, 8, 1e-12, x_exact); 
semilogy(errors_n,'bo')

[root_s, its_s, xs, errors_s ] = secant(f, 4, 6, 1e-12, x_exact); 
semilogy(errors_s ,'mo')

xlabel('iteration')
ylabel('error')
legend('Bisection', 'Newton', 'Secant')


figure(2)
plot(log(errors(1:end-1)), log(errors(2:end)), 'ko')
hold on
plot(log(errors_n(1:end-2)), log(errors_n(2:end-1)), 'bo')
plot(log(errors_s(1:end-2)), log(errors_s(2:end-1)), 'mo')