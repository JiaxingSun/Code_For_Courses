% Solution to problem 3e
f = @(x)((5-x).*exp(x)-5);
df = @(x)(-exp(x) + (5-x).*exp(x));
x_exact_1 = fzero(f,1)
x_exact_2 = fzero(f,4)

figure(1)
x0s = [-3 -2 0.5 3 4.7 9 15];
for ii=1:length(x0s)
    [root, its, xs] = newton(f, df, x0s(ii), 1e-14);
    its
    
    plot(xs,'o-')
    hold on
    
end


xlabel('iteration')
ylabel('approximation of root')
