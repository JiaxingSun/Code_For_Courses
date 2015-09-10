%3(c)
for i=3:6
    n = 2^i;
    h = 1/n;
    mi = 100;%maximum number of iterates
    tm = spdiags(ones(n,1)*[-1 2 -1],[-1 0 1],n,n)/(h*h);
    tm = full(tm);
    [va,ve,es] = inversepower(tm,mi);
    ratio = (2*n*n-2*n*n*cos(pi/(n+1)))/(2*n*n-2*n*n*cos(2*pi/(n+1)))/2;
    bound = ratio.^(linspace(1,mi,mi));
    semilogy(es,'b');
    hold on;
    semilogy(bound,'--b');
end
axis([0,100,1e-20,3])
hold off;
ylabel('error in the smallest eigenvalue');
xlabel('No. of iterates');