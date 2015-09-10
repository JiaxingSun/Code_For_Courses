% 3(b)
for i = 3:6
    n = 2^i;
    h = 1/n;
    tm = spdiags(ones(n,1)*[-1 2 -1],[-1 0 1],n,n)/(h*h);
    [va,ve,es] = power1(tm,rand(n,1),0,3000);
    ratio = (2*n*n-2*n*n*cos((n-1)*pi/(n+1)))/(2*n*n-2*n*n*cos(n*pi/(n+1)));
    bound = ratio.^(linspace(1,3000,3000));
    semilogy(es,'b');
    hold on;
    semilogy(bound,'--b');
end
    axis([0,3000,1e-18,3])
    hold off;
ylabel('error in the largest eigenvalue');
xlabel('No. of iterates');