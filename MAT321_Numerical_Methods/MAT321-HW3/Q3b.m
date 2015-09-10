% 3(b)
    n = 2^3;
    h = 1/n;
    tm = spdiags(ones(n,1)*[-1 2 -1],[-1 0 1],n,n)/(h*h);
    [va,ve,es] = power1(full(tm),rand(n,1),0,6000);
    ratio = (2*n*n-2*n*n*cos((n-1)*pi/(n+1)))/(2*n*n-2*n*n*cos(n*pi/(n+1)));
    bound = ratio.^(linspace(1,6000,6000));
    semilogy(es,'b');
    hold on;
    semilogy(bound,'--b');
    
    n = 2^4;
    h = 1/n;
    tm = spdiags(ones(n,1)*[-1 2 -1],[-1 0 1],n,n)/(h*h);
    [va,ve,es] = power1(full(tm),rand(n,1),0,6000);
    ratio = (2*n*n-2*n*n*cos((n-1)*pi/(n+1)))/(2*n*n-2*n*n*cos(n*pi/(n+1)));
    bound = ratio.^(linspace(1,6000,6000));
    semilogy(es,'r');
    hold on;
    semilogy(bound,'--r');
    
    n = 2^5;
    h = 1/n;
    tm = spdiags(ones(n,1)*[-1 2 -1],[-1 0 1],n,n)/(h*h);
    [va,ve,es] = power1(full(tm),rand(n,1),0,6000);
    ratio = (2*n*n-2*n*n*cos((n-1)*pi/(n+1)))/(2*n*n-2*n*n*cos(n*pi/(n+1)));
    bound = ratio.^(linspace(1,6000,6000));
    semilogy(es,'g');
    hold on;
    semilogy(bound,'--g');
    
    n = 2^6;
    h = 1/n;
    tm = spdiags(ones(n,1)*[-1 2 -1],[-1 0 1],n,n)/(h*h);
    [va,ve,es] = power1(full(tm),rand(n,1),0,6000);
    ratio = (2*n*n-2*n*n*cos((n-1)*pi/(n+1)))/(2*n*n-2*n*n*cos(n*pi/(n+1)));
    bound = ratio.^(linspace(1,6000,6000));
    semilogy(es,'k');
    hold on;
    semilogy(bound,'--k');
    axis([0,6000,1e-20,3])
    hold off;
ylabel('error in the largest eigenvalue');
xlabel('No. of iterates');