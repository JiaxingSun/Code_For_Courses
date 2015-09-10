function [ root,its,xs,errors ] = newton(f,df,x0,TOL,exact)

MAXIT = 20;
x = x0;
xs = [x0];
errors = [];

for i=1:MAXIT,
    fx = f(x);
    
    % extra stopping condition
    if abs(fx) < TOL
        root = x; its = i-1; 
        return
    end;
    
    dfx = df(x);
    % extra sanity check
    if abs(dfx) < eps
        error('Derivative became zero.'); 
    end
    
    % caclulate new approximation
    xnew = x - fx/dfx;
    if nargin == 5
        errors = [errors abs(exact - xnew)]; 
    end
    xs = [xs xnew];
    
    % stopping condition
    if abs(xnew -x)< TOL
        root = x; its =i; 
        return
    end;
    
    x=xnew;        
    
end

warning('The method failed.');
root = x;
its = i;

end

