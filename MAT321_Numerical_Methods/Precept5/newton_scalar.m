function [ root,its,errors ] = newton_scalar(f,df,x0,TOL,exact)
% The exact value is optional. If supplied the errors are returned.

MAXIT = 100;
x = x0;
errors = [];

for i=1:MAXIT,
    fx = f(x);
    
    if abs(fx) < TOL
        root = x; its = i; 
        return 
    end
    
    dfx = df(x);
    if abs(dfx) < eps 
        error('Derivative became zero.')
    end
    
    xnew = x - fx/dfx;
    if nargin == 5
        errors = [errors abs(exact - xnew)];
    end
    
    if abs(xnew -x)< TOL
        root =x; its =i; 
        return
    end
    
    x=xnew;        
    
end

its = i;

end

