function [ root,its,xs,errors ] = newtonmr(f,df,ddf,x0,TOL,exact)

MAXIT = 50;
x = x0;
errors = [];
xs = [x0];

for i=1:MAXIT,
    fx = f(x);
    if abs(fx) < TOL, root = x; its = i-1; return; end;
    dfx = df(x);
    ddfx = ddf(x);
    numerator = dfx^2 - fx*ddfx;
    if abs(numerator) < eps, error('Numerator should be non-zero.'); end
    xnew = x - (fx * dfx)/numerator;
    
    if nargin == 6, errors = [errors exact - xnew]; end
    if abs(xnew -x)< TOL, root =x; its =i; return; end;
    x=xnew;
    
    xs = [xs x];
end

error('The method failed after');

end
