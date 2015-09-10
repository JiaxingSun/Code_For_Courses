function [ root,its,xs,errors ] = secant(f,x0,x1,TOL,exact)

MAXIT = 20;
p0 = x0;
p1 = x1;

q0 = f(p0);
q1 = f(p1);

errors = [];
xs = [x0];

for i=2:MAXIT,
    
    if abs(q1) < TOL, root = p1; its = i-2; return; end;
    
    r = (q1 - q0)/(p1 - p0);
    if abs(r) < eps, error('slope of secant line became 0'); end
    
    pnew = p1 - q1/r;
    xs =[xs pnew];
    
    if nargin == 5, errors = [errors abs(exact - pnew)]; end
    if abs(pnew - p1)< TOL, root =p1; its =i-1; return; end;
    
    p0 = p1;
    q0 = q1;
    p1 = pnew;
    q1 = f(p1);
    
    
end

warning('The method failed.');
root = p1;
its = i;

end