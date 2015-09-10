function [root,i,errors] = bisect_debug(f, a, b, tol,exact)
% Function that finds a root of a continuous function f up to tol. It
% returns the root and the number of iterations needed to find that root.
% The user must supply two points a and b such that
% f(a) and f(b) have opposite signs.

errors = [];
i=0;
fa = f(a); fb = f(b);
if fa==0
  root = a; return
end
if fb==0
  root = b; return
end

if sign(fa)==sign(fb)
  error('f(a) and f(b) should have opposite signs')
end

MAXIT = 200;

while abs(b-a) > 2*tol    
    %fprintf('Interval [%0.10g %0.10g]\n',a,b) %uncomment this to print out
    %the interval at each step.
    
  c = 0.5*(a+b); fc = f(c); % Bisect and evaluate midpoint  
  errors = [errors abs(exact -c)];
  i= i+1;
  if fc==0
    root = c; return
  end
  
  if sign(fc)==sign(fb)  % New interval is [a c]
    b = c; fb = fc;
  else
    a = c; fa = fc;      % New interval is [c b]
  end
   
  if i>MAXIT
      disp('Did not converge. Tolerance too high.')
      break
  end
end

root = 0.5*(a+b); % Take midpoint as final approximation