function [q,m,err,E] = myromberg(fun_f,a,b,tol,max_power)
% 2^max_power is the maximum number of intervals that are allowed be used
%
% Return:
% q is the approximate integral
% m is the number of intervals used
% err is an estimate of the error.
% E is the Romber matrix

h  = b - a;%subinterval size
m  = 1;%No. of intervals used
err = 1;%initialize the error
mi  = 1;
j = 0;%No. of extrapolations done
E = zeros(2,2);% initialize the Romberg matrix
E(1,1) = h*(feval(fun_f,a) + feval(fun_f,b))/2;

while ((err>tol)&(j<max_power))|(j<2) %at least do 2 layers of extrapolations
  h = h/2;
  m = m*2;
  j = j+1;
  s = 0;
  
  %%%%Using the recursive formula to calculate the trapezoid rule value
  for p = 1:mi;
    x = a + h*(2*p-1);%locate the collocation points
    s = s + feval(fun_f,x);%sum of the function value at the collocation points
  end
  E(j+1,1) = E(j,1)/2 + h*s;%updat the 1st element of the (j+1)th row
  mi = 2*mi;
  
  %%%%%update the Romberg matrix
  for qu=1:j
      k=j+1-qu;
      E(k,j+2-k) = E(k+1,j+1-k) + (E(k+1,j+1-k)-E(k,j+1-k))/(4^(j+1-k)-1);
  end
  err = max([abs(E(1,j)-E(1,j+1));abs(E(2,j)-E(1,j+1))]);%estimate the error
end
q = E(1,j+1);%produce the approximate integral