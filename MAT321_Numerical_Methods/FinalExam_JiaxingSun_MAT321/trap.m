function [inte] = trap(f,a,b,n)
%INPUT
%f is the function to be integrated
%(a,b) is the integration interval
% n is the number of intervals

%OUTPUT
%inte is the integral value
beg = f(linspace(a,b,n));
en = beg(2:n);
beg = beg(1:(n-1));
inte = sum(en+beg)*(b-a)/n*0.5;
