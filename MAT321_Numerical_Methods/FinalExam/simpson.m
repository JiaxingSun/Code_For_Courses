function [inte] = simpson(f,a,b,n)
%INPUT
%f is the function to be integrated
%(a,b) is the integration interval
% n is the number of intervals

%OUTPUT
%inte is the integral value
beg = f(linspace(a,b,n));
en = beg(2:n);
mid = f(linspace(a+(b-a)/(n*2),b-(b-a)/(n*2),n-1));
beg = beg(1:(n-1));
inte = sum(en+beg)/6*(b-a)/n+sum(mid)*(b-a)/n*2/3;
