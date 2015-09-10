%%%%%%%%%%%%%%function:Legendre Integration%%%%%%%%%%%%%
function [value] = legendre_integrate(fh,n)
% this is the file for Problem2(d)

% Input
% fh is the function handle
% n is the number of integration nodes

% Output
% value is the integral

value = 0; %initialize value
[wts,nodes] = legendreroot(n);%produce the weights and nodes 
for i = 1:n
    value = wts(i)*fh(nodes(i))+value;
end
