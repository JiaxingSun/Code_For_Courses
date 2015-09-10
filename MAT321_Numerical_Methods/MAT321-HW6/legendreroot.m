%%%%%%%%%%%%%function:Find the integrations nodes%%%%%%%%%%%%
function [wts,roots] = legendreroot(ord)
% Input:
% ord is the order of the legendre polynomial
%
% Return:
% roots is a vector of the roots of legendre polynomial of order 'ord'
% wts is the weights for the integration

Mn = zeros(ord,ord);%initialize the matrix Mn
%modify the elements of Mn
for i = 1:(ord-1)
    j = i-1;
    Mn(i,i+1) = 1/(sqrt((2*j+1)*(2*j+3)/(j+1)^2));
end
for i = 2:ord
    j = i-1;
    Mn(i,i-1) = (sqrt(j^2*(2*j+3)/((j+1)^2*(2*j-1))))/(sqrt((2*j+1)*(2*j+3)/(j+1)^2));
end
[vect,roots] = eig(Mn);%find the eigenvalues of the matrix Mn
roots = diag(roots);
wts = zeros(ord,1);
for i = 1:ord
    normvec = vect(:,i)/sqrt(sum(vect(:,i).^2));
    wts(i) = 2*(normvec(1))^2;
end
