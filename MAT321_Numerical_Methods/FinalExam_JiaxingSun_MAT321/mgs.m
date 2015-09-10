%%%%%%%%%%%%%%%%%modified Gram Schmidt%%%%%%%%%%%%%%%%%%%%%%%
function [Q,R] = mgs(A)
%INPUT A
%OUTPUT 
% Q is orthogonal
% R is upper triangular
[n,m] = size(A);
Q = zeros(n,m);
R = zeros(m,m);
for i = 1:m
    v = A(:,i);
    for j = 1:i-1
        R(j,i) = Q(:,j)'*A(:,i);
        v = v - R(j,i)*Q(:,j);
    end
    R(i,i) = norm(v);
    Q(:,i) = v/R(i,i);
end
