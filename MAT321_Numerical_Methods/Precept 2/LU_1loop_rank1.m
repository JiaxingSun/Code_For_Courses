function [L,U] = LU_1loop_rank1(A)

[n,m] = size(A);
if n~=m
    error('A has to be square')
end

L = eye(n,n);
U = A;

for k=1:n-1    
    L(k+1:n,k) = U(k+1:n,k)/U(k,k);            
    U(k+1:n,k:n) = U(k+1:n,k:n) - L(k+1:n,k)*U(k,k:n);
end