function [L,U] = LU_1loop(A)

[n,m] = size(A);
if n~=m
    error('A has to be square')
end

L = eye(n,n);
U = A;

for k=1:n-1    
    L(k+1:n,k) = U(k+1:n,k)/U(k,k);        
    Mults = repmat(L(k+1:n,k), 1, n-k+1);
    PivotRow = repmat(U(k,k:n), n-k,1);
    U(k+1:n,k:n) = U(k+1:n,k:n) - Mults.*PivotRow;            
end