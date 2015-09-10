function [L,U] = LU_3loops(A)

[n,m] = size(A);
if n~=m
    error('A has to be square')
end

L = eye(n,n);
U = A;

for k=1:n-1
    for j=k+1:n
        L(j,k) = U(j,k)/U(k,k);
        for l=k:n
            U(j,l) = U(j,l) - L(j,k)*U(k,l);
        end
    end
end