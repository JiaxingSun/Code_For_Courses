function [ Q,R ] = clgs( A )
Q = zeros(size(A));


n = size(A,2);
R = zeros(n);

for j=1:n
    vj = A(:,j);
    
    for i=1:j-1
        R(i,j) = Q(:,i)'*A(:,j);
        vj = vj - R(i,j)*Q(:,i);
    end
    
    R(j,j) = norm(vj);
    Q(:,j) = vj/R(j,j);

end

