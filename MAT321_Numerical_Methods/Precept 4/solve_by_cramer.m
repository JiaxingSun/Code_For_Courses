function x = solve_by_cramer(A,b)
% Solve Ax = b by Cramer's rule.

n = size(A,1);
x = zeros(n,1);
detA = det(A);
for j=1:n
    Aj = A;
    Aj(:,j) = b;
    x(j) = det(Aj)/detA;
end