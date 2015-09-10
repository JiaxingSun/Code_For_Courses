function [v e Er] = inversepower (A , n)
% INPUT
% A is the input matrix
% n is the maximum No. of iterates

% OUTPUT
% v is the eigenvector
% e is the eigenvalue
% Er is the error series

[c d] = eig(A);
tv = min(diag(abs(d)));
Er = zeros(1,1);
[ L U P] = lu(A ); 
m = size (A ,1);
v = ones (m ,1); 
for i = 1: n
pv = P*v ;
y = L \ pv ;
v = U \y;
M = max ( v );
m = min ( v );
if abs (M) >= abs (m)
el = M;
else
el = m;
end
v = v / el ;
Er = [Er, abs(tv-1/el)/tv];
end
e = 1/ el ;