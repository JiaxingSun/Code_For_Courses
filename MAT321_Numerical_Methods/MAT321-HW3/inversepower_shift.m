function [v e Er] = inversepower_shift (A ,shift, n)
global lambda_exact
% INPUT
% A is the input matrix
% n is the maximum No. of iterates

% OUTPUT
% v is the eigenvector
% e is the eigenvalue
% Er is the error series

Er = [];

m = size (A ,1);
v = ones (m ,1);
[ L U P] = lu(A-shift*eye(m) );
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
Er = [Er,abs((1/el+shift-1))];
disp(i);
end
e = 1/ el +shift;