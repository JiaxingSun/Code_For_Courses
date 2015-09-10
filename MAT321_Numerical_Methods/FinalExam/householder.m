function [Q,R] = householder(A)
% INPUT
% A is the input matrix

% OUTPUT
% Q is the orthonormal matrix 
% R is the upper triangular matrix

[n,m] = size(A);% get the size of input matrix A
if (m < n)
  steps = m;
else
  steps = n-1;
end

R = A;% R is the working matrix
Q = eye(n);% Q is initialized

for j=1:steps
  % Compute the Householder vector v
  x = R(:,j);
  le = length(x);
  v = zeros(le,1);
  v(j:n) = x(j:n);
  v(j) = v(j) + sign(x(j)) * norm(x(j:n));
  if ((v'*v) > 0)
  v = v*sqrt(2/(v'*v));
  end

  % Apply the transformation, that is compute R = H*R.
  R = R - v*(v'*R);

  % Calculate Q, multiply from the right hand side to get the inverse of
  % products of householder reflectors
  Q = Q - (Q*v)*v';
end

