function [V,D,Er] = jacobi1(A,epsi)
%OUTPUT
%V is the output of eigenvalues
%D is the output of eigenvector in the form of a matrix
%Er is the norm of the off-diagonal elements

%INPUT
%A is a symmetric matix
%epsi is the relative error tolerance

D = A;%initialize D
Er = zeros(1,1);%initialize the norm of off-diagonal elements
[n,n] = size(A);
V = eye(n);%initialize A
status = 1;
count = 0;
Er(1,1) = sum(sum((D-diag(diag(D))).^2)); 
[m1 p] = max(abs(D-diag(diag(D)))); % Select the largest non-diagonal element
[m2 q] = max(m1);                   
p = p(q);  

while (status==1),
  t = D(p,q)/(D(q,q) - D(p,p));
  c = 1/sqrt(t*t+1);
  s = c*t;
  R = [c s; -s c];
  D([p q],:) = R'*D([p q],:);
  D(:,[p q]) = D(:,[p q])*R;
  V(:,[p q]) = V(:,[p q])*R;
  count = count+1;
  Er = [Er,sum(sum((D-diag(diag(D))).^2))];% update the norm of off-diagonal elements
  [m1 p] = max(abs(D-diag(diag(D))));
  [m2 q] = max(m1);
  p = p(q);
  if (abs(D(p,q))<epsi*sqrt(sum(diag(D).^2)/n)), 
      status = 0; 
  end
end
D = diag(diag(D));
