function  [tva,tve,Er] = power1(A,X,epsi,it)

% INPUT
% A is the matrix
% X is the initial vector
% epsi is the tolerance
% it is the maximum number of iterations

%OUTPUT
% tva is the dominant eigenvalue
% tve is the dominant eigenvector

[ve,va] = eig(A);
largest = max(max(va));% calculate the largest eigenvalue through built-in function;

tva = 0;
count = 0;
err = 1;
state = 1;
Er = zeros(1,1);
while ((count<=it) & (state==1))
  Y = A*X;               
  [m j] = max(abs(Y));
  c1 = Y(j);             % find the largest element in order to normalize Y
  dc = abs(tva-c1);
   Y = (1/c1)*Y;         
  dv = norm(X-Y);
  err = max(dc,dv);
  X = Y;                 % update eigenvector
  tva = c1;           % update eigenvalue
  state = 0;
  Er = [Er,abs((largest-tva)/largest)];
  if (err>epsi),      % check for convergence
    state = 1;
  end
  count = count+1;
end
tve = X;
