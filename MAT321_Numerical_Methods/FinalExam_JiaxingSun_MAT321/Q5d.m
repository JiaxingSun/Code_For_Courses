A = rand(15,15);
[Q,R] = householder(A);%householder triagularization
orth_sign = Q*Q';
[Q1,R1] = mgs(A);%modified Gram Schmidt
[Q2,R2] = qr(A);%Matlab built-in 