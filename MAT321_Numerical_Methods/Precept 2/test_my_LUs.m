A = rand(4,4); b = rand(4,1);

% 3 loops
[L,U] = LU_3loops(A)
% is LU = A?
norm(L*U - A)
% is U upper triangular?
norm(tril(U,-1))
% is L lower triangular with unit diagonal?
norm(triu(L) - eye(size(A)))

% 2 loops
[L,U] = LU_2loops(A)
% is LU = A?
norm(L*U - A)
% is U upper triangular?
norm(tril(U,-1))
% is L lower triangular with unit diagonal?
norm(triu(L) - eye(size(A)))


% 1 loop
[L,U] = LU_1loop(A)
% is LU = A?
norm(L*U - A)
% is U upper triangular?
norm(tril(U,-1))
% is L lower triangular with unit diagonal?
norm(triu(L) - eye(size(A)))

% 1 loop with rank 1 trick
[L,U] = LU_1loop_rank1(A)
% is LU = A?
norm(L*U - A)
% is U upper triangular?
norm(tril(U,-1))
% is L lower triangular with unit diagonal?
norm(triu(L) - eye(size(A)))